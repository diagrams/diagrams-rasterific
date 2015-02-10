{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific
-- Copyright   :  (c) 2014 diagrams-rasterific team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams using Rasterific,
-- implemented natively in Haskell (making it easy to use on any
-- platform). Can create png, tif, bmp, jpg, and animated GIFs.
--
-- To invoke the Rasterific backend, you have three options.
--
-- * You can use the "Diagrams.Backend.Rasterific.CmdLine" module to create
--   standalone executables which output images when invoked.
--
-- * You can use the 'renderRasterific' function provided by this module,
--   which gives you more flexible programmatic control over when and
--   how images are output (making it easy to, for example, write a
--   single program that outputs multiple images, or one that outputs
--   images dynamically based on user input, and so on).
--
-- * For the most flexibility (/e.g./ if you want access to the
--   resulting Rasterific value directly in memory without writing it to
--   disk), you can manually invoke the 'renderDia' method from the
--   'Diagrams.Core.Types.Backend' instance for @Rasterific@.  In particular,
--   'Diagrams.Core.Types.renderDia' has the generic type
--
-- > renderDia :: b -> Options b v n -> QDiagram b v n m -> Result b v n
--
-- (omitting a few type class constraints).  @b@ represents the
-- backend type, @v@ the vector space, @n@ the numeric field, and @m@ the type
-- of monoidal query annotations on the diagram.  'Options' and 'Result' are
-- associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend.  For @b ~ Rasterific@, @v ~ V2@, and @n ~ Float@, we have
--
-- > data Options Rasterific V2 Float = RasterificOptions
-- >          { _size      :: SizeSpec2D Float -- ^ The requested size of the output
-- >          }
--
-- @
-- data family Render Rasterific V2 Float = 'R (RenderM ())'
-- @
--
-- @
-- type family Result Rasterific V2 Float = 'Image PixelRGBA8'
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: Rasterific -> Options Rasterific V2 Float -> QDiagram Rasterific V2 Float m -> 'Image PixelRGBA8'
-- @
--
-- which you could call like @renderDia Rasterific (RasterificOptions (Width 250))
-- myDiagram@.
--
-------------------------------------------------------------------------------
module Diagrams.Backend.Rasterific
  ( Rasterific(..)
  , B -- rendering token
  , Options(..)

  , renderRasterific
  , size

  , writeJpeg

  ) where

import           Diagrams.Core.Compile
import           Diagrams.Core.Transform     (matrixHomRep)


import           Diagrams.Prelude            hiding (opacity, view)
import           Diagrams.TwoD.Adjust        (adjustDia2D)
import           Diagrams.TwoD.Attributes    (splitTextureFills)
import           Diagrams.TwoD.Path          (Clip (Clip), getFillRule)
import           Diagrams.TwoD.Text          hiding (Font)

import           Codec.Picture
import           Codec.Picture.Types         (dropTransparency, convertPixel
                                             ,promoteImage, convertImage)

import qualified Graphics.Rasterific         as R
import           Graphics.Rasterific.Texture (uniformTexture, Gradient
                                             ,linearGradientTexture ,withSampler
                                             ,radialGradientWithFocusTexture
                                             ,transformTexture)

import qualified Graphics.Rasterific.Transformations as R

import           Graphics.Text.TrueType


import           Control.Lens                hiding (transform, ( # ))
import           Control.Monad               (when)
import           Control.Monad.StateStack
import           Control.Monad.Trans         (lift)


import qualified Data.ByteString.Lazy as L   (writeFile)
import           Data.Default.Class
import qualified Data.Foldable               as F
import           Data.Hashable               (Hashable(..))
import           Data.Maybe                  (fromMaybe, isJust, fromJust)
import           Data.Tree
import           Data.Typeable
import           Data.Word                   (Word8)

import           System.FilePath             (takeExtension)
import           System.IO.Unsafe            (unsafePerformIO)
import           Paths_diagrams_rasterific   (getDataFileName)

--------------------------------------------------------------------------------
-- | This data declaration is simply used as a token to distinguish
--   the Rasterific backend: (1) when calling functions where the type
--   inference engine would otherwise have no way to know which
--   backend you wanted to use, and (2) as an argument to the
--   'Backend' and 'Renderable' type classes.
data Rasterific = Rasterific
  deriving (Eq,Ord,Read,Show,Typeable)

type B = Rasterific

type instance V Rasterific = V2
type instance N Rasterific = Float

data RasterificState =
  RasterificState { _accumStyle :: Style V2 Float
                    -- ^ The current accumulated style.
                  }

makeLenses ''RasterificState

instance Default RasterificState where
  def = RasterificState
        { _accumStyle       = mempty
        }

-- | The custom monad in which intermediate drawing options take
--   place; 'Graphics.Rasterific.Drawing' is Rasterific's own rendering
--   monad.
type RenderM a = StateStackT RasterificState RenderR a

type RenderR = R.Drawing PixelRGBA8

liftR :: RenderR a -> RenderM a
liftR = lift

runRenderM :: RenderM a -> RenderR a
runRenderM = flip evalStateStackT def

-- From Diagrams.Core.Types.
instance Backend Rasterific V2 Float where
  data Render  Rasterific V2 Float = R (RenderM ())
  type Result  Rasterific V2 Float = Image PixelRGBA8
  data Options Rasterific V2 Float = RasterificOptions
          { _sizeSpec  :: SizeSpec V2 Float -- ^ The requested size of the output
          }
    deriving Show

  renderRTree _ opts t =
    R.renderDrawing (round w) (round h) bgColor r
    where
      r       = runRenderM . runR . toRender $ t
      V2 w h  = specToSize 100 (opts^.sizeSpec)
      bgColor = PixelRGBA8 0 0 0 0

  adjustDia c opts d = adjustDia2D sizeSpec c opts (d # reflectY)

toRender :: RTree Rasterific V2 Float a -> Render Rasterific V2 Float
toRender = fromRTree
  . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
  . (:[])
  . splitTextureFills
    where
      fromRTree (Node (RPrim p) _) = render Rasterific p
      fromRTree (Node (RStyle sty) rs) = R $ do
        save
        accumStyle %= (<> sty)
        aStyle <- use accumStyle
        let R r = F.foldMap fromRTree rs
            m = evalStateStackT r (RasterificState aStyle)
        clip sty m
        restore
      fromRTree (Node _ rs) = F.foldMap fromRTree rs

runR :: Render Rasterific V2 Float -> RenderM ()
runR (R r) = r

instance Monoid (Render Rasterific V2 Float) where
  mempty = R $ return ()
  (R rd1) `mappend` (R rd2) = R (rd1 >> rd2)

instance Hashable (Options Rasterific V2 Float) where
  hashWithSalt s (RasterificOptions sz) = s `hashWithSalt` sz

sizeSpec :: Lens' (Options Rasterific V2 Float) (SizeSpec V2 Float)
sizeSpec = lens (\(RasterificOptions {_sizeSpec = s}) -> s)
                     (\o s -> o {_sizeSpec = s})

rasterificStrokeStyle :: Style v n
                     -> (Float, R.Join, (R.Cap, R.Cap), Maybe (R.DashPattern, Float))
rasterificStrokeStyle s = (strokeWidth, strokeJoin, strokeCaps, strokeDash)
  where
    strokeWidth = case getLineWidth <$> getAttr s of
                      Just o -> o
                      _    ->  1
    strokeJoin = fromMaybe (R.JoinMiter 0) (fromLineJoin . getLineJoin <$> getAttr s)
    strokeCaps = (strokeCap, strokeCap)
    strokeCap  = fromMaybe (R.CapStraight 0) (fromLineCap . getLineCap <$> getAttr s)
    strokeDash = fromDashing . getDashing <$> getAttr s

fromLineCap :: LineCap -> R.Cap
fromLineCap LineCapButt   = R.CapStraight 0
fromLineCap LineCapRound  = R.CapRound
fromLineCap LineCapSquare = R.CapStraight 1

fromLineJoin :: LineJoin -> R.Join
fromLineJoin LineJoinMiter = R.JoinMiter 0
fromLineJoin LineJoinRound = R.JoinRound
fromLineJoin LineJoinBevel = R.JoinMiter 1

fromDashing :: Dashing Float -> (R.DashPattern, Float)
fromDashing (Dashing ds d) = (ds, d)

fromFillRule :: FillRule -> R.FillMethod
fromFillRule EvenOdd = R.FillEvenOdd
fromFillRule _ = R.FillWinding

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: (AttributeClass a) => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

rasterificColor :: SomeColor -> Double -> PixelRGBA8
rasterificColor c o = PixelRGBA8 r g b a
  where
    (r, g, b, a) = (int r', int g', int b', int (o * a'))
    (r', g', b', a') = colorToSRGBA (toAlphaColour c)
    int x = round (255 * x)

rasterificSpreadMethod :: SpreadMethod -> R.SamplerRepeat
rasterificSpreadMethod GradPad      = R.SamplerPad
rasterificSpreadMethod GradReflect  = R.SamplerReflect
rasterificSpreadMethod GradRepeat   = R.SamplerRepeat

rasterificStops :: [GradientStop Float] -> Gradient PixelRGBA8
rasterificStops = map fromStop
  where
    fromStop (GradientStop c v) = (v, rasterificColor c 1)
      -- (double2Float v, rasterificColor c 1)

rasterificLinearGradient :: LGradient Float -> R.Texture PixelRGBA8
rasterificLinearGradient g = transformTexture tr tx
  where
    tr = rasterificMatTransf (inv $ g^.lGradTrans)
    tx = withSampler spreadMethod (linearGradientTexture gradDef p0 p1)
    spreadMethod = rasterificSpreadMethod (g^.lGradSpreadMethod)
    gradDef = rasterificStops (g^.lGradStops)
    p0 = p2v2 (g^.lGradStart)
    p1 = p2v2 (g^.lGradEnd)


rasterificRadialGradient :: RGradient Float -> R.Texture PixelRGBA8
rasterificRadialGradient g = transformTexture tr tx
  where
    tr = rasterificMatTransf (inv $ g^.rGradTrans)
    tx = withSampler spreadMethod (radialGradientWithFocusTexture gradDef c r1 f)
    spreadMethod = rasterificSpreadMethod (g^.rGradSpreadMethod)
    c = p2v2 (g^.rGradCenter1)
    f = p2v2 (g^.rGradCenter0)
    gradDef = rasterificStops ss

    -- Adjust the stops so that the gradient begins at the perimeter of
    -- the inner circle (center0, radius0) and ends at the outer circle.
    r0 = g^.rGradRadius0
    r1 = g^.rGradRadius1
    stopFracs = r0 / r1 : map (\s -> (r0 + (s^.stopFraction) * (r1 - r0)) / r1)
                (g^.rGradStops)
    gradStops = case g^.rGradStops of
      []       -> []
      xs@(x:_) -> x : xs
    ss = zipWith (\gs sf -> gs & stopFraction .~ sf ) gradStops stopFracs

-- Convert a diagrams @Texture@ and opacity to a rasterific texture.
rasterificTexture :: Texture Float -> Double -> R.Texture PixelRGBA8
rasterificTexture (SC c) o = uniformTexture $ rasterificColor c o
rasterificTexture (LG g) _ = rasterificLinearGradient g
rasterificTexture (RG g) _ = rasterificRadialGradient g

p2v2 :: P2 Float -> R.Point
p2v2 (P (V2 x y)) = R.V2 x y
{-# INLINE p2v2 #-}

r2v2 :: V2 Float -> R.Point
r2v2 (V2 x y) = R.V2 x y
{-# INLINE r2v2 #-}

rasterificPtTransf :: T2 Float -> R.Point -> R.Point
rasterificPtTransf tr p = p2v2 $ transform tr p'
  where
    p' = mkP2 x y
    R.V2 x y = p

rasterificMatTransf :: T2 Float -> R.Transformation
rasterificMatTransf tr = R.Transformation a c e b d f
  where
    [[a, b], [c, d], [e, f]] = matrixHomRep tr

-- Note: Using view patterns confuses ghc to think there are missing patterns,
-- so we avoid them here.
renderSeg :: Located (Segment Closed V2 Float) -> R.Primitive
renderSeg l =
  case viewLoc l of
    (p, Linear (OffsetClosed v)) ->
      R.LinePrim $ R.Line p' (p' + r2v2 v)
      where
        p' = p2v2 p
    (p, Cubic u1 u2 (OffsetClosed u3)) ->
      R.CubicBezierPrim $ R.CubicBezier q0 q1 q2 q3
      where
        (q0, q1, q2, q3) = (p2v2 p, q0 + r2v2 u1, q0 + r2v2 u2, q0 + r2v2 u3)

renderPath :: Path V2 Float -> [[R.Primitive]]
renderPath p = (map . map) renderSeg (pathLocSegments p)

clip :: Style V2 Float -> RenderR () -> RenderM ()
clip sty d =
  maybe (liftR d)
        (\paths -> liftR $ R.withClipping
                  (R.fill (concat . concat $ map renderPath paths)) d)
        (op Clip <$> getAttr sty)

-- Stroke both dashed and solid lines.
mkStroke :: Float ->  R.Join -> (R.Cap, R.Cap) ->  Maybe (R.DashPattern, Float)
      -> [[R.Primitive]] -> RenderR ()
mkStroke l j c d  primList =
  maybe (mapM_ (R.stroke l j c) primList)
        (\(dsh, off) -> mapM_ (R.dashedStrokeWithOffset off dsh l j c) primList)
        d

instance Renderable (Path V2 Float) Rasterific where
  render _ p = R $ do
    f <- getStyleAttrib getFillTexture
    s <- fromMaybe (SC (SomeColor (black :: Colour Double))) 
         <$> getStyleAttrib getLineTexture
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    r <- fromMaybe Winding <$> getStyleAttrib getFillRule
    sty <- use accumStyle

    let (l, j, c, d) = rasterificStrokeStyle sty
        rule = fromFillRule r

        -- For stroking we need to keep all of the contours separate.
        primList = renderPath p

        -- For filling we need to concatenate them into a flat list.
        prms = concat primList

    when (isJust f) $ liftR (R.withTexture (rasterificTexture (fromJust f) o)
                    $ R.fillWithMethod rule prms)
    liftR (R.withTexture (rasterificTexture s o) $ mkStroke l j c d primList)

-- read only of static data (safe)
ro :: FilePath -> FilePath
ro = unsafePerformIO . getDataFileName

openSansRegular :: Font
openSansRegular = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ ro "fonts/OpenSans-Regular.ttf"

openSansBold :: Font
openSansBold = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ ro "fonts/OpenSans-Bold.ttf"

openSansItalic :: Font
openSansItalic = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ ro "fonts/OpenSans-Italic.ttf"

openSansBoldItalic :: Font
openSansBoldItalic = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ ro "fonts/OpenSans-BoldItalic.ttf"

fromFontStyle :: FontSlant -> FontWeight -> Font
fromFontStyle FontSlantItalic FontWeightBold = openSansBoldItalic
fromFontStyle FontSlantOblique FontWeightBold = openSansBoldItalic
fromFontStyle FontSlantNormal FontWeightBold = openSansBold
fromFontStyle FontSlantItalic FontWeightNormal = openSansItalic
fromFontStyle FontSlantOblique FontWeightNormal = openSansItalic
fromFontStyle _ _ = openSansRegular

textBox :: Font -> R.PointSize -> String -> (Float, Float)
textBox f p s = (_xMax bb - _xMin bb, _yMax bb - _yMin bb)
  where
    bb = stringBoundingBox f 96 p s

instance Renderable (Text Float) Rasterific where
  render _ (Text tr al str) = R $ do
    fs      <- fromMaybe 12 <$> getStyleAttrib (getFontSize :: FontSize Float -> Float)
    slant   <- fromMaybe FontSlantNormal <$> getStyleAttrib getFontSlant
    fw      <- fromMaybe FontWeightNormal <$> getStyleAttrib getFontWeight
    f       <- fromMaybe (SC (SomeColor (black :: Colour Double))) 
               <$> getStyleAttrib getFillTexture
    o       <- fromMaybe 1 <$> getStyleAttrib getOpacity
    let fColor = rasterificTexture f o
        fs' = R.PointSize fs
        fnt = fromFontStyle slant fw
        (x, y) = textBox fnt fs' str
        (refX, refY) = case al of
          BaselineText -> (0, y)
          BoxAlignedText xt yt -> (x * xt, (1 - yt) * y)
        p = rasterificPtTransf (moveOriginBy (r2 (refX, refY)) mempty) (R.V2 0 0)
    liftR (R.withTransformation (rasterificMatTransf (tr <> reflectionY))
          (R.withTexture fColor $ R.printTextAt fnt fs' p str))

toImageRGBA8 :: DynamicImage -> Image PixelRGBA8
toImageRGBA8 (ImageRGBA8 i)  = i
toImageRGBA8 (ImageRGB8 i)   = promoteImage i
toImageRGBA8 (ImageYCbCr8 i) = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 (ImageY8 i)     = promoteImage i
toImageRGBA8 (ImageYA8 i)    = promoteImage i
toImageRGBA8 (ImageCMYK8 i)  = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 _               = error "Unsupported Pixel type"

instance Renderable (DImage Float Embedded) Rasterific where
  render _ (DImage iD w h tr) = R $ liftR
                               (R.withTransformation
                               (rasterificMatTransf (tr <> reflectionY))
                               (R.drawImage img 0 p))
    where
      ImageRaster dImg = iD
      img = toImageRGBA8 dImg
      trl = moveOriginBy (r2 (fromIntegral w / 2, fromIntegral h / 2)) mempty 
      p   = rasterificPtTransf trl (R.V2 0 0)

writeJpeg :: Word8 -> FilePath -> Result Rasterific V2 Float -> IO ()
writeJpeg quality outFile img = L.writeFile outFile bs
  where
    bs = encodeJpegAtQuality quality (pixelMap (convertPixel . dropTransparency) img)

renderRasterific :: FilePath -> SizeSpec V2 Float -> Word8 -> Diagram Rasterific -> IO ()		
renderRasterific outFile spec quality d = writer outFile img		
  where
    writer = case takeExtension outFile of
              ".png" -> writePng
              ".tif" -> writeTiff
              ".bmp" -> writeBitmap
              ".jpg" -> writeJpeg q
              _      -> writePng
    img = renderDia Rasterific (RasterificOptions spec) d
    q = min quality 100

