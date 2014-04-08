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
-- > renderDia :: b -> Options b v -> QDiagram b v m -> Result b v
--
-- (omitting a few type class constraints).  @b@ represents the
-- backend type, @v@ the vector space, and @m@ the type of monoidal
-- query annotations on the diagram.  'Options' and 'Result' are
-- associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend.  For @b ~ Rasterific@ and @v ~ R2@, we have
--
-- > data Options Rasterific R2 = RasterificOptions
-- >          { _rasterificSizeSpec      :: SizeSpec2D -- ^ The requested size of the output
-- >          , _rasterificBypassAdjust  :: Bool       -- ^ Should the 'adjustDia' step be bypassed during rendering?
-- >          }
--
-- @
-- data family Render Rasterific R2 = 'R (RenderM ())'
-- @
--
-- @
-- type family Result Rasterific R2 = 'Image PixelRGBA8'
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: Rasterific -> Options Rasterific R2 -> QDiagram Rasterific R2 m -> 'Image PixelRGBA8'
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
import           Diagrams.Core.Transform

import           Diagrams.Prelude            hiding (Image, opacity, view)
import           Diagrams.TwoD.Adjust        (adjustDia2D)
import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Path          (Clip (Clip), getFillRule, isInsideEvenOdd)
import           Diagrams.TwoD.Size          (sizePair)
import           Diagrams.TwoD.Text          hiding (Font)

import           Codec.Picture
import           Codec.Picture.Types         (dropTransparency, convertPixel)

import           GHC.Float                   (double2Float, float2Double)

import qualified Graphics.Rasterific         as R
import           Graphics.Rasterific.Texture (uniformTexture)
import           Graphics.Text.TrueType      (loadFontFile, Font, stringBoundingBox)

import           Control.Lens                hiding (transform, ( # ))
import           Control.Monad               (when)
import           Control.Monad.StateStack
import           Control.Monad.Trans         (lift)
import           Control.Arrow               ((&&&))


import qualified Data.ByteString.Lazy as L   (writeFile)
import           Data.ByteString             (ByteString)
import           Data.Default.Class
import qualified Data.Foldable               as F
import           Data.Maybe                  (fromMaybe, isJust)
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

data RasterificState =
  RasterificState { _accumStyle :: Style R2
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
instance Backend Rasterific R2 where
  data Render  Rasterific R2 = R (RenderM ())
  type Result  Rasterific R2 = Image PixelRGBA8
  data Options Rasterific R2 = RasterificOptions
          { _size      :: SizeSpec2D -- ^ The requested size of the output
          }
    deriving (Show)

  renderRTree _ opts t =
    R.renderDrawing (round w) (round h) bgColor r
    where
      r = runRenderM . runR . toRender $ t
      (w,h) = sizePair (opts^.size)
      bgColor = PixelRGBA8 255 255 255 0

  adjustDia c opts d = adjustDia2D size c opts (d # reflectY # fontSizeO 12)

toRender :: RTree Rasterific R2 a -> Render Rasterific R2
toRender = fromRTree
  . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
  . (:[])
  . splitFills
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

runR :: Render Rasterific R2 -> RenderM ()
runR (R r) = r

instance Monoid (Render Rasterific R2) where
  mempty  = R $ return ()
  (R rd1) `mappend` (R rd2) = R (rd1 >> rd2)

size :: Lens' (Options Rasterific R2) SizeSpec2D
size = lens (\(RasterificOptions {_size = s}) -> s)
                     (\o s -> o {_size = s})

rasterificStrokeStyle :: Style v
                     -> (Float, R.Join, (R.Cap, R.Cap), Maybe (R.DashPattern, Float))
rasterificStrokeStyle s = (strokeWidth, strokeJoin, strokeCaps, strokeDash)
  where
    strokeWidth = double2Float $ case getLineWidth <$> getAttr s of
                      Just o ->  fromOutput o
                      _               ->  1
    strokeJoin = fromMaybe (R.JoinMiter 0) (fromLineJoin . getLineJoin <$> getAttr s)
    strokeCaps = (strokeCap, strokeCap)
    strokeCap = fromMaybe (R.CapStraight 0) (fromLineCap . getLineCap <$> getAttr s)
    strokeDash = fromDashing . getDashing <$> getAttr s

fromLineCap :: LineCap -> R.Cap
fromLineCap LineCapButt   = R.CapStraight 0
fromLineCap LineCapRound  = R.CapRound
fromLineCap LineCapSquare = R.CapStraight 1

fromLineJoin :: LineJoin -> R.Join
fromLineJoin LineJoinMiter = R.JoinMiter 0
fromLineJoin LineJoinRound = R.JoinRound
fromLineJoin LineJoinBevel = R.JoinMiter 1

fromDashing :: Dashing -> (R.DashPattern, Float)
fromDashing (Dashing ds d) = (map double2Float ds', double2Float d')
  where
    ds' = map fromOutput ds
    d' = fromOutput d

fromFillRule :: FillRule -> R.FillMethod
fromFillRule EvenOdd = R.FillEvenOdd
fromFillRule _ = R.FillWinding

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

sourceColor :: Maybe (AlphaColour Double) -> Double -> PixelRGBA8
sourceColor Nothing  _ = PixelRGBA8 0 0 0 0
sourceColor (Just c) o = PixelRGBA8 r g b a
  where
    (r, g, b, a) = (int r', int g', int b', int (o * a'))
    (r',g',b', a') = colorToSRGBA c
    int x = round (255 * x)

v2 :: Double -> Double -> R.Point
v2 x y = R.V2 x' y'
  where
    (x', y') = (double2Float x, double2Float y)

p2v2 :: P2 -> R.Point
p2v2 p = uncurry v2 $ unp2 p

r2v2 :: R2 -> R.Point
r2v2 r = uncurry v2 $ unr2 r

rasterificTransf :: T2 -> R.Point -> R.Point
rasterificTransf tr p =  p2v2 $ transform tr p'
  where
    p' = mkP2 (float2Double x) (float2Double y)
    R.V2 x y = p

-- Note: Using view patterns confuses ghc to think there are missing patterns,
-- so we avoid them here.
renderSeg :: Located (Segment Closed R2) -> R.Primitive
renderSeg l =
  case viewLoc l of
    (p, (Linear (OffsetClosed v))) ->
      R.LinePrim $ R.Line p' (p' + r2v2 v)
      where
        p' = p2v2 p
    (p, (Cubic u1 u2 (OffsetClosed u3))) ->
      R.CubicBezierPrim $ R.CubicBezier q0 q1 q2 q3
      where
        (q0, q1, q2, q3) = (p2v2 p, q0 + r2v2 u1, q0 + r2v2 u2, q0 + r2v2 u3)

renderPath :: Path R2 -> [[R.Primitive]]
renderPath p = (map . map) renderSeg (pathLocSegments p)

clip :: Style v -> RenderR () -> RenderM ()
clip sty d =
  maybe (liftR d)
        (\paths -> liftR $ R.withClipping
                  (R.fill (concat . concat $ (map renderPath paths))) d)
        (op Clip <$> getAttr sty)

-- Stroke both dashed and solid lines.
mkStroke :: Float ->  R.Join -> (R.Cap, R.Cap) ->  Maybe (R.DashPattern, Float)
      -> [[R.Primitive]] -> RenderR ()
mkStroke l j c d  primList =
  maybe (mapM_ (R.stroke l j c) primList)
        (\(dsh, off) -> mapM_ (R.dashedStrokeWithOffset off dsh l j c) primList)
        d

instance Renderable (Path R2) Rasterific where
  render _ p = R $ do
    f <- getStyleAttrib (toAlphaColour . getFillColor)
    s <- getStyleAttrib (toAlphaColour . getLineColor)
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    r <- fromMaybe Winding <$> getStyleAttrib getFillRule
    sty <- use accumStyle

    let fColor = uniformTexture $ sourceColor f o
        sColor = uniformTexture $ sourceColor s o
        (l, j, c, d) = rasterificStrokeStyle sty
        rule = fromFillRule r

        -- For stroking we need to keep all of the contours separate.
        primList = renderPath p

        -- For filling we need to concatenate them into a flat list.
        prms = concat primList

    when (isJust f) $ liftR (R.withTexture fColor $ R.fillWithMethod rule prms)
    liftR (R.withTexture sColor $ mkStroke l j c d primList)

instance Renderable (Segment Closed R2) Rasterific where
  render b = render b . (fromSegments :: [Segment Closed R2] -> Path R2) . (:[])

instance Renderable (Trail R2) Rasterific where
  render b = render b . pathFromTrail

-- read only of static data (safe)
ro :: FilePath -> FilePath
ro = unsafePerformIO . getDataFileName

openSansRegular :: Font
openSansRegular = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ (ro "fonts/OpenSans-Regular.ttf")

openSansBold :: Font
openSansBold = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ (ro "fonts/OpenSans-Bold.ttf")

openSansItalic :: Font
openSansItalic = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ (ro "fonts/OpenSans-Italic.ttf")

openSansBoldItalic :: Font
openSansBoldItalic = fnt
  where
    Right fnt = unsafePerformIO . loadFontFile $ (ro "fonts/OpenSans-BoldItalic.ttf")

fromFontStyle :: FontSlant -> FontWeight -> Font
fromFontStyle FontSlantItalic FontWeightBold = openSansBoldItalic
fromFontStyle FontSlantOblique FontWeightBold = openSansBoldItalic
fromFontStyle FontSlantNormal FontWeightBold = openSansBold
fromFontStyle FontSlantItalic FontWeightNormal = openSansItalic
fromFontStyle FontSlantOblique FontWeightNormal = openSansItalic
fromFontStyle _ _ = openSansRegular

textBox :: Font -> Int -> String -> (Double, Double)
textBox f ps str = (float2Double w, float2Double h)
  where
    (w, h) = stringBoundingBox f 96 ps str

instance Renderable Text Rasterific where
  render _ (Text tr al str) = R $ do
    fs <- fromMaybe 12 <$> getStyleAttrib (fromOutput . getFontSize)
    slant <- fromMaybe FontSlantNormal <$> getStyleAttrib getFontSlant
    fw <- fromMaybe FontWeightNormal <$> getStyleAttrib getFontWeight
    f <- getStyleAttrib (toAlphaColour . getFillColor)
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    let fColor = uniformTexture $ sourceColor f o
        fs' = round fs
        fnt = fromFontStyle slant fw
        (x, y) = textBox fnt fs' str
        (refX, refY) = case al of
          BaselineText -> (0, y)
          BoxAlignedText xt yt -> (x * xt, (1 - yt) * y)
        p = rasterificTransf ((moveOriginBy (r2 (refX, refY)) mempty) <> tr) (R.V2 0 0)
    liftR (R.withTexture fColor $ R.printTextAt fnt fs' p str)

instance Renderable (DImage Embedded) Rasterific where
  render _ (DImage iD w h tr) = R . liftR $ R.drawImageAtSize img 0 p w' (-h')
    where
      ImageRaster dImg = iD
      img = case dImg of
              ImageRGBA8 i -> i
              _            -> error "Invalid image type"
      tr' = dropTransl tr
      R.V2 w' h' = rasterificTransf tr' (v2 (fromIntegral w) (fromIntegral h))
      p = rasterificTransf ((moveOriginBy
                           (r2 ((float2Double w' / 2), (-float2Double h' / 2))) mempty)
                         <> tr) (R.V2 (0) 0)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

writeJpeg :: Word8 -> FilePath -> Result Rasterific R2 -> IO ()
writeJpeg quality outFile img = L.writeFile outFile bs
  where
    bs = encodeJpegAtQuality quality (pixelMap (convertPixel . dropTransparency) img)

renderRasterific :: FilePath -> SizeSpec2D -> Word8 -> Diagram Rasterific R2 -> IO ()
renderRasterific outFile sizeSpec quality d = writer outFile img
  where
    writer = case takeExtension outFile of
              ".png" -> writePng
              ".tif" -> writeTiff
              ".bmp" -> writeBitmap
              ".jpg" -> writeJpeg q
              _      -> writePng
    img = renderDia Rasterific (RasterificOptions sizeSpec) d
    q = max quality 100
