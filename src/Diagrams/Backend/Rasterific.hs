{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific
-- Copyright   :  (c) 2014-2015 diagrams-rasterific team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams using Rasterific,
-- implemented natively in Haskell (making it easy to use on any
-- platform). Can create png, tif, bmp, jpg, pdf, and animated GIFs.
--
-- To invoke the Rasterific backend, you have three options.
--
-- * You can use @'mainWith' 'Rasterific'@ to create standalone
--   executables which output images when invoked.
--
-- * You can use the 'renderRasterific' function provided by this module,
--   which gives you more flexible programmatic control over when and
--   how images are output (making it easy to, for example, write a
--   single program that outputs multiple images, or one that outputs
--   images dynamically based on user input, and so on).
--
-- * For the most flexibility (/e.g./ if you want access to the
--   resulting Rasterific value directly in memory without writing it to
--   disk), you can manually invoke the 'renderImage' function to return
--   an @'Image' 'PixelRGBA8'
--
-------------------------------------------------------------------------------
module Diagrams.Backend.Rasterific
  ( -- * Rasterific backend
    Rasterific(..)
  , Options(..)

    -- * Rendering
  , renderRasterific
  , renderPdf
  , renderImage

  -- , writeJpeg
  , GifDelay
  , GifLooping (..)
  , animatedGif

    -- * Text with envelopes
  , texterific
  , texterific'

    -- * Internals
    -- | These are low level functions whose implimentaion may change in
    --   the future. They're exported because they can sometimes be
    --   useful.
  , PaletteOptions (..)
  , defaultPaletteOptions
  , rasterGif

  ) where

import           Diagrams.Backend
import           Diagrams.Backend.Compile
import           Diagrams.Prelude                    hiding (clip, opacity,
                                                      output)
import           Diagrams.TwoD.Text                  hiding (Font)
import           Diagrams.Types                      hiding (local)
import           Geometry.Transform
import           GHC.Generics
import qualified Options.Applicative                 as OP
import           System.IO

import           Codec.Picture
import           Codec.Picture.ColorQuant            (defaultPaletteOptions)
import           Codec.Picture.Types                 (convertPixel,
                                                      dropTransparency)
import qualified Graphics.Rasterific                 as R
import           Graphics.Rasterific.Texture         (Gradient,
                                                      linearGradientTexture,
                                                      radialGradientWithFocusTexture,
                                                      transformTexture,
                                                      uniformTexture,
                                                      withSampler)
import qualified Graphics.Rasterific.Transformations as R

import           Control.Monad                       (join, when)
import           Data.ByteString.Lazy                (ByteString)
import qualified Data.ByteString.Lazy                as L (writeFile)
import qualified Data.Foldable                       as F
import           Data.Hashable                       (Hashable (..))
import           Data.Maybe                          (fromMaybe)
import           Data.Sequence                       (Seq)
import           Data.Typeable
import           System.FilePath                     (takeExtension)

import           Diagrams.Backend.Rasterific.Text


--------------------------------------------------------------------------------
-- | This data declaration is simply used as a token to distinguish
--   the Rasterific backend: (1) when calling functions where the type
--   inference engine would otherwise have no way to know which
--   backend you wanted to use, and (2) as an argument to the
--   'Backend' and type classe.
data Rasterific = Rasterific
  deriving (Eq,Ord,Read,Show,Typeable)

type instance V Rasterific = V2
type instance N Rasterific = Double

default2DAttrs :: Diagram V2 -> Diagram V2
default2DAttrs = lineWidth medium

-- From Diagrams.Core.Types.
instance Backend Rasterific where
  type Result  Rasterific = R.Drawing PixelRGBA8 ()
  data Options Rasterific = RasterificOptions
    { rasterificSizeSpec  :: SizeSpec V2 Int
    } deriving Show

  backendInfo _ = rasterificInfo

  renderDiaT opts dia = (sz, t2 <> reflectionY, drawing) where
    (sz, t2, dia') = adjustSize2D (opts^.sizeSpec) (default2DAttrs dia # reflectY)
    drawing        = render t2 dia'

instance BackendBuild Rasterific where
  saveDiagram' path (RasterificOptions sz) = renderRasterific path sz

  mkOptions    = RasterificOptions
  sizeSpec f o = RasterificOptions <$> f (rasterificSizeSpec o)
  showOptions  = show

instance Hashable (Options Rasterific) where
  hashWithSalt s (RasterificOptions sz) = s `hashWithSalt` sz

type Draw = R.Drawing PixelRGBA8 ()

render :: T2 Double -> Diagram V2 -> Draw
render = foldDiaA renderPrim renderAnnot where
  renderPrim t2 attrs prim =
    case renderPrimitive t2 attrs prim of
      Just r  -> r
      Nothing -> error "Unknown primitive"

renderPrimitive
  :: T2 Double -> Attributes -> Prim V2 Double -> Maybe Draw
renderPrimitive t2 attrs = \case
  Path_ path         -> Just $ drawPath attrs (transform t2 path)
  Text_ t            -> Just $ drawText t2 attrs t
  EmbeddedImage_ img -> Just $ drawImage t2 img
  Prim _             -> Nothing

renderAnnot :: Annotation V2 Double -> Draw -> Draw
renderAnnot = \case
  GroupOpacity_ x -> R.withGroupOpacity (round $ 255 * x)
  Clip_ c         -> clip c
  _               -> id

-- Drawing -------------------------------------------------------------

clip :: Seq (Path V2 Double) -> Draw -> Draw
clip ps r = foldr (\p -> R.withClipping (R.fill $ pathPrims p)) r ps

drawPath :: Attributes -> Path V2 Double -> Draw
drawPath s path = do
  let attr :: (Default a, Typeable a) => Getting r a r -> r
      attr g   = fromMaybe (def^.g) $ getAttr g s
  let opa      = attr _Opacity
      fTexture = fromTexture (attr _FillTexture) opa
      fRule    = fromFillRule (attr _FillRule)
      lTexture = fromTexture (attr _LineTexture) opa
      lJoin    = fromLineJoin (attr _LineJoin)
      lWidth   = realToFrac @Double $ attr _LineWidth
      lCaps    = join (,) $ fromLineCap (attr _LineCap)
      lDash    = fmap fromDashing (getAttr _Dashing s)

  -- XXX Need to separate lines and loops
  let canFill = (attr _FillTexture ^? _AC) /= Just transparent

  let prims = pathPrims path

  when canFill $
    R.withTexture fTexture $ R.fillWithMethod fRule (concat prims)

  R.withTexture lTexture $
    case lDash of
      Nothing      -> F.for_ prims $ R.stroke lWidth lJoin lCaps
      Just (d,off) ->
        F.for_ prims $
          R.dashedStrokeWithOffset off d lWidth lJoin lCaps

drawText :: T2 Double -> Attributes -> Text Double -> Draw
drawText tr attrs (Text a str) = do
  let attr :: (Default a, Typeable a) => Getting r a r -> r
      attr g   = fromMaybe (def^.g) $ getAttr g attrs
      lerp' t u v = realToFrac $ t * u + (1 - t) * v

      opa      = attr _Opacity
      fSize    = R.PointSize . realToFrac @Double $ attr _FontSize
      fStyle   = fromFontStyle (attr _FontSlant) (attr _FontWeight)
      fTexture = fromTexture (attr _FillTexture) opa
      bb       = textBoundingBox fStyle fSize str
      fAlign   =
        case a of
          BaselineText         -> R.V2 0 0
          BoxAlignedText xt yt -> case getCorners bb of
            Just (P (V2 xl yl), P (V2 xu yu)) -> R.V2 (-lerp' xt xu xl) (lerp' yt yu yl)
            Nothing                           -> R.V2 0 0

  R.withTransformation (fromT2 (tr <> reflectionY))
     (R.withTexture fTexture $ R.printTextAt fStyle fSize fAlign str)

drawImage :: T2 Double -> DynamicImage -> Draw
drawImage tr (convertRGBA8 -> img@(Image w h _)) = R.withTransformation t d
  where
    d = R.drawImage img 0 p
    t = fromT2 (tr <> reflectionY)
    p = R.V2 (- fromIntegral w /2) (- fromIntegral h / 2)

-- Converting primitives -----------------------------------------------

fromLineCap :: LineCap -> R.Cap
fromLineCap LineCapButt   = R.CapStraight 0
fromLineCap LineCapRound  = R.CapRound
fromLineCap LineCapSquare = R.CapStraight 1

fromLineJoin :: LineJoin -> R.Join
fromLineJoin LineJoinMiter = R.JoinMiter 0
fromLineJoin LineJoinRound = R.JoinRound
fromLineJoin LineJoinBevel = R.JoinMiter 1

fromDashing :: Dashing Double -> (R.DashPattern, Float)
fromDashing (Dashing ds d) = (map realToFrac ds, realToFrac d)

fromFillRule :: FillRule -> R.FillMethod
fromFillRule EvenOdd = R.FillEvenOdd
fromFillRule _       = R.FillWinding

fromColour :: AlphaColour Double -> Double -> PixelRGBA8
fromColour c o = PixelRGBA8 (w r) (w g) (w b) (w $ o*a)
  where
    (r, g, b, a) = colorToSRGBA c
    w x = round (255 * x)

fromSpreadMethod :: SpreadMethod -> R.SamplerRepeat
fromSpreadMethod GradPad     = R.SamplerPad
fromSpreadMethod GradReflect = R.SamplerReflect
fromSpreadMethod GradRepeat  = R.SamplerRepeat

fromGradientStops :: [GradientStop] -> Gradient PixelRGBA8
fromGradientStops = map $ \(GradientStop c v) -> (realToFrac v, fromColour c 1)

fromLinearGradient :: LGradient -> R.Texture PixelRGBA8
fromLinearGradient g = transformTexture tr tx
  where
    tr = fromT2 (inv $ g^.gradientTransform)
    tx = withSampler spreadMethod (linearGradientTexture gradDef p0 p1)
    spreadMethod = fromSpreadMethod (g^.gradientSpreadMethod)
    gradDef = fromGradientStops (g^.gradientStops)
    p0 = fromP2 (g^.gradientStart)
    p1 = fromP2 (g^.gradientEnd)

fromRadialGradient :: RGradient -> R.Texture PixelRGBA8
fromRadialGradient g = transformTexture tr tx
  where
    tr = fromT2 (inv $ g^.gradientTransform)
    tx = withSampler spreadMethod (radialGradientWithFocusTexture gradDef c (realToFrac r1) f)
    spreadMethod = fromSpreadMethod (g^.gradientSpreadMethod)
    c = fromP2 (g^.gradientCenter1)
    f = fromP2 (g^.gradientCenter0)
    gradDef = fromGradientStops ss

    -- Adjust the stops so that the gradient begins at the perimeter of
    -- the inner circle (center0, radius0) and ends at the outer circle.
    r0 = g^.gradientRadius0
    r1 = g^.gradientRadius1
    stopFracs = r0 / r1 : map (\s -> (r0 + (s^.stopFraction) * (r1 - r0)) / r1)
                (g^.gradientStops)
    gradStops = case g^.gradientStops of
      []       -> []
      xs@(x:_) -> x : xs
    ss = zipWith (\gs sf -> gs & stopFraction .~ sf ) gradStops stopFracs

-- Convert a diagrams @Texture@ and opacity to a rasterific texture.
fromTexture :: Texture -> Double -> R.Texture PixelRGBA8
fromTexture (SC c) o = uniformTexture $ fromColour (toAlphaColour c) o
fromTexture (LG g) _ = fromLinearGradient g
fromTexture (RG g) _ = fromRadialGradient g

fromP2 :: P2 Double -> R.Point
fromP2 (P v) = fromV2 v

fromV2 :: V2 Double -> R.Point
fromV2 (V2 x y) = R.V2 (realToFrac x) (realToFrac y)

fromT2 :: T2 Double -> R.Transformation
fromT2 (T m _ v) = R.Transformation a c e b d f
  where
    V2 (V2 a b) (V2 c d) = (fmap.fmap) realToFrac m
    V2 e f               = fmap realToFrac v

segPrim :: Located (Segment V2 Double) -> R.Primitive
segPrim (viewLoc -> (fromP2 -> p, s)) =
  case s of
    Linear v       -> R.LinePrim $ R.Line p (p + fromV2 v)
    Cubic u1 u2 u3 -> R.CubicBezierPrim $ R.CubicBezier q0 q1 q2 q3
      where
        (q0, q1, q2, q3) = (p, q0 + fromV2 u1, q0 + fromV2 u2, q0 + fromV2 u3)

pathPrims :: Path V2 Double -> [[R.Primitive]]
pathPrims p = (map . map) segPrim (pathLocSegments p)

-- Rendering -----------------------------------------------------------

-- | Render a 'Rasterific' diagram to a pdf file with given width and height
renderPdf :: Options Rasterific -> Diagram V2 -> ByteString
renderPdf opts dia = R.renderDrawingAtDpiToPDF w h 96 drawing
  where
    (fmap round -> V2 w h, _, drawing) = renderDiaT opts dia

renderImage :: SizeSpec V2 Int -> Diagram V2 -> Image PixelRGBA8
renderImage sz dia = R.renderDrawing w h bgColor drawing
  where
    (fmap round -> V2 w h, _, drawing) = renderDiaT (RasterificOptions sz) dia
    bgColor = PixelRGBA8 0 0 0 0

-- | Render a 'Rasterific' diagram to a file with the given size. The
--   format is determined by the extension (@.png@, @.tif@, @.bmp@, @.jpg@ and
--   @.pdf@ supported. (jpeg quality is 80, use 'writeJpeg' to choose
--   quality).
renderRasterific :: FilePath -> SizeSpec V2 Int -> Diagram V2 -> IO ()
renderRasterific outPath spec d =
  case takeExtension outPath of
    ".png"  -> L.writeFile outPath (encodePng img)
    ".tif"  -> L.writeFile outPath (encodeTiff img)
    ".bmp"  -> L.writeFile outPath (encodeBitmap img)
    ".jpg"  -> L.writeFile outPath (encodeJpeg imgYCbCr8)
    ".jpeg" -> L.writeFile outPath (encodeJpeg imgYCbCr8)
    ".pdf"  -> L.writeFile outPath pdf
    _       -> writePng outPath img
  where
    (sz,_,drawing) = renderDiaT (RasterificOptions spec) d
    img            = R.renderDrawing w h (PixelRGBA8 0 0 0 0) drawing
    imgYCbCr8      = pixelMap (convertPixel . dropTransparency) img
    pdf            = R.renderDrawingAtDpiToPDF w h 96 drawing
    V2 w h         = fmap round sz

-- Gifs ----------------------------------------------------------------

-- | Render a 'Rasterific' diagram to an animated gif with the given
--   size and uniform delay. Diagrams should be the same size.
animatedGif
  :: FilePath
  -> SizeSpec V2 Int
  -> GifLooping
  -> GifDelay -- ^ Delay in 100th of seconds ('Int')
  -> [Diagram V2]
  -> IO ()
animatedGif outFile sz gOpts i ds =
  case rasterGif sz gOpts defaultPaletteOptions (map (,i) ds) of
    Right bs -> L.writeFile outFile bs
    Left e   -> putStrLn e

-- | Turn a list of diagrams into a gif.
rasterGif
  :: SizeSpec V2 Int      -- ^ Size of output (in pixels)
  -> GifLooping           -- ^ looping options
  -> PaletteOptions       -- ^ palette options
  -> [(Diagram V2, Int)]  -- ^ Diagram zipped with its delay (100th of seconds)
  -> Either String ByteString
rasterGif sz gOpts pOpts ds = encodeGifImages gOpts (map pal imgs)
  where
    imgs = over (each . _1) (pixelMap dropTransparency . renderImage sz) ds
    pal (palettize pOpts -> (img,p), d) = (p, d, img)

-- CmdLine -------------------------------------------------------------

instance RenderOutcome Rasterific (Diagram V2) where
  type MainOpts Rasterific (Diagram V2) = (FilePath, SizeSpec V2 Int)

  resultParser _ _ = (,) <$> outputParser <*> sizeParser
  renderOutcome _ (path, sz) = saveDiagram' path (mkOptions @Rasterific sz)

-- | Extra options for animated GIFs.
data GifOpts = GifOpts
  { _dither     :: Bool
  , _noLooping  :: Bool
  , _loopRepeat :: Maybe Int
  } deriving (Show, Eq, Ord, Generic, Hashable)

makeLenses ''GifOpts

-- | Command line parser for 'GifOpts':
--     - @--dither@ turn dithering on.
--     - @--looping-off@ turn looping off, i.e play GIF once.
--     - @--loop-repeat INT@ number of times to repeat the GIF after the first playing.
--       this option is only used if @--looping-off@ is not set.
gifOptsParser :: OP.Parser GifOpts
gifOptsParser =
  GifOpts
    <$> OP.switch (OP.long "dither" <> OP.help "Turn on dithering.")
    <*> OP.switch (OP.long "looping-off" <> OP.help "Turn looping off")
    <*> (OP.optional . OP.option OP.auto)
      (OP.long "loop-repeat" <> OP.help "Number of times to repeat")

-- | An animated GIF can be a result.
instance RenderOutcome Rasterific [(Diagram V2, GifDelay)] where
  type MainOpts Rasterific [(Diagram V2, GifDelay)] = (FilePath, SizeSpec V2 Int, GifOpts)
  resultParser _ _ = (,,) <$> outputParser <*> sizeParser <*> gifOptsParser

  renderOutcome _ (path, sz, gOpts) ids
    | null path = hPutStrLn stderr "No output file given. Specify output file with -o"
    | otherwise = case rasterGif sz lOpts pOpts ids of
        Right bs -> L.writeFile path bs
        Left e   -> hPutStrLn stderr e
    where
      lOpts
        | gOpts^.noLooping = LoopingNever
        | otherwise        = maybe LoopingForever (LoopingRepeat . fromIntegral)
                               (gOpts^.loopRepeat)
      pOpts = defaultPaletteOptions {enableImageDithering=gOpts^.dither}

