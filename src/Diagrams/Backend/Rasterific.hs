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

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific
-- Copyright   :  (c) 2014 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Rasterific
  ( Rasterific(..) -- rendering token
  , B
  , Options(..)

  , renderRasterifc
  ) where

import           Diagrams.Core.Compile           (RNode (..), RTree, toRTree)
import           Diagrams.Core.Transform

import           Diagrams.Prelude                hiding (opacity, view, Image)
import           Diagrams.TwoD.Adjust            (adjustDia2D,
                                                  setDefault2DAttributes)
import           Diagrams.TwoD.Path              (Clip (Clip), getFillRule)
import           Diagrams.TwoD.Size              (requiredScaleT)

import qualified Graphics.Rasterific             as R
import           Graphics.Rasterific.Texture     (uniformTexture)
import           Codec.Picture                   (PixelRGBA8 (..), Image (..))

import           Control.Lens                    hiding (transform, ( # ))
import           Control.Monad                   (when)
import           Control.Monad.Trans             (lift)
import           Control.Monad.StateStack
import           Data.Default.Class
import qualified Data.Foldable                   as F
import           Data.Maybe                      (isJust, fromMaybe)
import           Data.Tree
import           Data.Typeable
import           GHC.Generics                    (Generic)

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
                  , _ignoreFill :: Bool
                  }

makeLenses ''RasterificState

instance Default RasterificState where
  def = RasterificState
        { _accumStyle       = mempty
        , _ignoreFill       = False
        }

-- | The custom monad in which intermediate drawing options take
--   place; 'Graphics.Rasterific.Drawing' is Rasterific's own rendering
--   monad.
type RenderM a = StateStackT RasterificState RenderR a

type RenderR a = R.Drawing PixelRGBA8 a

liftR :: RenderR a -> RenderM a
liftR = lift

runRenderM :: RenderM a -> RenderR a
runRenderM = flip evalStateStackT def

instance Backend Rasterific R2 where
  data Render  Rasterific R2 = R (RenderM ())
  type Result  Rasterific R2 = Image PixelRGBA8
  data Options Rasterific R2 = RasterificOptions
          { _rasterificFileName      :: String     -- ^ The name of the file you want generated
          , _rasterificSizeSpec      :: SizeSpec2D -- ^ The requested size of the output
          , _rasterificBypassAdjust  :: Bool    -- ^ Should the 'adjustDia' step be bypassed during rendering?
          }
    deriving (Show)

  doRender _ (RasterificOptions file size out _) (R r) =
    R.renderDrawing (round w) (round h) PixelRGBA8 r'
    where
      r' = runRenderM r
      (w,h) = case size of
                Width w'   -> (w',w')
                Height h'  -> (h',h')
                Dims w' h' -> (w',h')
                Absolute   -> (100,100)

  renderData _ = renderRTree . toRTree

  adjustDia c opts d = if _rasterificBypassAdjust opts
                         then (opts, d # setDefault2DAttributes)
                         else adjustDia2D _rasterificSizeSpec
                                          setRasterificSizeSpec
                                          c opts (d # reflectY)
    where setRasterificSizeSpec sz o = o { _rasterificSizeSpec = sz }

runR :: Render  Rasterific R2 -> RenderM ()
runR (R r) = r

instance Monoid (Render Rasterific R2) where
  mempty  = R $ return ()
  (R rd1) `mappend` (R rd2) = R (rd1 >> rd2)

renderRTree :: RTree Rasterific R2 a -> Render Rasterific R2
renderRTree (Node (RPrim accTr p) _) = render Rasterific (transform accTr p)
renderRTree (Node (RStyle sty) ts)   = R $ do
  save
  accumStyle %= (<> sty)
  runR $ F.foldMap renderRTree ts
  restore
renderRTree (Node (RFrozenTr tr) ts) = R $ do
  save
  --liftR $ rasterificTransf tr
  runR $ F.foldMap renderRTree ts
  restore
renderRTree (Node _ ts)              = F.foldMap renderRTree ts

-- | Render an object that the Rasterific backend knows how to render.
renderR :: (Renderable a Rasterific, V a ~ R2) => a -> RenderM ()
renderR = runR . render Rasterific

rasterificStrokeStyle :: Style v -> (Float, R.Join, (R.Cap, R.Cap), R.DashPattern)
rasterificStrokeStyle s = (strokeWidth, strokeJoin, strokeCaps, dashPattern)
  where
    strokeWidth = fromMaybe 0.01 (getLineWidth <$> getAttr s)
    strokeJoin = fromMaybe (R.JoinMiter 0) (fromLineJoin . getLineJoin <$> getAttr s)
    strokeCaps = (strokeCap, strokeCap)
    strokeCap = fromMaybe (R.CapStraight 0) (fromLineCap . getLineCap <$> getAttr s)
    dashPattern = [5, 10, 5]

fromLineCap :: LineCap -> R.Cap
fromLineCap LineCapButt   = R.CapStraight 0
fromLineCap LineCapRound  = R.CapRound
fromLineCap LineCapSquare = R.CapStraight 1

fromLineJoin :: LineJoin -> R.Join
fromLineJoin LineJoinMiter = R.JoinMiter 0
fromLineJoin LineJoinRound = R.JoinRound
fromLineJoin LineJoinBevel = R.JoinMiter 1

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

setSourceColor :: Maybe (AlphaColour Double) -> RenderM ()
setSourceColor Nothing  = return ()
setSourceColor (Just c) = do
  o <- fromMaybe 1 <$> getStyleAttrib getOpacity
  liftR (R.withTexture $ uniformTexture drawColor)
  where
    drawColor = PixelRGBA8 r g b a
    (r,g,b,a) = colorToSRGBA c

renderSeg :: Segment Closed R2 -> R.PathCommand
renderSeg  (Linear (OffsetClosed (unr2 -> (x,y)))) = R.PathLineTo (R.V2 x y)
renderSeg  (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (OffsetClosed (unr2 -> (x3,y3)))) =
  R.PathCubicBezierCurveTo (R.V2 x1 y1) (R.V2 x2 y2) (R.V2 x3 y3)

renderTrail :: Located (Trail R2) -> [R.Primitive]
renderTrail (viewLoc -> (unp2 -> (x,y), t)) =
  R.pathToPrimitives $ withTrail (renderLine t) (renderLoop t)
  where
    renderLine l = R.Path (R.V2 x y) False (map renderSeg (lineSegments l))
    renderLoop lp = case loopSegments lp of
      (segs, Linear _) -> R.Path (R.V2 x y) False (map renderSeg segs)
      _ -> R.Path (R.V2 x y) True (map renderSeg (lineSegments . cutLoop $ lp))

instance Renderable (Path R2) Rasterific where
  render _ p = R $ do
    f <- getStyleAttrib (toAlphaColour . getFillColor)
    s <- getStyleAttrib (toAlphaColour . getLineColor)
    ign <- use ignoreFill
    setSourceColor f
    when (isJust f && not ign) $ liftR R.fill prims
    setSourceColor s
    sty <- use accumStyle
    let (l, j, c, _) = rasterificStrokeStyle sty
    liftR R.stroke l j c prims
    where
      prims = map renderTrail (op Path p)


instance Renderable (Segment Closed R2) Rasterific where
  render c = render c . (fromSegments :: [Segment Closed R2] -> Path R2) . (:[])

instance Renderable (Trail R2) Rasterific where
  render c = render c . pathFromTrail

renderRasterifc :: FilePath -> SizeSpec2D -> Diagram Rasterific R2 -> IO ()
renderRasterifc outFile sizeSpec d =
  renderDia Rasterific (RasterificOptions outFile sizeSpec  False) d
