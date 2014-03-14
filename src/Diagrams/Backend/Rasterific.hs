{-# LANGUAGE TemplateHaskell           #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific
-- Copyright   :  (c) 2014 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Rasterific where

import           Diagrams.Core.Compile           (RNode (..), RTree, toRTree)
import           Diagrams.Core.Transform

import           Diagrams.Prelude                hiding (opacity, view)
import           Diagrams.TwoD.Adjust            (adjustDia2D,
                                                  setDefault2DAttributes)
import           Diagrams.TwoD.Path              (Clip (Clip), getFillRule)
import           Diagrams.TwoD.Size              (requiredScaleT)

import qualified Graphics.Rasterific             as R

import           Control.Lens                    hiding (transform, ( # ))
import           Control.Monad.Trans             (lift)
import           Control.Monad.StateStack
import           Data.Default.Class
import qualified Data.Foldable                   as F
import           Data.Typeable

-- | This data declaration is simply used as a token to distinguish
--   the Rasterific backend: (1) when calling functions where the type
--   inference engine would otherwise have no way to know which
--   backend you wanted to use, and (2) as an argument to the
--   'Backend' and 'Renderable' type classes.
data Rasterific = Rasterific
  deriving (Eq,Ord,Read,Show,Typeable)

type B = Rasterific

data OutputType =
    PNG         -- ^ Portable Network Graphics output.
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Typeable, Generic)

data RasterificState =
  RasterificState { _accumStyle :: Style R2
                    -- ^ The current accumulated style.
                  , _ignoreFill :: Bool
                  }

makeLenes ''RasterificState

instance Default RasterificState where
  def = RasterificState
        { _accumStyle       = mempty
        , _ignoreFill       = False
        }

-- | The custom monad in which intermediate drawing options take
--   place; 'Graphics.Rasterific.Drawing' is Rasterific's own rendering
--   monad.
type RenderM a = SS.StateStackT CairoState RenderR a

type RenderR a = R.Drawing PixelRGBA8 a

liftR :: RenderR a -> RenderM px a
liftR = lift

runRenderM :: RenderM a -> RenderR a
runRenderM = flip SS.evalStateStackT def

instance Backend Rasterific R2 where
  data Render  Rasterific R2 = R (RenderM ())
  type Result  Rasterific R2 = (IO (), RenderR ())
  data Options Rasterific R2 = RasterificOptions
          { _rasterificFileName      :: String     -- ^ The name of the file you want generated
          , _rasterificSizeSpec      :: SizeSpec2D -- ^ The requested size of the output
          , _rasterificOutputType    :: OutputType -- ^ the output format and associated options
          , _rasterificBypassAdjust  :: Bool    -- ^ Should the 'adjustDia' step be bypassed during rendering?
          }
    deriving (Show)

doRender _ (RasterificOptions file size out _) (R r) = (renderIO, r')
    where r' = runRenderM r
          renderIO = do
            let surfaceF s = C.renderWith s r'

                -- Everything except Dims is arbitrary. The backend
                -- should have first run 'adjustDia' to update the
                -- final size of the diagram with explicit dimensions,
                -- so normally we would only expect to get Dims anyway.
                (w,h) = case size of
                          Width w'   -> (w',w')
                          Height h'  -> (h',h')
                          Dims w' h' -> (w',h')
                          Absolute   -> (100,100)

            case out of
              PNG ->
                C.withImageSurface C.FormatARGB32 (round w) (round h) $ \surface -> do
                  surfaceF surface
                  C.surfaceWriteToPNG surface file

-- renderData :: Monoid' m => b -> QDiagram b v m -> Render b v
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
  rasterificStyle sty
  accumStyle %= (<> sty)
  runR $ F.foldMap renderRTree ts
  restore
renderRTree (Node (RFrozenTr tr) ts) = R $ do
  save
  liftR $ rasterificTransf tr
  runR $ F.foldMap renderRTree ts
  restore
renderRTree (Node _ ts)              = F.foldMap renderRTree ts

-- | Render an object that the cairo backend knows how to render.
renderC :: (Renderable a Cairo, V a ~ R2) => a -> RenderM ()
renderC = runC . render Cairo

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

-- | Handle those style attributes for which we can immediately emit
--   cairo instructions as we encounter them in the tree (clip, font
--   size, fill rule, line width, cap, join, and dashing).  Other
--   attributes (font face, slant, weight; fill color, stroke color,
--   opacity) must be accumulated.
rasterificStyle :: Style v -> RenderM ()
rasterificStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle fSize
                , handle lFillRule
                , handle lWidth
                , handle lCap
                , handle lJoin
                , handle lDashing
                ]
  where handle :: AttributeClass a => (a -> RenderM ()) -> Maybe (RenderM ())
        handle f = f `fmap` getAttr s
        clip       = mapM_ (\p -> rasterificPath p >> liftC R.clip) . op Clip
        fSize      = liftR . XXX . getFontSize
        lFillRule  = liftR . XXX . fromFillRule . getFillRule
        lWidth     = liftR . XXX . getLineWidth
        lCap       = liftR . XXX . fromLineCap . getLineCap
        lJoin      = liftR . XXX . fromLineJoin . getLineJoin
        lDashing (getDashing -> Dashing ds offs) =
          liftR $ XXX ds offs

-- |
rasterificTransf :: T2 -> RenderR ()