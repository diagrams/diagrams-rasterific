{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific
-- Copyright   :  (c) 2014-2015 diagrams-rasterific team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Experimental module to create text with an envelope and trace. The texterifc
-- functions build diagrams with text size of Local 1 and a s specified slant
-- and weight. The size should be changed only with the scale functions and
-- changing the slant and/or weight can result in an slightly incorrect envelope.
-------------------------------------------------------------------------------
module Diagrams.Backend.Rasterific.Text
  ( texterific'
  , texterific
  ) where

import           Data.Monoid.Recommend
import           Data.Typeable

import           Graphics.Text.TrueType       hiding (BoundingBox)

import           Diagrams.TwoD.Text           hiding (Font)
import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific (fromFontStyle)



mkBoundingBox :: Font -> PointSize -> String -> BoundingBox V2 Double
mkBoundingBox f p s = fromCorners
                      (P $ V2 0 (realToFrac $ -_baselineHeight bb))
                      (P $ V2 (realToFrac w) (realToFrac h))
  where
    (w, h) = (_xMax bb - _xMin bb, _yMax bb - _yMin bb)
    bb = stringBoundingBox f 96 p s

-- XXX This function is copied from Diagrams.TwoD.Text for convenience
-- and should be deleted here and exprot from -lib before merging.
recommendFontSize :: (N a ~ n, Typeable n, HasStyle a) => Measure n -> a -> a
recommendFontSize = applyMAttr . fmap (FontSize . Recommend . Last)

-- | Create a primitive text diagram from the given 'FontSlant', 'FontWeight',
--   and string, with center alignment, envelope and trace based on
--   the 'BoundingBox' of the text.
texterific' :: Renderable (Text Double) b
            => FontSlant -> FontWeight -> String -> QDiagram b V2 Double Any
texterific' fs fw s = center . recommendFillColor black
                    . recommendFontSize (local 1)
                    . fontSlant fs . fontWeight fw
                    $ mkQD (Prim $ Text mempty (BoxAlignedText 0 0) s)
                           (getEnvelope bb)
                           (getTrace bb)
                           mempty
                           (boundingBoxQuery bb)
  where
    bb = mkBoundingBox fnt (PointSize 1) s
    fnt = fromFontStyle fs fw

-- | Create a primitive text diagram from the given string, with center
--   alignment, envelope and trace based on the 'BoundingBox' of the text.
--   Designed to be a replacement for the function 'text' in Diagrams.TwoD.Text.
texterific :: Renderable (Text Double) b => String -> QDiagram b V2 Double Any
texterific s = texterific' FontSlantNormal FontWeightNormal s
