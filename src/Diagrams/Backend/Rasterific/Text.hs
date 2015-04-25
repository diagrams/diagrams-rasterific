{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

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
-- changing the slant and/or weight after the text has benn created
-- can result in an slightly incorrect envelope.
-------------------------------------------------------------------------------
module Diagrams.Backend.Rasterific.Text
  ( texterific'
  , texterific
  , fromFontStyle
  , mkBoundingBox
  ) where

import           Graphics.Text.TrueType    hiding (BoundingBox)

import           Diagrams.Prelude
import           Diagrams.TwoD.Text        hiding (Font)

import           Paths_diagrams_rasterific (getDataFileName)
import           System.IO.Unsafe          (unsafePerformIO)



mkBoundingBox :: RealFloat n => Font -> PointSize -> String -> BoundingBox V2 n
mkBoundingBox f p s = fromCorners
                      (P $ V2 0 (realToFrac $ -_baselineHeight bb))
                      (P $ V2 (realToFrac w) (realToFrac h))
  where
    (w, h) = (_xMax bb - _xMin bb, _yMax bb - _yMin bb)
    bb = stringBoundingBox f 96 p s

-- | Create a primitive text diagram from the given 'FontSlant', 'FontWeight',
--   and string, with center alignment, envelope and trace based on
--   the 'BoundingBox' of the text.
texterific' :: (TypeableFloat n, Renderable (Text n) b)
            => FontSlant -> FontWeight -> String -> QDiagram b V2 n Any
texterific' fs fw s = recommendFillColor black . fontSizeL 1
                    . fontSlant fs . fontWeight fw
                    $ mkQD (Prim $ Text mempty BaselineText s)
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
texterific :: (TypeableFloat n, Renderable (Text n) b) => String -> QDiagram b V2 n Any
texterific s = texterific' FontSlantNormal FontWeightNormal s

fromFontStyle :: FontSlant -> FontWeight -> Font
fromFontStyle FontSlantItalic  FontWeightBold   = openSansBoldItalic
fromFontStyle FontSlantOblique FontWeightBold   = openSansBoldItalic
fromFontStyle FontSlantNormal  FontWeightBold   = openSansBold
fromFontStyle FontSlantItalic  FontWeightNormal = openSansItalic
fromFontStyle FontSlantOblique FontWeightNormal = openSansItalic
fromFontStyle _                _                = openSansRegular

-- Read a static font file which is included with the package. This
-- should be safe as long as it will installed properly.
staticFont :: String -> Font
staticFont nm =
  case unsafePerformIO $ getDataFileName nm >>= loadFontFile of
    Right f -> f
    Left e  -> error e

openSansRegular :: Font
openSansRegular = staticFont "fonts/OpenSans-Regular.ttf"

openSansBold :: Font
openSansBold = staticFont "fonts/OpenSans-Bold.ttf"

openSansItalic :: Font
openSansItalic = staticFont "fonts/OpenSans-Italic.ttf"

openSansBoldItalic :: Font
openSansBoldItalic = staticFont "fonts/OpenSans-BoldItalic.ttf"

