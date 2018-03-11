{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific.Text
-- Copyright   :  (c) 2015 diagrams-rasterific team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Experimental module to create text with an envelope and trace. The
-- texterifc functions build diagrams with text size of @'local' 1@ and
-- a s specified slant and weight. The size should be changed only with
-- the scale functions and changing the slant and/or weight after the
-- text has benn created can result in an slightly incorrect envelope.
-------------------------------------------------------------------------------
module Diagrams.Backend.Rasterific.Text
  ( texterific'
  , texterific
  , fromFontStyle
  , textBoundingBox
  ) where

import           Graphics.Text.TrueType    hiding (BoundingBox)

import           Diagrams.Prelude
import           Diagrams.Types
import           Diagrams.TwoD.Text        hiding (Font)

import           Data.FileEmbed            (embedDir)
import           Data.ByteString           (ByteString)
import           Data.ByteString.Lazy      (fromStrict)

-- | Get the 'BoundingBox' for some font with the origin at the start of
--   the baseline.
textBoundingBox :: RealFloat n => Font -> PointSize -> String -> BoundingBox V2 n
textBoundingBox f p s = fromCorners
                        (mkP2 (2*r2f _xMin bb)              (r2f _yMin bb))
                        (mkP2 (r2f _xMax bb + r2f _xMin bb) (r2f _yMax bb))
  where
    r2f = fmap realToFrac
    bb = stringBoundingBox f 96 p s

-- | Create a primitive text diagram from the given 'FontSlant',
--   'FontWeight', and string, with baseline alignment, envelope and trace
--   based on the 'BoundingBox' of the text.
texterific' :: FontSlant -> FontWeight -> String -> Diagram V2
texterific' fs fw s = backupFillColor black . fontSizeL 1
                    . fontSlant fs . fontWeight fw
                    $ mkQD (Prim $ Text BaselineText s)
                           (getEnvelope bb)
                           (getTrace bb)
                           (getQuery bb)
  where
    bb = textBoundingBox fnt (PointSize 1) s
    fnt = fromFontStyle fs fw

-- | Create a primitive text diagram from the given string, with
--   baseline alignment, envelope and trace based on the 'BoundingBox'
--   of the text.  Designed to be a replacement for the function 'text'
--   in Diagrams.TwoD.Text.
texterific :: String -> Diagram V2
texterific s = texterific' FontSlantNormal FontWeightNormal s

-- | Get an OpenSans font with the given 'FontSlant' and 'FontWeight'.
fromFontStyle :: FontSlant -> FontWeight -> Font
fromFontStyle FontSlantItalic  FontWeightBold   = openSansBoldItalic
fromFontStyle FontSlantOblique FontWeightBold   = openSansBoldItalic
fromFontStyle FontSlantNormal  FontWeightBold   = openSansBold
fromFontStyle FontSlantItalic  FontWeightNormal = openSansItalic
fromFontStyle FontSlantOblique FontWeightNormal = openSansItalic
fromFontStyle _                _                = openSansRegular

fonts :: [(FilePath,ByteString)]
fonts = $(embedDir "fonts")

-- Read a static font file which is included with the package.
staticFont :: String -> Font
staticFont nm = case lookup nm fonts of
   Nothing -> error ("Font not found: " ++ nm)
   Just f  -> case decodeFont (fromStrict f) of
                Right r -> r
                Left e  -> error e

openSansRegular :: Font
openSansRegular = staticFont "OpenSans-Regular.ttf"

openSansBold :: Font
openSansBold = staticFont "OpenSans-Bold.ttf"

openSansItalic :: Font
openSansItalic = staticFont "OpenSans-Italic.ttf"

openSansBoldItalic :: Font
openSansBoldItalic = staticFont "OpenSans-BoldItalic.ttf"

