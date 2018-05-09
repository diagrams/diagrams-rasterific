{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific.CmdLine
-- Copyright   :  (c) 2014-2015 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Rasterific backend. Create
-- png, tif, bmp, jpg, pdf, or animated GIF files.
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'animMain' is like 'defaultMain' but for animations instead of
--   diagrams.
--
-- * `gifMain` creates an executable to generate an animated GIF.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a suitable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * Use a function with 'mainWith'.  This may require making
--   'Parseable' instances for custom argument types.
--
-- * Make a new 'Mainable' instance.  This may require a newtype
--   wrapper on your diagram type to avoid the existing instances.
--   This gives you more control over argument parsing, intervening
--   steps, and diagram creation.
--
-- * Build option records and pass them along with a diagram to 'mainRender'
--   from "Diagrams.Backend.CmdLine".
--
-- * You can use 'Diagrams.Backend.Rasterific.renderRasterific' to render a
--   diagram to a file directly; see "Diagrams.Backend.Rasterific".
--
-- * A more flexible approach is to directly call 'renderDia'; see
--   "Diagrams.Backend.Rasterific" for more information.
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Rasterific.CmdLine
  (
   -- * General form of @main@
   -- $mainwith
   mainWith

    -- * Supported forms of @main@
  , defaultMain
  , multiMain
  , animMain
  , gifMain
  , uniformGifMain

   -- * GIF support
  , GifOpts(..)

    -- * Backend tokens
  , Rasterific
  , B
  ) where

import           Diagrams.Backend.CmdLine
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (height, interval, option,
                                              output, width)

import qualified Data.ByteString.Lazy        as L (writeFile)

import           Options.Applicative

-- | 'mainWith' specialised to 'Diagram' 'Rasterific'.
defaultMain :: Diagram Rasterific -> IO ()
defaultMain = mainWith

instance TypeableFloat n => Mainable (QDiagram Rasterific V2 n Any) where
  type MainOpts (QDiagram Rasterific V2 n Any) = (DiagramOpts, DiagramLoopOpts)

  mainRender (opts,loopOpts) d = do
      chooseRender opts d
      defaultLoopRender loopOpts

chooseRender :: TypeableFloat n => DiagramOpts -> QDiagram Rasterific V2 n Any -> IO ()
chooseRender opts d
  | null path = noFileError
  | otherwise = renderRasterific path sz d
  where
    path = opts^.output
    sz   = fromIntegral <$> mkSizeSpec2D (opts^.width) (opts^.height)

noFileError :: IO ()
noFileError = putStrLn "No output file given. Specify output file with -o"

-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The
--   list of available diagrams may also be printed by passing the
--   option @--list@.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiTest
-- [1 of 1] Compiling Main             ( MultiTest.hs, MultiTest.o )
-- Linking MultiTest ...
-- $ ./MultiTest --list
-- Available diagrams:
--   foo bar
-- $ ./MultiTest --selection bar -o Bar.png -w 200
-- @

multiMain :: [(String, Diagram Rasterific)] -> IO ()
multiMain = mainWith

instance TypeableFloat n => Mainable [(String,QDiagram Rasterific V2 n Any)] where
  type MainOpts [(String,QDiagram Rasterific V2 n Any)]
      = (MainOpts (QDiagram Rasterific V2 n Any), DiagramMultiOpts)

  mainRender = defaultMultiMainRender

-- | @animMain@ is like 'defaultMain', but renders an animation
-- instead of a diagram.  It takes as input an animation and produces
-- a command-line program which will crudely \"render\" the animation
-- by rendering one image for each frame, named by extending the given
-- output file name by consecutive integers.  For example if the given
-- output file name is @foo\/blah.png@, the frames will be saved in
-- @foo\/blah001.png@, @foo\/blah002.png@, and so on (the number of
-- padding digits used depends on the total number of frames).  It is
-- up to the user to take these images and stitch them together into
-- an actual animation format (using, /e.g./ @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
-- The @--fpu@ option can be used to control how many frames will be
-- output for each second (unit time) of animation.
animMain :: Animation Rasterific V2 Double -> IO ()
animMain = mainWith

instance TypeableFloat n => Mainable (Animation Rasterific V2 n) where
  type MainOpts (Animation Rasterific V2 n) =
    ((DiagramOpts, DiagramAnimOpts), DiagramLoopOpts)

  mainRender (opts, l) d = do
    defaultAnimMainRender chooseRender output opts d
    defaultLoopRender l

-- | Make an animated gif main by pairing diagrams with a delay ('Int'
--   measured in 100th seconds).
gifMain :: [(Diagram Rasterific, GifDelay)] -> IO ()
gifMain = mainWith

-- | Make an animated gif main with the same delay for each diagram.
uniformGifMain :: GifDelay -> [Diagram Rasterific] -> IO ()
uniformGifMain i = mainWith . map (,i)

-- | Extra options for animated GIFs.
data GifOpts = GifOpts { _dither     :: Bool
                       , _noLooping  :: Bool
                       , _loopRepeat :: Maybe Int}

makeLenses ''GifOpts

-- | Command line parser for 'GifOpts'.
--   @--dither@ turn dithering on.
--   @--looping-off@ turn looping off, i.e play GIF once.
--   @--loop-repeat@ number of times to repeat the GIF after the first playing.
--   this option is only used if @--looping-off@ is not set.
instance Parseable GifOpts where
  parser = GifOpts <$> switch
                       ( long "dither"
                      <> help "Turn on dithering." )
                   <*> switch
                       ( long "looping-off"
                      <> help "Turn looping off" )
                   <*> ( optional . option auto)
                       ( long "loop-repeat"
                      <> help "Number of times to repeat" )

-- | An animated GIF can be a result.
instance ToResult [(QDiagram Rasterific V2 n Any, GifDelay)] where
  type Args [(QDiagram Rasterific V2 n Any, GifDelay)] = ()
  type ResultOf [(QDiagram Rasterific V2 n Any, GifDelay)] = [(QDiagram Rasterific V2 n Any, GifDelay)]

  toResult ds _ = ds

instance TypeableFloat n => Mainable [(QDiagram Rasterific V2 n Any, GifDelay)] where
  type MainOpts [(QDiagram Rasterific V2 n Any, GifDelay)] = (DiagramOpts, GifOpts)

  mainRender (dOpts, gOpts) ids
    | null path = noFileError
    | otherwise = case rasterGif sz lOpts pOpts ids of
        Right bs -> L.writeFile path bs
        Left e   -> putStrLn e
    where
      sz   = fromIntegral <$> mkSizeSpec2D (dOpts^.width) (dOpts^.height)
      path = dOpts^.output
      lOpts
        | gOpts^.noLooping = LoopingNever
        | otherwise        = maybe LoopingForever (LoopingRepeat . fromIntegral)
                               (gOpts^.loopRepeat)
      pOpts = defaultPaletteOptions {enableImageDithering=gOpts^.dither}

