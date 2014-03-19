{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Rasterific.CmdLine
-- Copyright   :  (c) 2014 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Rasterific backend.
-----------------------------------------------------------------------------

module Diagrams.Backend.Rasterific.CmdLine
        (
         -- * General form of @main@
         -- $mainwith

         mainWith

         -- * Supported forms of @main@

       , defaultMain

         -- * Backend tokens

       , Rasterific
       , B
       ) where

import            Diagrams.Prelude      hiding (width, height, interval)
import            Diagrams.Backend.Rasterific
import            Diagrams.Backend.CmdLine

import            Codec.Picture         (writePng, writeTiff)

import            Control.Lens          ((^.), Lens', makeLenses)

import            Data.List.Split

#ifdef CMDLINELOOP
import            Data.Maybe            (fromMaybe)
import            Control.Monad         (when, mplus)


import            System.Environment    (getArgs, getProgName)
import            System.Directory      (getModificationTime)
import            System.Process        (runProcess, waitForProcess)
import            System.IO             (openFile, hClose, IOMode(..) ,hSetBuffering
                                        ,BufferMode(..), stdout)
import            System.Exit           (ExitCode(..))
import            Control.Concurrent    (threadDelay)
import            Control.Exception     (catch, SomeException(..), bracket)
import            System.Posix.Process  (executeFile)

#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock (UTCTime,getCurrentTime)
type ModuleTime = UTCTime
getModuleTime :: IO  ModuleTime
getModuleTime = getCurrentTime
#else
import System.Time         (ClockTime, getClockTime)
type ModuleTime = ClockTime
getModuleTime :: IO  ModuleTime
getModuleTime = getClockTime
#endif
#endif

defaultMain :: Diagram Rasterific R2 -> IO ()
defaultMain = mainWith

instance Mainable (Diagram Rasterific R2) where
#ifdef CMDLINELOOP
    type MainOpts (Diagram Rasterific R2) = (DiagramOpts, DiagramLoopOpts)

    mainRender (opts,loopOpts) d = do
        chooseRender opts d
        when (loopOpts^.loop) (waitForChange Nothing loopOpts)
#else
    type MainOpts (Diagram Rasterific R2) = DiagramOpts

    mainRender opts d = chooseRender opts d
#endif

chooseRender :: DiagramOpts -> Diagram Rasterific R2 -> IO ()
chooseRender opts d =
  case splitOn "." (opts ^. output) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["png", "tif"] -> do
           let img = renderDia Rasterific
                        ( RasterificOptions
                          (opts^.output)
                          (mkSizeSpec
                             (fromIntegral <$> opts ^. width )
                             (fromIntegral <$> opts ^. height)
                          )
                          False
                        )
                        d
           case last ps of
             "png" -> writePng (opts^.output) img
             "tif" -> writeTiff (opts^.output) img
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

#ifdef CMDLINELOOP
waitForChange :: Maybe ModuleTime -> DiagramLoopOpts -> IO ()
waitForChange lastAttempt opts = do
    prog <- getProgName
    args <- getArgs
    hSetBuffering stdout NoBuffering
    go prog args lastAttempt
  where go prog args lastAtt = do
          threadDelay (1000000 * opts^.interval)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAtt prog (opts^.src)
          if newBin
            then executeFile prog False args Nothing
            else go prog args $ newAttempt `mplus` lastAtt

-- | @recompile t prog@ attempts to recompile @prog@, assuming the
--   last attempt was made at time @t@.  If @t@ is @Nothing@ assume
--   the last attempt time is the same as the modification time of the
--   binary.  If the source file modification time is later than the
--   last attempt time, then attempt to recompile, and return the time
--   of this attempt.  Otherwise (if nothing has changed since the
--   last attempt), return @Nothing@.  Also return a Bool saying
--   whether a successful recompilation happened.
recompile :: Maybe ModuleTime -> String -> Maybe String -> IO (Bool, Maybe ModuleTime)
recompile lastAttempt prog mSrc = do
  let errFile = prog ++ ".errors"
      srcFile = fromMaybe (prog ++ ".hs") mSrc
  binT <- maybe (getModTime prog) (return . Just) lastAttempt
  srcT <- getModTime srcFile
  if (srcT > binT)
    then do
      putStr "Recompiling..."
      status <- bracket (openFile errFile WriteMode) hClose $ \h ->
        waitForProcess =<< runProcess "ghc" ["--make", srcFile]
                           Nothing Nothing Nothing Nothing (Just h)

      if (status /= ExitSuccess)
        then putStrLn "" >> putStrLn (replicate 75 '-') >> readFile errFile >>= putStr
        else putStrLn "done."

      curTime <- getModuleTime
      return (status == ExitSuccess, Just curTime)

    else return (False, Nothing)

 where getModTime f = catch (Just <$> getModificationTime f)
                            (\(SomeException _) -> return Nothing)
#endif
