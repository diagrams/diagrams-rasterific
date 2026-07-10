{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Diagrams.Prelude
import           Diagrams.Backend
import           Geometry
import           Diagrams.Backend.Rasterific

dia :: Diagram V2
dia = circle 35 # fc red
   <> regPoly 16 200 # fc blue
   <> regPoly 5 400 # fc orange # rotateBy (1/6)

main :: IO ()
main = mainWith Rasterific (frame 2 dia)
