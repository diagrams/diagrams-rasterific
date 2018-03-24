import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific

dia :: Diagram B
dia = circle 35 # fc red
   <> regPoly 16 200 # fc blue
   <> regPoly 5 400 # fc orange # rotateBy (1/6)

sizeSpec :: SizeSpec V2 Double
sizeSpec = mkSizeSpec2D (Just 600) Nothing

main :: IO ()
main = renderRasterific "test0.png" sizeSpec dia
  --(dia # translateX 0 # translateY (-300))
