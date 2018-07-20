import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific

{- non-square diagram should yield non-square output -}

dia :: Diagram B
dia = ellipseXY 1 1.5

sizeSpec :: SizeSpec V2 Double
sizeSpec = mkSizeSpec2D (Just 200) Nothing

sizeSpecBoth :: SizeSpec V2 Double
sizeSpecBoth = mkSizeSpec2D (Just 200) (Just 300)

main :: IO ()
main = do
  renderRasterific "test-size.png" sizeSpec dia
  renderRasterific "test-size.pdf" sizeSpec dia
  renderRasterific "test-size-both.png" sizeSpecBoth dia
  renderRasterific "test-size-both.pdf" sizeSpecBoth dia
