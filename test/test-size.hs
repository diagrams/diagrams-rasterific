import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific

{- non-square diagram should yield non-square output -}

dia :: Diagram B
dia = roundedRect 2 1 0.2 === strutY 0.2

sizeSpec :: SizeSpec V2 Double
sizeSpec = mkSizeSpec2D (Just 600) Nothing

sizeSpecBoth :: SizeSpec V2 Double
sizeSpecBoth = mkSizeSpec2D (Just 600) (Just 500)

main :: IO ()
main = do
  renderRasterific "test-size.png" sizeSpec dia
  renderRasterific "test-size.pdf" sizeSpec dia
  renderRasterific "test-size-both.png" sizeSpecBoth dia
  renderRasterific "test-size-both.pdf" sizeSpecBoth dia
