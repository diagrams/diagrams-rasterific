import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific

dia :: Diagram B
dia = bgFrame 1 white $
  vsep 0.5
    [ l # lw thick
    , l # lw thin
    , l # lwG 0
    ]
  where
    l = stroke (straight unitX `at` origin)

sizeSpec :: SizeSpec V2 Double
sizeSpec = mkSizeSpec2D (Just 600) Nothing

main :: IO ()
main = do
  renderRasterific "test1.png" sizeSpec dia
  renderRasterific "test1.pdf" sizeSpec dia
