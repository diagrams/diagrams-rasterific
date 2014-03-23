_diagrams-rasterific_ is a an Rasterific backend for [diagrams]. Diagrams is a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell]. It supports png, tif, bmp and
animated GIF output.

The Rasterific backend is a work in progress, and may contain bugs and lack
some features. These should all be fixed as both Rasterific and diagrams
evolve.

- line widths wont scale with image size (use approx 10X the norm). This will
  be fixed once the units branch is merged.
- No support for images.
- No fill rules.
- No Dash offset

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell
