_diagrams-rasterific_ is a an Rasterific backend for [diagrams]. Diagrams is a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

The Rasterific backend is a work in progress, in particular

- line widths wont scale with image size (use approx 10X the norm). This will
  be fixed once the units branch is merged.
- No support for text.
- No support for images.
- No fill rules.
- No multiMain, animMain or GIF animation

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell