## [v1.4.1.1](https://github.com/diagrams/diagrams-rasterific/tree/v1.4.1.1) (2018-10-12)

- Require `Rasterific-0.7.4` to
  - fix PDF rendering of zero-width paths ([#43](https://github.com/diagrams/diagrams-rasterific/issues/43))
  - fix disappearing small elements ([#51](https://github.com/diagrams/diagrams-rasterific/issues/51))

- Allow:
    - `base-4.12` (GHC 8.6)
    - `lens-4.17`
    - `containers-0.6`
    - `JuicyPixels-3.3`

## [v1.4.1](https://github.com/diagrams/diagrams-rasterific/tree/v1.4.1) (2018-05-09)

    - Bug fix: stroke entire paths at once ([#42](https://github.com/diagrams/diagrams-rasterific/issues/42))
    - New `Semigroup (Render Rasterific V2 n)` instance
    - Allow:
        - `base-4.11` (GHC 8.4)
        - `lens-4.16`

## [v1.4](https://github.com/diagrams/diagrams-rasterific/tree/v1.4) (2016-10-26)

* **New features**

    - Can now output PDFs.

    - Font files are now embedded with `file-embed`.

    - Various functions for better GIF support.

    - Support for group opacity.

* **New instances**

    - `ToResult` instance for animated GIFs.

## [v1.3.1.9](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.9) (2016-08-19)

- Require `optparse-applicative-0.13`
- Fix compilation error when building with `optparse-applicative-0.13`

## [v1.3.1.8](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.8) (2016-08-16)

- Allow `optparse-applicative-0.13`

## [v1.3.1.7](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.7) (2016-06-06)

- Allow:
    - `base-4.9`
    - `lens-4.14`
    - `data-default-class-0.1`

- Test with GHC 8.0.1

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v1.3.1.6...v1.3.1.7)

## [v1.3.1.6](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.6) (2016-05-01)

- allow `lens-4.14`

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v1.3.1.5...v1.3.1.6)

## [v1.3.1.5](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.5) (2015-09-29)

- allow `optparse-applicative-0.12`

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v1.3.1.4...v1.3.1.5)

## [v1.3.1.4](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.4) (2015-09-22)

- allow `lens-4.13`

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v1.3.1.3...v1.3.1.4)

## [v1.3.1.3](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.3) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v1.3.1.2...v1.3.1.3)

## [v1.3.1.2](https://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.2) (2015-05-26)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v1.3.1.1...v1.3.1.2)

## [v1.3.1.1](http://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.1)
(2015-04-25)

- New module `Diagrams.Backend.Rasterific.Text`
- New `texterific` function exported from
  `Diagrams.Backend.Rasterific`
- Fixes:
    - Fix bounding box calculation for text
    - Fix `BoxAlignedText` positioning

## [v1.3.1](http://github.com/diagrams/diagrams-rasterific/tree/v1.3.1.0) (2015-04-25)

- group opacity

## [v1.3](http://github.com/diagrams/diagrams-rasterific/tree/v1.3) (2015-04-19)

- bump version to 1.3

## [v0.1.0.8](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.8) (2015-04-03)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1.0.7...v0.1.0.8)

**Merged pull requests:**

- Generalise float [\#26](https://github.com/diagrams/diagrams-rasterific/pull/26) ([cchalmers](https://github.com/cchalmers))

## [v0.1.0.7](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.7) (2015-02-10)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1.0.6...v0.1.0.7)

**Merged pull requests:**

- Twinside master [\#25](https://github.com/diagrams/diagrams-rasterific/pull/25) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Preparing for float text size and PointSize new type in FontyFruity. [\#24](https://github.com/diagrams/diagrams-rasterific/pull/24) ([Twinside](https://github.com/Twinside))

## [v0.1.0.6](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.6) (2015-01-13)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1.0.5...v0.1.0.6)

**Merged pull requests:**

- Lower bound on hashable. [\#19](https://github.com/diagrams/diagrams-rasterific/pull/19) ([fryguybob](https://github.com/fryguybob))

- Hashable instance for Options. [\#18](https://github.com/diagrams/diagrams-rasterific/pull/18) ([acowley](https://github.com/acowley))

## [v0.1.0.5](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.5) (2014-12-07)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1.0.4...v0.1.0.5)

**Closed issues:**

- Bumping dependencies to Rasterific 0.4 [\#17](https://github.com/diagrams/diagrams-rasterific/issues/17)

**Merged pull requests:**

- Bumping JuicyPixels dependency [\#16](https://github.com/diagrams/diagrams-rasterific/pull/16) ([Twinside](https://github.com/Twinside))

## [v0.1.0.4](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.4) (2014-11-17)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1.0.3...v0.1.0.4)

**Closed issues:**

- q = max 0 \(min 100 quality\), shurely? [\#15](https://github.com/diagrams/diagrams-rasterific/issues/15)

**Merged pull requests:**

- Bump lens upper version bounds [\#14](https://github.com/diagrams/diagrams-rasterific/pull/14) ([RyanGlScott](https://github.com/RyanGlScott))

- New stuff [\#13](https://github.com/diagrams/diagrams-rasterific/pull/13) ([cchalmers](https://github.com/cchalmers))

- Diagram B [\#12](https://github.com/diagrams/diagrams-rasterific/pull/12) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Linear [\#10](https://github.com/diagrams/diagrams-rasterific/pull/10) ([cchalmers](https://github.com/cchalmers))

## [v0.1.0.3](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.3) (2014-10-08)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1.0.2...v0.1.0.3)

**Merged pull requests:**

- use defaultLoopRender for looping [\#9](https://github.com/diagrams/diagrams-rasterific/pull/9) ([bergey](https://github.com/bergey))

- call defaultAnimMainRender with ASetter for loop options [\#8](https://github.com/diagrams/diagrams-rasterific/pull/8) ([bergey](https://github.com/bergey))

## [v0.1.0.2](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.2) (2014-09-07)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1.0.1...v0.1.0.2)

## [v0.1.0.1](https://github.com/diagrams/diagrams-rasterific/tree/v0.1.0.1) (2014-08-22)

[Full Changelog](https://github.com/diagrams/diagrams-rasterific/compare/v0.1...v0.1.0.1)

## [v0.1](https://github.com/diagrams/diagrams-rasterific/tree/v0.1) (2014-06-02)

**Merged pull requests:**

- pixelMap promotePixel = promoteImage [\#6](https://github.com/diagrams/diagrams-rasterific/pull/6) ([Twinside](https://github.com/Twinside))

- fix text scaling [\#5](https://github.com/diagrams/diagrams-rasterific/pull/5) ([byorgey](https://github.com/byorgey))

- Gradient [\#4](https://github.com/diagrams/diagrams-rasterific/pull/4) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Image2 [\#3](https://github.com/diagrams/diagrams-rasterific/pull/3) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Image [\#2](https://github.com/diagrams/diagrams-rasterific/pull/2) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Units [\#1](https://github.com/diagrams/diagrams-rasterific/pull/1) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
