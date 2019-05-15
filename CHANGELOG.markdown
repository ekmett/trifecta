next [????.??.??]
-----------------
* Support building with `base-4.13` (GHC 8.8).
* Print line numbers in the gutter.

2 [2018.07.03]
--------------
* `stepParser` no longer takes a `ByteString`.
* Add a `Text.Trifecta.Tutorial` module, as well as lots of documentation.
* Add a `foldResult` function to `Text.Trifecta.Result`.
* Allow building with `containers-0.6`.

1.7.1.1
-------
* Support `ansi-wl-pprint-0.6.8`

1.7.1
-----
* Support `doctest-0.12`

1.7
---
* Make `trifecta` forward `-Wcompat`ible:
  * Adding `Semigroup` instances to correspond to every existing `Monoid`
    instance. This requires adding a `Semigroup` constraint to the `Monoid`
    instance for `Parser` to emulate the `Semigroup`-`Monoid` superclass
    relation that will be present in future versions of GHC.
  * Adding a `MonadFail` instance for `Parser`
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

1.6.2.1
-------
* Add this changelog to the `extra-souce-files` in `trifecta.cabal` so that the
  changelog will appear on Hackage

1.6.2
-----
* Enable support for `blaze-html-0.9` and `blaze-markup-0.8`

1.6.1
-----
* Remove redundant constraints from `DeltaParsing`'s class methods. This is
  required for `trifecta` to build on GHC 8.0.2.

1.6
-----
* Version bumps to support GHC 8
* Add line/col numbers to parse results by giving a list of all deltas when errors happen.

1.5.2
-----
* `lens` 4.13 support
* `It` is a `Profunctor`
* Builds clean on GHC 7.10.

1.5.1.3
-------
* Support newer `utf8-string` versions and GHC 7.10

1.5.1.2
-------
* Work around lack of the old `preEscapedString` export in near-current `blaze-markup`.

1.5.1.1
-------
* Work around new exports in `blaze`.

1.5.1
-----
* Parsers 0.12.1 support. This removes many `Show` constraints introduced after 1.4

1.5
-----
* Properly PVP compliant point release for the `parsers` changes to properly handle `notFollowedBy`

1.4.3
-----
* Accidentally non-PVP compliant point release.

1.4.1
-----
* GHC 7.8.1 compatibility

1.4
---
* Simplified AsResult
* `lens` 4.0 compatibility

1.2.1.1
-------
* Updated `array` dependency for compatibility with GHC 7.8

1.2.1
-----
* Bug fix for the `Monoid` instance in response to [issue #15](https://github.com/ekmett/trifecta/issues/14)
* Made the `Semigroup` instance match the `Monoid` as well.

1.2
---
* Changed the `Monoid` instance for `Parser` in response to [issue #14](https://github.com/ekmett/trifecta/issues/14)
* Exported `MonadErr` class for raising `Err`s
