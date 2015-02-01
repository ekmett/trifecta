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
