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
