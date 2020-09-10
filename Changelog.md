[0.11.0.0] — September 2020
 * [Change internal representation of `ByteString`, removing offset](https://github.com/haskell/bytestring/pull/175)
   * The old `PS` constructor has been turned into a pattern synonym that is available with GHC >= 8.0 for backwards compatibility.
 * [Remove deprecated functions `findSubstring` and `findSubstrings`](https://github.com/haskell/bytestring/pull/181)
 * [Speed up sorting of short strings](https://github.com/haskell/bytestring/pull/267)
 * [Improve handling of literal strings in `Data.ByteString.Builder`](https://github.com/haskell/bytestring/pull/132)
 * [Compute length at compile time for literal strings](https://github.com/haskell/bytestring/pull/191)
   * This improves optimization opportunities for functions that scrutinize the length of a `ByteString`.
 * [Add `indexMaybe` and synonym `(!?)` for indexing that returns `Maybe`](https://github.com/haskell/bytestring/pull/261)
 * [Add rewrite rules for `{take,drop}While ({=,/}= x)`](https://github.com/haskell/bytestring/pull/275)
 * [Add rewrite rules for `any (== x)` and `all (/= x)`](https://github.com/haskell/bytestring/pull/273)
 * [Add rewrite rules for `findInd{ex,ices} (== x)`](https://github.com/haskell/bytestring/pull/270)
 * [Improve folds to pass less argument on each recursive invocation](https://github.com/haskell/bytestring/pull/273)
 * [Improve performance of `findIndices`](https://github.com/haskell/bytestring/pull/270)

[0.11.0.0]: https://github.com/haskell/bytestring/compare/0.10.12.0...0.11.0.0

[0.10.12.0] – August 2020

 * **Note:** There are several breaking changes planned to be included in v0.11.
   Please ensure that your packages have appropriate upper bounds on bytestring,
   in order to minimize avoidable breakage.
 * [Add `takeWhileEnd` and `dropWhileEnd` to `Data.ByteString` and `Data.ByteString.Char8`, and add `dropSpace` and `strip` to `Data.ByteString.Char8`](https://github.com/haskell/bytestring/pull/121)
 * [Add `findIndexEnd` to `Data.ByteString` and `Data.ByteString.Lazy`](https://github.com/haskell/bytestring/pull/155)
 * [Add `partition` to `Data.ByteString.Char8` and `Data.ByteString.Lazy.Char8`](https://github.com/haskell/bytestring/pull/251)
 * [Add `IsList` instances for strict and lazy `ByteString` and for `ShortByteString`](https://github.com/haskell/bytestring/pull/219)
 * [Add `createUpToN'` and `unsafeCreateUpToN'` to `Data.ByteString.Internal`](https://github.com/haskell/bytestring/pull/245)
 * [Add `boundedPrim` to `Data.ByteString.Builder.Prim.Internal` and deprecate `boudedPrim`](https://github.com/haskell/bytestring/pull/246)
 * [Deprecate the `Data.ByteString.Lazy.Builder` and `Data.ByteString.Lazy.Builder.{ASCII,Extras}` modules](https://github.com/haskell/bytestring/pull/250)
 * [Fix documented complexity of `Data.ByteString.Lazy.length`](https://github.com/haskell/bytestring/pull/255)
 * [Assorted documentation fixes](https://github.com/haskell/bytestring/pull/248)

[0.10.12.0]: https://github.com/haskell/bytestring/compare/0.10.10.1...0.10.12.0

0.10.10.1 – June 2020

 * Fix off-by-one infinite loop in primMapByteStringBounded ([#203])
 * Don't perform unaligned writes when it isn't known to be safe ([#133])
 * Improve the performance of sconcat for lazy and strict bytestrings ([#142])
 * Document inadvertent 0.10.6.0 behaviour change in findSubstrings
 * Fix benchmark builds ([#52])
 * Documentation fixes
 * Test fixes

[#52]: https://github.com/haskell/bytestring/issues/52
[#133]: https://github.com/haskell/bytestring/pull/133
[#142]: https://github.com/haskell/bytestring/pull/142
[#203]: https://github.com/haskell/bytestring/issues/203

0.10.10.0 July 2019 <duncan+haskell@dcoutts.me.uk> July 2019

 * Build with GHC 8.8, and tests with QC 2.10+
 * Add conversions between ShortByteString and CString (#126)
 * Documentation fixes (#65, #118, #144, #150, #152, #172)
 * Resolve potential copyright issue with test data (#165)

0.10.8.2 Duncan Coutts <duncan@community.haskell.org> Feb 2017

 * Make readFile work for files with no size like /dev/null
 * Extend the cases in which concat and toStrict can avoid copying data
 * Fix building with ghc-7.0
 * Minor documentation improvements
 * Internal code cleanups

0.10.8.1 Duncan Coutts <duncan@community.haskell.org> May 2016

 * Fix Builder output on big-endian architectures
 * Fix building with ghc-7.6 and older

0.10.8.0 Duncan Coutts <duncan@community.haskell.org> May 2016

 * Use Rabin-Karp substring search for `breakSubstring` and `findSubstring`
 * Improve the performance of `partition` for lazy and strict bytestrings
 * Added `stripPrefix` and `stripSuffix` for lazy and strict bytestrings
 * Fix building with ghc 8.0 & base 4.9 (Semigroup etc)

0.10.6.0 Duncan Coutts <duncan@community.haskell.org> Mar 2015

 * Rename inlinePerformIO so people don't misuse it
 * Fix a corner case in unfoldrN
 * Export isSuffixOf from D.B.Lazy.Char8
 * Add D.B.Lazy.elemIndexEnd
 * Fix readFile for files with incorrectly reported file size
 * Fix for builder performance with ghc 7.10
 * Fix building with ghc 6.12

0.10.4.1 Duncan Coutts <duncan@community.haskell.org> Nov 2014

 * Fix integer overflow in concatenation functions
 * Fix strictness of lazy bytestring foldl'
 * Numerous minor documentation fixes
 * Various testsuite improvements
