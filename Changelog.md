[0.12.0.0] — Unreleased

* [`readInt` returns `Nothing`, if the sequence of digits cannot be represented by an `Int`, instead of overflowing silently](https://github.com/haskell/bytestring/pull/309)
* [Remove `zipWith` rewrite rule](https://github.com/haskell/bytestring/pull/387)

[0.12.0.0]: https://github.com/haskell/bytestring/compare/0.11.2.0...0.12.0.0

[0.11.2.0] — December 2021

* [Add `Data.ByteString.isValidUtf8`](https://github.com/haskell/bytestring/pull/423)
* [Speed up `floatDec` and `doubleDec` using the Ryu algorithm](https://github.com/haskell/bytestring/pull/365)
  - `Data.ByteString.Builder.RealFloat` offers additional custom formatters
    for floating point numbers.
* [Add `StrictByteString` and `LazyByteString` type aliases](https://github.com/haskell/bytestring/pull/378)
* [Add `foldr'`, `foldr1'`, `scanl1`, `scanr`, `scanr1` to `Data.ByteString.Lazy{,.Char8}`](https://github.com/haskell/bytestring/pull/364)
* [Add `takeEnd`, `dropEnd`, `takeWhileEnd`, `dropWhileEnd`, `spanEnd`, `breakEnd` to `Data.ByteString.Lazy{,.Char8}`](https://github.com/haskell/bytestring/pull/395)
* [Add `Data.ByteString.Builder.writeFile` to write `Builder` to file directly](https://github.com/haskell/bytestring/pull/408)
* [Add `Data.ByteString.{from,to}FilePath` for encoding-aware conversions](https://github.com/haskell/bytestring/pull/403)
* [Add `Lift` instances for all flavors of `ByteString`](https://github.com/haskell/bytestring/pull/392)
* [Add `HasCallStack` for partial functions](https://github.com/haskell/bytestring/pull/440)
* [Define `foldl`, `foldl'`, `foldr`, `foldr'`, `mapAccumL`, `mapAccumR`, `scanl`, `scanr` and `filter` with one argument less to allow more inlining](https://github.com/haskell/bytestring/pull/345)
* [Speed up internal loop in `unfoldrN`](https://github.com/haskell/bytestring/pull/356)
* [Speed up `count` with SSE and AVX instructions](https://github.com/haskell/bytestring/pull/202)
* [Improve performance of certain `Builder`s by using a static table for Base16](https://github.com/haskell/bytestring/pull/418)
* [Use `unsafeWithForeignPtr` whenever possible](https://github.com/haskell/bytestring/pull/401)
* [Remove `integer-simple` flag](https://github.com/haskell/bytestring/pull/371)
* [Remove misleading mentions of fusion](https://github.com/haskell/bytestring/pull/412)

[0.11.2.0]: https://github.com/haskell/bytestring/compare/0.11.1.0...0.11.2.0

[0.11.1.0] — February 2021

* [Add `Data.ByteString.Char8.findIndexEnd` and `Data.ByteString.Lazy.Char8.{elemIndexEnd,findIndexEnd,unzip}`](https://github.com/haskell/bytestring/pull/342)
* [Expose `ShortByteString` constructor from `Data.ByteString.Short`](https://github.com/haskell/bytestring/pull/313)
* [Add `compareLength` function, which is lazier than comparison of lengths](https://github.com/haskell/bytestring/pull/300)
* [Add strict `takeEnd` and `dropEnd`](https://github.com/haskell/bytestring/pull/290)
* [Expose `packZipWith` to zip two `ByteString`](https://github.com/haskell/bytestring/pull/295)
* [Add `instance Show Builder`](https://github.com/haskell/bytestring/pull/296)
* [Improve lazy `pack` to carry fewer arguments in the inner loop](https://github.com/haskell/bytestring/pull/292)
* [Improve `map`, `findIndex` and `findIndexEnd` to carry fewer arguments in the inner loop](https://github.com/haskell/bytestring/pull/347)
* [Improve lazy `{take,drop}While`, `break` and `group{,By}` to carry fewer arguments in the inner loop](https://github.com/haskell/bytestring/pull/337)
* [Speed up `intersperse` using SSE2 instructions](https://github.com/haskell/bytestring/pull/310)
* [`fromShort` does not reallocate its argument, if it is pinned](https://github.com/haskell/bytestring/pull/317)
* [Speed up `words` using a faster test for spaces](https://github.com/haskell/bytestring/pull/315)
* [Implement `stimes` more efficiently than default definition](https://github.com/haskell/bytestring/pull/301)

[0.11.1.0]: https://github.com/haskell/bytestring/compare/0.11.0.0...0.11.1.0

[0.10.12.1] – January 2021

* [Replace `withForeignPtr` with `unsafeWithForeignPtr` where appropriate](https://github.com/haskell/bytestring/pull/333)

[0.10.12.1]: https://github.com/haskell/bytestring/compare/0.10.12.0...0.10.12.1

[0.11.0.0] — September 2020
 * [Change internal representation of `ByteString`, removing offset](https://github.com/haskell/bytestring/pull/175)
   * The old `PS` constructor has been turned into a pattern synonym that is available with GHC >= 8.0 for backwards compatibility. Consider adding `if !impl(ghc >=8.0) { build-depends: bytestring < 0.11 }` to packages, which use `PS` and still support GHC < 8.0.
 * [Fill `ForeignPtrContents` of `nullForeignPtr` with `FinalPtr` instead of a bottom](https://github.com/haskell/bytestring/pull/284)
 * [Remove deprecated functions `findSubstring` and `findSubstrings`](https://github.com/haskell/bytestring/pull/181)
 * [Speed up sorting of short strings](https://github.com/haskell/bytestring/pull/267)
 * [Improve handling of literal strings in `Data.ByteString.Builder`](https://github.com/haskell/bytestring/pull/132)
 * [Compute length at compile time for literal strings](https://github.com/haskell/bytestring/pull/191)
   * This improves optimization opportunities for functions that scrutinize the length of a `ByteString`.
 * [Add `indexMaybe` and synonym `(!?)` for indexing that returns `Maybe`](https://github.com/haskell/bytestring/pull/261)
 * [Add rewrite rules for `{take,drop}While ({=,/}= x)`](https://github.com/haskell/bytestring/pull/275)
 * [Add rewrite rules for `any (== x)` and `all (/= x)`](https://github.com/haskell/bytestring/pull/273)
 * [Add rewrite rules for `findInd{ex,ices} (== x)`](https://github.com/haskell/bytestring/pull/270)
 * [Improve folds to pass fewer arguments on each recursive invocation](https://github.com/haskell/bytestring/pull/273)
 * [Improve performance of `findIndices`](https://github.com/haskell/bytestring/pull/270)
 * [Re-export `Data.ByteString.Lazy.{from,to}Strict` from `Data.ByteString`](https://github.com/haskell/bytestring/pull/281)
 * [Remove deprecated modules and functions](https://github.com/haskell/bytestring/pull/286)
   * Use `Data.ByteString.Builder{,.Extra}` instead of `Data.ByteString.Lazy.Builder{,.ASCII,.Extras}`.
   * Use `Data.ByteString.Char8.{,h}putStrLn` instead of `Data.ByteString.{,h}putStrLn` and `Data.ByteString.Lazy.Char8.putStrLn` instead of `Data.ByteString.Char8.putStrLn`.
   * Use `Data.ByteString.break (== x)` instead of `Data.ByteString.breakByte x`.
   * Use `Data.ByteString.Internal.accursedUnutterablePerformIO` instead of `Data.ByteString.Internal.inlinePerformIO`.

[0.11.0.0]: https://github.com/haskell/bytestring/compare/0.10.12.0...0.11.0.0

[0.10.12.0] – August 2020

 * **Note:** There are several breaking changes planned to be included in v0.11.
   Please ensure that your packages have appropriate upper bounds on bytestring,
   in order to minimize avoidable breakage.
 * [Add `takeWhileEnd` and `dropWhileEnd` to `Data.ByteString` and `Data.ByteString.Char8`, and add `dropSpace` and `strip` to `Data.ByteString.Char8`](https://github.com/haskell/bytestring/pull/121)
 * [Add `findIndexEnd` to `Data.ByteString` and `Data.ByteString.Lazy`](https://github.com/haskell/bytestring/pull/155)
 * [Add `partition` to `Data.ByteString.Char8` and `Data.ByteString.Lazy.Char8`](https://github.com/haskell/bytestring/pull/251)
 * [Add `IsList` instances for strict and lazy `ByteString` and for `ShortByteString`](https://github.com/haskell/bytestring/pull/219)
 * [Add `createUptoN'` and `unsafeCreateUptoN'` to `Data.ByteString.Internal`](https://github.com/haskell/bytestring/pull/245)
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
