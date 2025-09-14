[0.13.0.0] — circa 2026-2027

* __Breaking Changes__:
  * [`Data.Data.dataTypeOf` for `StrictByteString` and `LazyByteString` now returns a `DataType` that uses `AlgRep` instead of `NoRep`.](https://github.com/haskell/bytestring/pull/614)
    * This allows utilities like `syb:Data.Generics.Text.gread` to be meaningfully used at these types containing `ByteString`s.
  * [`fromListN` in `instance IsList ByteString` truncates input list if it's longer than the size hint](https://github.com/haskell/bytestring/pull/672)

<!--
[minor version number] — Month Year

* Bug fixes:
* API additions and behavior changes:
* Deprecations:
* Performance improvements:
* Miscellaneous:
* Internal stuff:
-->

[0.12.3.0] — TBA 2025

* Bug fixes:
  * [Fix build in the presence of sketchy upstream CPP that triggers `-Wundef`](https://github.com/haskell/bytestring/pull/711)
    * (See [#665](https://github.com/haskell/bytestring/issues/665) and [#709](https://github.com/haskell/bytestring/issues/709).)
* API additions and behavior changes:
  * [`Data.ByteString.Short` now provides `lazyToShort` and `lazyFromShort`.](https://github.com/haskell/bytestring/pull/711)
* Deprecations:
* Performance improvements:
  * [Use only unsigned ints in the C itoa functions](https://github.com/haskell/bytestring/pull/702)
  * [Remove a dead branch in `integerDec`](https://github.com/haskell/bytestring/pull/702)
* Miscellaneous:
  * [Remove useless INLINE on `compareLength` for lazy bytestrings.](https://github.com/haskell/bytestring/pull/705)
  * [Drop support for GHC < 8.6](https://github.com/haskell/bytestring/pull/710)
  * Various documentation improvements ([1](https://github.com/haskell/bytestring/pull/710), [2](https://github.com/haskell/bytestring/pull/713))
<!--
* Internal stuff:
  * Various CI tweaks ([1](https://github.com/haskell/bytestring/pull/703), [2](https://github.com/haskell/bytestring/pull/706))
-->

[0.12.2.0] — October 2024

* Bug fixes:
  * [`Builder`: avoid unsound buffer reuse, introduced in `bytestring-0.11.5.0`](https://github.com/haskell/bytestring/pull/691)
  * [Fix several bugs around the `byteString` family of `Builders`](https://github.com/haskell/bytestring/pull/671)
  * [Make `Data.ByteString.Lazy.zipWith` properly lazy](https://github.com/haskell/bytestring/pull/668)
* API additions:
  * [Add `instance IsList Builder`](https://github.com/haskell/bytestring/pull/672)
  * [Add `instance NFData BufferRange` and `instance NFData Buffer`](https://github.com/haskell/bytestring/pull/680)
  * [Export `toLazyByteString` from `Data.ByteString.Builder.Internal`](https://github.com/haskell/bytestring/pull/672)
* Performance improvements:
  * [Remove another dead branch from `toStrict`](https://github.com/haskell/bytestring/pull/663)
* Miscellaneous:
  * [Remove support for GHC < 8.4](https://github.com/haskell/bytestring/pull/682)
  * Various documentation improvements ([1](https://github.com/haskell/bytestring/pull/683), [2](https://github.com/haskell/bytestring/pull/692))
<!--
* Internal stuff:
  * Various CI tweaks ([1](https://github.com/haskell/bytestring/pull/670), [2](https://github.com/haskell/bytestring/pull/681), [3](https://github.com/haskell/bytestring/pull/686), [4](https://github.com/haskell/bytestring/pull/656), [5](https://github.com/haskell/bytestring/pull/693), [6](https://github.com/haskell/bytestring/pull/699), [7](https://github.com/haskell/bytestring/pull/700))
  * [Use `default-extensions` to tidy up a bit](https://github.com/haskell/bytestring/pull/669)
  * [Remove `includes` from Cabal file](https://github.com/haskell/bytestring/pull/685)
  * [Improve benchmarks for small `Builders`](https://github.com/haskell/bytestring/pull/680)
  * [Add a constraint reflecting](https://github.com/haskell/bytestring/pull/698) [#665](https://github.com/haskell/bytestring/issues/665) [to the package description](https://github.com/haskell/bytestring/pull/698)
-->

[0.12.2.0]: https://github.com/haskell/bytestring/compare/0.12.1.0...0.12.2.0

[0.12.1.0] — February 2024

* [Provisional support has been added for using `bytestring` with GHC's JavaScript back-end.](https://github.com/haskell/bytestring/pull/631)
  * This support is relatively un-tested and un-optimised. There may be bugs! Please report any you discover to [`bytestring`'s issue tracker](https://github.com/haskell/bytestring/issues).
  * The JavaScript back-end's limited support for the Haskell-C foreign function interface would previously result in many operations failing with errors like `ReferenceError: h$fps_count is not defined`.
  * The new `pure-haskell` package flag allows the new fallback Haskell implementations (used to support the JavaScript backend) to be used on most other platforms as well.
* Bug fixes:
  * [`stimes 0 sbs :: ShortByteString` now returns the empty `ShortByteString` instead of throwing an exception](https://github.com/haskell/bytestring/pull/611)
  * [`stimes 0 b :: Builder` now returns the empty `Builder` instead of throwing an exception](https://github.com/haskell/bytestring/pull/611)
  * [Several alignment-related bug fixes](https://github.com/haskell/bytestring/pull/587)
  * [Fix a bug in `isValidUtf8`](https://github.com/haskell/bytestring/pull/621)
  * [`sconcat @ShortByteString` is no longer terribly inefficient](https://github.com/haskell/bytestring/pull/650)
  * [Fix the type on the foreign import used for `Data.ByteString.Short.elemIndex`](https://github.com/haskell/bytestring/pull/661)
  * [Ensure that the result of `fromShort` is protected by `mkDeferredByteString`](https://github.com/haskell/bytestring/pull/662)
* Behavior changes:
  * [The `Data.Data.Data` instances for `StrictByteString` and `LazyByteString` have been changed:](https://github.com/haskell/bytestring/pull/614)
    * `toConstr` now returns the a `pack` pseudo-constructor instead of throwing an exception.
    * Due to this pseudo-constructor, `gunfold` can now be meaningfully used at these types. (Previously, it would always raise an exception.)
    * These changes allow `syb:Data.Generics.Text.gshow` to be meaningfully used at types containing `ByteString`s.
  * [A derived `instance Generic ShortByteString` has been added.](https://github.com/haskell/bytestring/pull/662)
  * [`sconcat @Builder` is now lazy in the tail of its input](https://github.com/haskell/bytestring/pull/650)
* Deprecations:
  * [`Data.ByteString.Builder.Prim.Internal.storableToF`](https://github.com/haskell/bytestring/pull/649)
* Performance improvements:
  * Various raw-binary `Builder` primitives like `intHost` or `word32BE` are much less inefficient on architectures not known to support unaligned writes. ([1](https://github.com/haskell/bytestring/pull/587), [2](https://github.com/haskell/bytestring/pull/645))
  * [Hexadecimal encoding suffers one indirection fewer](https://github.com/haskell/bytestring/pull/624)
  * [`Data.ByteString.Lazy.takeEnd` is somewhat faster](https://github.com/haskell/bytestring/pull/629)
  * [`Data.ByteString.Lazy.dropEnd` is much faster](https://github.com/haskell/bytestring/pull/629)
* Miscellaneous:
  * Various documentation improvements ([1](https://github.com/haskell/bytestring/pull/628), [2](https://github.com/haskell/bytestring/pull/609), [3](https://github.com/haskell/bytestring/pull/612), [4](https://github.com/haskell/bytestring/pull/623), [5](https://github.com/haskell/bytestring/pull/654))
  * [Eta-expand `Data.ByteString.Builder.Internal.empty`](https://github.com/haskell/bytestring/pull/616)
    * This can variously help or hurt performance; it undoes the performance changes caused by [CLC proposal 132](https://github.com/haskell/core-libraries-committee/issues/132) with ghc-9.8 and restores the baseline performance seen with older GHCs.
<!--
* Internal stuff:
  * [Delete cabal.project](https://github.com/haskell/bytestring/pull/613)
  * Remove some non-exposed data declarations from internal modules:
    * [`Data.ByteString.Short.Internal.BA`](https://github.com/haskell/bytestring/pull/615)
    * [`Data.ByteString.Short.Internal.MBA`](https://github.com/haskell/bytestring/pull/617)
  * Various CI tweaks ([1](https://github.com/haskell/bytestring/pull/626), [2](https://github.com/haskell/bytestring/pull/651))
  * [Use `NonEmpty` to prune dead code in `integerDec`](https://github.com/haskell/bytestring/pull/655)
    * This might have a performance impact due to result unboxing (CPR).
  * [Consolidate internal CPP for byte-order/endianness](https://github.com/haskell/bytestring/pull/659)
  * [Remove remaining uses of FFI under -fpure-haskell](https://github.com/haskell/bytestring/pull/660)
    * Doesn't warrant a separate visible changelog entry from #631.
-->

[0.12.1.0]: https://github.com/haskell/bytestring/compare/0.12.0.2...0.12.1.0

[0.12.0.2] — August 2023

* Bug fixes:
  * [Fix `clockid_t`-related build failures on some platforms](https://github.com/haskell/bytestring/pull/607)

[0.12.0.2]: https://github.com/haskell/bytestring/compare/0.12.0.1...0.12.0.2

[0.12.0.1] — August 2023

* Bug fixes:
  * [Work around a GHC runtime linker issue on i386/PowerPC](https://github.com/haskell/bytestring/pull/604)

[0.12.0.1]: https://github.com/haskell/bytestring/compare/0.12.0.0...0.12.0.1

[0.12.0.0] — July 2023

* __Breaking Changes__:
  * [`readInt` returns `Nothing`, if the sequence of digits cannot be represented by an `Int`, instead of overflowing silently](https://github.com/haskell/bytestring/pull/309)
  * [Remove `zipWith` rewrite rule](https://github.com/haskell/bytestring/pull/387)
  * [`ShortByteString` is now a wrapper around `Data.Array.Byte.ByteArray` instead of `ByteArray#` directly](https://github.com/haskell/bytestring/pull/410)
    * As a compatibility measure, `SBS` remains available as a pattern synonym.
    * The compatibility package `data-array-byte` is used when `base` does not provide `Data.Array.Byte`.
  * [`fromListN` from `instance IsList ShortByteString` now throws an exception if the first argument does not match the length of the second](https://github.com/haskell/bytestring/pull/410)
    * Previously, it would ignore the first argument entirely.
* Bug fixes:
  * Size-related calculations are more resistant to `Int` overflow in the following places:
    * [`Data.ByteString.intercalate`](https://github.com/haskell/bytestring/pull/468)
    * [`stimes @StrictByteString`](https://github.com/haskell/bytestring/pull/443)
    * [`Data.ByteString.Short.concat`](https://github.com/haskell/bytestring/pull/443)
    * [`Data.ByteString.Short.append`](https://github.com/haskell/bytestring/pull/443)
    * [`Data.ByteString.Short.snoc`](https://github.com/haskell/bytestring/pull/599)
    * [`Data.ByteString.Short.cons`](https://github.com/haskell/bytestring/pull/599)
* API additions:
  * [New sized and/or unsigned variants of `readInt` and `readInteger`](https://github.com/haskell/bytestring/pull/438)
  * [`Data.ByteString.Internal` now provides `SizeOverflowException`, `overflowError`, and `checkedMultiply`](https://github.com/haskell/bytestring/pull/443)
* Deprecations:
  * `Data.ByteString.getLine`: prefer `Data.ByteString.Char8.getLine`
  * `Data.ByteString.hGetLine`: prefer `Data.ByteString.Char8.hGetLine`


[0.12.0.0]: https://github.com/haskell/bytestring/compare/0.11.5.0...0.12.0.0

[0.11.5.3] — October 2023

* Bug fixes:
  * [Fix a bug in `isValidUtf8`](https://github.com/haskell/bytestring/pull/621)

[0.11.5.3]: https://github.com/haskell/bytestring/compare/0.11.5.2...0.11.5.3

[0.11.5.2] — August 2023

* Bug fixes:
  * [Fix `clockid_t`-related build failures on some platforms](https://github.com/haskell/bytestring/pull/607)

[0.11.5.2]: https://github.com/haskell/bytestring/compare/0.11.5.1...0.11.5.2

[0.11.5.1] — August 2023

* Bug fixes:
  * [Work around a GHC runtime linker issue on i386/PowerPC](https://github.com/haskell/bytestring/pull/604)

[0.11.5.1]: https://github.com/haskell/bytestring/compare/0.11.5.0...0.11.5.1

[0.11.5.0] — July 2023

* Bug fixes:
  * [Fix multiple bugs with ASCII blocks in the SIMD implementations for `isValidUtf8`](https://github.com/haskell/bytestring/pull/582)
  * [Prevent unsound optimizations with the `Data.ByteString.Internal.create*` family of functions](https://github.com/haskell/bytestring/pull/580)
* API additions:
  * [`Data.ByteString.Internal` now provides `mkDeferredByteString` and `deferForeignPtrAvailability`](https://github.com/haskell/bytestring/pull/580)
* Deprecations:
  * `Data.ByteString.Internal.memcpy`: prefer `Foreign.Marshal.Utils.copyBytes`
  * `Data.ByteString.Internal.memset`: prefer `Foreign.Marshal.Utils.fillBytes`
* Performance improvements:
  * [Many functions returning `StrictByteString` can now return their results unboxed](https://github.com/haskell/bytestring/pull/580)
  * [Dead branches removed from `Lazy.toStrict`](https://github.com/haskell/bytestring/pull/590)
  * [`Builder.toLazyByteString` re-uses under-filled buffers after copying their contents](https://github.com/haskell/bytestring/pull/581)
* Miscellaneous:
  * [Minor benchmarking improvements](https://github.com/haskell/bytestring/pull/577)
<!--
* Internal stuff:
  * Various CI tweaks ([1](https://github.com/haskell/bytestring/pull/571), [2](https://github.com/haskell/bytestring/pull/565), [3](https://github.com/haskell/bytestring/pull/583), [4](https://github.com/haskell/bytestring/pull/584))
  * [`accursedUnutterablePerformIO`'s trail of destruction extended](https://github.com/haskell/bytestring/pull/579)
  * [Add type signatures for subfunction of `buildStepToCIOS`](https://github.com/haskell/bytestring/pull/586)
  * [`foldl'`-related import list tweaks](https://github.com/haskell/bytestring/pull/585)
-->

[0.11.5.0]: https://github.com/haskell/bytestring/compare/0.11.4.0...0.11.5.0

[0.11.4.0] — January 2023

* Bug fixes:
  * [Prevent commoning-up of `ShortByteString` literals produced by `TemplateHaskell`](https://github.com/haskell/bytestring/pull/542)
  * [Make `Builder` literals demand a sane amount of memory at chunk boundaries](https://github.com/haskell/bytestring/pull/538)
* API additions and behavior changes:
  * [Export `unsafeIndex` for `ShortByteString` which had been accidentally removed in v0.11.3.0](https://github.com/haskell/bytestring/pull/532)
  * [Make `Data.ByteString.Lazy.Char8.lines` less strict](https://github.com/haskell/bytestring/pull/562)
  * [Add `NonEmpty` variants of `inits` and `tails`](https://github.com/haskell/bytestring/pull/557)
* Performance improvements:
  * [Speed up `unpack` and folds for `ShortByteString`](https://github.com/haskell/bytestring/pull/526)
  * [Speed up `Builder`s for non-host endianness](https://github.com/haskell/bytestring/pull/531)
  * [Work around upstream `keepAlive#` performance regressions](https://github.com/haskell/bytestring/pull/536)
  * [Improve performance of `uncons` for `LazyByteString`](https://github.com/haskell/bytestring/pull/559)
  * [Simplify `useAsCString`](https://github.com/haskell/bytestring/pull/516)
  * [Remove redundant comparisons in `Data.ByteString.Short.splitAt`](https://github.com/haskell/bytestring/pull/528)
* Miscellaneous:
  * [Document possible interleaving of `hPutStrLn` and friends](https://github.com/haskell/bytestring/pull/518)
  * [Documentation tweaks](https://github.com/haskell/bytestring/pull/523)
  * [Add lower bound for `tasty-quickcheck`](https://github.com/haskell/bytestring/pull/520)
<!--
* Internal stuff:
  * Various CI tweaks ([1](https://github.com/haskell/bytestring/pull/539), [2](https://github.com/haskell/bytestring/pull/550), [3](https://github.com/haskell/bytestring/pull/551), [4](https://github.com/haskell/bytestring/pull/563), [5](https://github.com/haskell/bytestring/pull/566), [6](https://github.com/haskell/bytestring/pull/568))
  * [Avoid pattern-matching with `SBS`, for consistency with master](https://github.com/haskell/bytestring/pull/556)
  * [Avoid `Prelude.head` and `Prelude.tail`](https://github.com/haskell/bytestring/pull/553)
-->

[0.11.4.0]: https://github.com/haskell/bytestring/compare/0.11.3.1...0.11.4.0

[0.11.3.1] — May 2022

* [Windows: Do not link against `gcc_s`](https://github.com/haskell/bytestring/pull/500)
* [Windows: Do not link against `gcc`  when GHC >= 9.4](https://github.com/haskell/bytestring/pull/512)
* [Refine CPP for obsolete versions of `gcc`](https://github.com/haskell/bytestring/pull/505)

[0.11.3.1]: https://github.com/haskell/bytestring/compare/0.11.3.0...0.11.3.1

[0.11.3.0] — February 2022

Erratum: `unsafeIndex` was accidentally removed from the export list of `Data.ByteString.Short.Internal` in this release. This was corrected in 0.11.4.0.

* [Enhance `ShortByteString` API](https://github.com/haskell/bytestring/pull/471)
  - Add `all`, `any`, `append`, `break`, `breakEnd`, `breakSubstring`, `concat`, `cons`, `count`, `drop`, `dropEnd`, `dropWhile`, `dropWhileEnd`, `elem`, `elemIndex`, `elemIndices`, `filter`, `find`, `findIndex`, `findIndices`, `foldl'`, `foldl`, `foldl1'`, `foldl1`, `foldr'`, `foldr`, `foldr1'`, `foldr1`, `head`, `init`, `intercalate`, `isInfixOf`, `isPrefixOf`, `isSuffixOf`, `last`, `map`, `partition`, `replicate`, `reverse`, `singleton`, `snoc`, `span`, `spanEnd`, `split`, `splitAt`, `splitWith`, `stripPrefix`, `stripSuffix`, `tail`, `take`, `takeEnd`, `takeWhile`, `takeWhileEnd`, `uncons`, `unfoldr`, `unfoldrN`, `unsnoc` to `Data.ByteString.Short`.
* [Add `Data.ByteString.Short.isValidUtf8`](https://github.com/haskell/bytestring/pull/450)
* [Use safe `isValidUtf8` for large inputs](https://github.com/haskell/bytestring/pull/470)
* [Make `unlines` lazier](https://github.com/haskell/bytestring/pull/477)
* [Improve performance of `unlines`](https://github.com/haskell/bytestring/pull/479)
* [Make `singleton` return a slice of a static buffer](https://github.com/haskell/bytestring/pull/480)
* [Improve performance of `intercalate`](https://github.com/haskell/bytestring/pull/459)

[0.11.3.0]: https://github.com/haskell/bytestring/compare/0.11.2.0...0.11.3.0

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
