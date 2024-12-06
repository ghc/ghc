# Changelog for `ghc-bignum` package

## 1.4

- `ghc-bignum`'s implementation has been merged into `ghc-internal`.
  Downstream users should import `GHC.Num.{Integer,Natural,BigNat}` stable
  modules from `base` instead.

## 1.3

- Expose backendName
- Add `naturalSetBit[#]` (#21173), `naturalClearBit[#]` (#21175), `naturalComplementBit[#]` (#21181)

## 1.2

- Moved naturalToDouble# and naturalToFloat# to `base` package

## 1.1

- Moved integerToDouble# and integerToFloat# to `base` package with fixed
  rounding (#15926, #17231, #17782)
- Added Eq and Ord instances for BigNat (#19647)
- Bump in-tree GMP to version 6.2.1
- Support for autoconf >= 2.70 especially on Windows (#19189)
- Fixed extra-source-files in .cabal file
- Fixed incorrect declaration in .hs-boot file (#19638)
