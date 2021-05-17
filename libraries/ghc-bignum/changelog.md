# Changelog for `ghc-bignum` package

## 1.1

- Moved integerToDouble# and integerToFloat# to `base` package with fixed
  rounding (#15926, #17231, #17782)
- Added Eq and Ord instances for BigNat (#19647)
- Bump in-tree GMP to version 6.2.1
- Support for autoconf >= 2.70 especially on Windows (#19189)
- Fixed extra-source-files in .cabal file
- Fixed incorrect declaration in .hs-boot file (#19638)
