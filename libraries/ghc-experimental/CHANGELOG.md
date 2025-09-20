# Revision history for ghc-experimental

## 9.1601.0

- New and/or/xor SIMD primops for bitwise logical operations, such as andDoubleX4#, orWord32X4#, xorInt8X16#, etc.
  These are supported by the LLVM backend and by the X86_64 NCG backend (for the latter, only for 128-wide vectors).

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
