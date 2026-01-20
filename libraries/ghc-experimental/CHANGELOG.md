# Revision history for ghc-experimental

## 10.001.0

- New and/or/xor SIMD primops for bitwise logical operations, such as andDoubleX4#, orWord32X4#, xorInt8X16#, etc.
  These are supported by the LLVM backend and by the X86_64 NCG backend (for the latter, only for 128-wide vectors).

## ghc-experimental-9.1402.0

- Add optional `SrcLoc` to `StackAnnotation` class in `GHC.Stack.Annotation.Experimental`

## ghc-experimental-9.1401.0

- Expose access to RTS flags via `GHC.RTS.Flags.Experimental`
- Expose access to era profiling interface via `GHC.Profiling.Eras`
- Expose access to runtime stack annotations via `GHC.Stack.Annotation.Experimental`
- Expose custom allocation limit handler via `System.Mem.Experimental`
- Expose access to Stack Annotations via `GHC.Stack.Annotation.Experimental`
- Expose module Prelude.Experimental, which reexports some modules from ghc-experimental for convenience, like Prelude does for base.

## ghc-experimental-9.1201.0

- Expose `GHC.TypeLits.Experimental` and `GHC.TypeNats.Experimental`

## ghc-experimental-9.1002.0

- Expose primops via `GHC.PrimOps`

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
