# Changelog for [`base` package](http://hackage.haskell.org/package/base)

## 4.7.1.0 *TBA*

  * Bundled with GHC 7.10.1

  * Add reverse application operator `Data.Function.(&)`

  * Add `Data.List.sortOn` sorting function

  * Add `System.Exit.die`

  * Weaken RealFloat constraints on some `Data.Complex` functions

  * Add `Control.Monad.(<$!>)` as a strict version of `(<$>)`

## 4.7.0.1  *Jul 2014*

  * Bundled with GHC 7.8.3

  * Unhide `Foreign.ForeignPtr` in Haddock (#8475)

  * Fix recomputation of `TypeRep` in `Typeable` type-application instance
    (#9203)

  * Fix regression in Data.Fixed Read instance (#9231)

  * Fix `fdReady` to honor `FD_SETSIZE` (#9168)

## 4.7.0.0  *Apr 2014*

  * Bundled with GHC 7.8.1

  * Add `/Since: 4.[4567].0.0/` Haddock annotations to entities
    denoting the package version, when the given entity was introduced
    (or its type signature changed in a non-compatible way)

  * The `Control.Category` module now has the `PolyKinds` extension
    enabled, meaning that instances of `Category` no longer need be of
    kind `* -> * -> *`.

  * There are now `Foldable` and `Traversable` instances for `Either a`,
   `Const r`, and `(,) a`.

  * There is now a `Monoid`, `Generic`, and `Generic1` instance for `Const`.

  * There is now a `Data` instance for `Data.Version`.

  * A new `Data.Bits.FiniteBits` class has been added to represent
    types with fixed bit-count. The existing `Bits` class is extended
    with a `bitSizeMaybe` method to replace the now obsolete
    `bitsize` method.

  * `Data.Bits.Bits` gained a new `zeroBits` method which completes the
    `Bits` API with a direct way to introduce a value with all bits cleared.

  * There are now `Bits` and `FiniteBits` instances for `Bool`.

  * There are now `Eq`, `Ord`, `Show`, `Read`, `Generic`. and `Generic1`
    instances for `ZipList`.

  * There are now `Eq`, `Ord`, `Show` and `Read` instances for `Down`.

  * There are now `Eq`, `Ord`, `Show`, `Read` and `Generic` instances
    for types in GHC.Generics (`U1`, `Par1`, `Rec1`, `K1`, `M1`,
    `(:+:)`, `(:*:)`, `(:.:)`).

  * `Data.Monoid`: There are now `Generic` instances for `Dual`, `Endo`,
    `All`, `Any`, `Sum`, `Product`, `First`, and `Last`; as well as
    `Generic1` instances for `Dual`, `Sum`, `Product`, `First`, and `Last`.

  * The `Data.Monoid.{Product,Sum}` newtype wrappers now have `Num` instances.

  * There are now `Functor` instances for `System.Console.GetOpt`'s
    `ArgOrder`, `OptDescr`, and `ArgDescr`.

  * A zero-width unboxed poly-kinded `Proxy#` was added to
    `GHC.Prim`. It can be used to make it so that there is no the
    operational overhead for passing around proxy arguments to model
    type application.

  * New `Data.Proxy` module providing a concrete, poly-kinded proxy type.

  * New `Data.Coerce` module which exports the new `Coercible` class
    together with the `coerce` primitive which provide safe coercion
    (wrt role checking) between types with same representation.

  * `Control.Concurrent.MVar` has a new implementation of `readMVar`,
    which fixes a long-standing bug where `readMVar` is only atomic if
    there are no other threads running `putMVar`.  `readMVar` now is
    atomic, and is guaranteed to return the value from the first
    `putMVar`.  There is also a new `tryReadMVar` which is a
    non-blocking version.

  * New `Control.Concurrent.MVar.withMVarMasked` which executes
    `IO` action with asynchronous exceptions masked in the same style
    as the existing `modifyMVarMasked` and `modifyMVarMasked_`.

  * New `threadWait{Read,Write}STM :: Fd -> IO (STM (), IO ())`
    functions added to `Control.Concurrent` for waiting on FD
    readiness with STM actions.

  * Expose `Data.Fixed.Fixed`'s constructor.

  * There are now byte endian-swapping primitives
    `byteSwap{16,32,64}` available in `Data.Word`, which use
    optimized machine instructions when available.

  * `Data.Bool` now exports `bool :: a -> a -> Bool -> a`, analogously
    to `maybe` and `either` in their respective modules.

  * `Data.Either` now exports `isLeft, isRight :: Either a b -> Bool`.

  * `Debug.Trace` now exports `traceId`, `traceShowId`, `traceM`,
    and `traceShowM`.

  * `Data.Functor` now exports `($>)` and `void`.

  * Rewrote portions of `Text.Printf`, and made changes to `Numeric`
    (added `Numeric.showFFloatAlt` and `Numeric.showGFloatAlt`) and
    `GHC.Float` (added `formatRealFloatAlt`) to support it.  The
    rewritten version is extensible to user types, adds a "generic"
    format specifier "`%v`", extends the `printf` spec to support much
    of C's `printf(3)` functionality, and fixes the spurious warnings
    about using `Text.Printf.printf` at `(IO a)` while ignoring the
    return value.  These changes were contributed by Bart Massey.

  * The minimal complete definitions for all type-classes with cyclic
    default implementations have been explicitly annotated with the
    new `{-# MINIMAL #-}` pragma.

  * `Control.Applicative.WrappedMonad`, which can be used to convert a
    `Monad` to an `Applicative`, has now a
    `Monad m => Monad (WrappedMonad m)` instance.

  * There is now a `Generic` and a `Generic1` instance for `WrappedMonad`
    and `WrappedArrow`.

  * Handle `ExitFailure (-sig)` on Unix by killing process with signal `sig`.

  * New module `Data.Type.Bool` providing operations on type-level booleans.

  * Expose `System.Mem.performMinorGC` for triggering minor GCs.

  * New `System.Environment.{set,unset}Env` for manipulating
    environment variables.

  * Add `Typeable` instance for `(->)` and `RealWorld`.

  * Declare CPP header `<Typeable.h>` officially obsolete as GHC 7.8+
    does not support hand-written `Typeable` instances anymore.

  * Remove (unmaintained) Hugs98 and NHC98 specific code.

  * Optimize `System.Timeout.timeout` for the threaded RTS.

  * Remove deprecated functions `unsafeInterleaveST`, `unsafeIOToST`,
    and `unsafeSTToIO` from `Control.Monad.ST`.

  * Add a new superclass `SomeAsyncException` for all asynchronous exceptions
    and makes the existing `AsyncException` and `Timeout` exception children
    of `SomeAsyncException` in the hierarchy.

  * Remove deprecated functions `blocked`, `unblock`, and `block` from
    `Control.Exception`.

  * Remove deprecated function `forkIOUnmasked` from `Control.Concurrent`.

  * Remove deprecated function `unsafePerformIO` export from `Foreign`
    (still available via `System.IO.Unsafe.unsafePerformIO`).

  * Various fixes and other improvements (see Git history for full details).
