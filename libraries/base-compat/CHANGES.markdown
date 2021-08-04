## Changes in 0.11.2 [2020.09.30]
 - Sync with `base-4.15`/GHC 9.0
 - Backport `singleton` to `Data.List` and `Data.List.NonEmpty`
 - Backport `hGetContents'`, `getContents'`, and `readFile'` added to `System.IO`

## Changes in 0.11.1 [2020.01.27]
 - Sync with `base-4.14`/GHC 8.10
 - Backport `isResourceVanishedError`, `resourceVanishedErrorType`, and
   `isResourceVanishedErrorType` to `System.IO.Error.Compat`.

## Changes in 0.11.0 [2019.09.06]
 - Sync with `base-4.13`/GHC 8.8
 - Backport `MonadFail(fail)` to `Prelude.Compat` and `Control.Monad.Compat`.

   Because `Prelude.Compat.fail` now corresponds to the `fail` from `MonadFail`
   instead of `Monad`, some care is required to implement `Monad.fail` on
   pre-8.8 versions of GHC. The following template is recommended:

   ```haskell
   import Prelude.Compat
   import qualified Control.Monad      as Monad
   import qualified Control.Monad.Fail as Fail

   data Blah a = ...

   instance Functor Blah where ...
   instance Applicative Blah where ...

   instance Monad.Monad Blah where
     (>>=) = ...
   #if !(MIN_VERSION_base(4,13,0))
     fail = Fail.fail
   #endif

   instance Fail.MonadFail Blah where
     fail = ...
   ```

   This approach is also backwards-compatible with previous releases of
   `base-compat`.

   Note that the `MonadFail` class has only been in `base` since
   `base-4.9`/GHC 8.0, so accordingly, this can only be backported back
   to GHC 8.0. If you wish to have a version of
   `Prelude.Compat`/`Control.Monad.Compat` that backports
   `MonadFail` to older GHCs (by conditionally depending on the `fail`
   library), use the `Prelude.Compat`/`Control.Monad.Compat` modules from the
   `base-compat-batteries` package.

 - Introduce the `Data.Type.Equality.Compat` module, which re-exports
   `Data.Type.Equality` if using `base-4.7`/GHC-7.8 or later. If using an older
   version of `base`, this module is empty.

   If you wish to have a version of
   `Data.Type.Equality.Compat` with older GHCs
   (by conditionally depending on the `type-equality` library),
   use the `Data.Type.Equality.Compat` module from the
   `base-compat-batteries` package.

## Changes in 0.10.5 [2018.10.18]
 - Enable `BangPatterns` in `Prelude.Compat`.

## Changes in 0.10.4 [2018.07.03]
 - Make more modules `Trustworthy`. In particular, fix a regression in which
   `Prelude.Compat` was inferred as `Unsafe` by explicitly marking it as
   `Trustwothy`.

## Changes in 0.10.3 [2018.07.02]
 - Backport the proper fixity for `($!)`, which was accidentally omitted in
   `base-compat-0.10.2`.

## Changes in 0.10.2 [2018.07.02]
 - Sync with `base-4.12`/GHC 8.6
 - Backport `RuntimeRep`-polymorphic versions of `($!)` and `throw` to
   `Prelude.Compat` and `Control.Exception.Compat`, respectively
   (if using `base-4.10`/GHC 8.2 or later).
 - Introduce the `Data.Functor.Contravariant.Compat` module, which reexports
   `Data.Functor.Contravariant` if using `base-4.12`/GHC 8.6 or later.

   See `Data.Functor.Contravariant.Compat` in the corresponding
   `base-compat-batteries` release for a version with a wider support window.

## Changes in 0.10.1 [2018.04.10]
 - Add `Data.List.NonEmpty.Compat`.
 - Reexport `(Data.Semigroup.<>)` from `Data.Monoid.Compat` back to `base-4.9`.

## Changes in 0.10.0 [2018.04.05]
 - Sync with `base-4.11`/GHC 8.4
 - Backport `Semigroup((<>))` to `Prelude.Compat`.

   Note that the `Semigroup` class has only been in `base` since
   `base-4.9`/GHC 8.0, so accordingly, this can only be backported back
   to GHC 8.0. If you wish to have a version of `Prelude.Compat` that backports
   `Semigroup` to older GHCs (by conditionally depending on the `semigroups`
   library), use the `Prelude.Compat` module from the `base-compat-batteries`
   package.
 - Backport `(<&>)` to `Data.Functor.Compat`
 - Backport `iterate'` to `Data.List.Compat`
 - Backport `showHFloat` to `Numeric.Compat`
 - Backport a `RuntimeRep`-polymorphic `withTypeable` function to
   `Type.Reflection.Compat`. (This is only exported on `base-4.10`/GHC 8.2.)
 - Introduce the following modules, back until the oldest version of `base`
   that can support backporting them. If you wish to use them in conjunction
   with older versions of `base`, use the `base-compat-batteries` package.
   - `Control.Monad.Fail.Compat` (back until `base-4.9`/GHC 8.0)
   - `Control.Monad.IO.Class.Compat` (back until `base-4.9`/GHC 8.0)
   - `Data.Bifunctor` (back until `base-4.8`/GHC 7.10)
   - `Data.Bifoldable` and `Data.Bitraversable` (back until `base-4.10`/GHC 8.2)
   - `Data.Functor.Compose.Compat`, `Data.Functor.Product.Compat`, and
     `Data.Functor.Sum.Compat` (back until `base-4.9`/GHC 8.0)
   - `Data.Functor.Identity.Compat` (back until `base-4.8`/GHC 7.10)
   - `Data.Semigroup.Compat` (back until `base-4.9`/GHC 8.0)
   - `Data.Void.Compat` (back until `base-4.8`/GHC 7.10)
   - `Numeric.Natural.Compat` (back until `base-4.8`/GHC 7.10)
 - Introduce versions of modules with the suffix `.Repl`. These simply reexport
   the contents of their counterparts without the `.Repl` suffix to provide
   a globally unique namespace to import from in the event one wants to import
   `base-compat` modules into GHCi. (In `base-compat-batteries`, the
   corresponding suffix is `.Repl.Batteries`.)

## Changes in 0.9.3 [2017.04.10]
 - Sync with `base-4.10`/GHC 8.2
 - Backport `fromLeft`/`fromRight` to `Data.Either.Compat`
 - Backport implementations of `maximumBy`/`minimumBy` which use constant stack
   space to `Data.Foldable.Compat`
 - Backport `asProxyTypeOf` with a generalized type signature to
   `Data.Proxy.Compat`
 - Backport `gcoerceWith` to `Data.Type.Coercion.Compat`
 - Backport `plusForeignPtr` to `Foreign.ForeignPtr.Compat`

## Changes in 0.9.2
 - Allow building on the HaLVM

## Changes in 0.9.1
 - Use the more efficient version of `replicateM` and `replicateM_` introduced
   in `base-4.9`

## Changes in 0.9.0
 - Sync with `base-4.9`/GHC 8.0
 - Weakened `RealFloat` constraints on `realPart`, `imagPart`, `conjugate`,
   `mkPolar`, and `cis` in `Data.Complex.Compat`
 - Backport `Foreign.ForeignPtr.Safe` and `Foreign.Marshal.Safe`
 - Generalize `filterM`, `forever`, `mapAndUnzipM`, `zipWithM`, `zipWithM_`,
   `replicateM`, and `replicateM_` in `Control.Monad` from `Monad` to
   `Applicative`
 - Backport `.Unsafe.Compat` modules (for `Control.Monad.ST`,
   `Control.Monad.ST.Lazy`, `Foreign.ForeignPtr`, and `Foreign.Marshal`)
 - Backport `forkFinally` and `forkOSWithUnmask` to `Control.Concurrent.Compat`
 - Backport `Data.Functor.Const`
 - Backport `modifyIORef'`, `atomicModifyIORef'` and `atomicWriteIORef` to
   `Data.IORef.Compat`
 - `Data.Ratio.{denominator,numerator}` have no `Integral` constraint anymore
 - Backport `modifySTRef'` to `Data.STRef.Compat`
 - Export `String`, `lines`, `words`, `unlines`, and `unwords` to
   `Data.String.Compat`
 - Generalize `Debug.Trace.{traceM, traceShowM}` from `Monad` to `Applicative`
 - Backport `errorWithoutStackTrace` to `Prelude.Compat`
 - Backport `unsafeFixIO` and `unsafeDupablePerformIO` to
   `System.IO.Unsafe.Compat`

## Changes in 0.8.2
 - Backport `bitDefault`, `testBitDefault`, and `popCountDefault` in
   `Data.Bits.Compat` to all versions of `base`
   - Backport `toIntegralSized` to `base-4.7`
 - Backport `nub` and `nubBy` (as well as `union` and `unionBy`, which are
   implemented in terms of them) to fix logic error in `Data.List.Compat`
 - Backport `byteSwap16`, `byteSwap32`, and `byteSwap64` to `Data.Word.Compat`
 - Backport `fillBytes` in `Foreign.Marshal.Utils.Compat`
 - Backport `showFFloatAlt` and `showGFloatAlt` to `Numeric.Compat`

## Changes in 0.8.1.1
 - Fixed Windows build

## Changes in 0.8.1
 - Implement `setEnv` and `unsetEnv` in `System.Environment.Compat` (which were
   ported from the `setenv` package). As a result, `base-compat` now depends
   on `unix` on POSIX-like operating systems.
 - Drop GHC 6.12 (and `base-4.2.0.0`) compatibility

## Changes in 0.8.0.1
 - Retrospective version bump updating the changelog to reflect the changes
   made in 0.8.0

## Changes 0.8.0
 - All orphan instances were split off into a separate package,
   [`base-orphans`](https://github.com/haskell-compat/base-orphans)
 - `base-compat` no longer redefines the data types `Down` and `Alt`. See
   [here](https://github.com/haskell-compat/base-compat/issues/17) for
   the discussion that led to this change.
 - Update `Control.Monad.Compat` for `base-4.8.0.0`
 - Update `Data.List.Compat` for `base-4.8.0.0`
 - Update `Data.Foldable.Compat` for `base-4.8.0.0`

## Changes in 0.7.1
 - Backported `Alt` to `Data.Monoid.Compat`
 - Backported `Down` to `Data.Ord.Compat`

## Changes in 0.7.0
 - Add functions and orphan instances introduced by changes to
   `base-4.7.0.0` and `base-4.8.0.0`

## Changes in 0.6.0
 - Update `Prelude.Compat` for `base-4.8.0.0` and AMP

## Changes in 0.5.0
 - Remove Control.Exception.Base.Compat and GHC.Exception.Compat
 - Add System.Exit.Compat.die
 - Compatibility with base-4.7.1

## Changes in 0.4.1
 - Add `setEnv` and `unsetEnv` to `System.Environment.Compat`

## Changes in 0.4.0
 - Major refactoring: base-compat no longer aims to replace all base,
   only new code is included in module .Compat
 - Removed stubbed modules
 - Removed generation scripts

## Changes in 0.3
 - Added functions from Base 4.7 (bool, isLeft, isRight)
 - Added instances from Base 4.7 (Either Foldable, Traversable,...)

## Changes in 0.2.1
 - Fix build on windows

## Changes in 0.2.0
 - Re-export everything from base
 - provides access to `VERSION_base` and `MIN_VERSION_base` CPP macros (with
   `#include "base-compat.h"`)
 - Do not re-export `System.IO.Error.catch` from `Prelude` for `base` < 4.6.0
 - Add `Eq`/`Ord` instance for `ErrorCall`
 - Remove `GHC.IOBase`, `GHC.Handle`, `Control.Concurrent.QSem`,
   `Control.Concurrent.QSemN`, `Control.Concurrent.SampleVar`, `Data.HashTable`

## Changes in 0.1.0
 - Remove getExecutablePath, it did not work with GHC < 7.2 (patches welcome!)
 - Add `<>`
