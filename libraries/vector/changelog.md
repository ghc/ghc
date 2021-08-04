# Changes in version 0.12.3.0

 * Fix performance regression due to introduction of `keepAlive#` primop in ghc-9.0: [#372](https://github.com/haskell/vector/pull/372)

 * Add monadic functions for mutable vectors: [#338](https://github.com/haskell/vector/pull/338)

   * Added folds for monadic functions: `mapM_`, `imapM_`, `forM_`, `iforM_`,
     `foldl`, `foldl'`, `foldM`, `foldM'`, `ifoldl`, `ifoldl'`, `ifoldM`,
     `ifoldM'`
   * Added `modifyM` and `unsafeModifyM` for mutable vectors
   * Added `generate` and `generateM` for mutable vectors

# Changes in version 0.12.2.0

 * Add `MINIMAL` pragma to `Vector` & `MVector` type classes: [#11](https://github.com/haskell/vector/issues/11)
 * Export `unstreamM` from`from Data.Vector.Generic`: [#70](https://github.com/haskell/vector/issues/70)
 * New functions: `unfoldrExactN` and `unfoldrExactNM`: [#140](https://github.com/haskell/vector/issues/140)
 * Added `iforM` and `iforM_`: [#262](https://github.com/haskell/vector/issues/262)
 * Added `MonadFix` instance for boxed vectors: [#178](https://github.com/haskell/vector/issues/178)
 * Added `uncons` and `unsnoc`: [#212](https://github.com/haskell/vector/issues/212)
 * Added `foldMap` and `foldMap'`: [#263](https://github.com/haskell/vector/issues/263)
 * Added `isSameVector` for storable vectors
 * Added `toArray`, `fromArray`, `toMutableArray` and `fromMutableArray`
 * Added `iscanl`, `iscanl'`, `iscanr`, `iscanr'` to `Primitive`, `Storable` and `Unboxed`
 * Added `izipWithM`, `izipWithM_`, `imapM` and `imapM_` to `Primitive` and `Storable`
 * Added `ifoldM`, `ifoldM'`, `ifoldM_` and `ifoldM'_` to `Primitive` and `Storable`
 * Added `eqBy` and `cmpBy`
 * Added `findIndexR` to `Generic`: [#172](https://github.com/haskell/vector/issues/172)
 * Added `catMaybes`: [#329](https://github.com/haskell/vector/issues/329)
 * Added `mapMaybeM` and `imapMaybeM`: [#183](https://github.com/haskell/vector/issues/183)

# Changes in version 0.12.1.2

 * Fix for lost function `Data.Vector.Generic.mkType`: [#287](https://github.com/haskell/vector/issues/287)

# Changes in version 0.12.1.1 (deprecated)
 * add semigrioups dep to test suite so CI actually runs again on GHC < 8

# Changes in version 0.12.1.0 (deprecated)
 * Fix integer overflows in specializations of Bundle/Stream enumFromTo on Integral types
 * Fix possibility of OutOfMemory with `take` and very large arguments.
 * Fix `slice` function causing segfault and not checking the bounds properly.
 * updated specialization rule for EnumFromTo on Float and Double
  to make sure it always matches the version in GHC Base (which changed as of 8.6)
  Thanks to Aleksey Khudyakov @Shimuuar for this fix.
 * fast rejection short circuiting in eqBy operations
 * the O2 test suite now has reasonable memory usage on every GHC version,
    special thanks to Alexey Kuleshevich (@lehins).
 * The `Mutable` type family is now injective on GHC 8.0 or later.
 * Using empty `Storable` vectors no longer results in division-by-zero
   errors.
 * The `Data` instances for `Vector` types now have well defined
   implementations for `toConstr`, `gunfold`, and `dataTypeOf`.
 * New function: `partitionWith`.
 * Add `Unbox` instances for `Identity`, `Const`, `Down`, `Dual`, `Sum`,
   `Product`, `Min`, `Max`, `First`, `Last`, `WrappedMonoid`, `Arg`, `Any`,
   `All`, `Alt`, and `Compose`.
 * Add `NFData1` instances for applicable `Vector` types.

# Changes in version 0.12.0.3
  * Monad Fail support

# Changes in version 0.12.0.2
  * Fixes issue #220, compact heap operations crashing on boxed vectors constructed
    using traverse.
  * backport injective type family support
  * Cleanup the memset code internal to storable vector modules to be
    compatible with future Primitive releases

# Changes in version 0.12.0.1

 * Make sure `length` can be inlined
 * Include modules that test-suites depend on in other-modules

# Changes in version 0.12.0.0

 * Documentation fixes/additions
 * New functions: createT, iscanl/r, iterateNM, unfoldrM, uniq
 * New instances for various vector types: Semigroup, MonadZip
 * Made `Storable` vectors respect memory alignment
 * Changed some macros to ConstraintKinds
   - Dropped compatibility with old GHCs to support this
 * Add `Eq1`, `Ord1`, `Show1`, and `Read1` `Vector` instances, and related
   helper functions.
 * Relax context for `Unbox (Complex a)`.

# Changes in version 0.11.0.0

 * Define `Applicative` instances for `Data.Vector.Fusion.Util.{Box,Id}`
 * Define non-bottom `fail` for `instance Monad Vector`
 * New generalized stream fusion framework
 * Various safety fixes
   - Various overflows due to vector size have been eliminated
   - Memory is initialized on creation of unboxed vectors
 * Changes to SPEC usage to allow building under more conditions

# Changes in version 0.10.12.3

 * Allow building with `primtive-0.6`

# Changes in version 0.10.12.2

 * Add support for `deepseq-1.4.0.0`

# Changes in version 0.10.12.1

 * Fixed compilation on non-head GHCs

# Changes in version 0.10.12.0

 * Export MVector constructor from Data.Vector.Primitive to match Vector's
   (which was already exported).

 * Fix building on GHC 7.9 by adding Applicative instances for Id and Box

# Changes in version 0.10.11.0

 * Support OverloadedLists for boxed Vector in GHC >= 7.8

# Changes in version 0.10.10.0

 * Minor version bump to rectify PVP violation occured in 0.10.9.3 release

# Changes in version 0.10.9.3 (deprecated)

 * Add support for OverloadedLists in GHC >= 7.8

# Changes in version 0.10.9.2

 * Fix compilation with GHC 7.9

# Changes in version 0.10.9.1

 * Implement poly-kinded Typeable

# Changes in version 0.10.0.1

 * Require `primitive` to include workaround for a GHC array copying bug

# Changes in version 0.10

 * `NFData` instances
 * More efficient block fills
 * Safe Haskell support removed
