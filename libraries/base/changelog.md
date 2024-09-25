# Changelog for [`base` package](http://hackage.haskell.org/package/base)

## 4.21.0.0 *TBA*
  * `GHC.Desugar` has been deprecated and should be removed in GHC 9.14. ([CLC proposal #216](https://github.com/haskell/core-libraries-committee/issues/216))
  * Add a `readTixFile` field to the `HpcFlags` record in `GHC.RTS.Flags` ([CLC proposal #276](https://github.com/haskell/core-libraries-committee/issues/276))
  * Add `compareLength` to `Data.List` and `Data.List.NonEmpty` ([CLC proposal #257](https://github.com/haskell/core-libraries-committee/issues/257))
  * Add `INLINE[1]` to `compareInt` / `compareWord` ([CLC proposal #179](https://github.com/haskell/core-libraries-committee/issues/179))
  * Refactor `GHC.RTS.Flags` in preparation for new I/O managers: introduce `data IoManagerFlag` and use it in `MiscFlags`, remove `getIoManagerFlag`, deprecate re-export of `IoSubSystem` ([CLC proposal #263](https://github.com/haskell/core-libraries-committee/issues/263))
  * Add the `MonadFix` instance for `(,) a`, similar to the one for `Writer a` ([CLC proposal #238](https://github.com/haskell/core-libraries-committee/issues/238))
  * Improve `toInteger :: Word32 -> Integer` on 64-bit platforms ([CLC proposal #259](https://github.com/haskell/core-libraries-committee/issues/259))
  * Make `flip` representation polymorphic ([CLC proposal #245](https://github.com/haskell/core-libraries-committee/issues/245))
  * The `HasField` class now supports representation polymorphism ([CLC proposal #194](https://github.com/haskell/core-libraries-committee/issues/194))
  * Make `read` accept binary integer notation ([CLC proposal #177](https://github.com/haskell/core-libraries-committee/issues/177))
  * Improve the performance of `Data.List.sort` using an improved merging strategy. Instead of `compare`, `sort` now uses `(>)` which may break *malformed* `Ord` instances ([CLC proposal #236](https://github.com/haskell/core-libraries-committee/issues/236))
  * Add `inits1` and `tails1` to `Data.List`, factored from the corresponding functions in `Data.List.NonEmpty` ([CLC proposal #252](https://github.com/haskell/core-libraries-committee/issues/252))
  * Add `firstA` and `secondA` to `Data.Bitraversable`. ([CLC proposal #172](https://github.com/haskell/core-libraries-committee/issues/172))
  * Deprecate `GHC.TypeNats.Internal`, `GHC.TypeLits.Internal`, `GHC.ExecutionStack.Internal` ([CLC proposal #217](https://github.com/haskell/core-libraries-committee/issues/217))
  * Define `Eq1`, `Ord1`, `Show1` and `Read1` instances for basic `Generic` representation types. ([CLC proposal #273](https://github.com/haskell/core-libraries-committee/issues/273))
  * `setNonBlockingMode` will no longer throw an exception when called on a FD associated with a unknown device type. ([CLC proposal #282](https://github.com/haskell/core-libraries-committee/issues/282))
  * Add exception type metadata to default exception handler output.
    ([CLC proposal #231](https://github.com/haskell/core-libraries-committee/issues/231)
    and [CLC proposal #261](https://github.com/haskell/core-libraries-committee/issues/261))
  * The [deprecation process of GHC.Pack](https://gitlab.haskell.org/ghc/ghc/-/issues/21461) has come its term. The module has now been removed from `base`.

## 4.20.0.0 May 2024
  * Shipped with GHC 9.10.1
  * Deprecate `GHC.Pack` ([#21461](https://gitlab.haskell.org/ghc/ghc/-/issues/21461))
  * Export `foldl'` from `Prelude` ([CLC proposal #167](https://github.com/haskell/core-libraries-committee/issues/167))
  * The top-level handler for uncaught exceptions now displays the output of `displayException` rather than `show`  ([CLC proposal #198](https://github.com/haskell/core-libraries-committee/issues/198))
  * Add `permutations` and `permutations1` to `Data.List.NonEmpty` ([CLC proposal #68](https://github.com/haskell/core-libraries-committee/issues/68))
  * Add a `RULE` to `Prelude.lookup`, allowing it to participate in list fusion ([CLC proposal #175](https://github.com/haskell/core-libraries-committee/issues/175))
  * Implement `stimes` for `instance Semigroup (Endo a)` explicitly ([CLC proposal #4](https://github.com/haskell/core-libraries-committee/issues/4))
  * Add `startTimeProfileAtStartup` to `GHC.RTS.Flags` to expose new RTS flag
    `--no-automatic-heap-samples` in the Haskell API ([CLC proposal #243](https://github.com/haskell/core-libraries-committee/issues/243)).
  * Implement `sconcat` for `instance Semigroup Data.Semigroup.First` and `instance Semigroup Data.Monoid.First` explicitly, increasing laziness ([CLC proposal #246](https://github.com/haskell/core-libraries-committee/issues/246))
  * Add laws relating between `Foldable` / `Traversable` with `Bifoldable` / `Bitraversable` ([CLC proposal #205](https://github.com/haskell/core-libraries-committee/issues/205))
  * The `Enum Int64` and `Enum Word64` instances now use native operations on 32-bit platforms, increasing performance by up to 1.5x on i386 and up to 5.6x with the JavaScript backend. ([CLC proposal #187](https://github.com/haskell/core-libraries-committee/issues/187))
  * Exceptions can now be decorated with user-defined annotations via `ExceptionContext` ([CLC proposal #200](https://github.com/haskell/core-libraries-committee/issues/200))
  * Exceptions now capture backtrace information via their `ExceptionContext`. GHC
    supports several mechanisms by which backtraces can be collected which can be
    individually enabled and disabled via
    `GHC.Exception.Backtrace.setBacktraceMechanismState` ([CLC proposal #199](https://github.com/haskell/core-libraries-committee/issues/199))
  * Add `HasCallStack` constraint to `Control.Exception.throw{,IO}` ([CLC proposal #201](https://github.com/haskell/core-libraries-committee/issues/201))
  * Update to [Unicode 15.1.0](https://www.unicode.org/versions/Unicode15.1.0/).
  * Fix `withFile`, `withFileBlocking`, and `withBinaryFile` to not incorrectly annotate exceptions raised in wrapped computation. ([CLC proposal #237](https://github.com/haskell/core-libraries-committee/issues/237))
  * Fix `fdIsNonBlocking` to always be `0` for regular files and block devices on unix, regardless of `O_NONBLOCK`
  * Always use `safe` call to `read` for regular files and block devices on unix if the RTS is multi-threaded, regardless of `O_NONBLOCK`.
    ([CLC proposal #166](https://github.com/haskell/core-libraries-committee/issues/166))
  * Export List from Data.List ([CLC proposal #182](https://github.com/haskell/core-libraries-committee/issues/182)).
  * Add `{-# WARNING in "x-data-list-nonempty-unzip" #-}` to `Data.List.NonEmpty.unzip`.
    Use `{-# OPTIONS_GHC -Wno-x-data-list-nonempty-unzip #-}` to disable it.
     ([CLC proposal #86](https://github.com/haskell/core-libraries-committee/issues/86)
     and [CLC proposal #258](https://github.com/haskell/core-libraries-committee/issues/258))
  * Add `System.Mem.performMajorGC` ([CLC proposal #230](https://github.com/haskell/core-libraries-committee/issues/230))
  * Fix exponent overflow/underflow bugs in the `Read` instances for `Float` and `Double` ([CLC proposal #192](https://github.com/haskell/core-libraries-committee/issues/192))
  * `Foreign.C.Error.errnoToIOError` now uses the reentrant `strerror_r` to render system errors when possible ([CLC proposal #249](https://github.com/haskell/core-libraries-committee/issues/249))
  * Implement `many` and `some` methods of `instance Alternative (Compose f g)` explicitly. ([CLC proposal #181](https://github.com/haskell/core-libraries-committee/issues/181))
  * Change the types of the `GHC.Stack.StackEntry.closureType` and `GHC.InfoProv.InfoProv.ipDesc` record fields to use `GHC.Exts.Heap.ClosureType` rather than an `Int`.
    To recover the old value use `fromEnum`. ([CLC proposal #210](https://github.com/haskell/core-libraries-committee/issues/210))
  * The functions `GHC.Exts.dataToTag#` and `GHC.Base.getTag` have had
    their types changed to the following:

    ```haskell
    dataToTag#, getTag
      :: forall {lev :: Levity} (a :: TYPE (BoxedRep lev))
      .  DataToTag a => a -> Int#
    ```

    In particular, they are now applicable only at some (not all)
    lifted types.  However, if `t` is an algebraic data type (i.e. `t`
    matches a `data` or `data instance` declaration) with all of its
    constructors in scope and the levity of `t` is statically known,
    then the constraint `DataToTag t` can always be solved.
    ([CLC proposal #104](https://github.com/haskell/core-libraries-committee/issues/104))
  * `GHC.Exts` no longer exports the GHC-internal `whereFrom#` primop ([CLC proposal #214](https://github.com/haskell/core-libraries-committee/issues/214))
  * `GHC.InfoProv.InfoProv` now provides a `ipUnitId :: String` field encoding the unit ID of the unit defining the info table ([CLC proposal #214](https://github.com/haskell/core-libraries-committee/issues/214))
  * Add `sortOn` to `Data.List.NonEmpty`
    ([CLC proposal #227](https://github.com/haskell/core-libraries-committee/issues/227))
  * Add more instances for `Compose`: `Fractional`, `RealFrac`, `Floating`, `RealFloat` ([CLC proposal #226](https://github.com/haskell/core-libraries-committee/issues/226))
  * Treat all FDs as "nonblocking" on wasm32 ([CLC proposal #234](https://github.com/haskell/core-libraries-committee/issues/234))
  * Add `HeapByEra`, `eraSelector` and `automaticEraIncrement` to `GHC.RTS.Flags` to
    reflect the new RTS flags: `-he` profiling mode, `-he` selector and `--automatic-era-increment`.
    ([CLC proposal #254](https://github.com/haskell/core-libraries-committee/issues/254))
  * Document that certain modules are unstable and not meant to be consumed by the general public ([CLC proposal #146](https://github.com/haskell/core-libraries-committee/issues/146))
  * Add unaligned `Addr#` primops ([CLC proposal #154](https://github.com/haskell/core-libraries-committee/issues/154))
  * Deprecate `stgDoubleToWord{32,64}` and `stgWord{32,64}ToDouble` in favor of new primops `castDoubleToWord{32,64}#` and `castWord{32,64}ToDouble#` ([CLC proposal #253](https://github.com/haskell/core-libraries-committee/issues/253))
  * Add `unsafeThawByteArray#`, opposite to the existing `unsafeFreezeByteArray#` ([CLC proposal #184](https://github.com/haskell/core-libraries-committee/issues/184))

## 4.19.0.0 *October 2023*
  * Shipped with GHC 9.8.1
  * Add `{-# WARNING in "x-partial" #-}` to `Data.List.{head,tail}`.
    Use `{-# OPTIONS_GHC -Wno-x-partial #-}` to disable it.
    ([CLC proposal #87](https://github.com/haskell/core-libraries-committee/issues/87) and [#114](https://github.com/haskell/core-libraries-committee/issues/114))
  * Add `fromThreadId :: ThreadId -> Word64` to `GHC.Conc.Sync`, which maps a thread to a per-process-unique identifier ([CLC proposal #117](https://github.com/haskell/core-libraries-committee/issues/117))
  * Add `Data.List.!?` ([CLC proposal #110](https://github.com/haskell/core-libraries-committee/issues/110))
  * Mark `maximumBy`/`minimumBy` as `INLINE` improving performance for unpackable
    types significantly.
  * Add INLINABLE pragmas to `generic*` functions in Data.OldList ([CLC proposal #129](https://github.com/haskell/core-libraries-committee/issues/130))
  * Export `getSolo` from `Data.Tuple`.
      ([CLC proposal #113](https://github.com/haskell/core-libraries-committee/issues/113))
  * Add `Type.Reflection.decTypeRep`, `Data.Typeable.decT` and `Data.Typeable.hdecT` equality decisions functions.
      ([CLC proposal #98](https://github.com/haskell/core-libraries-committee/issues/98))
  * Add `Data.Functor.unzip` ([CLC proposal #88](https://github.com/haskell/core-libraries-committee/issues/88))
  * Add `System.Mem.Weak.{get,set}FinalizerExceptionHandler`, which allows the user to set the global handler invoked by when a `Weak` pointer finalizer throws an exception. ([CLC proposal #126](https://github.com/haskell/core-libraries-committee/issues/126))
  * Add `System.Mem.Weak.printToHandleFinalizerExceptionHandler`, which can be used with `setFinalizerExceptionHandler` to print exceptions thrown by finalizers to the given `Handle`. ([CLC proposal #126](https://github.com/haskell/core-libraries-committee/issues/126))
  * Add `Data.List.unsnoc` ([CLC proposal #165](https://github.com/haskell/core-libraries-committee/issues/165))
  * Implement more members of `instance Foldable (Compose f g)` explicitly.
      ([CLC proposal #57](https://github.com/haskell/core-libraries-committee/issues/57))
  * Add `Eq` and `Ord` instances for `SSymbol`, `SChar`, and `SNat`.
      ([CLC proposal #148](https://github.com/haskell/core-libraries-committee/issues/148))
  * Add `COMPLETE` pragmas to the `TypeRep`, `SSymbol`, `SChar`, and `SNat` pattern synonyms.
      ([CLC proposal #149](https://github.com/haskell/core-libraries-committee/issues/149))
  * Make `($)` representation polymorphic ([CLC proposal #132](https://github.com/haskell/core-libraries-committee/issues/132))
  * Implement [GHC Proposal #433](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0433-unsatisfiable.rst),
    adding the class `Unsatisfiable :: ErrorMessage -> TypeError` to `GHC.TypeError`,
    which provides a mechanism for custom type errors that reports the errors in
    a more predictable behaviour than `TypeError`.
  * Add more instances for `Compose`: `Enum`, `Bounded`, `Num`, `Real`, `Integral` ([CLC proposal #160](https://github.com/haskell/core-libraries-committee/issues/160))
  * Make `(&)` representation polymorphic in the return type ([CLC proposal #158](https://github.com/haskell/core-libraries-committee/issues/158))
  * Implement `GHC.IORef.atomicSwapIORef` via a new dedicated primop `atomicSwapMutVar#` ([CLC proposal #139](https://github.com/haskell/core-libraries-committee/issues/139))
  * Change `BufferCodec` to use an unboxed implementation, while providing a compatibility layer using pattern synonyms. ([CLC proposal #134](https://github.com/haskell/core-libraries-committee/issues/134) and [#178](https://github.com/haskell/core-libraries-committee/issues/178))
  * Add nominal role annotations to `SNat` / `SSymbol` / `SChar` ([CLC proposal #170](https://github.com/haskell/core-libraries-committee/issues/170))
  * Make `Semigroup`'s `stimes` specializable. ([CLC proposal #8](https://github.com/haskell/core-libraries-committee/issues/8))
  * Implement `copyBytes`, `fillBytes`, `moveBytes` and `stimes` for `Data.Array.Byte.ByteArray` using primops ([CLC proposal #188](https://github.com/haskell/core-libraries-committee/issues/188))
  * Add rewrite rules for conversion between `Int64` / `Word64` and `Float` / `Double` on 64-bit architectures ([CLC proposal #203](https://github.com/haskell/core-libraries-committee/issues/203)).
  * `Generic` instances for tuples now expose `Unit`, `Tuple2`, `Tuple3`, ..., `Tuple64` as the actual names for tuple type constructors ([GHC proposal #475](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0475-tuple-syntax.rst)).
  * Reject `FilePath`s containing interior `NUL`s ([CLC proposal #144](https://github.com/haskell/core-libraries-committee/issues/144))
  * Add `GHC.JS.Foreign.Callback` module for JavaScript backend ([CLC proposal #150](https://github.com/haskell/core-libraries-committee/issues/150))
  * Generalize the type of `keepAlive#` and `touch#` ([CLC proposal #152](https://github.com/haskell/core-libraries-committee/issues/152))

## 4.18.0.0 *March 2023*
  * Shipped with GHC 9.6.1
  * `Foreign.C.ConstPtr.ConstrPtr` was added to encode `const`-qualified
    pointer types in foreign declarations when using `CApiFFI` extension. ([CLC proposal #117](https://github.com/haskell/core-libraries-committee/issues/117))
  * Add `forall a. Functor (p a)` superclass for `Bifunctor p` ([CLC proposal #91](https://github.com/haskell/core-libraries-committee/issues/91))
  * Add `forall a. Functor (p a)` superclass for `Bifunctor p`.
  * Add Functor instances for `(,,,,) a b c d`, `(,,,,,) a b c d e` and
    `(,,,,,) a b c d e f`.
  * Exceptions thrown by weak pointer finalizers can now be reported by setting
    a global exception handler, using `System.Mem.Weak.setFinalizerExceptionHandler`.
    The default behaviour is unchanged (exceptions are ignored and not reported).
  * `Numeric.Natural` re-exports `GHC.Natural.minusNaturalMaybe`
    ([CLC proposal #45](https://github.com/haskell/core-libraries-committee/issues/45))
  * Add `Data.Foldable1` and `Data.Bifoldable1`
    ([CLC proposal #9](https://github.com/haskell/core-libraries-committee/issues/9))
  * Add `applyWhen` to `Data.Function`
    ([CLC proposal #71](https://github.com/haskell/core-libraries-committee/issues/71))
  * Add functions `mapAccumM` and `forAccumM` to `Data.Traversable`
    ([CLC proposal #65](https://github.com/haskell/core-libraries-committee/issues/65))
  * Add default implementation of `(<>)` in terms of `sconcat` and `mempty` in
    terms of `mconcat` ([CLC proposal #61](https://github.com/haskell/core-libraries-committee/issues/61)).
  * `GHC.Conc.Sync.listThreads` was added, allowing the user to list the threads
    (both running and blocked) of the program.
  * `GHC.Conc.Sync.labelThreadByteArray#` was added, allowing the user to specify
    a thread label by way of a `ByteArray#` containing a UTF-8-encoded string.
    The old `GHC.Conc.Sync.labelThread` is now implemented in terms of this
    function.
  * `GHC.Conc.Sync.threadLabel` was added, allowing the user to query the label
    of a given `ThreadId`.
  * Add `inits1` and `tails1` to `Data.List.NonEmpty`
    ([CLC proposal #67](https://github.com/haskell/core-libraries-committee/issues/67))
  * Change default `Ord` implementation of `(>=)`, `(>)`, and `(<)` to use
    `(<=)` instead of `compare` ([CLC proposal #24](https://github.com/haskell/core-libraries-committee/issues/24)).
  * Export `liftA2` from `Prelude`. This means that the entirety of `Applicative`
    is now exported from `Prelude`
    ([CLC proposal #50](https://github.com/haskell/core-libraries-committee/issues/50),
    [the migration
    guide](https://github.com/haskell/core-libraries-committee/blob/main/guides/export-lifta2-prelude.md))
  * Switch to a pure Haskell implementation of `GHC.Unicode`
    ([CLC proposals #59](https://github.com/haskell/core-libraries-committee/issues/59)
    and [#130](https://github.com/haskell/core-libraries-committee/issues/130))
  * Update to [Unicode 15.0.0](https://www.unicode.org/versions/Unicode15.0.0/).
  * Add standard Unicode case predicates `isUpperCase` and `isLowerCase` to
    `GHC.Unicode` and `Data.Char`. These predicates use the standard Unicode
    case properties and are more intuitive than `isUpper` and `isLower`
    ([CLC proposal #90](https://github.com/haskell/core-libraries-committee/issues/90))
  * Add `Eq` and `Ord` instances for `Generically1`.
  * Relax instances for Functor combinators; put superclass on Class1 and Class2
    to make non-breaking ([CLC proposal #10](https://github.com/haskell/core-libraries-committee/issues/10),
    [migration guide](https://github.com/haskell/core-libraries-committee/blob/main/guides/functor-combinator-instances-and-class1s.md))
  * Add `gcdetails_block_fragmentation_bytes` to `GHC.Stats.GCDetails` to track heap fragmentation.
  * `GHC.TypeLits` and `GHC.TypeNats` now export the `natSing`, `symbolSing`,
    and `charSing` methods of `KnownNat`, `KnownSymbol`, and `KnownChar`,
    respectively. They also export the `SNat`, `SSymbol`, and `SChar` types
    that are used in these methods and provide an API to interact with these
    types, per
    [CLC proposal #85](https://github.com/haskell/core-libraries-committee/issues/85).
  * The `Enum` instance of `Down a` now enumerates values in the opposite
    order as the `Enum a` instance ([CLC proposal #51](https://github.com/haskell/core-libraries-committee/issues/51))
  * `Foreign.Marshal.Pool` now uses the RTS internal arena instead of libc
    `malloc` for allocation. It avoids the O(n) overhead of maintaining a list
    of individually allocated pointers as well as freeing each one of them when
    freeing a `Pool` (#14762, #18338)
  * `Type.Reflection.Unsafe` is now marked as unsafe.
  * Add `Data.Typeable.heqT`, a kind-heterogeneous version of
    `Data.Typeable.eqT`
    ([CLC proposal #99](https://github.com/haskell/core-libraries-committee/issues/99))
  * Various declarations GHC's new info-table provenance feature have been
    moved from `GHC.Stack.CCS` to a new `GHC.InfoProv` module:
    * The `InfoProv`, along its `ipName`, `ipDesc`, `ipTyDesc`, `ipLabel`,
      `ipMod`, and `ipLoc` fields, have been moved.
    * `InfoProv` now has additional `ipSrcFile` and `ipSrcSpan` fields. `ipLoc`
      is now a function computed from these fields.
    * The `whereFrom` function has been moved
  * Add functions `traceWith`, `traceShowWith`, `traceEventWith` to
    `Debug.Trace`, per
    [CLC proposal #36](https://github.com/haskell/core-libraries-committee/issues/36).
  * Export `List` from `GHC.List`
    ([CLC proposal #186](https://github.com/haskell/core-libraries-committee/issues/186)).

## 4.17.0.0 *August 2022*

  * Shipped with GHC 9.4.1

  * Add explicitly bidirectional `pattern TypeRep` to `Type.Reflection`.

  * Add `Generically` and `Generically1` to `GHC.Generics` for deriving generic
    instances with `DerivingVia`. `Generically` instances include `Semigroup` and
    `Monoid`. `Generically1` instances: `Functor`, `Applicative`, `Alternative`,
    `Eq1` and `Ord1`.

  * Introduce `GHC.ExecutablePath.executablePath`, which is more robust than
    `getExecutablePath` in cases when the executable has been deleted.

  * Add `Data.Array.Byte` module, providing boxed `ByteArray#` and `MutableByteArray#` wrappers.

  * `fromEnum` for `Natural` now throws an error for any number that cannot be
    repesented exactly by an `Int` (#20291).

  * `returnA` is defined as `Control.Category.id` instead of `arr id`.

  * Added symbolic synonyms for `xor` and shift operators to `Data.Bits`:

    - `.^.` (`xor`),
    - `.>>.` and `!>>.` (`shiftR` and `unsafeShiftR`),
    - `.<<.` and `!<<.` (`shiftL` and `unsafeShiftL`).

    These new operators have the same fixity as the originals.

  * `GHC.Exts` now re-exports `Multiplicity` and `MultMul`.

  * A large number of partial functions in `Data.List` and `Data.List.NonEmpty` now
    have an HasCallStack constraint. Hopefully providing better error messages in case
    they are used in unexpected ways.

  * Fix the `Ord1` instance for `Data.Ord.Down` to reverse sort order.

  * Any Haskell type that wraps a C pointer type has been changed from
    `Ptr ()` to `CUIntPtr`. For typical glibc based platforms, the
    affected type is `CTimer`.

  * Remove instances of `MonadFail` for the `ST` monad (lazy and strict) as per
    the [Core Libraries proposal](https://github.com/haskell/core-libraries-committee/issues/33).
    A [migration guide](https://github.com/haskell/core-libraries-committee/blob/main/guides/no-monadfail-st-inst.md)
    is available.

  * Re-export `augment` and `build` function from `GHC.List`

  * Re-export the `IsList` typeclass from the new `GHC.IsList` module.

  * There's a new special function ``withDict`` in ``GHC.Exts``: ::

        withDict :: forall {rr :: RuntimeRep} cls meth (r :: TYPE rr). WithDict cls meth => meth -> (cls => r) -> r

    where ``cls`` must be a class containing exactly one method, whose type
    must be ``meth``.

    This function converts ``meth`` to a type class dictionary.
    It removes the need for ``unsafeCoerce`` in implementation of reflection
    libraries. It should be used with care, because it can introduce
    incoherent instances.

    For example, the ``withTypeable`` function from the
    ``Type.Reflection`` module can now be defined as: ::

          withTypeable :: forall k (a :: k) rep (r :: TYPE rep). ()
                       => TypeRep a -> (Typeable a => r) -> r
          withTypeable rep k = withDict @(Typeable a) rep k

    Note that the explicit type application is required, as the call to
    ``withDict`` would be ambiguous otherwise.

    This replaces the old ``GHC.Exts.magicDict``, which required
    an intermediate data type and was less reliable.

  * `Data.Word.Word64` and `Data.Int.Int64` are now always represented by
    `Word64#` and `Int64#`, respectively. Previously on 32-bit platforms these
    were rather represented by `Word#` and `Int#`. See GHC #11953.

  * Add `GHC.TypeError` module to contain functionality related to custom type
    errors. `TypeError` is re-exported from `GHC.TypeLits` for backwards
    compatibility.

  * Comparison constraints in `Data.Type.Ord` (e.g. `<=`) now use the new
    `GHC.TypeError.Assert` type family instead of type equality with `~`.

## 4.16.3.0 *May 2022*

  * Shipped with GHC 9.2.4

  * winio: make consoleReadNonBlocking not wait for any events at all.

  * winio: Add support to console handles to handleToHANDLE

## 4.16.2.0 *May 2022*

  * Shipped with GHC 9.2.2

  * Export GHC.Event.Internal on Windows (#21245)

  # Documentation Fixes

## 4.16.1.0 *Feb 2022*

  * Shipped with GHC 9.2.2

  * The following Foreign C types now have an instance of `Ix`:
    CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt, CLong, CULong,
    CPtrdiff, CSize, CWchar, CSigAtomic, CLLong, CULLong, CBool, CIntPtr, CUIntPtr,
    CIntMax, CUIntMax.

## 4.16.0.0 *Nov 2021*

  * Shipped with GHC 9.2.1

  * The unary tuple type, `Solo`, is now exported by `Data.Tuple`.

  * Add a `Typeable` constraint to `fromStaticPtr` in the class `GHC.StaticPtr.IsStatic`.

  * Make it possible to promote `Natural`s and remove the separate `Nat` kind.
    For backwards compatibility, `Nat` is now a type synonym for `Natural`.
    As a consequence, one must enable `TypeSynonymInstances`
    in order to define instances for `Nat`. Also, different instances for `Nat` and `Natural`
    won't typecheck anymore.

  * Add `Data.Type.Ord` as a module for type-level comparison operations.  The
    `(<=?)` type operator from `GHC.TypeNats`, previously kind-specific to
    `Nat`, is now kind-polymorphic and governed by the `Compare` type family in
    `Data.Type.Ord`.  Note that this means GHC will no longer deduce `0 <= n`
    for all `n` any more.

  * Add `cmpNat`, `cmpSymbol`, and `cmpChar` to `GHC.TypeNats` and `GHC.TypeLits`.

  * Add `CmpChar`, `ConsSymbol`, `UnconsSymbol`, `CharToNat`, and `NatToChar`
    type families to `GHC.TypeLits`.

  * Add the `KnownChar` class, `charVal` and `charVal'` to `GHC.TypeLits`.

  * Add `Semigroup` and `Monoid` instances for `Data.Functor.Product` and
    `Data.Functor.Compose`.

  * Add `Functor`, `Applicative`, `Monad`, `MonadFix`, `Foldable`, `Traversable`,
    `Eq`, `Ord`, `Show`, `Read`, `Eq1`, `Ord1`, `Show1`, `Read1`, `Generic`,
    `Generic1`, and `Data` instances for `GHC.Tuple.Solo`.

  * Add `Eq1`, `Read1` and `Show1` instances for `Complex`;
    add `Eq1/2`, `Ord1/2`, `Show1/2` and `Read1/2` instances for 3 and 4-tuples.

  * Remove `Data.Semigroup.Option` and the accompanying `option` function.

  * Make `allocaBytesAligned` and `alloca` throw an IOError when the
    alignment is not a power-of-two. The underlying primop
    `newAlignedPinnedByteArray#` actually always assumed this but we didn't
    document this fact in the user facing API until now.

    `Generic1`, and `Data` instances for `GHC.Tuple.Solo`.

  * Under POSIX, `System.IO.openFile` will no longer leak a file descriptor if it
    is interrupted by an asynchronous exception (#19114, #19115).

  * Additionally export `asum` from `Control.Applicative`

  * `fromInteger :: Integer -> Float/Double` now consistently round to the
    nearest value, with ties to even.

  * Additions to `Data.Bits`:

    - Newtypes `And`, `Ior`, `Xor` and `Iff` which wrap their argument,
      and whose `Semigroup` instances are defined using `(.&.)`, `(.|.)`, `xor`
      and ```\x y -> complement (x `xor` y)```, respectively.

    - `oneBits :: FiniteBits a => a`, `oneBits = complement zeroBits`.

## 4.15.0.0 *Feb 2021*

  * Shipped with GHC 9.0.1

  * `openFile` now calls the `open` system call with an `interruptible` FFI
    call, ensuring that the call can be interrupted with `SIGINT` on POSIX
    systems.

  * Make `openFile` more tolerant of asynchronous exceptions: more care taken
    to release the file descriptor and the read/write lock (#18832)

  * Add `hGetContents'`, `getContents'`, and `readFile'` in `System.IO`:
    Strict IO variants of `hGetContents`, `getContents`, and `readFile`.

  * Add `singleton` function for `Data.List.NonEmpty`.

  * The planned deprecation of `Data.Monoid.First` and `Data.Monoid.Last`
    is scrapped due to difficulties with the suggested migration path.

  * `Data.Semigroup.Option` and the accompanying `option` function are
    deprecated and scheduled for removal in 4.16.

  * Add `Generic` instances to `Fingerprint`, `GiveGCStats`, `GCFlags`,
    `ConcFlags`, `DebugFlags`, `CCFlags`, `DoHeapProfile`, `ProfFlags`,
    `DoTrace`, `TraceFlags`, `TickyFlags`, `ParFlags`, `RTSFlags`, `RTSStats`,
    `GCStats`, `ByteOrder`, `GeneralCategory`, `SrcLoc`

  * Add rules `unpackUtf8`, `unpack-listUtf8` and `unpack-appendUtf8` to `GHC.Base`.
    They correspond to their ascii versions and hopefully make it easier
    for libraries to handle utf8 encoded strings efficiently.

  * An issue with list fusion and `elem` was fixed. `elem` applied to known
    small lists will now compile to a simple case statement more often.

  * Add `MonadFix` and `MonadZip` instances for `Complex`

  * Add `Ix` instances for tuples of size 6 through 15

  * Correct `Bounded` instance and remove `Enum` and `Integral` instances for
    `Data.Ord.Down`.

  * `catMaybes` is now implemented using `mapMaybe`, so that it is both a "good
    consumer" and "good producer" for list-fusion (#18574)

  * `Foreign.ForeignPtr.withForeignPtr` is now less aggressively optimised,
    avoiding the soundness issue reported in
    [#17760](https://gitlab.haskell.org/ghc/ghc/-/issues/17760) in exchange for
    a small amount more allocation. If your application regresses significantly
    *and* the continuation given to `withForeignPtr` will *not* provably
    diverge then the previous optimisation behavior can be recovered by instead
    using `GHC.ForeignPtr.unsafeWithForeignPtr`.

  * Correct `Bounded` instance and remove `Enum` and `Integral` instances for
    `Data.Ord.Down`.

  * `Data.Foldable` methods `maximum{,By}`, `minimum{,By}`, `product` and `sum`
    are now stricter by default, as well as in the class implementation for List.

## 4.14.0.0 *Jan 2020*
  * Bundled with GHC 8.10.1

  * Add a `TestEquality` instance for the `Compose` newtype.

  * `Data.Ord.Down` now has a field name, `getDown`

  * Add `Bits`, `Bounded`, `Enum`, `FiniteBits`, `Floating`, `Fractional`,
    `Integral`, `Ix`, `Real`, `RealFrac`, `RealFloat` and `Storable` instances
    to `Data.Ord.Down`.

  * Fix the `integer-gmp` variant of `isValidNatural`: Previously it would fail
    to detect values `<= maxBound::Word` that were incorrectly encoded using
    the `NatJ#` constructor.

  * The type of `coerce` has been generalized. It is now runtime-representation
    polymorphic:
    `forall {r :: RuntimeRep} (a :: TYPE r) (b :: TYPE r). Coercible a b => a -> b`.
    The type argument `r` is marked as `Inferred` to prevent it from
    interfering with visible type application.

  * Make `Fixed` and `HasResolution` poly-kinded.

  * Add `HasResolution` instances for `Nat`s.

  * Add `Functor`, `Applicative`, `Monad`, `Alternative`, `MonadPlus`,
    `Generic` and `Generic1` instances to `Kleisli`

  * `openTempFile` is now fully atomic and thread-safe on Windows.

  * Add `isResourceVanishedError`, `resourceVanishedErrorType`, and
    `isResourceVanishedErrorType` to `System.IO.Error`.

  * Add newtypes for `CSocklen` (`socklen_t`) and `CNfds` (`nfds_t`) to
    `System.Posix.Types`.

  * Add `Functor`, `Applicative` and `Monad` instances to `(,,) a b`
    and `(,,,) a b c`.

  * Add `resizeSmallMutableArray#` to `GHC.Exts`.

  * Add a `Data` instance to `WrappedArrow`, `WrappedMonad`, and `ZipList`.

  * Add `IsList` instance for `ZipList`.

## 4.13.0.0 *July 2019*
  * Bundled with GHC 8.8.1

  * The final phase of the `MonadFail` proposal has been implemented:

    * The `fail` method of `Monad` has been removed in favor of the method of
      the same name in the `MonadFail` class.

    * `MonadFail(fail)` is now re-exported from the `Prelude` and
      `Control.Monad` modules.

  * Fix `Show` instance of `Data.Fixed`: Negative numbers are now parenthesized
    according to their surrounding context. I.e. `Data.Fixed.show` produces
    syntactically correct Haskell for expressions like `Just (-1 :: Fixed E2)`.
    (#16031)

  * Support the characters from recent versions of Unicode (up to v. 12) in
    literals (#5518).

  * The `StableName` type parameter now has a phantom role instead of
    a representational one. There is really no reason to care about the
    type of the underlying object.

  * Add `foldMap'`, a strict version of `foldMap`, to `Foldable`.

  * The `shiftL` and `shiftR` methods in the `Bits` instances of `Int`, `IntN`,
    `Word`, and `WordN` now throw an overflow exception for negative shift
    values (instead of being undefined behaviour).

  * `scanr` no longer crashes when passed a fusable, infinite list. (#16943)

## 4.12.0.0 *21 September 2018*
  * Bundled with GHC 8.6.1

  * The STM invariant-checking mechanism (`always` and `alwaysSucceeds`), which
    was deprecated in GHC 8.4, has been removed (as proposed in
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0011-deprecate-stm-invariants.rst>).
    This is a bit earlier than proposed in the deprecation pragma included in
    GHC 8.4, but due to community feedback we decided to move ahead with the
    early removal.

    Existing users are encouraged to encapsulate their STM operations in safe
    abstractions which can perform the invariant checking without help from the
    runtime system.

  * Add a new module `GHC.ResponseFile` (previously defined in the `haddock`
    package). (#13896)

  * Move the module `Data.Functor.Contravariant` from the
    `contravariant` package to `base`.

  * `($!)` is now representation-polymorphic like `($)`.

  * Add `Applicative` (for `K1`), `Semigroup` and `Monoid` instances in
    `GHC.Generics`. (#14849)

  * `asinh` for `Float` and `Double` is now numerically stable in the face of
    non-small negative arguments and enormous arguments of either sign. (#14927)

  * `Numeric.showEFloat (Just 0)` now respects the user's requested precision.
    (#15115)

  * `Data.Monoid.Alt` now has `Foldable` and `Traversable` instances. (#15099)

  * `Data.Monoid.Ap` has been introduced

  * `Control.Exception.throw` is now levity polymorphic. (#15180)

  * `Data.Ord.Down` now has a number of new instances. These include:
    `MonadFix`, `MonadZip`, `Data`, `Foldable`, `Traversable`, `Eq1`, `Ord1`,
    `Read1`, `Show1`, `Generic`, `Generic1`. (#15098)


## 4.11.1.0 *19 April 2018*
  * Bundled with GHC 8.4.2

  * Add the `readFieldHash` function to `GHC.Read` which behaves like
    `readField`, but for a field that ends with a `#` symbol (#14918).

## 4.11.0.0 *8 March 2018*
  * Bundled with GHC 8.4.1

  * `System.IO.openTempFile` is now thread-safe on Windows.

  * Deprecated `GHC.Stats.GCStats` interface has been removed.

  * Add `showHFloat` to `Numeric`

  * Add `Div`, `Mod`, and `Log2` functions on type-level naturals
    in `GHC.TypeLits`.

  * Add `Alternative` instance for `ZipList` (#13520)

  * Add instances `Num`, `Functor`, `Applicative`, `Monad`, `Semigroup`
    and `Monoid` for `Data.Ord.Down` (#13097).

  * Add `Semigroup` instance for `EventLifetime`.

  * Make `Semigroup` a superclass of `Monoid`;
    export `Semigroup((<>))` from `Prelude`; remove `Monoid` reexport
    from `Data.Semigroup` (#14191).

  * Generalise `instance Monoid a => Monoid (Maybe a)` to
    `instance Semigroup a => Monoid (Maybe a)`.

  * Add `infixl 9 !!` declaration for `Data.List.NonEmpty.!!`

  * Add `<&>` operator to `Data.Functor` (#14029)

  * Remove the deprecated `Typeable{1..7}` type synonyms (#14047)

  * Make `Data.Type.Equality.==` a closed type family. It now works for all
  kinds out of the box. Any modules that previously declared instances of this
  family will need to remove them. Whereas the previous definition was somewhat
  ad hoc, the behavior is now completely uniform. As a result, some applications
  that used to reduce no longer do, and conversely. Most notably, `(==)` no
  longer treats the `*`, `j -> k`, or `()` kinds specially; equality is
  tested structurally in all cases.

  * Add instances `Semigroup` and `Monoid` for `Control.Monad.ST` (#14107).

  * The `Read` instances for `Proxy`, `Coercion`, `(:~:)`, `(:~~:)`, and `U1`
    now ignore the parsing precedence. The effect of this is that `read` will
    be able to successfully parse more strings containing `"Proxy"` _et al._
    without surrounding parentheses (e.g., `"Thing Proxy"`) (#12874).

  * Add `iterate'`, a strict version of `iterate`, to `Data.List`
    and `Data.OldList` (#3474)

  * Add `Data` instances for `IntPtr` and `WordPtr` (#13115)

  * Add missing `MonadFail` instance for `Control.Monad.Strict.ST.ST`

  * Make `zipWith` and `zipWith3` inlinable (#14224)

  * `Type.Reflection.App` now matches on function types (fixes #14236)

  * `Type.Reflection.withTypeable` is now polymorphic in the `RuntimeRep` of
    its result.

  * Add `installSEHHandlers` to `MiscFlags` in `GHC.RTS.Flags` to determine if
    exception handling is enabled.

  * The deprecated functions `isEmptyChan` and `unGetChan` in
    `Control.Concurrent.Chan` have been removed (#13561).

  * Add `generateCrashDumpFile` to `MiscFlags` in `GHC.RTS.Flags` to determine
    if a core dump will be generated on crashes.

  * Add `generateStackTrace` to `MiscFlags` in `GHC.RTS.Flags` to determine if
    stack traces will be generated on unhandled exceptions by the RTS.

  * `getExecutablePath` now resolves symlinks on Windows (#14483)

  * Deprecated STM invariant checking primitives (`checkInv`, `always`, and
    `alwaysSucceeds`) in `GHC.Conc.Sync` (#14324).

  * Add a `FixIOException` data type to `Control.Exception.Base`, and change
    `fixIO` to throw that instead of a `BlockedIndefinitelyOnMVar` exception
    (#14356).

## 4.10.1.0 *November 2017*
  * Bundled with GHC 8.2.2

  * The file locking primitives provided by `GHC.IO.Handle` now use
    Linux open file descriptor locking if available.

  * Fixed bottoming definition of `clearBit` for `Natural`

## 4.10.0.0 *July 2017*
  * Bundled with GHC 8.2.1

  * `Data.Type.Bool.Not` given a type family dependency (#12057).

  * `Foreign.Ptr` now exports the constructors for `IntPtr` and `WordPtr`
    (#11983)

  * `Generic1`, as well as the associated datatypes and typeclasses in
    `GHC.Generics`, are now poly-kinded (#10604)

  * `New modules `Data.Bifoldable` and `Data.Bitraversable` (previously defined
    in the `bifunctors` package) (#10448)

  * `Data.Either` now provides `fromLeft` and `fromRight` (#12402)

  * `Data.Type.Coercion` now provides `gcoerceWith` (#12493)

  * New methods `liftReadList(2)` and `liftReadListPrec(2)` in the
    `Read1`/`Read2` classes that are defined in terms of `ReadPrec` instead of
    `ReadS`, as well as related combinators, have been added to
    `Data.Functor.Classes` (#12358)

  * Add `Semigroup` instance for `IO`, as well as for `Event` and `Lifetime`
    from `GHC.Event` (#12464)

  * Add `Data` instance for `Const` (#12438)

  * Added `Eq1`, `Ord1`, `Read1` and `Show1` instances for `NonEmpty`.

  * Add wrappers for `blksize_t`, `blkcnt_t`, `clockid_t`, `fsblkcnt_t`,
    `fsfilcnt_t`, `id_t`, `key_t`, and `timer_t` to System.Posix.Types (#12795)

  * Add `CBool`, a wrapper around C's `bool` type, to `Foreign.C.Types`
    (#13136)

  * Raw buffer operations in `GHC.IO.FD` are now strict in the buffer, offset, and length operations (#9696)

  * Add `plusForeignPtr` to `Foreign.ForeignPtr`.

  * Add `type family AppendSymbol (m :: Symbol) (n :: Symbol) :: Symbol` to `GHC.TypeLits`
    (#12162)

  * Add `GHC.TypeNats` module with `Natural`-based `KnownNat`. The `Nat`
    operations in `GHC.TypeLits` are a thin compatibility layer on top.
    Note: the `KnownNat` evidence is changed from an `Integer` to a `Natural`.

  * The type of `asProxyTypeOf` in `Data.Proxy` has been generalized (#12805)

  * `liftA2` is now a method of the `Applicative` class. `liftA2` and
    `<*>` each have a default implementation based on the other. Various
    library functions have been updated to use `liftA2` where it might offer
    some benefit. `liftA2` is not yet in the `Prelude`, and must currently be
    imported from `Control.Applicative`. It is likely to be added to the
    `Prelude` in the future. (#13191)

  * A new module, `Type.Reflection`, exposing GHC's new type-indexed type
    representation mechanism is now provided.

  * `Data.Dynamic` now exports the `Dyn` data constructor, enabled by the new
    type-indexed type representation mechanism.

  * `Data.Type.Equality` now provides a kind heterogeneous type equality
    evidence type, `(:~~:)`.

  * The `CostCentresXML` constructor of `GHC.RTS.Flags.DoCostCentres` has been
    replaced by `CostCentresJSON` due to the new JSON export format supported by
    the cost centre profiler.

  * The `ErrorCall` pattern synonym has been given a `COMPLETE` pragma so that
    functions which solely match again `ErrorCall` do not produce
    non-exhaustive pattern-match warnings (#8779)

  * Change the implementations of `maximumBy` and `minimumBy` from
    `Data.Foldable` to use `foldl1` instead of `foldr1`. This makes them run
    in constant space when applied to lists. (#10830)

  * `mkFunTy`, `mkAppTy`, and `mkTyConApp` from `Data.Typeable` no longer exist.
    This functionality is superceded by the interfaces provided by
    `Type.Reflection`.

  * `mkTyCon3` is no longer exported by `Data.Typeable`. This function is
    replaced by `Type.Reflection.Unsafe.mkTyCon`.

  * `Data.List.NonEmpty.unfold` has been deprecated in favor of `unfoldr`,
    which is functionally equivalent.

## 4.9.0.0  *May 2016*

  * Bundled with GHC 8.0

  * `error` and `undefined` now print a partial stack-trace alongside the error message.

  * New `errorWithoutStackTrace` function throws an error without printing the stack trace.

  * The restore operation provided by `mask` and `uninterruptibleMask` now
    restores the previous masking state whatever the current masking state is.

  * New `GHC.Generics.packageName` operation

  * Redesigned `GHC.Stack.CallStack` data type. As a result, `CallStack`'s
    `Show` instance produces different output, and `CallStack` no longer has an
    `Eq` instance.

  * New `GHC.Generics.packageName` operation

  * New `GHC.Stack.Types` module now contains the definition of
    `CallStack` and `SrcLoc`

  * New `GHC.Stack.Types.emptyCallStack` function builds an empty `CallStack`

  * New `GHC.Stack.Types.freezeCallStack` function freezes a `CallStack` preventing future `pushCallStack` operations from having any effect

  * New `GHC.Stack.Types.pushCallStack` function pushes a call-site onto a `CallStack`

  * New `GHC.Stack.Types.fromCallSiteList` function creates a `CallStack` from
    a list of call-sites (i.e., `[(String, SrcLoc)]`)

  * `GHC.SrcLoc` has been removed

  * `GHC.Stack.showCallStack` and `GHC.SrcLoc.showSrcLoc` are now called
    `GHC.Stack.prettyCallStack` and `GHC.Stack.prettySrcLoc` respectively

  * add `Data.List.NonEmpty` and `Data.Semigroup` (to become
    super-class of `Monoid` in the future). These modules were
    provided by the `semigroups` package previously. (#10365)

  * Add `selSourceUnpackedness`, `selSourceStrictness`, and
    `selDecidedStrictness`, three functions which look up strictness
    information of a field in a data constructor, to the `Selector` type class
    in `GHC.Generics` (#10716)

  * Add `URec`, `UAddr`, `UChar`, `UDouble`, `UFloat`, `UInt`, and `UWord` to
    `GHC.Generics` as part of making GHC generics capable of handling
    unlifted types (#10868)

  * The `Eq`, `Ord`, `Read`, and `Show` instances for `U1` now use lazier
    pattern-matching

  * Keep `shift{L,R}` on `Integer` with negative shift-arguments from
    segfaulting (#10571)

  * Add `forkOSWithUnmask` to `Control.Concurrent`, which is like
    `forkIOWithUnmask`, but the child is run in a bound thread.

  * The `MINIMAL` definition of `Arrow` is now `arr AND (first OR (***))`.

  * The `MINIMAL` definition of `ArrowChoice` is now `left OR (+++)`.

  * Exported `GiveGCStats`, `DoCostCentres`, `DoHeapProfile`, `DoTrace`,
    `RtsTime`, and `RtsNat` from `GHC.RTS.Flags`

  * New function `GHC.IO.interruptible` used to correctly implement
    `Control.Exception.allowInterrupt` (#9516)

  * Made `PatternMatchFail`, `RecSelError`, `RecConError`, `RecUpdError`,
    `NoMethodError`, and `AssertionFailed` newtypes (#10738)

  * New module `Control.Monad.IO.Class` (previously provided by `transformers`
    package). (#10773)

  * New modules `Data.Functor.Classes`, `Data.Functor.Compose`,
    `Data.Functor.Product`, and `Data.Functor.Sum` (previously provided by
    `transformers` package). (#11135)

  * New instances for `Proxy`: `Eq1`, `Ord1`, `Show1`, `Read1`. All
    of the classes are from `Data.Functor.Classes` (#11756).

  * New module `Control.Monad.Fail` providing new `MonadFail(fail)`
    class (#10751)

  * Add `GHC.TypeLits.TypeError` and `ErrorMessage` to allow users
    to define custom compile-time error messages.

  * Redesign `GHC.Generics` to use type-level literals to represent the
    metadata of generic representation types (#9766)

  * The `IsString` instance for `[Char]` has been modified to eliminate
    ambiguity arising from overloaded strings and functions like `(++)`.

  * Move `Const` from `Control.Applicative` to its own module in
   `Data.Functor.Const`. (#11135)

  * Re-export `Const` from `Control.Applicative` for backwards compatibility.

  * Expand `Floating` class to include operations that allow for better
    precision: `log1p`, `expm1`, `log1pexp` and `log1mexp`. These are not
    available from `Prelude`, but the full class is exported from `Numeric`.

  * New `Control.Exception.TypeError` datatype, which is thrown when an
    expression fails to typecheck when run using `-fdefer-type-errors` (#10284)

  * The `bitSize` method of `Data.Bits.Bits` now has a (partial!)
    default implementation based on `bitSizeMaybe`. (#12970)

### New instances

  * `Alt`, `Dual`, `First`, `Last`, `Product`, and `Sum` now have `Data`,
    `MonadZip`, and `MonadFix` instances

  * The datatypes in `GHC.Generics` now have `Enum`, `Bounded`, `Ix`,
    `Functor`, `Applicative`, `Monad`, `MonadFix`, `MonadPlus`, `MonadZip`,
    `Foldable`, `Foldable`, `Traversable`, `Generic1`, and `Data` instances
    as appropriate.

  * `Maybe` now has a `MonadZip` instance

  * `All` and `Any` now have `Data` instances

  * `Dual`, `First`, `Last`, `Product`, and `Sum` now have `Foldable` and
    `Traversable` instances

  * `Dual`, `Product`, and `Sum` now have `Functor`, `Applicative`, and
    `Monad` instances

  * `(,) a` now has a `Monad` instance

  * `ZipList` now has `Foldable` and `Traversable` instances

  * `Identity` now has `Semigroup` and `Monoid` instances

  * `Identity` and `Const` now have `Bits`, `Bounded`, `Enum`, `FiniteBits`,
    `Floating`, `Fractional`, `Integral`, `IsString`, `Ix`, `Num`, `Real`,
    `RealFloat`, `RealFrac` and `Storable` instances. (#11210, #11790)

  * `()` now has a `Storable` instance

  * `Complex` now has `Generic`, `Generic1`, `Functor`, `Foldable`, `Traversable`,
    `Applicative`, and `Monad` instances

  * `System.Exit.ExitCode` now has a `Generic` instance

  * `Data.Version.Version` now has a `Generic` instance

  * `IO` now has a `Monoid` instance

  * Add `MonadPlus IO` and `Alternative IO` instances
    (previously orphans in `transformers`) (#10755)

  * `CallStack` now has an `IsList` instance

  * The field `spInfoName` of `GHC.StaticPtr.StaticPtrInfo` has been removed.
    The value is no longer available when constructing the `StaticPtr`.

  * `VecElem` and `VecCount` now have `Enum` and `Bounded` instances.

### Generalizations

  * Generalize `Debug.Trace.{traceM, traceShowM}` from `Monad` to `Applicative`
    (#10023)

  * Redundant typeclass constraints have been removed:
     - `Data.Ratio.{denominator,numerator}` have no `Integral` constraint anymore
     - **TODO**

  * Generalise `forever` from `Monad` to `Applicative`

  * Generalize `filterM`, `mapAndUnzipM`, `zipWithM`, `zipWithM_`, `replicateM`,
    `replicateM_` from `Monad` to `Applicative` (#10168)

  * The `Generic` instance for `Proxy` is now poly-kinded (#10775)

  * Enable `PolyKinds` in the `Data.Functor.Const` module to give `Const`
    the kind `* -> k -> *`. (#10039)


## 4.8.2.0  *Oct 2015*

  * Bundled with GHC 7.10.3

  * The restore operation provided by `mask` and `uninterruptibleMask` now
    restores the previous masking state whatever the current masking state is.

  * Exported `GiveGCStats`, `DoCostCentres`, `DoHeapProfile`, `DoTrace`,
    `RtsTime`, and `RtsNat` from `GHC.RTS.Flags`

## 4.8.1.0  *Jul 2015*

  * Bundled with GHC 7.10.2

  * `Lifetime` is now exported from `GHC.Event`

  * Implicit-parameter based source location support exposed in `GHC.SrcLoc` and `GHC.Stack`.
    See GHC User's Manual for more information.

## 4.8.0.0  *Mar 2015*

  * Bundled with GHC 7.10.1

  * Make `Applicative` a superclass of `Monad`

  * Add reverse application operator `Data.Function.(&)`

  * Add `Data.List.sortOn` sorting function

  * Add `System.Exit.die`

  * Deprecate `versionTags` field of `Data.Version.Version`.
    Add `makeVersion :: [Int] -> Version` constructor function to aid
    migration to a future `versionTags`-less `Version`.

  * Add `IsList Version` instance

  * Weaken RealFloat constraints on some `Data.Complex` functions

  * Add `Control.Monad.(<$!>)` as a strict version of `(<$>)`

  * The `Data.Monoid` module now has the `PolyKinds` extension
    enabled, so that the `Monoid` instance for `Proxy` are polykinded
    like `Proxy` itself is.

  * Make `abs` and `signum` handle (-0.0) correctly per IEEE-754.

  * Re-export `Data.Word.Word` from `Prelude`

  * Add `countLeadingZeros` and `countTrailingZeros` methods to
    `Data.Bits.FiniteBits` class

  * Add `Data.List.uncons` list destructor (#9550)

  * Export `Monoid(..)` from `Prelude`

  * Export `Foldable(..)` from `Prelude`
    (hiding `fold`, `foldl'`, `foldr'`, and `toList`)

  * Export `Traversable(..)` from `Prelude`

  * Set fixity for `Data.Foldable.{elem,notElem}` to match the
    conventional one set for `Data.List.{elem,notElem}` (#9610)

  * Turn `toList`, `elem`, `sum`, `product`, `maximum`, and `minimum`
    into `Foldable` methods (#9621)

  * Replace the `Data.List`-exported functions

    ```
    all, and, any, concat, concatMap, elem, find, product, sum,
    mapAccumL, mapAccumR
    ```

    by re-exports of their generalised `Data.Foldable`/`Data.Traversable`
    counterparts.  In other words, unqualified imports of `Data.List`
    and `Data.Foldable`/`Data.Traversable` no longer lead to conflicting
    definitions. (#9586)

  * New (unofficial) module `GHC.OldList` containing only list-specialised
    versions of the functions from `Data.List` (in other words, `GHC.OldList`
    corresponds to `base-4.7.0.2`'s `Data.List`)

  * Replace the `Control.Monad`-exported functions

    ```
    sequence_, msum, mapM_, forM_,
    forM, mapM, sequence
    ```

    by re-exports of their generalised `Data.Foldable`/`Data.Traversable`
    counterparts.  In other words, unqualified imports of `Control.Monad`
    and `Data.Foldable`/`Data.Traversable` no longer lead to conflicting
    definitions. (#9586)

  * Generalise `Control.Monad.{when,unless,guard}` from `Monad` to
    `Applicative` and from `MonadPlus` to `Alternative` respectively.

  * Generalise `Control.Monad.{foldM,foldM_}` to `Foldable`

  * `scanr`, `mapAccumL` and `filterM` now take part in list fusion (#9355,
    #9502, #9546)

  * Remove deprecated `Data.OldTypeable` (#9639)

  * New module `Data.Bifunctor` providing the `Bifunctor(bimap,first,second)`
    class (previously defined in `bifunctors` package) (#9682)

  * New module `Data.Void` providing the canonical uninhabited type `Void`
    (previously defined in `void` package) (#9814)

  * Update Unicode class definitions to Unicode version 7.0

  * Add `Alt`, an `Alternative` wrapper, to `Data.Monoid`. (#9759)

  * Add `isSubsequenceOf` to `Data.List` (#9767)

  * The arguments to `==` and `eq` in `Data.List.nub` and `Data.List.nubBy`
    are swapped, such that `Data.List.nubBy (<) [1,2]` now returns `[1]`
    instead of `[1,2]` (#2528, #3280, #7913)

  * New module `Data.Functor.Identity` (previously provided by `transformers`
    package). (#9664)

  * Add `scanl'`, a strictly accumulating version of `scanl`, to `Data.List`
    and `Data.OldList`. (#9368)

  * Add `fillBytes` to `Foreign.Marshal.Utils`.

  * Add new `displayException` method to `Exception` typeclass. (#9822)

  * Add `Data.Bits.toIntegralSized`, a size-checked version of
    `fromIntegral`. (#9816)

  * New module `Numeric.Natural` providing new `Natural` type
    representing non-negative arbitrary-precision integers.  The `GHC.Natural`
    module exposes additional GHC-specific primitives. (#9818)

  * Add `(Storable a, Integeral a) => Storable (Ratio a)` instance (#9826)

  * Add `Storable a => Storable (Complex a)` instance (#9826)

  * New module `GHC.RTS.Flags` that provides accessors to runtime flags.

  * Expose functions for per-thread allocation counters and limits in `GHC.Conc`

        disableAllocationLimit :: IO ()
        enableAllocationLimit :: IO ()
        getAllocationCounter :: IO Int64
        setAllocationCounter :: Int64 -> IO ()

    together with a new exception `AllocationLimitExceeded`.

  * Make `read . show = id` for `Data.Fixed` (#9240)

  * Add `calloc` and `callocBytes` to `Foreign.Marshal.Alloc`. (#9859)

  * Add `callocArray` and `callocArray0` to `Foreign.Marshal.Array`. (#9859)

  * Restore invariant in `Data (Ratio a)` instance (#10011)

  * Add/expose `rnfTypeRep`, `rnfTyCon`, `typeRepFingerprint`, and
    `tyConFingerprint` helpers to `Data.Typeable`.

  * Define proper `MINIMAL` pragma for `class Ix`. (#10142)

## 4.7.0.2  *Dec 2014*

  * Bundled with GHC 7.8.4

  * Fix performance bug in `Data.List.inits` (#9345)

  * Fix handling of null bytes in `Debug.Trace.trace` (#9395)

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

  * There are now `Show`, `Read`, `Eq`, `Ord`, `Monoid`, `Generic`, and
    `Generic1` instances for `Const`.

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
