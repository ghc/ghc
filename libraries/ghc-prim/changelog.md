## 0.14.0

- ghc-prim's modules have been merged into ghc-internal and ghc-prim is now deprecated.

## 0.13.0

- Shipped with GHC 9.12.1

- Add primops that allow users to distinguish weakly pinned byte arrays from unpinned ones.

         isMutableByteArrayWeaklyPinned# :: MutableByteArray# s -> Int#
         isByteArrayWeaklyPinned# :: ByteArray# s -> Int#

## 0.12.0

- Shipped with GHC 9.10.1

- Add unaligned addr access primops. These primops will be emulated on platforms that don't support unaligned access.

         readWord8OffAddrAsChar# :: Addr# -> Int# -> State# s -> (# State# s, Char# #)
         readWord8OffAddrAsAddr# :: Addr# -> Int# -> State# s -> (# State# s, Addr# #)
         readWord8OffAddrAsFloat# :: Addr# -> Int# -> State# s -> (# State# s, Float# #)
         readWord8OffAddrAsDouble# :: Addr# -> Int# -> State# s -> (# State# s, Double# #)
         readWord8OffAddrAsStablePtr# :: Addr# -> Int# -> State# s -> (# State# s, StablePtr# #)
         readWord8OffAddrAsInt16# :: Addr# -> Int# -> State# s -> (# State# s, Int16# #)
         readWord8OffAddrAsInt32# :: Addr# -> Int# -> State# s -> (# State# s, Int32# #)
         readWord8OffAddrAsInt64# :: Addr# -> Int# -> State# s -> (# State# s, Int64# #)
         readWord8OffAddrAsInt# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)

         readWord8OffAddrAsWord16# :: Addr# -> Int# -> State# s -> (# State# s, Word16# #)
         readWord8OffAddrAsWord32# :: Addr# -> Int# -> State# s -> (# State# s, Word32# #)
         readWord8OffAddrAsWord64# :: Addr# -> Int# -> State# s -> (# State# s, Word64# #)
         readWord8OffAddrAsWord# :: Addr# -> Int# -> State# s -> (# State# s, Word# #)

         indexWord8OffAddrAsChar# :: Addr# -> Int# -> Char#
         indexWord8OffAddrAsAddr# :: Addr# -> Int# -> Addr#
         indexWord8OffAddrAsFloat# :: Addr# -> Int# -> Float#
         indexWord8OffAddrAsDouble# :: Addr# -> Int# -> Double#
         indexWord8OffAddrAsStablePtr# :: Addr# -> Int# -> StablePtr#
         indexWord8OffAddrAsInt16# :: Addr# -> Int# -> Int16#
         indexWord8OffAddrAsInt32# :: Addr# -> Int# -> Int32#
         indexWord8OffAddrAsInt64# :: Addr# -> Int# -> Int64#
         indexWord8OffAddrAsInt# :: Addr# -> Int# -> Int#

         indexWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16#
         indexWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32#
         indexWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64#
         indexWord8OffAddrAsWord# :: Addr# -> Int# -> Word#

         writeWord8OffAddrAsChar# :: Addr# -> Int# -> Char# -> State# s -> State# s
         writeWord8OffAddrAsAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
         writeWord8OffAddrAsFloat# :: Addr# -> Int# -> Float# -> State# s -> State# s
         writeWord8OffAddrAsDouble# :: Addr# -> Int# -> Double# -> State# s -> State# s
         writeWord8OffAddrAsStablePtr# :: Addr# -> Int# -> StablePtr# -> State# s -> State# s

         writeWord8OffAddrAsInt16# :: Addr# -> Int# -> Int16# -> State# s -> State# s
         writeWord8OffAddrAsInt32# :: Addr# -> Int# -> Int32# -> State# s -> State# s
         writeWord8OffAddrAsInt64# :: Addr# -> Int# -> Int64# -> State# s -> State# s
         writeWord8OffAddrAsInt# :: Addr# -> Int# -> Int# -> State# s -> State# s

         writeWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16# -> State# s -> State# s
         writeWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32# -> State# s -> State# s
         writeWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64# -> State# s -> State# s
         writeWord8OffAddrAsWord# :: Addr# -> Int# -> Word# -> State# s -> State# s

- The `unsafeThawByteArray#` primop was added, serving as a inverse to the existing
  `unsafeFreezeByteArray#` primop (see #22710).

- `dataToTag#` has been moved to `GHC.Magic` and made the sole method
  of a new class:

  ```haskell
  type DataToTag :: forall {lev :: Levity}. TYPE (BoxedRep lev) -> Constraint
  class DataToTag a where
    dataToTag# :: a -> Int#
  ```

  In particular, it is now applicable only at some (not all)
  lifted types.  However, if `t` is an algebraic data type (i.e. `t`
  matches a `data` or `data instance` declaration) with all of its
  constructors in scope and the levity of `t` is statically known,
  then the constraint `DataToTag t` can always be solved.

- Renamed several built-in tycon syntaxes to avoid punning:

  - Unboxed tuple tycons are now `Tuple#<N>`
  - Unboxed sum tycons are now `Sum#<N>`
  - Constraint tuple classes are now `CTuple<N>`
  - Unit tycons are now `Unit#`, `CUnit`.
  - Solo tycons are now `Solo#`, `CSolo`.
  - `Tuple<N>` have been moved back to `GHC.Tuple`.

  See [https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0475-tuple-syntax.rst].

## 0.11.0

- Shipped with GHC 9.8.1

- Primitive pointer comparison functions are now levity-polymorphic, e.g.

  ```haskell
  sameArray# :: forall {l} (a :: TYPE (BoxedRep l)). Array# a -> Array# a -> Int#
  ```

  This change affects the following functions:
    - `sameArray#`, `sameMutableArray#`,
    - `sameSmallArray#`, `sameSmallMutableArray#`,
    - `sameMutVar#`, `sameTVar#`, `sameMVar#`
    - `sameIOPort#`, `eqStableName#`.

- `keepAlive#` and `touch#` are now polymorphic in their state token (#23163; [CLC#152](https://github.com/haskell/core-libraries-committee/issues/152))

- Several new primops were added:

  - `copyMutableByteArrayNonOverlapping#`
  - `copyAddrToAddr#`
  - `copyAddrToAddrNonOverlapping#`
  - `setAddrRange#`

- New primops for fused multiply-add operations. These primops combine a
  multiplication and an addition, compiling to a single instruction when
  the `-mfma` flag is enabled and the architecture supports it.

  The new primops are `fmaddFloat#, fmsubFloat#, fnmaddFloat#, fnmsubFloat# :: Float# -> Float# -> Float# -> Float#`
  and `fmaddDouble#, fmsubDouble#, fnmaddDouble#, fnmsubDouble# :: Double# -> Double# -> Double# -> Double#`.

  These implement the following operations, while performing one single
  rounding at the end, leading to a more accurate result:

    - `fmaddFloat# x y z`, `fmaddDouble# x y z` compute `x * y + z`.
    - `fmsubFloat# x y z`, `fmsubDouble# x y z` compute `x * y - z`.
    - `fnmaddFloat# x y z`, `fnmaddDouble# x y z` compute `- x * y + z`.
    - `fnmsubFloat# x y z`, `fnmsubDouble# x y z` compute `- x * y - z`.

  Warning: on unsupported architectures, the software emulation provided by
  the fallback to the C standard library is not guaranteed to be IEEE-compliant.

- `Unit`, `Tuple0`, `Tuple1`, `Tuple2`, `Tuple3` and so on (up to `Tuple64`)
  are now exported from `GHC.Tuple.Prim` and reexported from `GHC.Tuple`.
  GHC now uses these as the actual names for tuple data types. As a result,
  the "brackets with commas" syntax (e.g. `()`, `(,)`, etc.) now becomes just
  an alias to these names. This change may affect tools and libraries that
  rely on type names, such as `Generic` and Template Haskell.

## 0.10.0

- Shipped with GHC 9.6.1

- The `listThreads#` primop was added, allowing the user to enumerate all
  threads (running and blocked) in the program:
  ```haskell
  listThreads# :: State# RealWorld -> (# State# RealWorld, Array# ThreadId# #)
  ```

- The type of the `labelThread#` primop was changed from:
  ```haskell
  labelThread# :: ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
  ```
  to
  ```haskell
  labelThread# :: ThreadId# -> ByteArray# -> State# RealWorld -> State# RealWorld
  ```
  Where the `ByteArray#` must contain a UTF-8-encoded string.

- The `threadLabel#` primop was added, allowing the user to query the label of
  a given `ThreadId#`.

- `isByteArrayPinned#` now only considers an array pinned if it was explicitly pinned
  by the user. This is required to avoid ghc issue [#22255](https://gitlab.haskell.org/ghc/ghc/-/issues/22255)
  which showed that the old behaviour could cause segfaults when used in combination
  with compact regions.
  We are working on ways to allow users and library authors to get back the
  performance benefits of the old behaviour where possible.

- `List` is now exported from `GHC.Types`.

## 0.9.0 *August 2022*

- Shipped with GHC 9.4.1

- `magicDict` has been renamed to `withDict` and is now defined in
  `GHC.Magic.Dict` instead of `GHC.Prim`. `withDict` now has the type:

  ```haskell
  withDict :: forall {rr :: RuntimeRep} st dt (r :: TYPE rr). st -> (dt => r) -> r
  ```

  Unlike `magicDict`, `withDict` can be used without defining an
  intermediate data type. For example, the `withTypeable` function from the
  `Data.Typeable` module can now be defined as:

  ```haskell
  withTypeable :: forall k (a :: k) rep (r :: TYPE rep). ()
               => TypeRep a -> (Typeable a => r) -> r
  withTypeable rep k = withDict @(TypeRep a) @(Typeable a) rep k
  ```

  Note that the explicit type applications are required, as the call to
  `withDict` would be ambiguous otherwise.

- Primitive types and functions which handle boxed values are now levity-polymorphic,
  meaning that they now also work with unlifted boxed values (i.e. values whose type
  has kind `TYPE (BoxedRep Unlifted)`).

  The following type constructors are now levity-polymorphic:

    - `Array#`, `SmallArray#`, `Weak#`, `StablePtr#`, `StableName#`,

    - `MutableArray#`, `SmallMutableArray#`, `MutVar#`,
      `TVar#`, `MVar#`, `IOPort#`.

  For example, `Array#` used to have kind:

  ```haskell
  Type -> UnliftedType
  ```

  but it now has kind:

  ```haskell
  forall {l :: Levity}. TYPE (BoxedRep l) -> UnliftedType
  ```

  Similarly, `MutVar#` used to have kind:

  ```haskell
  Type -> Type -> UnliftedType
  ```

  but it now has kind:

  ```haskell
  forall {l :: Levity}. Type -> TYPE (BoxedRep l) -> UnliftedType
  ```

  This means that in `Array# a`, `MutableArray# s a`, `MutVar# s a`, ...,
  the element type `a`, must always be boxed, but it can now either be lifted
  or unlifted.
  In particular, arrays and mutable variables can now be used to store
  other arrays and mutable variables.

  All functions which use these updated primitive types are also levity-polymorphic:

    - all array operations (reading/writing/copying/...), for both arrays and small arrays,
      mutable and immutable:

      - `newArray#`, `readArray#`, `writeArray#`, `sizeofArray#`, `sizeofMutableArray#`, `indexArray#`,
        `unsafeFreezeArray#`, `unsafeThawArray#`, `copyArray#`, `copyMutableArray#`, `cloneArray#`,
        `cloneMutableArray#`, `freezeArray#`, `thawArray#`, `casArray#`,

      - `newSmallArray#`, `shrinkSmallMutableArray#`, `readSmallArray#`, `writeSmallArray#`, `sizeofSmallArray#`,
        `getSizeofSmallMutableArray#`, `indexSmallArray#`, `unsafeFreezeSmallArray#`,
        `unsafeThawSmallArray#`, `copySmallArray#`, `copySmallMutableArray#`, `cloneSmallArray#`,
        `cloneSmallMutableArray#`, `freezeSmallArray#`, `thawSmallArray#`, `casSmallArray#`,

    - `newMutVar#`, `readMutVar#`, `writeMutVar#`,`casMutVar#`,

    - operations on `MVar#` and `TVar#`:

      - `newTVar#`, `readTVar#`, `readTVarIO#`, `writeTVar#`,

      - `newMVar#`, `takeMVar#`, `tryTakeMVar#`, `putMVar#`,
        `tryPutMVar#`, `readMVar#`, `tryReadMVar#`,

    - `STM` operations `atomically#`, `retry#`, `catchRetry#` and `catchSTM#`.

    - `newIOPort#`, `readIOPort#`, `writeIOPort#`,

    - `mkWeak#`, `mkWeakNoFinalizer#`, `addCFinalizerToWeak#`, `deRefWeak#`, `finalizeWeak#`,

    - `makeStablePtr#`, `deRefStablePtr#`, `eqStablePtr#`, `makeStableName#`, `stableNameToInt#`,

  For example, the full type of `newMutVar#` is now:

  ```haskell
  newMutVar#
    :: forall {l :: Levity} s (a :: TYPE (BoxedRep l)).
       a -> State# s -> (# State# s, MVar# s a #)
  ```

  and the full type of `writeSmallArray#` is:

  ```haskell
  writeSmallArray#
    :: forall {l :: Levity} s (a :: TYPE ('BoxedRep l)).
       SmallMutableArray# s a -> Int# -> a -> State# s -> State# s
  ```

- `ArrayArray#` and `MutableArrayArray#` have been moved from `GHC.Prim` to `GHC.Exts`.
  They are deprecated, because their functionality is now subsumed by `Array#`
  and `MutableArray#`.

- `mkWeak#`, `mkWeakNoFinalizer#`, `touch#` and `keepAlive#` are now
  levity-polymorphic instead of representation-polymorphic. For instance:

  ```haskell
  mkWeakNoFinalizer#
    :: forall {l :: Levity} {k :: Levity}
              (a :: TYPE ('BoxedRep l))
              (b :: TYPE ('BoxedRep k)).
       a -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ```

  That is, the type signature now quantifies over the `Levity` of `a`
  instead of its `RuntimeRep`. In addition, this variable is now inferred,
  instead of specified, meaning that it is no longer eligible for visible type application.
  Note that `b` is now also levity-polymorphic, due to the change outlined in the
  previous point.

- Primitive functions for throwing and catching exceptions are now more polymorphic
  than before. For example, `catch#` now has type:

  ```haskell
  catch#
    :: forall {r :: RuntimeRep} {l :: Levity}
              (a :: TYPE r)
              (b :: TYPE ('BoxedRep l)).
        ( State# RealWorld -> (# State# RealWorld, a #) )
    -> ( b -> State# RealWorld -> (# State# RealWorld, a #) )
    -> State# RealWorld -> (# State# RealWorld, a #)
  ```

  The following functions are concerned:

    - `catch#`,

    - `raise#`, `raiseIO#`,

    - `maskAsyncExceptions#`, `maskUninterruptible#`, `unmaskAsyncExceptions#`.

  Note in particular that `raise#` is now both representation-polymorphic
  (with an inferred `RuntimeRep` argument) and levity-polymorphic, with type:

  ```haskell
  raise# :: forall {l :: Levity} {r :: RuntimeRep}
                   (a :: TYPE (BoxedRep l))
                   (b :: TYPE r).
            a -> b
  ```

- ``fork#`` and ``forkOn#`` are now representation-polymorphic. For example, ``fork#``
  now has type: ::

      fork# :: forall {r :: RuntimeRep} (a :: TYPE r).
               (State# RealWorld -> (# State# RealWorld, a #))
            -> (State# RealWorld -> (# State# RealWorld, a #))

- `reallyUnsafePtrEquality#` has been made more general, as it is now
   both levity-polymorphic and heterogeneous:

  ```haskell
  reallyUnsafePtrEquality#
    :: forall {l :: Levity} {k :: Levity}
              (a :: TYPE (BoxedRep l))
              (b :: TYPE (BoxedRep k))
    . a -> b -> Int#
  ```

   This means that `reallyUnsafePtrEquality#` can be used on primitive arrays
   such as `Array#` and `ByteArray#`. It can also be used on values of
   different types, without needing to call `unsafeCoerce#`.

- The following functions have been moved from `GHC.Prim` to `GHC.Exts`:
  - `sameMutableArray#`, `sameSmallMutableArray#`, `sameMutableByteArray#`
     and `sameMutableArrayArray#`,
  - `sameMutVar#`, `sameTVar#` and`sameMVar#`,
  - `sameIOPort#`,
  - `eqStableName#`.

- The following functions have been added to `GHC.Exts`:

  ```haskell
  sameArray# :: Array# a -> Array# a -> Int#
  sameSmallArray# :: SmallArray# a -> SmallArray# a -> Int#
  sameByteArray# :: ByteArray# -> ByteArray# -> Int#
  sameArrayArray# :: ArrayArray# -> ArrayArray# -> Int#
  ```

## 0.8.0

- Change array access primops to use type with size maxing the element size:

   - `index{Int,Word}<N>Array# :: ByteArray# -> Int# -> {Int,Word}<N>#`
   - `indexWord8ArrayAs{Int,Word}<N># :: ByteArray# -> Int# -> {Int,Word}<N>#`
   - `read{Int,Word}<N>Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, {Int,Word}<N># #)`
   - `write{Int,Word}<N>Array# :: MutableByteArray# s -> Int# -> {Int,Word}<N># -> State# s -> State# s`
   - `readWord8ArrayAs{Int,Word}<N># :: MutableByteArray# s -> Int# -> State# s -> (# State# s, {Int,Word}<N># #)`
   - `writeWord8ArrayAs{Int,Word}<N># :: MutableByteArray# s -> Int# -> {Int,Word}<N># -> State# s -> State# s`

  This was already the for the 64-bit access primops, but not the smaller ones.

- Rename some numeric prim type conversion primops:

   - `extend{Int,Word}<N>#` -> `extend<N>To{Int,Word}#`
   - `narrow{Int,Word}<N>#` -> `intTo{Int,Word}<N>#`

- Add primops for atomic compare and swap for sizes other that wordsize:

   	casInt8Array# :: MutableByteArray# s -> Int# -> Int8# -> Int8# -> State# s -> (# State# s, Int8# #)
	casInt16Array# :: MutableByteArray# s -> Int# -> Int16# -> Int16# -> State# s -> (# State# s, Int16# #)
	casInt32Array# :: MutableByteArray# s -> Int# -> Int32# -> Int32# -> State# s -> (# State# s, Int32# #)
	casInt64Array# :: MutableByteArray# s -> Int# -> Int64# -> Int64# -> State# s -> (# State# s, Int64# #)
	atomicCasWord8Addr# :: Addr# -> Word8# -> Word8# -> State# s -> (# State# s, Word8# #)
	atomicCasWord16Addr# :: Addr# -> Word16# -> Word16# -> State# s -> (# State# s, Word16# #)
	atomicCasWord32Addr# :: Addr# -> Word32# -> Word32# -> State# s -> (# State# s, Word32# #)
	atomicCasWord64Addr# :: Addr# -> WORD64 -> WORD64 -> State# s -> (# State# s, WORD64 #)

## 0.7.0

- Shipped with GHC 9.0.1

- Add known-key `cstringLength#` to `GHC.CString`. This is just the
  C function `strlen`, but a built-in rewrite rule allows GHC to
  compute the result at compile time when the argument is known.

- In order to support unicode better the following functions in `GHC.CString`
  gained UTF8 counterparts:

        unpackAppendCStringUtf8# :: Addr# -> [Char] -> [Char]
        unpackFoldrCStringUtf8# :: Addr# -> (Char -> a -> a) -> a -> a

- unpackFoldrCString* variants can now inline in phase [0].

  If the folding function is known this allows for unboxing of the
  Char argument resulting in much faster code.

- Renamed the singleton tuple `GHC.Tuple.Unit` to `GHC.Tuple.Solo`.

- Add primops for atomic exchange:

        atomicExchangeAddrAddr# :: Addr# -> Addr# -> State# s -> (# State# s, Addr# #)
        atomicExchangeWordAddr# :: Addr# -> Word# -> State# s -> (# State# s, Word# #)

- Add primops for atomic compare and swap at a given Addr#:

        atomicCasAddrAddr# :: Addr# -> Addr# -> Addr# -> State# s -> (# State# s, Addr# #)
        atomicCasWordAddr# :: Addr# -> Word# -> Word# -> State# s -> (# State# s, Word# #)

- Add an explicit fixity for `(~)` and `(~~)`:

        infix 4 ~, ~~

- Introduce `keepAlive#` to replace `touch#` in controlling object lifetime without
  the soundness issues of the latter (see
  [#17760](https://gitlab.haskell.org/ghc/ghc/-/issues/17760)).

## 0.6.1

- Shipped with GHC 8.10.1

- Add primop for shrinking `SmallMutableArray#`
  to `GHC.Prim`:

        shrinkSmallMutableArray# :: SmallMutableArray# s a -> Int# -> State# s -> State# s

  Note that `resizeSmallMutableArray#` is not included as
  as primitive. It has been implemented in library space in
  `GHC.Exts`. See the release notes of `base`.

- Added to `GHC.Prim`:

        closureSize# :: a -> Int#

- Added to `GHC.Prim`:

        bitReverse# :: Word# -> Word#
        bitReverse8# :: Word# -> Word#
        bitReverse16# :: Word# -> Word#
        bitReverse32# :: Word# -> Word#
        bitReverse64# :: Word# -> Word#

  `bitReverse#` is a primop that, for a `Word` of 8, 16, 32 or 64 bits,
  reverses the order of its bits e.g. `0b110001` becomes `0b100011`.
  These primitives use optimized machine instructions when available.

- Add Int# multiplication primop:

      timesInt2# :: Int# -> Int# -> (# Int#, Int#, Int# #)

   `timesInt2#` computes the multiplication of its two parameters and returns a
   triple (isHighNeeded,high,low) where high and low are respectively the high
   and low bits of the double-word result. isHighNeeded is a cheap way to test
   if the high word is a sign-extension of the low word (isHighNeeded = 0#) or
   not (isHighNeeded = 1#).

## 0.6.0

- Shipped with GHC 8.8.1

- Added to `GHC.Prim`:

        traceBinaryEvent# :: Addr# -> Int# -> State# s -> State# s

## 0.5.3

- Shipped with GHC 8.6.1

- Added to `GHC.Prim`:

        addWordC# :: Word# -> Word# -> (# Word#, Int# #)

- `unpackClosure#` can now unpack any valid Haskell closure.
  Previously it returned empty pointer and non-pointer arrays
  for thunks.

- Add unaligned bytearray access primops (#4442)

         readWord8ArrayAsChar# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
         readWord8ArrayAsAddr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
         readWord8ArrayAsFloat# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
         readWord8ArrayAsDouble# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
         readWord8ArrayAsStablePtr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# #)
         readWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int16# #)
         readWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int32# #)
         readWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int64# #)
         readWord8ArrayAsInt# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)

         readWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word16# #)
         readWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word32# #)
         readWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word64# #)
         readWord8ArrayAsWord# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)

         writeWord8ArrayAsChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
         writeWord8ArrayAsAddr# :: MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
         writeWord8ArrayAsFloat# :: MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
         writeWord8ArrayAsDouble# :: MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
         writeWord8ArrayAsStablePtr# :: MutableByteArray# s -> Int# -> StablePtr# -> State# s -> State# s

         writeWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> Int16# -> State# s -> State# s
         writeWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> Int32# -> State# s -> State# s
         writeWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> Int64# -> State# s -> State# s
         writeWord8ArrayAsInt# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s

         writeWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> Word16# -> State# s -> State# s
         writeWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> Word32# -> State# s -> State# s
         writeWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> Word64# -> State# s -> State# s
         writeWord8ArrayAsWord# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s

## 0.5.2.0

- Shipped with GHC 8.4.1

- Added to `GHC.Prim`:

        compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#

- Don't allocate a thunk for each unpacked UTF-8 character in `unpackCStringUtf8#`

## 0.5.1.1 *November 2017*

- Shipped with GHC 8.2.2

- Changed strictness properties of `catchRetry#` (#14171)

## 0.5.1.0 *July 2017*

- Shipped with GHC 8.2.1

- Added to `GHC.Prim`:

        fabsDouble# :: Double# -> Double#
        fabsFloat# :: Float# -> Float#
        isByteArrayPinned# :: ByteArray# -> Int#
        isMutableByteArrayPinned# :: MutableByteArray# s -> Int#
        anyToAddr# :: a -> State# (RealWorld) -> (# State# (RealWorld),Addr# #)

- New primitives for compact regions in `GHC.Prim`:

        Compact#
        compactNew#
        compactResize#
        compactContains#
        compactContainsAny#
        compactGetFirstBlock#
        compactGetNextBlock#
        compactAllocateBlock#
        compactFixupPointers#
        compactAdd#
        compactAddWithSharing#
        compactSize#

- Generalised `noDuplicate#` from

        noDuplicate# :: State# (RealWorld) -> State# (RealWorld)

    to

        noDuplicate# :: State# s -> State# s


## 0.5.0.0

- Shipped with GHC 8.0.1

- `GHC.Classes`: new `class IP (a :: Symbol) b | a -> b`

- `GHC.Prim`: changed type signatures from

        check# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, () #)
        finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, State# RealWorld -> (# State# RealWorld, () #) #)
        mkWeak# :: a -> b -> c -> State# RealWorld -> (# State# RealWorld, Weak# b #)

    to

        check# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> State# RealWorld
        finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, State# RealWorld -> (# State# RealWorld, b #) #)
        mkWeak# :: a -> b -> (State# RealWorld -> (# State# RealWorld, c #)) -> State# RealWorld -> (# State# RealWorld, Weak# b #)

- Removed from `GHC.Prim`:

        parAt# :: b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
        parAtAbs# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
        parAtForNow# :: b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
        parAtRel# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
        parGlobal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
        parLocal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#

- Added to `GHC.Prim`:

        getSizeofMutableByteArray# :: MutableByteArray# d -> State# d -> (# State# d, Int# #)
        subWordC# :: Word# -> Word# -> (# Word#, Int# #)
        runRW# :: (State# RealWorld -> (# State# RealWorld, o #)) -> (# State# RealWorld, o #)

- Added to `GHC.Types`:

        data Module = Module TrName TrName
        data Nat
        data Symbol
        data TrName = TrNameS Addr# | TrNameD [Char]
        data TyCon = TyCon Word# Word# Module TrName
