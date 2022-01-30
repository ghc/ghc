{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ForeignPtr
-- Copyright   :  (c) The University of Glasgow, 1992-2003
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC's implementation of the 'ForeignPtr' data type.
--
-----------------------------------------------------------------------------

module GHC.ForeignPtr
  (
        -- * Types
        ForeignPtr(..),
        ForeignPtrContents(..),
        Finalizers(..),
        FinalizerPtr,
        FinalizerEnvPtr,
        -- * Create
        newForeignPtr_,
        mallocForeignPtr,
        mallocPlainForeignPtr,
        mallocForeignPtrBytes,
        mallocPlainForeignPtrBytes,
        mallocForeignPtrAlignedBytes,
        mallocPlainForeignPtrAlignedBytes,
        newConcForeignPtr,
        -- * Add Finalizers
        addForeignPtrFinalizer,
        addForeignPtrFinalizerEnv,
        addForeignPtrConcFinalizer,
        -- * Conversion
        unsafeForeignPtrToPtr,
        castForeignPtr,
        plusForeignPtr,
        -- * Control over lifetype
        withForeignPtr,
        unsafeWithForeignPtr,
        touchForeignPtr,
        -- * Finalization
        finalizeForeignPtr
        -- * Commentary
        -- $commentary
  ) where

import Foreign.Storable
import Data.Foldable    ( sequence_ )

import GHC.Show
import GHC.Base
import GHC.IORef
import GHC.STRef        ( STRef(..) )
import GHC.Ptr          ( Ptr(..), FunPtr(..) )

import Unsafe.Coerce    ( unsafeCoerce )

-- |The type 'ForeignPtr' represents references to objects that are
-- maintained in a foreign language, i.e., that are not part of the
-- data structures usually managed by the Haskell storage manager.
-- The essential difference between 'ForeignPtr's and vanilla memory
-- references of type @Ptr a@ is that the former may be associated
-- with /finalizers/. A finalizer is a routine that is invoked when
-- the Haskell storage manager detects that - within the Haskell heap
-- and stack - there are no more references left that are pointing to
-- the 'ForeignPtr'.  Typically, the finalizer will, then, invoke
-- routines in the foreign language that free the resources bound by
-- the foreign object.
--
-- The 'ForeignPtr' is parameterised in the same way as 'Ptr'.  The
-- type argument of 'ForeignPtr' should normally be an instance of
-- class 'Storable'.
--
data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents
        -- The Addr# in the ForeignPtr object is intentionally stored
        -- separately from the finalizer. The primary aim of the
        -- representation is to make withForeignPtr efficient; in fact,
        -- withForeignPtr should be just as efficient as unpacking a
        -- Ptr, and multiple withForeignPtrs can share an unpacked
        -- ForeignPtr. As a secondary benefit, this representation
        -- allows pointers to subregions within the same overall block
        -- to share the same finalizer (see 'plusForeignPtr'). Note
        -- that touchForeignPtr only has to touch the ForeignPtrContents
        -- object, because that ensures that whatever the finalizer is
        -- attached to is kept alive.

-- | Functions called when a 'ForeignPtr' is finalized. Note that
-- C finalizers and Haskell finalizers cannot be mixed.
data Finalizers
  = NoFinalizers
    -- ^ No finalizer. If there is no intent to add a finalizer at
    -- any point in the future, consider 'FinalPtr' or 'PlainPtr' instead
    -- since these perform fewer allocations.
  | CFinalizers (Weak# ())
    -- ^ Finalizers are all C functions.
  | HaskellFinalizers [IO ()]
    -- ^ Finalizers are all Haskell functions.

-- | Controls finalization of a 'ForeignPtr', that is, what should happen
-- if the 'ForeignPtr' becomes unreachable. Visually, these data constructors
-- are appropriate in these scenarios:
--
-- >                           Memory backing pointer is
-- >                            GC-Managed   Unmanaged
-- > Finalizer functions are: +------------+-----------------+
-- >                 Allowed  | MallocPtr  | PlainForeignPtr |
-- >                          +------------+-----------------+
-- >              Prohibited  | PlainPtr   | FinalPtr        |
-- >                          +------------+-----------------+
data ForeignPtrContents
  = PlainForeignPtr !(IORef Finalizers)
    -- ^ The pointer refers to unmanaged memory that was allocated by
    -- a foreign function (typically using @malloc@). The finalizer
    -- frequently calls the C function @free@ or some variant of it.
  | FinalPtr
    -- ^ The pointer refers to unmanaged memory that should not be freed when
    -- the 'ForeignPtr' becomes unreachable. Functions that add finalizers
    -- to a 'ForeignPtr' throw exceptions when the 'ForeignPtr' is backed by
    -- 'PlainPtr'Most commonly, this is used with @Addr#@ literals.
    -- See Note [Why FinalPtr].
    --
    -- @since 4.15
  | MallocPtr (MutableByteArray# RealWorld) !(IORef Finalizers)
    -- ^ The pointer refers to a byte array.
    -- The 'MutableByteArray#' field means that the 'MutableByteArray#' is
    -- reachable (by GC) whenever the 'ForeignPtr' is reachable. When the
    -- 'ForeignPtr' becomes unreachable, the runtime\'s normal GC recovers
    -- the memory backing it. Here, the finalizer function intended to be used
    -- to @free()@ any ancillary *unmanaged* memory pointed to by the
    -- 'MutableByteArray#'. See the @zlib@ library for an example of this use.
    --
    -- 1. Invariant: The 'Addr#' in the parent 'ForeignPtr' is an interior
    --    pointer into this 'MutableByteArray#'.
    -- 2. Invariant: The 'MutableByteArray#' is pinned, so the 'Addr#' does not
    --    get invalidated by the GC moving the byte array.
    -- 3. Invariant: A 'MutableByteArray#' must not be associated with more than
    --    one set of finalizers. For example, this is sound:
    --
    --    > incrGood :: ForeignPtr Word8 -> ForeignPtr Word8
    --    > incrGood (ForeignPtr p (MallocPtr m f)) = ForeignPtr (plusPtr p 1) (MallocPtr m f)
    --
    --    But this is unsound:
    --
    --    > incrBad :: ForeignPtr Word8 -> IO (ForeignPtr Word8)
    --    > incrBad (ForeignPtr p (MallocPtr m _)) = do
    --    >   f <- newIORef NoFinalizers
    --    >   pure (ForeignPtr p (MallocPtr m f))
  | PlainPtr (MutableByteArray# RealWorld)
    -- ^ The pointer refers to a byte array. Finalization is not
    -- supported. This optimizes @MallocPtr@ by avoiding the allocation
    -- of a @MutVar#@ when it is known that no one will add finalizers to
    -- the @ForeignPtr@. Functions that add finalizers to a 'ForeignPtr'
    -- throw exceptions when the 'ForeignPtr' is backed by 'PlainPtr'.
    -- The invariants that apply to 'MallocPtr' apply to 'PlainPtr' as well.

-- Note [Why FinalPtr]
-- ~~~~~~~~~~~~~~~~~~~
-- FinalPtr exists as an optimization for foreign pointers created
-- from Addr# literals. Most commonly, this happens in the bytestring
-- library, where the combination of OverloadedStrings and a rewrite
-- rule overloads String literals as ByteString literals. See the
-- rule "ByteString packChars/packAddress" in
-- bytestring:Data.ByteString.Internal. Prior to the
-- introduction of FinalPtr, bytestring used PlainForeignPtr (in
-- Data.ByteString.Internal.unsafePackAddress) to handle such literals.
-- With O2 optimization, the resulting Core from a GHC patched with a
-- known-key cstringLength# function but without FinalPtr looked like:
--
--   RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
--   stringOne1 = "hello beautiful world"#
--   RHS size: {terms: 11, types: 17, coercions: 0, joins: 0/0}
--   stringOne
--     = case newMutVar# NoFinalizers realWorld# of
--       { (# ipv_i7b6, ipv1_i7b7 #) ->
--       PS stringOne1 (PlainForeignPtr ipv1_i7b7) 0# 21#
--       }
--
-- After the introduction of FinalPtr, the bytestring library was modified
-- so that the resulting Core was instead:
--
--   RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
--   stringOne1 = "hello beautiful world"#
--   RHS size: {terms: 5, types: 0, coercions: 0, joins: 0/0}
--   stringOne = PS stringOne1 FinalPtr 0# 21#
--
-- This improves performance in three ways:
--
-- 1. More optimization opportunities. GHC is willing to inline the FinalPtr
--    variant of stringOne into its use sites. This means the offset and length
--    are eligible for case-of-known-literal. Previously, this never happened.
-- 2. Smaller binaries. Setting up the thunk to call newMutVar# required
--    machine instruction in the generated code. On x86_64, FinalPtr reduces
--    the size of binaries by about 450 bytes per ByteString literal.
-- 3. Smaller memory footprint. Previously, every ByteString literal resulted
--    in the allocation of a MutVar# and a PlainForeignPtr data constructor.
--    These both hang around until the ByteString goes out of scope. FinalPtr
--    eliminates both of these sources of allocations. The MutVar# is not
--    allocated because FinalPtr does not allow it, and the data constructor
--    is not allocated because FinalPtr is a nullary data constructor.
--
-- For more discussion of FinalPtr, see GHC MR #2165 and bytestring PR #191.

-- | @since 2.01
instance Eq (ForeignPtr a) where
    p == q  =  unsafeForeignPtrToPtr p == unsafeForeignPtrToPtr q

-- | @since 2.01
instance Ord (ForeignPtr a) where
    compare p q  =  compare (unsafeForeignPtrToPtr p) (unsafeForeignPtrToPtr q)

-- | @since 2.01
instance Show (ForeignPtr a) where
    showsPrec p f = showsPrec p (unsafeForeignPtrToPtr f)


-- |A finalizer is represented as a pointer to a foreign function that, at
-- finalisation time, gets as an argument a plain pointer variant of the
-- foreign pointer that the finalizer is associated with.
--
-- Note that the foreign function /must/ use the @ccall@ calling convention.
--
type FinalizerPtr a        = FunPtr (Ptr a -> IO ())
type FinalizerEnvPtr env a = FunPtr (Ptr env -> Ptr a -> IO ())

newConcForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
--
-- ^Turns a plain memory reference into a foreign object by
-- associating a finalizer - given by the monadic operation - with the
-- reference.  The storage manager will start the finalizer, in a
-- separate thread, some time after the last reference to the
-- @ForeignPtr@ is dropped.  There is no guarantee of promptness, and
-- in fact there is no guarantee that the finalizer will eventually
-- run at all.
--
-- Note that references from a finalizer do not necessarily prevent
-- another object from being finalized.  If A's finalizer refers to B
-- (perhaps using 'touchForeignPtr', then the only guarantee is that
-- B's finalizer will never be started before A's.  If both A and B
-- are unreachable, then both finalizers will start together.  See
-- 'touchForeignPtr' for more on finalizer ordering.
--
newConcForeignPtr p finalizer
  = do fObj <- newForeignPtr_ p
       addForeignPtrConcFinalizer fObj finalizer
       return fObj

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
-- ^ Allocate some memory and return a 'ForeignPtr' to it.  The memory
-- will be released automatically when the 'ForeignPtr' is discarded.
--
-- 'mallocForeignPtr' is equivalent to
--
-- >    do { p <- malloc; newForeignPtr finalizerFree p }
--
-- although it may be implemented differently internally: you may not
-- assume that the memory returned by 'mallocForeignPtr' has been
-- allocated with 'Foreign.Marshal.Alloc.malloc'.
--
-- GHC notes: 'mallocForeignPtr' has a heavily optimised
-- implementation in GHC.  It uses pinned memory in the garbage
-- collected heap, so the 'ForeignPtr' does not require a finalizer to
-- free the memory.  Use of 'mallocForeignPtr' and associated
-- functions is strongly recommended in preference to
-- 'Foreign.ForeignPtr.newForeignPtr' with a finalizer.
--
mallocForeignPtr = doMalloc undefined
  where doMalloc :: Storable b => b -> IO (ForeignPtr b)
        doMalloc a
          | I# size < 0 = errorWithoutStackTrace "mallocForeignPtr: size must be >= 0"
          | otherwise = do
          r <- newIORef NoFinalizers
          IO $ \s ->
            case newAlignedPinnedByteArray# size align s of { (# s', mbarr# #) ->
             (# s', ForeignPtr (mutableByteArrayContents# mbarr#)
                               (MallocPtr mbarr# r) #)
            }
            where !(I# size)  = sizeOf a
                  !(I# align) = alignment a

-- | This function is similar to 'mallocForeignPtr', except that the
-- size of the memory required is given explicitly as a number of bytes.
mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes size | size < 0 =
  errorWithoutStackTrace "mallocForeignPtrBytes: size must be >= 0"
mallocForeignPtrBytes (I# size) = do
  r <- newIORef NoFinalizers
  IO $ \s ->
     case newPinnedByteArray# size s      of { (# s', mbarr# #) ->
       (# s', ForeignPtr (mutableByteArrayContents# mbarr#)
                         (MallocPtr mbarr# r) #)
     }

-- | This function is similar to 'mallocForeignPtrBytes', except that the
-- size and alignment of the memory required is given explicitly as numbers of
-- bytes.
mallocForeignPtrAlignedBytes :: Int -> Int -> IO (ForeignPtr a)
mallocForeignPtrAlignedBytes size _align | size < 0 =
  errorWithoutStackTrace "mallocForeignPtrAlignedBytes: size must be >= 0"
mallocForeignPtrAlignedBytes (I# size) (I# align) = do
  r <- newIORef NoFinalizers
  IO $ \s ->
     case newAlignedPinnedByteArray# size align s of { (# s', mbarr# #) ->
       (# s', ForeignPtr (mutableByteArrayContents# mbarr#)
                         (MallocPtr mbarr# r) #)
     }

-- | Allocate some memory and return a 'ForeignPtr' to it.  The memory
-- will be released automatically when the 'ForeignPtr' is discarded.
--
-- GHC notes: 'mallocPlainForeignPtr' has a heavily optimised
-- implementation in GHC.  It uses pinned memory in the garbage
-- collected heap, as for mallocForeignPtr. Unlike mallocForeignPtr, a
-- ForeignPtr created with mallocPlainForeignPtr carries no finalizers.
-- It is not possible to add a finalizer to a ForeignPtr created with
-- mallocPlainForeignPtr. This is useful for ForeignPtrs that will live
-- only inside Haskell (such as those created for packed strings).
-- Attempts to add a finalizer to a ForeignPtr created this way, or to
-- finalize such a pointer, will throw an exception.
--
mallocPlainForeignPtr :: Storable a => IO (ForeignPtr a)
mallocPlainForeignPtr = doMalloc undefined
  where doMalloc :: Storable b => b -> IO (ForeignPtr b)
        doMalloc a
          | I# size < 0 = errorWithoutStackTrace "mallocForeignPtr: size must be >= 0"
          | otherwise = IO $ \s ->
            case newAlignedPinnedByteArray# size align s of { (# s', mbarr# #) ->
             (# s', ForeignPtr (mutableByteArrayContents# mbarr#)
                               (PlainPtr mbarr#) #)
            }
            where !(I# size)  = sizeOf a
                  !(I# align) = alignment a

-- | This function is similar to 'mallocForeignPtrBytes', except that
-- the internally an optimised ForeignPtr representation with no
-- finalizer is used. Attempts to add a finalizer will cause an
-- exception to be thrown.
mallocPlainForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytes size | size < 0 =
  errorWithoutStackTrace "mallocPlainForeignPtrBytes: size must be >= 0"
mallocPlainForeignPtrBytes (I# size) = IO $ \s ->
    case newPinnedByteArray# size s      of { (# s', mbarr# #) ->
       (# s', ForeignPtr (mutableByteArrayContents# mbarr#)
                         (PlainPtr mbarr#) #)
     }

-- | This function is similar to 'mallocForeignPtrAlignedBytes', except that
-- the internally an optimised ForeignPtr representation with no
-- finalizer is used. Attempts to add a finalizer will cause an
-- exception to be thrown.
mallocPlainForeignPtrAlignedBytes :: Int -> Int -> IO (ForeignPtr a)
mallocPlainForeignPtrAlignedBytes size _align | size < 0 =
  errorWithoutStackTrace "mallocPlainForeignPtrAlignedBytes: size must be >= 0"
mallocPlainForeignPtrAlignedBytes (I# size) (I# align) = IO $ \s ->
    case newAlignedPinnedByteArray# size align s of { (# s', mbarr# #) ->
       (# s', ForeignPtr (mutableByteArrayContents# mbarr#)
                         (PlainPtr mbarr#) #)
     }

addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
-- ^ This function adds a finalizer to the given foreign object.  The
-- finalizer will run /before/ all other finalizers for the same
-- object which have already been registered.
addForeignPtrFinalizer (FunPtr fp) (ForeignPtr p c) = case c of
  PlainForeignPtr r -> insertCFinalizer r fp 0# nullAddr# p ()
  MallocPtr     _ r -> insertCFinalizer r fp 0# nullAddr# p c
  _ -> errorWithoutStackTrace "GHC.ForeignPtr: attempt to add a finalizer to a plain pointer or a final pointer"

-- Note [MallocPtr finalizers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Ticket: #10904
--
-- When we have C finalizers for a MallocPtr, the memory is
-- heap-resident and would normally be recovered by the GC before the
-- finalizers run.  To prevent the memory from being reused too early,
-- we attach the MallocPtr constructor to the "value" field of the
-- weak pointer when we call mkWeak# in ensureCFinalizerWeak below.
-- The GC will keep this field alive until the finalizers have run.

addForeignPtrFinalizerEnv ::
  FinalizerEnvPtr env a -> Ptr env -> ForeignPtr a -> IO ()
-- ^ Like 'addForeignPtrFinalizer' but the finalizer is passed an additional
-- environment parameter.
addForeignPtrFinalizerEnv (FunPtr fp) (Ptr ep) (ForeignPtr p c) = case c of
  PlainForeignPtr r -> insertCFinalizer r fp 1# ep p ()
  MallocPtr     _ r -> insertCFinalizer r fp 1# ep p c
  _ -> errorWithoutStackTrace "GHC.ForeignPtr: attempt to add a finalizer to a plain pointer or a final pointer"

addForeignPtrConcFinalizer :: ForeignPtr a -> IO () -> IO ()
-- ^This function adds a finalizer to the given @ForeignPtr@.  The
-- finalizer will run /before/ all other finalizers for the same
-- object which have already been registered.
--
-- This is a variant of @addForeignPtrFinalizer@, where the finalizer
-- is an arbitrary @IO@ action.  When it is invoked, the finalizer
-- will run in a new thread.
--
-- NB. Be very careful with these finalizers.  One common trap is that
-- if a finalizer references another finalized value, it does not
-- prevent that value from being finalized.  In particular, 'System.IO.Handle's
-- are finalized objects, so a finalizer should not refer to a
-- 'System.IO.Handle' (including 'System.IO.stdout', 'System.IO.stdin', or
-- 'System.IO.stderr').
--
addForeignPtrConcFinalizer (ForeignPtr _ c) finalizer =
  addForeignPtrConcFinalizer_ c finalizer

addForeignPtrConcFinalizer_ :: ForeignPtrContents -> IO () -> IO ()
addForeignPtrConcFinalizer_ (PlainForeignPtr r) finalizer = do
  noFinalizers <- insertHaskellFinalizer r finalizer
  if noFinalizers
     then IO $ \s ->
              case r of { IORef (STRef r#) ->
              case mkWeak# r# () (unIO $ foreignPtrFinalizer r) s of {
                (# s1, _ #) -> (# s1, () #) }}
     else return ()
addForeignPtrConcFinalizer_ f@(MallocPtr fo r) finalizer = do
  noFinalizers <- insertHaskellFinalizer r finalizer
  if noFinalizers
     then  IO $ \s ->
               case mkWeak# fo () finalizer' s of
                  (# s1, _ #) -> (# s1, () #)
     else return ()
  where
    finalizer' :: State# RealWorld -> (# State# RealWorld, () #)
    finalizer' = unIO (foreignPtrFinalizer r >> touch f)

addForeignPtrConcFinalizer_ _ _ =
  errorWithoutStackTrace "GHC.ForeignPtr: attempt to add a finalizer to plain pointer or a final pointer"

insertHaskellFinalizer :: IORef Finalizers -> IO () -> IO Bool
insertHaskellFinalizer r f = do
  !wasEmpty <- atomicModifyIORefP r $ \finalizers -> case finalizers of
      NoFinalizers -> (HaskellFinalizers [f], True)
      HaskellFinalizers fs -> (HaskellFinalizers (f:fs), False)
      _ -> noMixingError
  return wasEmpty

-- | A box around Weak#, private to this module.
data MyWeak = MyWeak (Weak# ())

insertCFinalizer ::
  IORef Finalizers -> Addr# -> Int# -> Addr# -> Addr# -> value -> IO ()
insertCFinalizer r fp flag ep p val = do
  MyWeak w <- ensureCFinalizerWeak r val
  IO $ \s -> case addCFinalizerToWeak# fp p flag ep w s of
      (# s1, 1# #) -> (# s1, () #)

      -- Failed to add the finalizer because some other thread
      -- has finalized w by calling foreignPtrFinalizer. We retry now.
      -- This won't be an infinite loop because that thread must have
      -- replaced the content of r before calling finalizeWeak#.
      (# s1, _ #) -> unIO (insertCFinalizer r fp flag ep p val) s1

-- Read the weak reference from an IORef Finalizers, creating it if necessary.
-- Throws an exception if HaskellFinalizers is encountered.
ensureCFinalizerWeak :: IORef Finalizers -> value -> IO MyWeak
ensureCFinalizerWeak ref@(IORef (STRef r#)) value = do
  fin <- readIORef ref
  case fin of
      CFinalizers weak -> return (MyWeak weak)
      HaskellFinalizers{} -> noMixingError
      NoFinalizers -> IO $ \s ->
          case mkWeakNoFinalizer# r# (unsafeCoerce value) s of { (# s1, w #) ->
             -- See Note [MallocPtr finalizers] (#10904)
          case atomicModifyMutVar2# r# (update w) s1 of
              { (# s2, _, (_, (weak, needKill )) #) ->
          if needKill
            then case finalizeWeak# w s2 of { (# s3, _, _ #) ->
              (# s3, weak #) }
            else (# s2, weak #) }}
  where
      update _ fin@(CFinalizers w) = (fin, (MyWeak w, True))
      update w NoFinalizers = (CFinalizers w, (MyWeak w, False))
      update _ _ = noMixingError

noMixingError :: a
noMixingError = errorWithoutStackTrace $
   "GHC.ForeignPtr: attempt to mix Haskell and C finalizers " ++
   "in the same ForeignPtr"

-- Swap out the finalizers with NoFinalizers and then run them.
foreignPtrFinalizer :: IORef Finalizers -> IO ()
foreignPtrFinalizer r = do
  fs <- atomicSwapIORef r NoFinalizers
             -- atomic, see #7170
  case fs of
    NoFinalizers -> return ()
    CFinalizers w -> IO $ \s -> case finalizeWeak# w s of
        (# s1, 1#, f #) -> f s1
        (# s1, _, _ #) -> (# s1, () #)
    HaskellFinalizers actions -> sequence_ actions

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
-- ^Turns a plain memory reference into a foreign pointer that may be
-- associated with finalizers by using 'addForeignPtrFinalizer'.
newForeignPtr_ (Ptr obj) =  do
  r <- newIORef NoFinalizers
  return (ForeignPtr obj (PlainForeignPtr r))

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
-- ^This is a way to look at the pointer living inside a
-- foreign object.  This function takes a function which is
-- applied to that pointer. The resulting 'IO' action is then
-- executed. The foreign object is kept alive at least during
-- the whole action, even if it is not used directly
-- inside. Note that it is not safe to return the pointer from
-- the action and use it after the action completes. All uses
-- of the pointer should be inside the
-- 'withForeignPtr' bracket.  The reason for
-- this unsafeness is the same as for
-- 'unsafeForeignPtrToPtr' below: the finalizer
-- may run earlier than expected, because the compiler can only
-- track usage of the 'ForeignPtr' object, not
-- a 'Ptr' object made from it.
--
-- This function is normally used for marshalling data to
-- or from the object pointed to by the
-- 'ForeignPtr', using the operations from the
-- 'Storable' class.
withForeignPtr fo@(ForeignPtr _ r) f = IO $ \s ->
  case f (unsafeForeignPtrToPtr fo) of
    IO action# -> keepAlive# r s action#

-- | This is similar to 'withForeignPtr' but comes with an important caveat:
-- the user must guarantee that the continuation does not diverge (e.g. loop or
-- throw an exception). In exchange for this loss of generality, this function
-- offers the ability of GHC to optimise more aggressively.
--
-- Specifically, applications of the form:
-- @
-- unsafeWithForeignPtr fptr ('Control.Monad.forever' something)
-- @
--
-- See GHC issue #17760 for more information about the unsoundness behavior
-- that this function can result in.
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr fo f = do
  r <- f (unsafeForeignPtrToPtr fo)
  touchForeignPtr fo
  return r

touchForeignPtr :: ForeignPtr a -> IO ()
-- ^This function ensures that the foreign object in
-- question is alive at the given place in the sequence of IO
-- actions. However, this comes with a significant caveat: the contract above
-- does not hold if GHC can demonstrate that the code preceeding
-- @touchForeignPtr@ diverges (e.g. by looping infinitely or throwing an
-- exception). For this reason, you are strongly advised to use instead
-- 'withForeignPtr' where possible.
--
-- Also, note that this function should not be used to express dependencies
-- between finalizers on 'ForeignPtr's.  For example, if the finalizer for a
-- 'ForeignPtr' @F1@ calls 'touchForeignPtr' on a second 'ForeignPtr' @F2@,
-- then the only guarantee is that the finalizer for @F2@ is never started
-- before the finalizer for @F1@.  They might be started together if for
-- example both @F1@ and @F2@ are otherwise unreachable, and in that case the
-- scheduler might end up running the finalizer for @F2@ first.
--
-- In general, it is not recommended to use finalizers on separate
-- objects with ordering constraints between them.  To express the
-- ordering robustly requires explicit synchronisation using @MVar@s
-- between the finalizers, but even then the runtime sometimes runs
-- multiple finalizers sequentially in a single thread (for
-- performance reasons), so synchronisation between finalizers could
-- result in artificial deadlock.  Another alternative is to use
-- explicit reference counting.
--
touchForeignPtr (ForeignPtr _ r) = touch r

touch :: ForeignPtrContents -> IO ()
touch r = IO $ \s -> case touch# r s of s' -> (# s', () #)

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
-- ^This function extracts the pointer component of a foreign
-- pointer.  This is a potentially dangerous operations, as if the
-- argument to 'unsafeForeignPtrToPtr' is the last usage
-- occurrence of the given foreign pointer, then its finalizer(s) will
-- be run, which potentially invalidates the plain pointer just
-- obtained.  Hence, 'touchForeignPtr' must be used
-- wherever it has to be guaranteed that the pointer lives on - i.e.,
-- has another usage occurrence.
--
-- To avoid subtle coding errors, hand written marshalling code
-- should preferably use 'Foreign.ForeignPtr.withForeignPtr' rather
-- than combinations of 'unsafeForeignPtrToPtr' and
-- 'touchForeignPtr'.  However, the latter routines
-- are occasionally preferred in tool generated marshalling code.
unsafeForeignPtrToPtr (ForeignPtr fo _) = Ptr fo

castForeignPtr :: ForeignPtr a -> ForeignPtr b
-- ^This function casts a 'ForeignPtr'
-- parameterised by one type into another type.
castForeignPtr = coerce

plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
-- ^Advances the given address by the given offset in bytes.
--
-- The new 'ForeignPtr' shares the finalizer of the original,
-- equivalent from a finalization standpoint to just creating another
-- reference to the original. That is, the finalizer will not be
-- called before the new 'ForeignPtr' is unreachable, nor will it be
-- called an additional time due to this call, and the finalizer will
-- be called with the same address that it would have had this call
-- not happened, *not* the new address.
--
-- @since 4.10.0.0
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c

-- | Causes the finalizers associated with a foreign pointer to be run
-- immediately. The foreign pointer must not be used again after this
-- function is called. If the foreign pointer does not support finalizers,
-- this is a no-op.
finalizeForeignPtr :: ForeignPtr a -> IO ()
finalizeForeignPtr (ForeignPtr _ c) = case c of
  PlainForeignPtr ref -> foreignPtrFinalizer ref
  MallocPtr _ ref -> foreignPtrFinalizer ref
  PlainPtr{} -> return ()
  FinalPtr{} -> return ()

{- $commentary

This is a high-level overview of how 'ForeignPtr' works.
The implementation of 'ForeignPtr' must accomplish several goals:

1. Invoke a finalizer once a foreign pointer becomes unreachable.
2. Support augmentation of finalizers, i.e. 'addForeignPtrFinalizer'.
   As a motivating example, suppose that the payload of a foreign
   pointer is C struct @bar@ that has an optionally NULL pointer field
   @foo@ to an unmanaged heap object. Initially, @foo@ is NULL, and
   later the program uses @malloc@, initializes the object, and assigns
   @foo@ the address returned by @malloc@. When the foreign pointer
   becomes unreachable, it is now necessary to first @free@ the object
   pointed to by @foo@ and then invoke whatever finalizer was associated
   with @bar@. That is, finalizers must be invoked in the opposite order
   they are added.
3. Allow users to invoke a finalizer promptly if they know that the
   foreign pointer is unreachable, i.e. 'finalizeForeignPtr'.

How can these goals be accomplished? Goal 1 suggests that weak references
and finalizers (via 'Weak#' and 'mkWeak#') are necessary. But how should
they be used and what should their key be?  Certainly not 'ForeignPtr' or
'ForeignPtrContents'. See the warning in "GHC.Weak" about weak pointers with
lifted (non-primitive) keys. The two finalizer-supporting data constructors of
'ForeignPtr' have an @'IORef' 'Finalizers'@ (backed by 'MutVar#') field.
This gets used in two different ways depending on the kind of finalizer:

* 'HaskellFinalizers': The first @addForeignPtrConcFinalizer_@ call uses
  'mkWeak#' to attach the finalizer @foreignPtrFinalizer@ to the 'MutVar#'.
  The resulting 'Weak#' is discarded (see @addForeignPtrConcFinalizer_@).
  Subsequent calls to @addForeignPtrConcFinalizer_@ (goal 2) just add
  finalizers onto the list in the 'HaskellFinalizers' data constructor.
* 'CFinalizers': The first 'addForeignPtrFinalizer' call uses
  'mkWeakNoFinalizer#' to create a 'Weak#'. The 'Weak#' is preserved in the
  'CFinalizers' data constructor. Both the first call and subsequent
  calls (goal 2) use 'addCFinalizerToWeak#' to attach finalizers to the
  'Weak#' itself. Also, see Note [MallocPtr finalizers] for discussion of
  the key and value of this 'Weak#'.

In either case, the runtime invokes the appropriate finalizers when the
'ForeignPtr' becomes unreachable.
-}
