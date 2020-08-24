{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}

module Main (main) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import System.Mem (performMajorGC)

import GHC.Exts
import GHC.Base (IO(IO))


-- | Test that we can allocate 'ByteArray#'s outside of the @HEAP_ALLOCED()@
-- address space without upsetting the GC. To be extra sure we attach weak
-- pointers with C finalizers to the 'ByteArray#'s. We keep them alive and run
-- a major GC so that the GC has to trace the live 'ByteArray#'s.
--
-- On older GHC versions this upsets the GC because it does not expect heap
-- objects with closure type @ARR_WORDS@ to exist outside the heap.
--
-- > internal error: evacuate(static): strange closure type 42
--
-- Finally we allow everything to be GC'd again, and check that the C
-- finalizers did run.
--
main :: IO ()
main = do
    heapallocedTest

    n1 <- getMallocByteArrayCount
    putStrLn ("malloc_count  = " ++ show n1)

    bytearrayTest

    -- check that all the C finalizers ran to free() the things we allocated
    performMajorGC
    n2 <- getMallocByteArrayCount
    putStrLn ("malloc_count  = " ++ show n1)

-- | This checks that the CMM impl of HEAP_ALLOCED matches the behaviour of
-- the C version. This test lives here since the CMM HEAP_ALLOCED is used by
-- the isByteArrayPinned# primop now that it supports byte arrays that are
-- not themselves HEAP_ALLOCED.
--
heapallocedTest :: IO ()
heapallocedTest = do
    let same = and [ c_HEAP_ALLOCED ptr == cmm_HEAP_ALLOCED ptr
                   | addr <- addrs
                   , let ptr = plusPtr nullPtr addr ]
    putStrLn ("HEAP_ALLOCED  = " ++ show same
                 ++ "\t(c_HEAP_ALLOCED ptr == cmm_HEAP_ALLOCED ptr)")
  where
    heapStart = 0x4200000000
    addrs
      -- Addresses around the start of the heap
      = [heapStart + offset | offset <- [-1000..1000]]
      -- 2k address probes, appropriate to the address space
      ++ case sizeOf (nullPtr :: Ptr ()) of
        4 -> [0, 2^21 .. 2^32-1] -- 0 to 4Gb-1 in 2Mb jumps
        8 -> [0, 2^30 .. 2^41-1] -- 0 to 2Tb-1 in 1Gb jumps
        _ -> error "sizeOf ptr not 4 or 8"

bytearrayTest :: IO ()
bytearrayTest = do

    -- malloc() a bunch of ByteArray#s on the C heap
    foreignBAs <- mapM newForeignHeapByteArray [0..99]
    n1 <- getMallocByteArrayCount
    putStrLn ("malloc_count  = " ++ show n1)

    -- allocate a bunch of ByteArray#s on the GHC heap
    nativeBAs <- mapM newNativeHeapByteArray [0..99]

    -- as a sanity check compare them to each other
    let same = and (zipWith equalByteArrays foreignBAs nativeBAs)
    putStrLn ("arrays equal  = " ++ show same
                ++ "\t(and (zipWith equalByteArrays foreignBAs nativeBAs))")

    -- and test the isByteArrayPinned# primop on them all
    let pinned = all isByteArrayPinned foreignBAs
    putStrLn ("arrays pinned = " ++ show pinned
                 ++ "\t(all isByteArrayPinned foreignBAs)")

    -- while they're still live, have the GC inspect them all
    performMajorGC
    n2 <- getMallocByteArrayCount
    putStrLn ("malloc_count  = " ++ show n2)
    mapM_ touchByteArray foreignBAs


data ByteArray        = ByteArray ByteArray#
data MutableByteArray = MutableByteArray (MutableByteArray# RealWorld)

newNativeHeapByteArray :: Int -> IO ByteArray
newNativeHeapByteArray n = do
    mba <- newByteArray n
    fillAndFreezeByteArray mba n

newForeignHeapByteArray :: Int -> IO ByteArray
newForeignHeapByteArray n = do
    ptr <- mallocByteArray (fromIntegral n)
    mba <- placeByteArray ptr n
    addCFinalizerToByteArray mba freeByteArray ptr
    fillAndFreezeByteArray mba n

newByteArray :: Int -> IO MutableByteArray
newByteArray  (I# n#) =
    IO $ \s0 ->
      case newByteArray# n# s0 of
        (# s1, mba# #) -> (# s1, MutableByteArray mba# #)

placeByteArray :: Ptr a -> Int -> IO MutableByteArray
placeByteArray (Ptr addr#) (I# n#) =
    IO $ \s0 ->
      case placeByteArray# addr# n# s0 of
        (# s1, mba# #) -> (# s1, MutableByteArray mba# #)

fillAndFreezeByteArray :: MutableByteArray -> Int -> IO ByteArray
fillAndFreezeByteArray (MutableByteArray mba#) (I# n#) =
    IO $ \s0 ->
      case setByteArray# mba# 0# n# n# s0 of
        s1 ->
          case unsafeFreezeByteArray# mba# s1 of
            (# s2, ba# #) -> (# s2, ByteArray ba# #)

addCFinalizerToByteArray :: MutableByteArray
                         -> FunPtr (Ptr a -> IO ())
                         -> Ptr a
                         -> IO ()
addCFinalizerToByteArray (MutableByteArray ba#) (FunPtr cfunptr#) (Ptr ptr#) =
    IO $ \s0 ->
      case mkWeakNoFinalizer# ba# () s0 of
        (# s1, weak# #) ->
          -- 0# flag here means don't pass any env ptr to the finalizer
          case addCFinalizerToWeak# cfunptr# ptr# 0# nullAddr# weak# s1 of
            (# s2, _success# #) -> (# s2, () #)

touchByteArray :: ByteArray -> IO ()
touchByteArray (ByteArray ba#) =
    IO $ \s0 ->
      case touch# ba# s0 of
        s1 -> (# s1, () #)

equalByteArrays :: ByteArray -> ByteArray -> Bool
equalByteArrays (ByteArray ba1#) (ByteArray ba2#)
  | let ba1len = I# (sizeofByteArray# ba1#)
        ba2len = I# (sizeofByteArray# ba2#)
        I# balen# = ba1len
  , ba1len == ba2len
  = isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# balen#)

isByteArrayPinned :: ByteArray -> Bool
isByteArrayPinned (ByteArray ba#) = isTrue# (isByteArrayPinned# ba#)

cmm_HEAP_ALLOCED :: Ptr a -> Int
cmm_HEAP_ALLOCED (Ptr addr#) = I# (cmm_HEAP_ALLOCED# addr#)


foreign import ccall unsafe "mallocByteArray"
  mallocByteArray :: CInt -> IO (Ptr a)

foreign import ccall unsafe "&freeByteArray"
  freeByteArray :: FunPtr (Ptr a -> IO ())

foreign import ccall unsafe "getMallocByteArrayCount"
  getMallocByteArrayCount :: IO CInt

foreign import ccall unsafe "c_HEAP_ALLOCED"
  c_HEAP_ALLOCED :: Ptr a -> Int

foreign import prim "cmm_HEAP_ALLOCED"
  cmm_HEAP_ALLOCED# :: Addr# -> Int#

