{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main ( main ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.IO

-- | Iterations per worker.
iters :: Int
iters = 1000000

main :: IO ()
main = do
    fetchAddSubTest
    fetchAndTest
    fetchNandTest
    fetchOrTest
    fetchXorTest
    casTest
    casTestAddr
    readWriteTest

-- | Test fetchAddIntArray# by having two threads concurrenctly
-- increment a counter and then checking the sum at the end.
fetchAddSubTest :: IO ()
fetchAddSubTest = do
    tot <- race 0
        (\ mba -> work fetchAddIntArray mba iters 2)
        (\ mba -> work fetchSubIntArray mba iters 1)
    assertEq 1000000 tot "fetchAddSubTest"
  where
    work :: (MByteArray -> Int -> Int -> IO ()) -> MByteArray -> Int -> Int
         -> IO ()
    work op mba 0 val = return ()
    work op mba n val = op mba 0 val >> work op mba (n-1) val

-- | Test fetchXorIntArray# by having two threads concurrenctly XORing
-- and then checking the result at the end. Works since XOR is
-- commutative.
--
-- Covers the code paths for AND, NAND, and OR as well.
fetchXorTest :: IO ()
fetchXorTest = do
    res <- race n0
        (\ mba -> work mba iters t1pat)
        (\ mba -> work mba iters t2pat)
    assertEq expected res "fetchXorTest"
  where
    work :: MByteArray -> Int -> Int -> IO ()
    work mba 0 val = return ()
    work mba n val = fetchXorIntArray mba 0 val >> work mba (n-1) val

    -- The two patterns are 1010...  and 0101...  The second pattern is larger
    -- than maxBound, avoid warnings by initialising as a Word.
    (n0, t1pat, t2pat)
        | sizeOf (undefined :: Int) == 8 =
            ( 0x00000000ffffffff, 0x5555555555555555
            , fromIntegral (0x9999999999999999 :: Word))
        | otherwise = ( 0x0000ffff, 0x55555555
                      , fromIntegral (0x99999999 :: Word))
    expected
        | sizeOf (undefined :: Int) == 8 = 4294967295
        | otherwise = 65535

-- The tests for AND, NAND, and OR are trivial for two reasons:
--
--  * The code path is already well exercised by 'fetchXorTest'.
--
--  * It's harder to test these operations, as a long sequence of them
--    convert to a single value but we'd like to write a test in the
--    style of 'fetchXorTest' that applies the operation repeatedly,
--    to make it likely that any race conditions are detected.
--
-- Right now we only test that they return the correct value for a
-- single op on each thread.

-- | Test an associative operation.
fetchOpTest :: (MByteArray -> Int -> Int -> IO ())
            -> Int -> String -> IO ()
fetchOpTest op expected name = do
    res <- race n0
        (\ mba -> work mba t1pat)
        (\ mba -> work mba t2pat)
    assertEq expected res name
  where
    work :: MByteArray -> Int -> IO ()
    work mba val = op mba 0 val

-- | Initial value and operation arguments for race test.
--
-- The two patterns are 1010...  and 0101...  The second pattern is larger than
-- maxBound, avoid warnings by initialising as a Word.
n0, t1pat, t2pat :: Int
(n0, t1pat, t2pat)
    | sizeOf (undefined :: Int) == 8 =
        ( 0x00000000ffffffff, 0x5555555555555555
        , fromIntegral (0x9999999999999999 :: Word))
    | otherwise = ( 0x0000ffff, 0x55555555
                  , fromIntegral (0x99999999 :: Word))

fetchAndTest :: IO ()
fetchAndTest = fetchOpTest fetchAndIntArray expected "fetchAndTest"
  where expected
            | sizeOf (undefined :: Int) == 8 = 286331153
            | otherwise = 4369

-- | Test NAND without any race, as NAND isn't associative.
fetchNandTest :: IO ()
fetchNandTest = do
    mba <- newByteArray (sizeOf (undefined :: Int))
    writeIntArray mba 0 n0
    fetchNandIntArray mba 0 t1pat
    fetchNandIntArray mba 0 t2pat
    res <- readIntArray mba 0
    assertEq expected res "fetchNandTest"
  where expected
            | sizeOf (undefined :: Int) == 8 = 7378697629770151799
            | otherwise = -2576976009

fetchOrTest :: IO ()
fetchOrTest = fetchOpTest fetchOrIntArray expected "fetchOrTest"
  where expected
            | sizeOf (undefined :: Int) == 8
            = fromIntegral (15987178197787607039 :: Word)
            | otherwise
            = fromIntegral (3722313727 :: Word)

-- | Test casIntArray# by using it to emulate fetchAddIntArray# and
-- then having two threads concurrenctly increment a counter,
-- checking the sum at the end.
casTest :: IO ()
casTest = do
    tot <- race 0
        (\ mba -> work mba iters 1)
        (\ mba -> work mba iters 2)
    assertEq (3 * iters) tot "casTest"
  where
    work :: MByteArray -> Int -> Int -> IO ()
    work mba 0 val = return ()
    work mba n val = add mba 0 val >> work mba (n-1) val

    -- Fetch-and-add implemented using CAS.
    add :: MByteArray -> Int -> Int -> IO ()
    add mba ix n = do
        old <- readIntArray mba ix
        old' <- casIntArray mba ix old (old + n)
        when (old /= old') $ add mba ix n

-- | Tests atomic reads and writes by making sure that one thread sees
-- updates that are done on another. This test isn't very good at the
-- moment, as this might work even without atomic ops, but at least it
-- exercises the code.
readWriteTest :: IO ()
readWriteTest = do
    mba <- newByteArray (sizeOf (undefined :: Int))
    writeIntArray mba 0 0
    latch <- newEmptyMVar
    done <- newEmptyMVar
    forkIO $ do
        takeMVar latch
        n <- atomicReadIntArray mba 0
        assertEq 1 n "readWriteTest"
        putMVar done ()
    atomicWriteIntArray mba 0 1
    putMVar latch ()
    takeMVar done

-- | Create two threads that mutate the byte array passed to them
-- concurrently. The array is one word large.
race :: Int                    -- ^ Initial value of array element
     -> (MByteArray -> IO ())  -- ^ Thread 1 action
     -> (MByteArray -> IO ())  -- ^ Thread 2 action
     -> IO Int                 -- ^ Final value of array element
race n0 thread1 thread2 = do
    done1 <- newEmptyMVar
    done2 <- newEmptyMVar
    mba <- newByteArray (sizeOf (undefined :: Int))
    writeIntArray mba 0 n0
    forkIO $ thread1 mba >> putMVar done1 ()
    forkIO $ thread2 mba >> putMVar done2 ()
    mapM_ takeMVar [done1, done2]
    readIntArray mba 0

-- | Test atomicCasWordAddr# by having two threads concurrenctly increment a
-- counter, checking the sum at the end.
casTestAddr :: IO ()
casTestAddr = do
    tot <- raceAddr 0
        (\ addr -> work addr (fromIntegral iters) 1)
        (\ addr -> work addr (fromIntegral iters) 2)
    assertEq (3 * fromIntegral iters) tot "casTestAddr"
  where
    work :: Ptr Word -> Word -> Word -> IO ()
    work ptr 0 val = return ()
    work ptr n val = add ptr val >> work ptr (n-1) val

    -- Fetch-and-add implemented using CAS.
    add :: Ptr Word -> Word -> IO ()
    add ptr n = peek ptr >>= go
      where
        go old = do
            old' <- atomicCasWordPtr ptr old (old + n)
            when (old /= old') $ go old'

    -- | Create two threads that mutate the byte array passed to them
    -- concurrently. The array is one word large.
    raceAddr :: Word                -- ^ Initial value of array element
            -> (Ptr Word -> IO ())  -- ^ Thread 1 action
            -> (Ptr Word -> IO ())  -- ^ Thread 2 action
            -> IO Word              -- ^ Final value of array element
    raceAddr n0 thread1 thread2 = do
        done1 <- newEmptyMVar
        done2 <- newEmptyMVar
        ptr <- asWordPtr <$> callocBytes (sizeOf (undefined :: Word))
        forkIO $ thread1 ptr >> putMVar done1 ()
        forkIO $ thread2 ptr >> putMVar done2 ()
        mapM_ takeMVar [done1, done2]
        peek ptr
      where
        asWordPtr :: Ptr a -> Ptr Word
        asWordPtr = castPtr

------------------------------------------------------------------------
-- Test helper

assertEq :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEq expected actual name
    | expected == actual = putStrLn $ name ++ ": OK"
    | otherwise = do
        putStrLn $ name ++ ": FAIL"
        putStrLn $ "Expected: " ++ show expected
        putStrLn $ "  Actual: " ++ show actual

------------------------------------------------------------------------
-- Wrappers around MutableByteArray#

data MByteArray = MBA (MutableByteArray# RealWorld)

fetchAddIntArray :: MByteArray -> Int -> Int -> IO ()
fetchAddIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case fetchAddIntArray# mba# ix# n# s# of
        (# s2#, _ #) -> (# s2#, () #)

fetchSubIntArray :: MByteArray -> Int -> Int -> IO ()
fetchSubIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case fetchSubIntArray# mba# ix# n# s# of
        (# s2#, _ #) -> (# s2#, () #)

fetchAndIntArray :: MByteArray -> Int -> Int -> IO ()
fetchAndIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case fetchAndIntArray# mba# ix# n# s# of
        (# s2#, _ #) -> (# s2#, () #)

fetchNandIntArray :: MByteArray -> Int -> Int -> IO ()
fetchNandIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case fetchNandIntArray# mba# ix# n# s# of
        (# s2#, _ #) -> (# s2#, () #)

fetchOrIntArray :: MByteArray -> Int -> Int -> IO ()
fetchOrIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case fetchOrIntArray# mba# ix# n# s# of
        (# s2#, _ #) -> (# s2#, () #)

fetchXorIntArray :: MByteArray -> Int -> Int -> IO ()
fetchXorIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case fetchXorIntArray# mba# ix# n# s# of
        (# s2#, _ #) -> (# s2#, () #)

newByteArray :: Int -> IO MByteArray
newByteArray (I# n#) = IO $ \ s# ->
    case newByteArray# n# s# of
        (# s2#, mba# #) -> (# s2#, MBA mba# #)

writeIntArray :: MByteArray -> Int -> Int -> IO ()
writeIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case writeIntArray# mba# ix# n# s# of
        s2# -> (# s2#, () #)

readIntArray :: MByteArray -> Int -> IO Int
readIntArray (MBA mba#) (I# ix#) = IO $ \ s# ->
    case readIntArray# mba# ix# s# of
        (# s2#, n# #) -> (# s2#, I# n# #)

atomicWriteIntArray :: MByteArray -> Int -> Int -> IO ()
atomicWriteIntArray (MBA mba#) (I# ix#) (I# n#) = IO $ \ s# ->
    case atomicWriteIntArray# mba# ix# n# s# of
        s2# -> (# s2#, () #)

atomicReadIntArray :: MByteArray -> Int -> IO Int
atomicReadIntArray (MBA mba#) (I# ix#) = IO $ \ s# ->
    case atomicReadIntArray# mba# ix# s# of
        (# s2#, n# #) -> (# s2#, I# n# #)

casIntArray :: MByteArray -> Int -> Int -> Int -> IO Int
casIntArray (MBA mba#) (I# ix#) (I# old#) (I# new#) = IO $ \ s# ->
    case casIntArray# mba# ix# old# new# s# of
        (# s2#, old2# #) -> (# s2#, I# old2# #)

------------------------------------------------------------------------
-- Wrappers around Addr#

-- Should this be added to Foreign.Storable?  Similar to poke, but does the
-- update atomically.
atomicCasWordPtr :: Ptr Word -> Word -> Word -> IO Word
atomicCasWordPtr (Ptr addr#) (W# old#) (W# new#) = IO $ \ s# ->
    case atomicCasWordAddr# addr# old# new# s# of
        (# s2#, old2# #) -> (# s2#, W# old2# #)
