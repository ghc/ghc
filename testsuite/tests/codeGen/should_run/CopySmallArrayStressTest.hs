{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, MagicHash,
             UnboxedTuples #-}

-- !!! stress tests of copying/cloning primitive arrays

-- Note: You can run this test manually with an argument (i.e.
-- ./CopySmallArrayStressTest 10000) if you want to run the stress
-- test for longer.

{-
Test strategy
=============

We create an array of arrays of integers. Repeatedly we then either

* allocate a new array in place of an old, or

* copy a random segment of an array into another array (which might be
  the source array).

By running this process long enough we hope to trigger any bugs
related to garbage collection or edge cases.

We only test copySmallMutableArray# and cloneSmallArray# as they are
representative of all the primops.
-}

module Main ( main ) where

import Debug.Trace (trace)

import Control.Exception (assert)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import GHC.Exts hiding (IsList(..))
import GHC.ST hiding (liftST)
import Prelude hiding (length, read)
import qualified Prelude as P
import qualified Prelude as P
import System.Environment
import System.Random

main :: IO ()
main = do
    args <- getArgs
    -- Number of copies to perform
    let numMods = case args of
            [] -> 100
            [n] -> P.read n :: Int
    putStr (test_copyMutableArray numMods ++ "\n" ++
            test_cloneMutableArray numMods ++ "\n"
           )

-- Number of arrays
numArrays :: Int
numArrays = 100

-- Maxmimum length of a sub-array
maxLen :: Int
maxLen = 1024

-- Create an array of arrays, with each sub-array having random length
-- and content.
setup :: Rng s (MArray s (MArray s Int))
setup = do
    len <- rnd (1, numArrays)
    marr <- liftST $ new_ len
    let go i
            | i >= len = return ()
            | otherwise = do
                n <- rnd (1, maxLen)
                subarr <- liftST $ fromList [j*j | j <- [(0::Int)..n-1]]
                liftST $ write marr i subarr
                go (i+1)
    go 0
    return marr

-- Replace one of the sub-arrays with a newly allocated array.
allocate :: MArray s (MArray s Int) -> Rng s ()
allocate marr = do
    ix <- rnd (0, length marr - 1)
    n <- rnd (1, maxLen)
    subarr <- liftST $ fromList [j*j | j <- [(0::Int)..n-1]]
    liftST $ write marr ix subarr

type CopyFunction s a =
    MArray s a -> Int -> MArray s a -> Int -> Int -> ST s ()

-- Copy a random segment of an array onto another array, using the
-- supplied copy function.
copy :: MArray s (MArray s a) -> CopyFunction s a
     -> Rng s (Int, Int, Int, Int, Int)
copy marr f = do
    six <- rnd (0, length marr - 1)
    dix <- rnd (0, length marr - 1)
    src <- liftST $ read marr six
    dst <- liftST $ read marr dix
    let srcLen = length src
    srcOff <- rnd (0, srcLen - 1)
    let dstLen = length dst
    dstOff <- rnd (0, dstLen - 1)
    n <- rnd (0, min (srcLen - srcOff) (dstLen - dstOff))
    liftST $ f src srcOff dst dstOff n
    return (six, dix, srcOff, dstOff, n)

type CloneFunction s a = MArray s a -> Int -> Int -> ST s (MArray s a)

-- Clone a random segment of an array, replacing another array, using
-- the supplied clone function.
clone :: MArray s (MArray s a) -> CloneFunction s a
      -> Rng s (Int, Int, Int, Int)
clone marr f = do
    six <- rnd (0, length marr - 1)
    dix <- rnd (0, length marr - 1)
    src <- liftST $ read marr six
    let srcLen = length src
    -- N.B. The array length might be zero if we previously cloned
    -- zero elements from some array.
    srcOff <- rnd (0, max 0 (srcLen - 1))
    n <- rnd (0, srcLen - srcOff)
    dst <- liftST $ f src srcOff n
    liftST $ write marr dix dst
    return (six, dix, srcOff, n)

------------------------------------------------------------------------
-- copySmallMutableArray#

-- Copy a slice of the source array into a destination array and check
-- that the copy succeeded.
test_copyMutableArray :: Int -> String
test_copyMutableArray numMods = runST $ run $ do
    marr <- local setup
    marrRef <- setup
    let go i
            | i >= numMods = return "test_copyMutableArray: OK"
            | otherwise = do
                -- Either allocate or copy
                alloc <- rnd (True, False)
                if alloc then doAlloc else doCopy
                go (i+1)

        doAlloc = do
            local $ allocate marr
            allocate marrRef

        doCopy = do
            inp <- liftST $ asList marr
            _ <- local $ copy marr copyMArray
            (six, dix, srcOff, dstOff, n) <- copy marrRef copyMArraySlow
            el <- liftST $ asList marr
            elRef <- liftST $ asList marrRef
            when (el /= elRef) $
                fail inp el elRef six dix srcOff dstOff n
    go 0
  where
    fail inp el elRef six dix srcOff dstOff n =
        error $ "test_copyMutableArray: FAIL\n"
        ++ "   Input: " ++ unlinesShow inp
        ++ "    Copy: six: " ++ show six ++ " dix: " ++ show dix ++ " srcOff: "
        ++ show srcOff ++ " dstOff: " ++ show dstOff ++ " n: " ++ show n ++ "\n"
        ++ "Expected: " ++ unlinesShow elRef
        ++ "  Actual: " ++ unlinesShow el

asList :: MArray s (MArray s a) -> ST s [[a]]
asList marr = toListM =<< mapArrayM toListM marr

unlinesShow :: Show a => [a] -> String
unlinesShow =  concatMap (\ x -> show x ++ "\n")

------------------------------------------------------------------------
-- cloneSmallMutableArray#

-- Copy a slice of the source array into a destination array and check
-- that the copy succeeded.
test_cloneMutableArray :: Int -> String
test_cloneMutableArray numMods = runST $ run $ do
    marr <- local setup
    marrRef <- setup
    let go i
            | i >= numMods = return "test_cloneMutableArray: OK"
            | otherwise = do
                -- Either allocate or clone
                alloc <- rnd (True, False)
                if alloc then doAlloc else doClone
                go (i+1)

        doAlloc = do
            local $ allocate marr
            allocate marrRef

        doClone = do
            inp <- liftST $ asList marr
            _ <- local $ clone marr cloneMArray
            (six, dix, srcOff, n) <- clone marrRef cloneMArraySlow
            el <- liftST $ asList marr
            elRef <- liftST $ asList marrRef
            when (el /= elRef) $
                fail inp el elRef six dix srcOff n
    go 0
  where
    fail inp el elRef six dix srcOff n =
        error $ "test_cloneMutableArray: FAIL\n"
        ++ "   Input: " ++ unlinesShow inp
        ++ "   Clone: six: " ++ show six ++ " dix: " ++ show dix ++ " srcOff: "
        ++ show srcOff ++ " n: " ++ show n ++ "\n"
        ++ "Expected: " ++ unlinesShow elRef
        ++ "  Actual: " ++ unlinesShow el

------------------------------------------------------------------------
-- Convenience wrappers for SmallArray# and SmallMutableArray#

data Array a = Array
    { unArray :: SmallArray# a
    , lengthA :: {-# UNPACK #-} !Int}

data MArray s a = MArray
    { unMArray :: SmallMutableArray# s a
    , lengthM :: {-# UNPACK #-} !Int}

class IArray a where
    length :: a -> Int
instance IArray (Array a) where
    length = lengthA
instance IArray (MArray s a) where
    length = lengthM

instance Eq a => Eq (Array a) where
    arr1 == arr2 = toList arr1 == toList arr2

new :: Int -> a -> ST s (MArray s a)
new n@(I# n#) a =
    assert (n >= 0) $
    ST $ \s# -> case newSmallArray# n# a s# of
        (# s2#, marr# #) -> (# s2#, MArray marr# n #)

new_ :: Int -> ST s (MArray s a)
new_ n = new n (error "Undefined element")

write :: MArray s a -> Int -> a -> ST s ()
write marr i@(I# i#) a =
    assert (i >= 0) $
    assert (i < length marr) $
    ST $ \ s# ->
    case writeSmallArray# (unMArray marr) i# a s# of
        s2# -> (# s2#, () #)

read :: MArray s a -> Int -> ST s a
read marr i@(I# i#) =
    assert (i >= 0) $
    assert (i < length marr) $
    ST $ \ s# ->
    readSmallArray# (unMArray marr) i# s#

index :: Array a -> Int -> a
index arr i@(I# i#) =
    assert (i >= 0) $
    assert (i < length arr) $
    case indexSmallArray# (unArray arr) i# of
        (# a #) -> a

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze marr = ST $ \ s# ->
    case unsafeFreezeSmallArray# (unMArray marr) s# of
        (# s2#, arr# #) -> (# s2#, Array arr# (length marr) #)

toList :: Array a -> [a]
toList arr = go 0
  where
    go i | i >= length arr = []
         | otherwise = index arr i : go (i+1)

fromList :: [e] -> ST s (MArray s e)
fromList es = do
    marr <- new_ n
    let go !_ [] = return ()
        go i (x:xs) = write marr i x >> go (i+1) xs
    go 0 es
    return marr
  where
    n = P.length es

mapArrayM :: (a -> ST s b) -> MArray s a -> ST s (MArray s b)
mapArrayM f src = do
    dst <- new_ n
    let go i
            | i >= n = return dst
            | otherwise = do
                el <- read src i
                el' <- f el
                write dst i el'
                go (i+1)
    go 0
  where
    n = length src

toListM :: MArray s e -> ST s [e]
toListM marr =
    sequence [read marr i | i <- [0..(length marr)-1]]

------------------------------------------------------------------------
-- Wrappers around copy/clone primops

copyMArray :: MArray s a -> Int -> MArray s a -> Int -> Int -> ST s ()
copyMArray src six@(I# six#) dst dix@(I# dix#) n@(I# n#) =
    assert (six >= 0) $
    assert (six + n <= length src) $
    assert (dix >= 0) $
    assert (dix + n <= length dst) $
    ST $ \ s# ->
    case copySmallMutableArray# (unMArray src) six# (unMArray dst) dix# n# s# of
        s2# -> (# s2#, () #)

cloneMArray :: MArray s a -> Int -> Int -> ST s (MArray s a)
cloneMArray marr off@(I# off#) n@(I# n#) =
    assert (off >= 0) $
    assert (off + n <= length marr) $
    ST $ \ s# ->
    case cloneSmallMutableArray# (unMArray marr) off# n# s# of
        (# s2#, marr2 #) -> (# s2#, MArray marr2 n #)

------------------------------------------------------------------------
-- Manual versions of copy/clone primops.  Used to validate the
-- primops

copyMArraySlow :: MArray s e -> Int -> MArray s e -> Int -> Int -> ST s ()
copyMArraySlow !src !six !dst !dix n =
    assert (six >= 0) $
    assert (six + n <= length src) $
    assert (dix >= 0) $
    assert (dix + n <= length dst) $
       if six < dix
       then goB (six+n-1) (dix+n-1) 0  -- Copy backwards
       else goF six dix 0  -- Copy forwards
  where
    goF !i !j c
        | c >= n = return ()
        | otherwise = do b <- read src i
                         write dst j b
                         goF (i+1) (j+1) (c+1)
    goB !i !j c
        | c >= n = return ()
        | otherwise = do b <- read src i
                         write dst j b
                         goB (i-1) (j-1) (c+1)

cloneMArraySlow :: MArray s a -> Int -> Int -> ST s (MArray s a)
cloneMArraySlow !marr !off n =
    assert (off >= 0) $
    assert (off + n <= length marr) $ do
        marr2 <- new_ n
        let go !i !j c
                | c >= n = return marr2
                | otherwise = do
                    b <- read marr i
                    write marr2 j b
                    go (i+1) (j+1) (c+1)
        go off 0 0

------------------------------------------------------------------------
-- Utilities for simplifying RNG passing

newtype Rng s a = Rng { unRng :: StateT StdGen (ST s) a }
                deriving Monad

-- Same as 'randomR', but using the RNG state kept in the 'Rng' monad.
rnd :: Random a => (a, a) -> Rng s a
rnd r = Rng $ do
    g <- get
    let (x, g') = randomR r g
    put g'
    return x

-- Run a sub-computation without affecting the RNG state.
local :: Rng s a -> Rng s a
local m = Rng $ do
    g <- get
    x <- unRng m
    put g
    return x

liftST :: ST s a -> Rng s a
liftST m = Rng $ lift m

run :: Rng s a -> ST s a
run = flip evalStateT (mkStdGen 13) . unRng

