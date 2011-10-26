-------------------------------------------------------------------------------
--
-- | Break Arrays in the IO monad
--
-- Entries in the array are Word sized Conceptually, a zero-indexed IOArray of
-- Bools, initially False.  They're represented as Words with 0==False, 1==True.
-- They're used to determine whether GHCI breakpoints are on or off.
--
-- (c) The University of Glasgow 2007
--
-------------------------------------------------------------------------------

module BreakArray
    (
      BreakArray
#ifdef GHCI
          (BA) -- constructor is exported only for ByteCodeGen
#endif
    , newBreakArray
#ifdef GHCI
    , getBreak 
    , setBreakOn 
    , setBreakOff
    , showBreakArray
#endif
    ) where

#ifdef GHCI
import Control.Monad

import GHC.Exts
import GHC.IO ( IO(..) )

import Constants

data BreakArray = BA (MutableByteArray# RealWorld)

breakOff, breakOn :: Word
breakOn  = 1
breakOff = 0

showBreakArray :: BreakArray -> IO ()
showBreakArray array = do
    forM_ [0..(size array - 1)] $ \i -> do
        val <- readBreakArray array i
        putStr $ ' ' : show val
    putStr "\n"

setBreakOn :: BreakArray -> Int -> IO Bool 
setBreakOn array index
    | safeIndex array index = do 
          writeBreakArray array index breakOn 
          return True
    | otherwise = return False 

setBreakOff :: BreakArray -> Int -> IO Bool 
setBreakOff array index
    | safeIndex array index = do
          writeBreakArray array index breakOff
          return True
    | otherwise = return False 

getBreak :: BreakArray -> Int -> IO (Maybe Word)
getBreak array index 
    | safeIndex array index = do
          val <- readBreakArray array index 
          return $ Just val 
    | otherwise = return Nothing

safeIndex :: BreakArray -> Int -> Bool
safeIndex array index = index < size array && index >= 0

size :: BreakArray -> Int
size (BA array) = (I# (sizeofMutableByteArray# array)) `div` wORD_SIZE

allocBA :: Int -> IO BreakArray 
allocBA (I# sz) = IO $ \s1 ->
    case newByteArray# sz s1 of { (# s2, array #) -> (# s2, BA array #) }

-- create a new break array and initialise elements to zero
newBreakArray :: Int -> IO BreakArray
newBreakArray entries@(I# sz) = do
    BA array <- allocBA (entries * wORD_SIZE) 
    case breakOff of 
        W# off -> do    -- Todo: there must be a better way to write zero as a Word!
            let loop n | n ==# sz = return ()
                       | otherwise = do
                             writeBA# array n off 
                             loop (n +# 1#)
            loop 0#
    return $ BA array

writeBA# :: MutableByteArray# RealWorld -> Int# -> Word# -> IO ()
writeBA# array i word = IO $ \s ->
    case writeWordArray# array i word s of { s -> (# s, () #) }

writeBreakArray :: BreakArray -> Int -> Word -> IO ()
writeBreakArray (BA array) (I# i) (W# word) = writeBA# array i word 

readBA# :: MutableByteArray# RealWorld -> Int# -> IO Word 
readBA# array i = IO $ \s -> 
    case readWordArray# array i s of { (# s, c #) -> (# s, W# c #) }

readBreakArray :: BreakArray -> Int -> IO Word 
readBreakArray (BA array) (I# i) = readBA# array i

#else /* !GHCI */

-- stub implementation to make main/, etc., code happier.
-- IOArray and IOUArray are increasingly non-portable,
-- still don't have quite the same interface, and (for GHCI)
-- presumably have a different representation.
data BreakArray = Unspecified

newBreakArray :: Int -> IO BreakArray
newBreakArray _ = return Unspecified

#endif /* GHCI */

