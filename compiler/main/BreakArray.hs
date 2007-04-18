-----------------------------------------------------------------------------
--
-- Break Arrays in the IO monad
-- Entries in the array are Word sized 
--
-- (c) The University of Glasgow 2007
--
-----------------------------------------------------------------------------

module BreakArray
  ( BreakArray (BA)
        -- constructor is exported only for ByteCodeGen
  , newBreakArray
  , getBreak 
  , setBreakOn 
  , setBreakOff
  , showBreakArray
  ) where

import GHC.Exts
import GHC.IOBase
import GHC.Prim
import GHC.Word
import Constants

data BreakArray = BA (MutableByteArray# RealWorld)

breakOff, breakOn :: Word
breakOn  = fromIntegral 1
breakOff = fromIntegral 0

-- XXX crude
showBreakArray :: BreakArray -> IO ()
showBreakArray array = do
   let loop count sz
          | count == sz = return ()
          | otherwise = do
               val <- readBreakArray array count 
               putStr $ " " ++ show val
               loop (count + 1) sz
   loop 0 (size array) 
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
         let loop n
                | n ==# sz = return ()
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
