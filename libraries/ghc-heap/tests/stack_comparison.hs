{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Data.Array.Byte
import GHC.Exts
import GHC.Exts.DecodeStack
import GHC.Exts.Heap
import GHC.Stack.CloneStack
import TestUtils

foreign import ccall "foldStackToArrayClosure" foldStackToArrayClosure# :: StackSnapshot# -> ByteArray#

foldStackToArrayClosure :: StackSnapshot -> ByteArray
foldStackToArrayClosure (StackSnapshot s#) = ByteArray (foldStackToArrayClosure# s#)

main :: IO ()
main = do
  stack <- cloneMyStack
  let ba = foldStackToArrayClosure stack
  let s = I# (sizeofByteArray# b#)
      (ByteArray b#) = ba
  print . show . toClosureTypes . toWords $ ba

toWords :: ByteArray -> [Word]
toWords ba@(ByteArray b#) =
  let s = I# (sizeofByteArray# b#)
   in -- TODO: Adjust 8 to machine word size
      [W# (indexWordArray# b# (toInt# i)) | i <- [0 .. maxWordIndex (ba)]]
  where
    maxWordIndex :: ByteArray -> Int
    maxWordIndex (ByteArray ba#) =
      let s = I# (sizeofByteArray# ba#)
          words = s `div` 8
       in case words of
            w | w == 0 -> error "ByteArray contains no content!"
            w -> w - 1

toClosureTypes :: [Word] -> [ClosureType]
toClosureTypes = map (toEnum . fromIntegral)

toInt# :: Int -> Int#
toInt# (I# i#) = i#
