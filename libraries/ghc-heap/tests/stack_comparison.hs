{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts.DecodeStack
import GHC.Stack.CloneStack
import TestUtils
import GHC.Exts
import Data.Array.Byte
import GHC.Exts.Heap

foreign import ccall "foldStackToArrayClosure" foldStackToArrayClosure# :: StackSnapshot# -> ByteArray#

foldStackToArrayClosure :: StackSnapshot -> ByteArray
foldStackToArrayClosure (StackSnapshot s#) = ByteArray (foldStackToArrayClosure# s#)

main :: IO ()
main = do
  stack <- cloneMyStack
  let ba = foldStackToArrayClosure stack
  let s = I# (sizeofByteArray# b#)
      (ByteArray b#) = ba
--  print $ "ByteArray size" ++ show (I# (sizeofByteArray# b#))
--  print $ "indices " ++ show [0..((wds s) -1)]
  print . show . toClosureTypes . toWords $ ba

maxWordIndex :: ByteArray -> Int
maxWordIndex (ByteArray ba#) =
  let s = I# (sizeofByteArray# ba#)
      words = s `div` 8
  in
    case words of
      w | w == 0 -> error "ByteArray contains no content!"
      w -> w - 1

toWords :: ByteArray -> [Word]
toWords ba@(ByteArray b#) =
  let s = I# (sizeofByteArray# b#)
  in
    -- TODO: Adjust 8 to machine word size
    [ W# (indexWordArray# b# (toInt# i)) | i <- [0..maxWordIndex(ba)] ]

toClosureTypes :: [Word] -> [ClosureType]
toClosureTypes = map (toEnum . fromIntegral)

toInt# :: Int -> Int#
toInt# (I# i#) = i#
