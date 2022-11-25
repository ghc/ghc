{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts.DecodeStack
import GHC.Stack.CloneStack
import TestUtils
import GHC.Exts
import Data.Array.Byte

foreign import ccall "foldStackToArrayClosure" foldStackToArrayClosure# :: StackSnapshot# -> ByteArray#

foldStackToArrayClosure :: StackSnapshot -> ByteArray
foldStackToArrayClosure (StackSnapshot s#) = ByteArray (foldStackToArrayClosure# s#)

main :: IO ()
main = do
  stack <- cloneMyStack
  let ba = foldStackToArrayClosure stack
  print . show . toWords $ ba

toWords :: ByteArray -> [Word]
toWords (ByteArray b#) =
  let s = I# (sizeofByteArray# b#)
  in
    -- TODO: Adjust 8 to machine word size
    [ W# (indexWordArray# b# (toInt# i)) | i <- [0..], i<=(s `div` 8)  ]

toInt# :: Int -> Int#
toInt# (I# i#) = i#
