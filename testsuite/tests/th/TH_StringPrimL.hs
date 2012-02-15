{-# LANGUAGE MagicHash #-}
module Main where

import Language.Haskell.TH
import GHC.Prim(Addr#)
import GHC.Ptr
import Foreign.Marshal.Array (peekArray)
import Data.Word (Word8)

check_equal :: [Word8] -> Addr# -> IO ()
check_equal bytes addr = do
  bytes' <- peekArray (length bytes) (Ptr addr)
  print (bytes == bytes')

main = do
  -- check round-trip
  check_equal [0..255] $(litE $ stringPrimL [0..255])

  -- check printing
  let e = LitE (StringPrimL [0..255])
  print e
  putStrLn (pprint e)
