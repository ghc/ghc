{-# LANGUAGE TemplateHaskell #-}

module Main where

import T16190_Embed
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import GHC.Ptr

ptr :: Ptr Word32
ptr = Ptr $(embedBytes (replicate (3 * 1000 * 1000) 0x37))

main :: IO ()
main = do
   w <- peek (ptr `plusPtr` 12) 
   print (w == (0x37373737 :: Word32))
