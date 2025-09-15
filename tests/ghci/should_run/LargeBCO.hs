
{-
   Test for BCOs that use larger than 16 bit stack offsets.

   Using Template Haskell because loading the code directly into
   GHCi produces different bytecode that does not have large stack
   offsets.

   testcase from #22888
 -}

{-# LANGUAGE TemplateHaskell #-}
module Main where

import LargeBCO_A

import Data.Binary.Get (runGet, Get, getWord32be)
import qualified Data.ByteString.Lazy as B
import Data.Bits (Bits(..))
import Data.Word (Word32)

import Language.Haskell.TH.Lib

result :: String
result = $(let initState = SHA256S 1 2 3 4 5 6 7 8
               input     = B.replicate 64 0
               output    = runGet (processSHA256Block initState) input
           in litE (stringL (show output))
          )

main :: IO ()
main = putStrLn result
