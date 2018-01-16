-- Test from https://github.com/haskell/pretty/issues/32#issuecomment-223073337
module UnitT32 where

import Text.PrettyPrint.HughesPJ

import TestUtils

testT32 :: IO ()
testT32 = simpleMatch "T3911" (replicate 10 'x') $ take 10 $ render $ hcat $ repeat $ text "x"
