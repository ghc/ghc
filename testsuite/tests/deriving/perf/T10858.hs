{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -ddump-if-trace #-}

import GHC.Exts

data TestData = First Int Double String Int Int Int Int
              | Second Char# Int# Word# Double#
              | Third TestData TestData TestData TestData
              deriving (Eq, Ord)

main = return ()
