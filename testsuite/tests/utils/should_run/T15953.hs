{-# OPTIONS_GHC -ddump-to-file #-}
{-# OPTIONS_GHC -fhpc -ddump-ticked -ddump-simpl-trace -ddump-tc #-}
{-# OPTIONS_GHC -ddump-types -ddump-core-stats #-}

module T15953 where

foo :: Int -> Int
foo 0 = 0
foo n = foo (n - 1)
