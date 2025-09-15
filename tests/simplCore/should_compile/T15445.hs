module T15445 where

import T15445a


foo :: IO ()
foo = do { print (plusTwoRec [1..10 :: Int])
         ; print (plusTwoRec' [1..20 :: Int]) }
