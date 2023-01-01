module T15445 where

import T15445a

-- The core dump should contain a call to the specialization of plusTwoRec and plusTwoRec'
foo :: IO ()
foo = do { print (plusTwoRec [1..10 :: Int])
         ; print (plusTwoRec' [1..20 :: Int]) }
