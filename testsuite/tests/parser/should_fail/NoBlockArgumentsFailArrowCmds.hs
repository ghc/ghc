{-# LANGUAGE NoBlockArguments #-}
{-# LANGUAGE Arrows #-}

module NoBlockArgumentsFailArrowCmds where

import Control.Arrow

cmdLam :: () -> ()
cmdLam = proc () -> (| id \() -> () >- returnA |) ()

cmdCaseOf :: Bool -> Integer
cmdCaseOf = proc x ->
  (| id case x of
      False -> 0 >- returnA
      True  -> 1 >- returnA |)

cmdIfThenElse :: Bool -> Integer
cmdIfThenElse = proc x ->
  (| id if x then 1 >- returnA
             else 0 >- returnA |)

cmdLetIn :: Integer -> (Integer, Integer)
cmdLetIn = proc x ->
  (| id let y = x+1 in (x,y) >- returnA |)

cmdDo :: a -> a
cmdDo = proc x -> (| id do x >- returnA |)

argProc :: String -> Int
argProc = (. length) proc x -> (*2) -< x+1
