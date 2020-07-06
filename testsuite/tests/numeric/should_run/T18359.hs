{-# LANGUAGE MagicHash #-}

import GHC.Num.BigNat
import GHC.Num.Primitives
import GHC.Prim.Exception
import GHC.Exts
import Control.Exception

main :: IO ()
main = do
   foo  `catch` \DivideByZero -> putStrLn "Caught DivideByZero exception in foo"
   foo2 `catch` \DivideByZero -> putStrLn "Caught DivideByZero exception in foo2"

foo2 = case raiseDivZero of
   I# _ -> print "NOPE"

foo :: IO ()
foo = print (W# (bigNatRemWord# (bigNatOne# void#) 0##))
