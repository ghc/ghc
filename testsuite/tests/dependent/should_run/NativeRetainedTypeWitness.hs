{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import NativeRetainedTypeWitnessLib
import Type.Reflection

data Token = Token Int deriving Show

main :: IO ()
main = do
  print (preserve (typeRep @Token) (Token 7))
  print (preserve (error "unused witness") (Token 8))
  print (onlyBool (typeRep @Bool) True)
  print (onlyBool (typeRep @Token) (error "mismatched payload"))
  let (first, second) = duplicate (typeRep @Token) (typeRep @Token)
  print (first == second)
  let (lazy, _) = duplicate (typeRep @Token) (error "unused second witness")
  print (lazy == typeRep @Token)
