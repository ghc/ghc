{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import NativeRetainedTypeWitnessLib
import Type.Reflection

data Token = Token Int deriving Show

useOrdinary :: (forall a. Typeable a => a -> String) -> [String]
useOrdinary f = [f False, f 'x']

selectInts :: [Int] -> [String]
selectInts = map selected

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
  print (map selected [(), ()], selectInts [4], useOrdinary selected)
  print (id reflected :: TypeRep Bool)
  print (selected (error "unused payload" :: Int))
  let label :: Label 18446744073709551616
      label = labelWith 18446744073709551616 (error "unused payload" :: Bool)
  print (readLabel label)
