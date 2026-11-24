{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import Provider

isApplication :: Typed 'Empty -> Bool
isApplication (Pack _ (App Id Zero)) = True
isApplication _ = False

main :: IO ()
main = do
  let direct :: Typed 'Empty
      direct = packApp Empty Nat Nat Nat Refl Id Zero
      inferred :: Typed 'Empty
      inferred = inferApp Empty Nat Nat Nat Refl Id Zero
  print (isApplication direct, isApplication inferred)
