{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import Provider

indices :: Maybe (Arrow 'Empty) -> Maybe (Ty, Ty)
indices Nothing = Nothing
indices (Just (MkArrow a b _)) = Just (a, b)

badPackage :: Ty -> Typed 'Empty
badPackage t = Pack t (error "unused payload")

main :: IO ()
main = do
  let nat = Pack Nat Zero
      arrow = Pack (Arr Nat Nat) Id
      other = Pack (Arr (Arr Nat Nat) Nat) (error "unused payload")
      badWitness :: Ty
      badWitness = error "required witness"
      bad = badPackage badWitness
  print (indices (refine Empty Nothing))
  print (indices (refine Empty (Just other)))
  print (indices (refineEither Empty (Just (arrow, nat))))
  print (indices (refineEither Empty (Just (arrow, other))))
  print (indices (refineEither Empty (Just (nat, bad))))
  demanded <- try (evaluate (indices (refine Empty (Just bad))))
  print (either (const True) (const False) (demanded :: Either ErrorCall (Maybe (Ty, Ty))))
  case nestedEquality (J (A B B) (A B B)) Refl of
    Refl -> putStrLn "nested Refl"
