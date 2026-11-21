{-# LANGUAGE DataKinds, GADTs, LinearTypes, Modifiers, TypeApplications #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Exts (FUN)
import GHC.Matchability (M, U)
import GHC.Types (Multiplicity(..))
import Type.Reflection

main :: IO ()
main = do
  let unmatchable = typeRep @(Int -> Bool)
      matchable = typeRep @(Int %M -> Bool)
      rawU = typeRep @((->) :: Type -> Type -> Type)
      rawM = typeRep @(FUN @M 'Many :: Type -> Type -> Type)
  print unmatchable
  print matchable
  print (typeRep @(Int %1 %M -> Bool))
  print rawU
  print rawM
  print (typeRep @(Proxy :: (Type %M -> Type) -> Type))
  print (typeRep @(Proxy :: (Type %U -> Type) -> Type))
  print (typeRep @(Proxy :: ((Type %U -> Type) %M -> Type) -> Type))
  print (typeRep @(Proxy :: ((Type %M -> Type) %M -> Type) -> Type))
  print (typeRep @(Type -> Type))
  print (typeRepKind (typeRep @Maybe))
  print (SomeTypeRep unmatchable /= SomeTypeRep matchable)
  print (SomeTypeRep rawU /= SomeTypeRep rawM)
  print $ case matchable of
    Fun' ma _ _ -> SomeTypeRep ma == SomeTypeRep (typeRep @M)
    _ -> False
  print $ case rawU of
    Con' _ [ma, _, _, _] -> ma == SomeTypeRep (typeRep @U)
    _ -> False
