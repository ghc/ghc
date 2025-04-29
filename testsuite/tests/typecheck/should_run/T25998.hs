{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Kind
import Type.Reflection

test :: forall (a :: Type) (b :: Type). TypeRep a -> TypeRep b -> String
test a b = case eqTypeRep a b of
             Just _ -> "Equal!\n"
             Nothing -> "Not equal:\n" <> show a <> "\n" <> show b <> "\n"

combine :: forall (t :: Type -> Type -> Type). Typeable t => TypeRep (t Bool Int)
combine = typeRep

main :: IO ()
main = do
  putStrLn $ test (typeRep @(Bool -> Int)) (combine @(->))
