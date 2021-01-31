{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, Safe, TypeFamilies #-}

module Main where

import Data.Maybe
import Data.Proxy
import Type.Reflection
import GHC.TypeLits

data Dat (x :: Proxy 1) = MkD1

evil :: Maybe (Nat :~~: Symbol)
evil = eqTypeRep (case (typeRepKind (typeRep :: TypeRep Dat)) of
                          (Fun (App _ x) _) -> typeRepKind x)
                 (typeRep :: TypeRep Symbol)


data family Cast k l r
newtype instance Cast Nat l r = CastNat { runCastNat :: l }
newtype instance Cast Symbol l r = CastSymbol { runCastSymbol :: r }

{-# NOINLINE castHelper #-}
castHelper :: Maybe (a :~~: b) -> Cast a l r -> Cast b l r
castHelper (Just HRefl) = id
castHelper Nothing      = error "No more bug!"

cast :: a -> b
cast = runCastSymbol . castHelper evil . CastNat

main :: IO ()
main = print (cast 'a' :: Int)
