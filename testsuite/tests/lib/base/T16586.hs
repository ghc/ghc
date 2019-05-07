{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Main where

import Data.Proxy
import GHC.TypeNats
import Numeric.Natural

newtype Foo (m :: Nat) = Foo { getVal :: Word }

mul :: KnownNat m => Foo m -> Foo m -> Foo m
mul mx@(Foo x) (Foo y) =
  Foo $ x * y `rem` fromIntegral (natVal mx)

pow :: KnownNat m => Foo m -> Int -> Foo m
pow x k = iterate (`mul` x) (Foo 1) !! k

modl :: (forall m. KnownNat m => Foo m) -> Natural -> Word
modl x m = case someNatVal m of
  SomeNat (_ :: Proxy m) -> getVal (x :: Foo m)

-- Should print 1
main :: IO ()
main = print $ (Foo 127 `pow` 31336) `modl` 31337

dummyValue :: Word
dummyValue = (Foo 33 `pow` 44) `modl` 456
