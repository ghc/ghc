{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
{-# LANGUAGE RequiredTypeArguments, TypeFamilies, TypeOperators #-}
module Main (main) where

#ifndef KNOWN_LENGTH
import ClosedLengthProducer (length)
import Prelude hiding (length)
#endif
import GHC.TypeNats (Nat, type (+))

data Vec (n :: Nat) a where
  VNil :: Vec 0 a
  (:.) :: a -> Vec n a -> Vec (n + 1) a

infixr 5 :.

#ifdef KNOWN_LENGTH
v :: Vec 3 Bool
#else
v :: Vec (length [1,2,3]) Bool
#endif
#ifdef SHORT_VECTOR
v = (True :. False :. VNil) :: Vec 2 Bool
#else
v = (True :. False :. True :. VNil) :: Vec 3 Bool
#endif

-- | Return the elements in vector order.
toList :: Vec n a -> [a]
toList VNil = []
toList (x :. xs) = x : toList xs

main :: IO ()
main = print (toList v)
