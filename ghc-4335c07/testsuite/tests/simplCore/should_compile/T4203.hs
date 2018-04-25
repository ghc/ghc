{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Triggered a lint error due to the specialiser

module T4203 where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

newtype NonNegative a = NonNegative a
 deriving (Eq, Num, Show)

instance (Eq a, Num a) => Arbitrary (NonNegative a) where
  arbitrary = return (rubble (rubble 0))
  coarbitrary = error "urk"

rubble :: (Eq a, Num a) => a -> a
rubble 0 = 1
rubble n = n * rubble (n-1)

newtype EmptyStackSet = EmptyStackSet (NonNegative Int)

instance Arbitrary EmptyStackSet where
  arbitrary = do
        x <- arbitrary :: Gen (NonNegative Int)
        return $ EmptyStackSet x
  coarbitrary = error "urk"

newtype Gen a = Gen a

instance Functor Gen where
    fmap = liftM

instance Applicative Gen where
    pure = return
    (<*>) = ap

instance Monad Gen where
  return a    = Gen a
  Gen m >>= k = Gen (let Gen m' = k m in m')

class Arbitrary a where
  arbitrary   :: Gen a
  coarbitrary :: a

data StackSet = StackSet
