{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DPH.Arbitrary.Int 
        ( Len (..)
        , SizedInt (..))
where
import Test.QuickCheck


-- | A non-negative integer
newtype Len 
        = Len Int 
        deriving (Eq, Ord, Enum, Show, Num)

instance Arbitrary Len where
  arbitrary = sized $ \n -> Len `fmap` choose (0,n)


-- | An integer whose max absolute value depends on the size parameter.
newtype SizedInt 
        = SizedInt Int 
        deriving (Eq, Ord, Enum, Show, Num)

instance Arbitrary SizedInt where
  arbitrary = SizedInt `fmap` arbitrarySizedIntegral

