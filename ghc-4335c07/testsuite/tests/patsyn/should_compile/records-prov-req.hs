{-# LANGUAGE PatternSynonyms, ViewPatterns, GADTs, RankNTypes,
 StandaloneDeriving, FlexibleInstances #-}
module ShouldCompile where

-- Testing that selectors work properly with prov and req thetas

data T a b where
  MkT :: (Show b) => a -> b -> T a b

deriving instance Show (T Int A)

data G a b = MkG { care :: a,  y :: (Show b => b) }

pattern ExNumPat :: (Eq b) => (Show b) => b -> T Int b
pattern ExNumPat{x} = MkT 42 x

data A = A | B deriving (Show, Eq)

f3 :: T Int A
f3 = (MkT 42 A) { x = B }

f5 :: T Int A
f5 = (ExNumPat A) { x = B }


f4 = (MkG 42 True) { y = False }
