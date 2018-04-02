{-# LANGUAGE GADTs  #-}
module T14978 where

data Equal a b where
  Refl :: Equal a a

data Goof a where
  Goof :: {-# UNPACK #-} !(Equal a Int) -> Goof a


foo :: Goof Int
foo = Goof Refl
