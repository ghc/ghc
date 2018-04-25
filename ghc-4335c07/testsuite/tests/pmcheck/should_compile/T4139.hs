{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module T4139 where

data F a where
  FInt :: F Int
  FBool :: F Bool

class Baz a where
  baz :: F a -> G a
instance Baz Int where
  baz _ = GInt
instance Baz Bool where
  baz _ = GBool

data G a where
  GInt :: G Int
  GBool :: G Bool

bar :: Baz a => F a -> ()
bar a@(FInt) =
  case baz a of
    GInt -> ()
    -- GBool -> ()
bar _ = ()


