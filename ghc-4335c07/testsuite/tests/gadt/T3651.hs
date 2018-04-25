{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module T3651 where

data Z a where
  U :: Z ()
  B :: Z Bool

unsafe1 :: Z a -> Z a -> a
unsafe1 B U = ()

unsafe2 :: a ~ b => Z b -> Z a -> a
unsafe2 B U = ()

unsafe3 :: a ~ b => Z a -> Z b -> a
unsafe3 B U = True
