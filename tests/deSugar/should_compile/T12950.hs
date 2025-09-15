{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O #-}

module T12950 where

class C a where
  type TF a; m :: a -> TF a

instance C Int where
  type TF Int = String; m = show

overloaded :: C a => a -> (a,TF a)
{-# INLINABLE overloaded #-}
overloaded a = (a,m a)

{-# SPECIALIZE overloaded :: Int -> (Int,TF Int) #-}
