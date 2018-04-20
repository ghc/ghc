{-# LANGUAGE TypeFamilies #-}

module T14033 where

newtype Zero = Zero
newtype Succ a = Succ a

type family Add n m :: * where
     Add Zero m = m
     Add (Succ n) m = Succ (Add n m)
