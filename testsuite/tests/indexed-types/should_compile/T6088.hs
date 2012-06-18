{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}  
{-# LANGUAGE EmptyDataDecls #-}

module T6088 where

class C a

newtype A n = A Int

type family Pos n
data True

instance (Pos n ~ True) => C (A n)

newtype B n = B (A n) deriving (C)
  -- This should work, giving
  -- instance (Pos n ~ True) => C (B n)
