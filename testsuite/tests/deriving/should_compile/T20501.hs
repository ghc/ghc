{-# LANGUAGE DatatypeContexts #-}
module T20501 where

data Show a => T1 a = MkT1
  deriving Eq

-- Should compile even with Eq constraints
eqT1 :: T1 a -> T1 a -> Bool
eqT1 MkT1 MkT1 = True

data Show a => T2 a = MkT2
deriving instance Eq (T2 a)
