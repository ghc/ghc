{-# LANGUAGE GADTs #-}

module T15009 where

-- T2 is an ordinary H98 data type,
-- and f2 should typecheck with no problem
data T2 a where
  MkT2 :: a -> T2 a

f2 (MkT2 x) = not x

-- T1 is a GADT, but the equality is really just a 'let'
-- so f1 should also typecheck no problem
data T1 a where
  MkT1 :: b ~ a => b -> T1 a

f1 (MkT1 x) = not x



