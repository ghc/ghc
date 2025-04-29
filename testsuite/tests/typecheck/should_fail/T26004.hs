{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module T26004 where

data T a where
  T1 :: T Bool
  T2 :: T a

-- This funcion should be rejected:
-- we should not infer a non-principal type for `f`
f w e = case e of
  T1 -> let y = not w in False
  T2 -> True
