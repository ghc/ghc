{-# LANGUAGE GADTs #-}
-- Trac #1999

module ShouldCompile where

data EqTypes a b where
  EqConstr :: EqTypes a b -> EqTypes (s a) (s b)

eqUnConstr :: EqTypes (s a) (s b) -> EqTypes a b
eqUnConstr (EqConstr eq) = eq