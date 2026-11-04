{-# LANGUAGE LinearTypes #-}
module T28095a where

-- CSE must find that f and g have identical right-hand sides. The two
-- lambdas differ only in the multiplicity of their binders, and CSE
-- ignores it. See Note [Binders] in GHC.Core.Map.Expr.

data P = MkP Int Int

f :: Int %1 -> Int -> P
f x y = MkP x (y * 3)

g :: Int -> Int -> P
g x y = MkP x (y * 3)
