{-# OPTIONS_GHC -g -O #-}
{-# OPTIONS
  -fno-strictness
  -fno-case-merge
  -fno-call-arity
  -fno-case-folding
  -fno-cse
  -fno-do-eta-reduction
  -fno-do-lambda-eta-expansion
  -fno-float-in
  -ffull-laziness
  -fno-enable-rewrite-rules
#-}
-- This used to fail with:
--
-- *** Core Lint errors : in result of Simplifier ***
-- <no location info>: warning:
--     [RHS of str_sZr :: Addr#]
--     Recursive or top-level binder has strict demand info: str_sZr
--     Binder's demand info: <L,U>
module T14779a where

mkConstr :: String -> String
mkConstr str = r
  where
    r = idx `seq` str
    idx = eqS r str `seq` [r]

conMkFixed :: String
conMkFixed = mkConstr "MkFixed"

eqS :: String -> String -> Bool
eqS [] [] = True
eqS _ _ = False
