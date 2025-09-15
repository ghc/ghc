module PmExprVars where

data D a = A | B

class C a where
  d :: D a

instance C Int where
  d = A

instance C Bool where
  d = B

-- | Demonstrates why we can't translate arbitrary 'HsVar'
-- occurrences as 'PmExprVar's (i.e., meta variables). If we did, the following
-- would warn that the cases 1 and 2 were redundant, which is clearly wrong
-- (case 1 is the only match). This is an artifact of translating from the
-- non-desugared 'HsExpr'. If we were to implement 'hsExprToPmExpr' in terms of
-- 'CoreExpr', we'd see the dictionary application and all would be well. The
-- solution is to look into the outer 'HsWrap' and determine whether we apply
-- or abstract over any evidence variables.
dictVarsAreTypeIndexed:: Int
dictVarsAreTypeIndexed = case d :: D Int of
  A -> case d :: D Bool of
    A -> 0
    B -> 1
  B -> case d :: D Bool of
    A -> 2
    B -> 3
