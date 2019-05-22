module PmExprVars where

-- | Demonstrates why we can't lower constructors as flexible
-- meta variables. If we did, we'd get a warning that the
-- first two cases were redundant, implying the latter two are
-- not. Arguably this might be better than not warning at all,
-- but it's very surprising having to supply the third case
-- but not the first two cases. And it's probably buggy
-- somwhere else. Delete this when we detect that all but the
-- last case is redundant.
consAreRigid :: Int
consAreRigid = case (False, False) of
  (True, False) -> 0
  (False, True) -> 1
  (True, True) -> 2
  (False, False) -> 3

data D a = A | B

class C a where
  d :: D a

instance C Int where
  d = A

instance C Bool where
  d = B

-- | Demonstrates why we can't translate arbitrary 'HsVar'
-- occurrences as 'PmExprVar's (i.e., meta variables). If we
-- did, the following would warn that the first two cases were
-- redundant, which is clearly wrong (the first case is the
-- only match). This is an artifact of translating from the
-- non-desugared 'HsExpr'. If we were to implement
-- 'hsExprToPmExpr' in terms of 'CoreExpr', we'd see the type
-- application and all would be well.
dictVarsAreTypeIndexed:: Int
dictVarsAreTypeIndexed = case (d :: D Int, d :: D Bool) of
  (A, B) -> 0
  (B, A) -> 1
  (A, A) -> 2
  (B, B) -> 3
