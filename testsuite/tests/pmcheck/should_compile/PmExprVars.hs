module PmExprVars where

-- | Demonstrates why we can't lower constructors as flexible meta variables.
-- If we did, we'd get a warning that cases 1 and 2 were redundant, implying
-- cases 0 and 3 are not. Arguably this might be better than not warning at
-- all, but it's very surprising having to supply the third case but not the
-- first two cases. And it's probably buggy somwhere else. Delete this when we
-- detect that all but the last case is redundant.
consAreRigid :: Int
consAreRigid = case False of
  False -> case False of
    False -> 0
    True -> 1
  True -> case False of
    False -> 2
    True -> 3

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
