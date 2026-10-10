{-# LANGUAGE GADTs, ViewPatterns, RankNTypes #-}
module SimpleOptInScope where

-- Matching MkG gives co :: a ~# Int.  The pattern-match checker runs
-- simpleOptExpr on the view-pattern expression, which mentions co but not a;
-- the in-scope set seeded from exprFreeVars must contain a (deep free
-- variables, see Note [The substitution invariant] in GHC.Core.TyCo.Subst),
-- otherwise checkValidSubst fails when substituting [b :-> Int] through the
-- cast.  Needs -fno-opt-coercion so that substCo is actually called, and
-- only fails in a build with assertions.

data G a where
  MkG :: G Int

f :: G a -> a -> Int -> Int
f MkG v (((\x -> (v + 1, x)) :: forall b. b -> (Int, b)) -> (y, _)) = y
