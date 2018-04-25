--
-- Substitutions and Unification of Prolog Terms
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
module Subst(Subst, nullSubst, (->>), (@@), apply, unify) where

import PrologData

infixr 3 @@
infix  4 ->>

--- Substitutions:

type Subst = Id -> Term

-- substitutions are represented by functions mapping identifiers to terms.
--
-- apply s   extends the substitution s to a function mapping terms to terms
-- nullSubst is the empty substitution which maps every identifier to the
--           same identifier (as a term).
-- i ->> t   is the substitution which maps the identifier i to the term t,
--           but otherwise behaves like nullSubst.
-- s1 @@ s2  is the composition of substitutions s1 and s2
--           N.B.  apply is a monoid homomorphism from (Subst,nullSubst,(@@))
--           to (Term -> Term, id, (.)) in the sense that:
--                  apply (s1 @@ s2) = apply s1 . apply s2
--                    s @@ nullSubst = s = nullSubst @@ s

apply                   :: Subst -> Term -> Term
apply s (Var i)          = s i
apply s (Struct a ts)    = Struct a (map (apply s) ts)

nullSubst               :: Subst
nullSubst i              = Var i

(->>)                   :: Id -> Term -> Subst
(->>) i t j | j==i       = t
            | otherwise  = Var j

(@@)                    :: Subst -> Subst -> Subst
s1 @@ s2                 = apply s1 . s2 

--- Unification:

-- unify t1 t2 returns a list containing a single substitution s which is
--             the most general unifier of terms t1 t2.  If no unifier
--             exists, the list returned is empty.

unify :: Term -> Term -> [Subst]
unify (Var x)       (Var y)       = if x==y then [nullSubst] else [x->>Var y]
unify (Var x)       t2            = [ x ->> t2 | not (x `elem` varsIn t2) ]
unify t1            (Var y)       = [ y ->> t1 | not (y `elem` varsIn t1) ]
unify (Struct a ts) (Struct b ss) = [ u | a==b, u<-listUnify ts ss ]

listUnify :: [Term] -> [Term] -> [Subst]
listUnify []     []     = [nullSubst]
listUnify []     (r:rs) = []
listUnify (t:ts) []     = []
listUnify (t:ts) (r:rs) = [ u2 @@ u1 | u1<-unify t r,
                                       u2<-listUnify (map (apply u1) ts)
                                                     (map (apply u1) rs) ]

--- End of Subst.hs
