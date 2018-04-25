--
-- Stack based Prolog inference engine
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
module Engine(prove) where

import PrologData
import Subst

--- Calculation of solutions:

-- the stack based engine maintains a stack of triples (s,goal,alts)
-- corresponding to backtrack points, where s is the substitution at that
-- point, goal is the outstanding goal and alts is a list of possible ways
-- of extending the current proof to find a solution.  Each member of alts
-- is a pair (tp,u) where tp is a new subgoal that must be proved and u is
-- a unifying substitution that must be combined with the substitution s.
--
-- the list of relevant clauses at each step in the execution is produced
-- by attempting to unify the head of the current goal with a suitably
-- renamed clause from the database.

type Stack = [ (Subst, [Term], [Alt]) ]
type Alt   = ([Term], Subst)

alts       :: Database -> Int -> Term -> [Alt]
alts db n g = [ (tp,u) | (tm:==tp) <- renClauses db n g, u <- unify g tm ]
      
-- The use of a stack enables backtracking to be described explicitly,
-- in the following `state-based' definition of prove:

prove      :: Database -> [Term] -> [Subst]
prove db gl = solve 1 nullSubst gl []
 where
   solve :: Int -> Subst -> [Term] -> Stack -> [Subst]
   solve n s []     ow          = s : backtrack n ow
   solve n s (g:gs) ow
                    | g==theCut = solve n s gs (cut ow)
                    | otherwise = choose n s gs (alts db n (apply s g)) ow

   choose :: Int -> Subst -> [Term] -> [Alt] -> Stack -> [Subst]
   choose n s gs []          ow = backtrack n ow
   choose n s gs ((tp,u):rs) ow = solve (n+1) (u@@s) (tp++gs) ((s,gs,rs):ow)

   backtrack                   :: Int -> Stack -> [Subst]
   backtrack n []               = []
   backtrack n ((s,gs,rs):ow)   = choose (n-1) s gs rs ow


--- Special definitions for the cut predicate:

theCut    :: Term
theCut     = Struct "!" []

cut                  :: Stack -> Stack
cut (top:(s,gl,_):ss) = top:(s,gl,[]):ss
cut ss                = ss

--- End of Engine.hs
