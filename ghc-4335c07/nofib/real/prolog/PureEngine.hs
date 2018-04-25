--
-- The Pure Prolog inference engine (using explicit prooftrees)
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
module Engine(prove) where

import PrologData
import Subst

--- Calculation of solutions:

-- Each node in a prooftree corresponds to:
-- either: a solution to the current goal, represented by Done s, where s
--         is the required substitution
-- or:     a choice between a number of subtrees ts, each corresponding to a
--         proof of a subgoal of the current goal, represented by Choice ts.
--         The proof tree corresponding to an unsolvable goal is Choice [] 

data Prooftree = Done Subst  |  Choice [Prooftree]

-- prooftree uses the rules of Prolog to construct a suitable proof tree for
--           a specified goal
prooftree   :: Database -> Int -> Subst -> [Term] -> Prooftree
prooftree db = pt
 where pt           :: Int -> Subst -> [Term] -> Prooftree
       pt n s []     = Done s
       pt n s (g:gs) = Choice [ pt (n+1) (u@@s) (map (apply u) (tp++gs))
                              | (tm:==tp)<-renClauses db n g, u<-unify g tm ]

-- search performs a depth-first search of a proof tree, producing the list
--        of solution substitutions as they are encountered.
search              :: Prooftree -> [Subst]
search (Done s)      = [s]
search (Choice pts)  = [ s | pt <- pts, s <- search pt ]

prove    :: Database -> [Term] -> [Subst]
prove db  = search . prooftree db 1 nullSubst

--- End of PureEngine.hs
