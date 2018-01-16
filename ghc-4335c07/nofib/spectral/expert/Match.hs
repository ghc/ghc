{------------------------------------------------------------------------------
                                   MATCHING

This module provides a `match' function which implements the famous unification
algorithm. It takes a pair of `patterns', ie structures with variables in them,
matches them against each other, and extracts information about the values
which variables must have in order for the match to be successful. For example,
if `X has stripes' is matched against `Y has Z' then the match is successful,
and the information X=Y and Z=stripes is gleaned. The information about
variables is stored using the `Environment' type; a table which maps variable
names to phrases. The exports from this module are the `Environment' type and
the `match' function.
------------------------------------------------------------------------------}

module Match where
import Result
import Table
import Knowledge

-- The `Environment' type stores information about variables. The `subst'
-- function is used whenever a phrase contains variables about which
-- information may be known. The variables in the phrase are (recursively)
-- substituted by their values in the given environment.

type Environment = Table String Phrase

subst env (Term x ps) = Term x [subst env p | p<-ps]
subst env (Var x) =
   if fails lookup then (Var x) else subst env (answer lookup) where
   lookup = find env x

-- The `match' function substitutes any known information about the variables
-- in its argument patterns before comparing them with `compear'.  The
-- `matchList' function deals with a list of pairs of patterns which need to be
-- matched. The information gleaned from each pair is used in matching the
-- next, and the final result contains all the information.

match env p1 p2 = compear env (subst env p1) (subst env p2)

matchList env [] = success env
matchList env ((p1,p2):pairs) =
   if fails res then res else matchList (answer res) pairs where
   res = match env p1 p2

-- The `compear' function is the heart of the algorithm. It compares two
-- phrases and updates the given environment accordingly. For normal terms, it
-- compares the joining words. If these are equal, then it compares
-- corresponding pairs of subphrases. If one or other of the phrases is a
-- variable, then it makes a suitable entry in the environment.

compear env (Term x1 ps1) (Term x2 ps2)
   | x1 == x2  = matchList env (zip ps1 ps2)
   | otherwise = failure "no match"
compear env (Var x) (Var y)
   | x /= y    = success (update env x (Var y))
   | otherwise = success env
compear env (Var x) p
   | not (occurs (Var x) p)  =  success (update env x p)
   | otherwise = failure "occurs check failed"
compear env p (Var x) =
   compear env (Var x) p

-- The `occurs' check makes sure that a variable does not itself occur in the
-- phrase which it is being set equal to. For example, if X were being set
-- equal to `the animal eats X', then there would be no solution for X,
-- indicating some sort of logical error.

occurs v (Term x ps) = or [occurs v p | p<-ps]
occurs (Var y) (Var x) = y == x
occurs p (Var x) = False
