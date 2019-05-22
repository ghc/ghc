{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

The term equality oracle. The main export of the module is function `tmOracle'.
-}

{-# LANGUAGE CPP, MultiWayIf #-}

module TmOracle (

        -- re-exported from PmExpr
        PmExpr(..), PmLit(..), PmAltCon(..), SimpleEq, ComplexEq, PmVarEnv,
        PmRefutEnv, eqPmLit, prepareRefuts, isNotPmExprOther,
        runPmPprM, lhsExprToPmExpr, hsExprToPmExpr, pprPmExprWithParens,

        -- the term oracle
        tmOracle, TmState, initialTmState, solveOneEq, extendSubst, canDiverge,
        addSolveRefutableAltCon, lookupRefutableAltCons,

        -- misc.
        toComplex, exprDeepLookup, pmLitType, flattenPmVarEnv
    ) where

#include "HsVersions.h"

import GhcPrelude

import PmExpr

import Id
import Name
import Type
import HsLit
import TcHsSyn
import MonadUtils
import ListSetOps (assocAlter, insertNoDup)
import Util
import Maybes
import Outputable

import NameEnv

{-
%************************************************************************
%*                                                                      *
                      The term equality oracle
%*                                                                      *
%************************************************************************
-}

-- | The type of substitutions.
type PmVarEnv = NameEnv PmExpr

-- | The environment of the oracle contains
--     1. A Bool (are there any constraints we cannot handle? (PmExprOther)).
--     2. A substitution with solutions we extend with every step and return
--        as a result.
--     3. A 'PmRefutEnv' assigning shapes to variables that immediately lead to
--        a refutation. See Note [Refutable shapes].
type TmOracleEnv = (Bool, PmVarEnv, PmRefutEnv)

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Name -> TmState -> Bool
canDiverge x (standby, (_unhandled, env, _refuts))
  -- If the variable seems not evaluated, there is a possibility for
  -- constraint x ~ BOT to be satisfiable. That's the case when we haven't found
  -- a solution (i.e. some equivalent literal or constructor) for it yet.
  | (_, PmExprVar y) <- varDeepLookup env x -- seems not forced
  -- If it is involved (directly or indirectly) in any equality in the
  -- worklist, we can assume that it is already indirectly evaluated,
  -- as a side-effect of equality checking. If not, then we can assume
  -- that the constraint is satisfiable.
  = not $ any (isForcedByEq x) standby || any (isForcedByEq y) standby
  -- Variable x is already in WHNF so the constraint is non-satisfiable
  | otherwise = False

  where
    isForcedByEq :: Name -> ComplexEq -> Bool
    isForcedByEq y (e1, e2) = varIn y e1 || varIn y e2

-- | Check whether a variable is in the free variables of an expression
varIn :: Name -> PmExpr -> Bool
varIn x e = case e of
  PmExprVar y    -> x == y
  PmExprCon _ es -> any (x `varIn`) es
  PmExprLit _    -> False
  PmExprOther _  -> False

-- | Check whether the equality @x ~ e@ leads to a refutation. Make sure that
-- @x@ and @e@ are completely substituted before!
isRefutable :: Name -> PmExpr -> PmRefutEnv -> Bool
isRefutable x e env
  = fromMaybe False $ elem <$> exprToAlt e <*> lookup x env

-- | Flatten the DAG (Could be improved in terms of performance.).
flattenPmVarEnv :: PmVarEnv -> PmVarEnv
flattenPmVarEnv env = mapNameEnv (exprDeepLookup env) env

-- | The state of the term oracle (includes complex constraints that cannot
-- progress unless we get more information).
type TmState = ([ComplexEq], TmOracleEnv)

-- | Initial state of the oracle.
initialTmState :: TmState
initialTmState = ([], (False, emptyNameEnv, []))

-- | Solve a complex equality (top-level).
solveOneEq :: TmState -> ComplexEq -> Maybe TmState
solveOneEq solver_env@(_,(_,env,_)) complex
  = solveComplexEq solver_env -- do the actual *merging* with existing state
  $ applySubstComplexEq env complex -- replace everything we already know

exprToAlt :: PmExpr -> Maybe PmAltCon
-- Note how this deliberately chooses bogus argument types for PmAltConLike.
-- This is only safe for doing lookup in a 'PmRefutEnv'!
exprToAlt (PmExprCon cl _) = Just (PmAltConLike cl [])
exprToAlt (PmExprLit l)    = Just (PmAltLit l)
exprToAlt _                = Nothing

-- | Record that a particular 'Id' can't take the shape of a 'PmAltCon' in the
-- 'TmState' and refute if that leads to a contradiction.
addSolveRefutableAltCon :: TmState -> Id -> PmAltCon -> Maybe TmState
addSolveRefutableAltCon original@(standby, (unhandled, env, refuts)) x nalt
  = case exprToAlt e of
      Nothing -> Just extended         -- Not solved yet
      Just alt                         -- We have a solution
        | alt == nalt -> Nothing       -- ... which is contradictory
        | otherwise   -> Just original -- ... which is compatible, rendering the
                                       --     refutation redundant
  where
    (y, e) = varDeepLookup env (idName x)
    extended = (standby, (unhandled, env, refuts'))
    refuts' = assocAlter (Just . (insertNoDup nalt) . fromMaybe []) y refuts

-- | Return all 'PmAltCon' shapes that are impossible for 'Id' to take, i.e.
-- would immediately lead to a refutation by the term oracle.
lookupRefutableAltCons :: Id -> TmState -> [PmAltCon]
lookupRefutableAltCons x (_, (_, _, refuts))
  = fromMaybe [] (lookup (idName x) refuts)

-- | Solve a complex equality.
-- Nothing => definitely unsatisfiable
-- Just tms => I have added the complex equality and added
--             it to the tmstate; the result may or may not be
--             satisfiable
solveComplexEq :: TmState -> ComplexEq -> Maybe TmState
solveComplexEq solver_state@(standby, (_unhandled, env, refuts)) eq@(e1, e2) = case eq of
  -- We cannot do a thing about these cases
  (PmExprOther _,_)            -> Just (standby, (True, env, refuts))
  (_,PmExprOther _)            -> Just (standby, (True, env, refuts))

  (PmExprLit l1, PmExprLit l2) -> case eqPmLit l1 l2 of
    -- See Note [Undecidable Equality for Overloaded Literals]
    True  -> Just solver_state
    False -> Nothing

  (PmExprCon c1 ts1, PmExprCon c2 ts2)
    | c1 == c2  -> foldlM solveComplexEq solver_state (zip ts1 ts2)
    | otherwise -> Nothing

  (PmExprVar x, PmExprVar y)
    | x == y    -> Just solver_state
    | otherwise -> extendSubstAndSolve x e2 solver_state

  (PmExprVar x, _) -> extendSubstAndSolve x e2 solver_state
  (_, PmExprVar x) -> extendSubstAndSolve x e1 solver_state

  _ -> WARN( True, text "solveComplexEq: Catch all" <+> ppr eq )
       Just (standby, (True, env, refuts)) -- I HATE CATCH-ALLS

-- | Extend the substitution and solve the (possibly updated) constraints.
extendSubstAndSolve :: Name -> PmExpr -> TmState -> Maybe TmState
extendSubstAndSolve x e (standby, (unhandled, env, refuts))
  | isRefutable x e refuts
  = Nothing
  | otherwise
  = foldlM solveComplexEq new_incr_state changed
  where
    -- Apply the substitution to the worklist and partition them to the ones
    -- that had some progress and the rest. Then, recurse over the ones that
    -- had some progress. Careful about performance:
    -- See Note [Representation of Term Equalities] in deSugar/Check.hs
    (changed, unchanged) = partitionWith (substComplexEq x e) standby
    new_incr_state       = (unchanged, (unhandled, extendNameEnv env x e, refuts))

-- | When we know that a variable is fresh, we do not actually have to
-- check whether anything changes, we know that nothing does. Hence,
-- `extendSubst` simply extends the substitution, unlike what
-- `extendSubstAndSolve` does.
extendSubst :: Id -> PmExpr -> TmState -> TmState
extendSubst y e (standby, (unhandled, env, refuts))
  | isNotPmExprOther simpl_e
  = (standby, (unhandled, extendNameEnv env x simpl_e, refuts))
  | otherwise = (standby, (True, env, refuts))
  where
    x = idName y
    simpl_e = exprDeepLookup env e

-- | Apply an (un-flattened) substitution to a simple equality.
applySubstComplexEq :: PmVarEnv -> ComplexEq -> ComplexEq
applySubstComplexEq env (e1,e2) = (exprDeepLookup env e1, exprDeepLookup env e2)

-- | Apply an (un-flattened) substitution to a variable and return its
-- representative in the triangular substitution @env@ and the completely
-- substituted expression. The latter may just be the representative wrapped
-- with 'PmExprVar' if we haven't found a solution for it yet.
varDeepLookup :: PmVarEnv -> Name -> (Name, PmExpr)
varDeepLookup env x = case lookupNameEnv env x of
  Just (PmExprVar y) -> varDeepLookup env y
  Just e             -> (x, exprDeepLookup env e) -- go deeper
  Nothing            -> (x, PmExprVar x)          -- terminal
{-# INLINE varDeepLookup #-}

-- | Apply an (un-flattened) substitution to an expression.
exprDeepLookup :: PmVarEnv -> PmExpr -> PmExpr
exprDeepLookup env (PmExprVar x)    = snd (varDeepLookup env x)
exprDeepLookup env (PmExprCon c es) = PmExprCon c (map (exprDeepLookup env) es)
exprDeepLookup _   other_expr       = other_expr -- PmExprLit, PmExprOther

-- | External interface to the term oracle.
tmOracle :: TmState -> [ComplexEq] -> Maybe TmState
tmOracle tm_state eqs = foldlM solveOneEq tm_state eqs

-- | Type of a PmLit
pmLitType :: PmLit -> Type -- should be in PmExpr but gives cyclic imports :(
pmLitType (PmSLit   lit) = hsLitType   lit
pmLitType (PmOLit _ lit) = overLitType lit

{- Note [Refutable shapes]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a pattern match like

    foo x
      | 0 <- x = 42
      | 0 <- x = 43
      | 1 <- x = 44
      | otherwise = 45

When we handle the first pattern guard in Check, it will be desugared to a match
of the form

    0 x
    x y

Where the first line is the pattern vector and the second line is a value vector
abstraction. In LitVar, this will split the value vector abstraction for `x`
into a positive `PmLit 0` and a negative `PmLit x [0]` value abstraction. While
the former is immediately matched against the pattern vector, the latter (vector
value abstraction `~[0] y`) is completely uncovered by the clause.

`pmcheck` proceeds by *discarding* the head of the value vector abstraction to
accomodate for the desugaring of the guard. But this also discards the valuable
information that `x` certainly is not the literal 0! Consequently, we wouldn't
be able to report the second clause as redundant.

That's a typical example of why we need the term oracle, and in this specific
case, the ability to encode that `x` certainly is not the literal 0. Equipped
with that knowledge, the term oracle can immediately refute the constraint
`x ~ 0` generated by the second clause and report the clause as redundant.
After the third clause, the set of such *refutable* literals is again extended
to `[0, 1]`.

In general, we want to store a set of refutable shapes (`PmAltCon`) for each
variable. That's the purpose of the `PmRefutEnv` in PmExpr. This extends to
`ConLike`s, where all value arguments are universally quantified implicitly.
So, if the `PmRefutEnv` contains an entry for `x` with `Just [Bool]`, then this
corresponds to the fact that `forall y. x ≁ Just @Bool y`.

`addSolveRefutableAltCon` will add such a refutable mapping to the `PmRefutEnv`
in the term oracles state and check if causes any immediate contradiction.
Whenever we record a solution in the substitution via `extendSubstAndSolve`, the
refutable environment is checked for any matching refutable `PmAltCon`.

Note that `PmAltConLike` carries a list of type arguments. This purely for the
purpose of being able to reconstruct all other constructors of the matching
group the `ConLike` is part of through calling `allCompleteMatches` in Check.
-}