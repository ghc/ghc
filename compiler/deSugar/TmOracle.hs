{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

The term equality oracle. The main export of the module is function `tmOracle'.
-}

{-# LANGUAGE CPP, MultiWayIf #-}

module TmOracle (

        -- re-exported from PmExpr
        PmExpr(..), PmLit(..), SimpleEq, ComplexEq, PmVarEnv, falsePmExpr,
        eqPmLit, filterComplex, isNotPmExprOther, runPmPprM, lhsExprToPmExpr,
        hsExprToPmExpr, pprPmExprWithParens,

        -- the term oracle
        tmOracle, TmState, initialTmState, solveOneEq, extendSubst, canDiverge,

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
import Util

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
--     2. A substitution we extend with every step and return as a result.
type TmOracleEnv = (Bool, PmVarEnv)

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Name -> TmState -> Bool
canDiverge x (standby, (_unhandled, env))
  -- If the variable seems not evaluated, there is a possibility for
  -- constraint x ~ BOT to be satisfiable.
  | PmExprVar y <- varDeepLookup env x -- seems not forced
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
  PmExprEq e1 e2 -> (x `varIn` e1) || (x `varIn` e2)
  PmExprOther _  -> False

-- | Flatten the DAG (Could be improved in terms of performance.).
flattenPmVarEnv :: PmVarEnv -> PmVarEnv
flattenPmVarEnv env = mapNameEnv (exprDeepLookup env) env

-- | The state of the term oracle (includes complex constraints that cannot
-- progress unless we get more information).
type TmState = ([ComplexEq], TmOracleEnv)

-- | Initial state of the oracle.
initialTmState :: TmState
initialTmState = ([], (False, emptyNameEnv))

-- | Solve a complex equality (top-level).
solveOneEq :: TmState -> ComplexEq -> Maybe TmState
solveOneEq solver_env@(_,(_,env)) complex
  = solveComplexEq solver_env -- do the actual *merging* with existing state
  $ simplifyComplexEq               -- simplify as much as you can
  $ applySubstComplexEq env complex -- replace everything we already know

-- | Solve a complex equality.
-- Nothing => definitely unsatisfiable
-- Just tms => I have added the complex equality and added
--             it to the tmstate; the result may or may not be
--             satisfiable
solveComplexEq :: TmState -> ComplexEq -> Maybe TmState
solveComplexEq solver_state@(standby, (unhandled, env)) eq@(e1, e2) = case eq of
  -- We cannot do a thing about these cases
  (PmExprOther _,_)            -> Just (standby, (True, env))
  (_,PmExprOther _)            -> Just (standby, (True, env))

  (PmExprLit l1, PmExprLit l2) -> case eqPmLit l1 l2 of
    -- See Note [Undecidable Equality for Overloaded Literals]
    True  -> Just solver_state
    False -> Nothing

  (PmExprCon c1 ts1, PmExprCon c2 ts2)
    | c1 == c2  -> foldlM solveComplexEq solver_state (zip ts1 ts2)
    | otherwise -> Nothing
  (PmExprCon _ [], PmExprEq t1 t2)
    | isTruePmExpr e1  -> solveComplexEq solver_state (t1, t2)
    | isFalsePmExpr e1 -> Just (eq:standby, (unhandled, env))
  (PmExprEq t1 t2, PmExprCon _ [])
    | isTruePmExpr e2   -> solveComplexEq solver_state (t1, t2)
    | isFalsePmExpr e2  -> Just (eq:standby, (unhandled, env))

  (PmExprVar x, PmExprVar y)
    | x == y    -> Just solver_state
    | otherwise -> extendSubstAndSolve x e2 solver_state

  (PmExprVar x, _) -> extendSubstAndSolve x e2 solver_state
  (_, PmExprVar x) -> extendSubstAndSolve x e1 solver_state

  (PmExprEq _ _, PmExprEq _ _) -> Just (eq:standby, (unhandled, env))

  _ -> Just (standby, (True, env)) -- I HATE CATCH-ALLS

-- | Extend the substitution and solve the (possibly updated) constraints.
extendSubstAndSolve :: Name -> PmExpr -> TmState -> Maybe TmState
extendSubstAndSolve x e (standby, (unhandled, env))
  = foldlM solveComplexEq new_incr_state (map simplifyComplexEq changed)
  where
    -- Apply the substitution to the worklist and partition them to the ones
    -- that had some progress and the rest. Then, recurse over the ones that
    -- had some progress. Careful about performance:
    -- See Note [Representation of Term Equalities] in deSugar/Check.hs
    (changed, unchanged) = partitionWith (substComplexEq x e) standby
    new_incr_state       = (unchanged, (unhandled, extendNameEnv env x e))

-- | When we know that a variable is fresh, we do not actually have to
-- check whether anything changes, we know that nothing does. Hence,
-- `extendSubst` simply extends the substitution, unlike what
-- `extendSubstAndSolve` does.
extendSubst :: Id -> PmExpr -> TmState -> TmState
extendSubst y e (standby, (unhandled, env))
  | isNotPmExprOther simpl_e
  = (standby, (unhandled, extendNameEnv env x simpl_e))
  | otherwise = (standby, (True, env))
  where
    x = idName y
    simpl_e = fst $ simplifyPmExpr $ exprDeepLookup env e

-- | Simplify a complex equality.
simplifyComplexEq :: ComplexEq -> ComplexEq
simplifyComplexEq (e1, e2) = (fst $ simplifyPmExpr e1, fst $ simplifyPmExpr e2)

-- | Simplify an expression. The boolean indicates if there has been any
-- simplification or if the operation was a no-op.
simplifyPmExpr :: PmExpr -> (PmExpr, Bool)
-- See Note [Deep equalities]
simplifyPmExpr e = case e of
  PmExprCon c ts -> case mapAndUnzip simplifyPmExpr ts of
                      (ts', bs) -> (PmExprCon c ts', or bs)
  PmExprEq t1 t2 -> simplifyEqExpr t1 t2
  _other_expr    -> (e, False) -- the others are terminals

-- | Simplify an equality expression. The equality is given in parts.
simplifyEqExpr :: PmExpr -> PmExpr -> (PmExpr, Bool)
-- See Note [Deep equalities]
simplifyEqExpr e1 e2 = case (e1, e2) of
  -- Varables
  (PmExprVar x, PmExprVar y)
    | x == y -> (truePmExpr, True)

  -- Literals
  (PmExprLit l1, PmExprLit l2) -> case eqPmLit l1 l2 of
    -- See Note [Undecidable Equality for Overloaded Literals]
    True  -> (truePmExpr,  True)
    False -> (falsePmExpr, True)

  -- Can potentially be simplified
  (PmExprEq {}, _) -> case (simplifyPmExpr e1, simplifyPmExpr e2) of
    ((e1', True ), (e2', _    )) -> simplifyEqExpr e1' e2'
    ((e1', _    ), (e2', True )) -> simplifyEqExpr e1' e2'
    ((e1', False), (e2', False)) -> (PmExprEq e1' e2', False) -- cannot progress
  (_, PmExprEq {}) -> case (simplifyPmExpr e1, simplifyPmExpr e2) of
    ((e1', True ), (e2', _    )) -> simplifyEqExpr e1' e2'
    ((e1', _    ), (e2', True )) -> simplifyEqExpr e1' e2'
    ((e1', False), (e2', False)) -> (PmExprEq e1' e2', False) -- cannot progress

  -- Constructors
  (PmExprCon c1 ts1, PmExprCon c2 ts2)
    | c1 == c2 ->
        let (ts1', bs1) = mapAndUnzip simplifyPmExpr ts1
            (ts2', bs2) = mapAndUnzip simplifyPmExpr ts2
            (tss, _bss) = zipWithAndUnzip simplifyEqExpr ts1' ts2'
            worst_case  = PmExprEq (PmExprCon c1 ts1') (PmExprCon c2 ts2')
        in  if | not (or bs1 || or bs2) -> (worst_case, False) -- no progress
               | all isTruePmExpr  tss  -> (truePmExpr, True)
               | any isFalsePmExpr tss  -> (falsePmExpr, True)
               | otherwise              -> (worst_case, False)
    | otherwise -> (falsePmExpr, True)

  -- We cannot do anything about the rest..
  _other_equality -> (original, False)

  where
    original = PmExprEq e1 e2 -- reconstruct equality

-- | Apply an (un-flattened) substitution to a simple equality.
applySubstComplexEq :: PmVarEnv -> ComplexEq -> ComplexEq
applySubstComplexEq env (e1,e2) = (exprDeepLookup env e1, exprDeepLookup env e2)

-- | Apply an (un-flattened) substitution to a variable.
varDeepLookup :: PmVarEnv -> Name -> PmExpr
varDeepLookup env x
  | Just e <- lookupNameEnv env x = exprDeepLookup env e -- go deeper
  | otherwise                  = PmExprVar x          -- terminal
{-# INLINE varDeepLookup #-}

-- | Apply an (un-flattened) substitution to an expression.
exprDeepLookup :: PmVarEnv -> PmExpr -> PmExpr
exprDeepLookup env (PmExprVar x)    = varDeepLookup env x
exprDeepLookup env (PmExprCon c es) = PmExprCon c (map (exprDeepLookup env) es)
exprDeepLookup env (PmExprEq e1 e2) = PmExprEq (exprDeepLookup env e1)
                                               (exprDeepLookup env e2)
exprDeepLookup _   other_expr       = other_expr -- PmExprLit, PmExprOther

-- | External interface to the term oracle.
tmOracle :: TmState -> [ComplexEq] -> Maybe TmState
tmOracle tm_state eqs = foldlM solveOneEq tm_state eqs

-- | Type of a PmLit
pmLitType :: PmLit -> Type -- should be in PmExpr but gives cyclic imports :(
pmLitType (PmSLit   lit) = hsLitType   lit
pmLitType (PmOLit _ lit) = overLitType lit

{- Note [Deep equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~
Solving nested equalities is the most difficult part. The general strategy
is the following:

  * Equalities of the form (True ~ (e1 ~ e2)) are transformed to just
    (e1 ~ e2) and then treated recursively.

  * Equalities of the form (False ~ (e1 ~ e2)) cannot be analyzed unless
    we know more about the inner equality (e1 ~ e2). That's exactly what
    `simplifyEqExpr' tries to do: It takes e1 and e2 and either returns
    truePmExpr, falsePmExpr or (e1' ~ e2') in case it is uncertain. Note
    that it is not e but rather e', since it may perform some
    simplifications deeper.
-}
