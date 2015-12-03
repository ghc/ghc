{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

The term equality oracle. The main export of the module is function `tmOracle'.
-}

{-# LANGUAGE CPP, MultiWayIf #-}

module TmOracle (

        -- re-exported from PmExpr
        PmExpr(..), PmLit(..), SimpleEq, ComplexEq, PmVarEnv, falsePmExpr,
        canDiverge, eqPmLit, filterComplex, isNotPmExprOther, runPmPprM,
        pprPmExprWithParens, lhsExprToPmExpr, hsExprToPmExpr,

        -- the term oracle
        tmOracle, TmState, initialTmState, containsEqLits,

        -- misc.
        exprDeepLookup, pmLitType, flattenPmVarEnv
    ) where

#include "HsVersions.h"

import PmExpr

import Id
import TysWiredIn
import Type
import HsLit
import TcHsSyn
import MonadUtils
import Util

import Data.Maybe (isJust)
import qualified Data.Map as Map

{-
%************************************************************************
%*                                                                      *
                      The term equality oracle
%*                                                                      *
%************************************************************************
-}

-- | The type of substitutions.
type PmVarEnv = Map.Map Id PmExpr

-- | The environment of the oracle contains
--     1. A Bool (are there any constraints we cannot handle? (PmExprOther)).
--     2. A substitution we extend with every step and return as a result.
type TmOracleEnv = (Bool, PmVarEnv)

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Id -> TmState -> Bool
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
    isForcedByEq :: Id -> ComplexEq -> Bool
    isForcedByEq y (e1, e2) = varIn y e1 || varIn y e2

-- | Check whether a variable is in the free variables of an expression
varIn :: Id -> PmExpr -> Bool
varIn x e = case e of
  PmExprVar y    -> x == y
  PmExprCon _ es -> any (x `varIn`) es
  PmExprLit _    -> False
  PmExprEq e1 e2 -> (x `varIn` e1) || (x `varIn` e2)
  PmExprOther _  -> False

-- | Flatten the DAG (Could be improved in terms of performance.).
flattenPmVarEnv :: PmVarEnv -> PmVarEnv
flattenPmVarEnv env = Map.map (exprDeepLookup env) env

-- | The state of the term oracle (includes complex constraints that cannot
-- progress unless we get more information).
type TmState = ([ComplexEq], TmOracleEnv)

-- | Initial state of the oracle.
initialTmState :: TmState
initialTmState = ([], (False, Map.empty))

-- | Solve a simple equality.
solveSimpleEq :: TmState -> SimpleEq -> Maybe TmState
solveSimpleEq solver_env@(_,(_,env)) simple
  = solveComplexEq solver_env -- do the actual *merging* with existing state
  $ simplifyComplexEq             -- simplify as much as you can
  $ applySubstSimpleEq env simple -- replace everything we already know

-- | Solve a complex equality.
solveComplexEq :: TmState -> ComplexEq -> Maybe TmState
solveComplexEq solver_state@(standby, (unhandled, env)) eq@(e1, e2) = case eq of
  -- We cannot do a thing about these cases
  (PmExprOther _,_)            -> Just (standby, (True, env))
  (_,PmExprOther _)            -> Just (standby, (True, env))

  (PmExprLit l1, PmExprLit l2) -> case eqPmLit l1 l2 of
    Just True  -> Just solver_state           -- we are sure: equal
    Just False -> Nothing                     -- we are sure: not equal
    Nothing    -> Just (eq:standby, (unhandled, env)) -- no clue

  (PmExprCon c1 ts1, PmExprCon c2 ts2)
    | c1 == c2  -> foldlM solveComplexEq solver_state (zip ts1 ts2)
    | otherwise -> Nothing
  (PmExprCon c [], PmExprEq t1 t2)
    | c == trueDataCon  -> solveComplexEq solver_state (t1, t2)
    | c == falseDataCon -> Just (eq:standby, (unhandled, env))
  (PmExprEq t1 t2, PmExprCon c [])
    | c == trueDataCon  -> solveComplexEq solver_state (t1, t2)
    | c == falseDataCon -> Just (eq:standby, (unhandled, env))

  (PmExprVar x, PmExprVar y)
    | x == y    -> Just solver_state
    | otherwise -> extendSubstAndSolve x e2 solver_state

  (PmExprVar x, _) -> extendSubstAndSolve x e2 solver_state
  (_, PmExprVar x) -> extendSubstAndSolve x e1 solver_state

  (PmExprEq _ _, PmExprEq _ _) -> Just (eq:standby, (unhandled, env))

  _ -> Just (standby, (True, env)) -- I HATE CATCH-ALLS

-- | Extend the substitution and solve the (possibly updated) constraints.
extendSubstAndSolve :: Id -> PmExpr -> TmState -> Maybe TmState
extendSubstAndSolve x e (standby, (unhandled, env))
  = foldlM solveComplexEq new_incr_state (map simplifyComplexEq changed)
  where
    -- Apply the substitution to the worklist and partition them to the ones
    -- that had some progress and the rest. Then, recurse over the ones that
    -- had some progress.
    (changed, unchanged) = partitionWith (substComplexEq x e) standby
    new_incr_state       = (unchanged, (unhandled, Map.insert x e env))

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
    Just True  -> (truePmExpr,  True)
    Just False -> (falsePmExpr, True)
    Nothing    -> (original,   False)

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

-- | Apply an (un-flattened) substitution on a simple equality.
applySubstSimpleEq :: PmVarEnv -> SimpleEq -> ComplexEq
applySubstSimpleEq env (x, e) = (varDeepLookup env x, exprDeepLookup env e)

-- | Apply an (un-flattened) substitution on a variable.
varDeepLookup :: PmVarEnv -> Id -> PmExpr
varDeepLookup env x
  | Just e <- Map.lookup x env = exprDeepLookup env e -- go deeper
  | otherwise                  = PmExprVar x          -- terminal
{-# INLINE varDeepLookup #-}

-- | Apply an (un-flattened) substitution on an expression.
exprDeepLookup :: PmVarEnv -> PmExpr -> PmExpr
exprDeepLookup env (PmExprVar x)    = varDeepLookup env x
exprDeepLookup env (PmExprCon c es) = PmExprCon c (map (exprDeepLookup env) es)
exprDeepLookup env (PmExprEq e1 e2) = PmExprEq (exprDeepLookup env e1)
                                               (exprDeepLookup env e2)
exprDeepLookup _   other_expr       = other_expr -- PmExprLit, PmExprOther

-- | External interface to the term oracle.
tmOracle :: TmState -> [SimpleEq] -> Maybe TmState
tmOracle tm_state eqs = foldlM solveSimpleEq tm_state eqs

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

Note [Undecidable Equality on Overloaded Literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Overloaded literals do not offer equality like constructors do. A literal @lit@
is used to actually represent @from lit@. For example, we can have the
following:

  instance Num Bool where
    ...
    fromInteger 0 = False
    fromInteger _ = True

  g :: Bool -> Bool
  g 4 = ...          -- clause A
  g 2 = ...          -- clause B

Both clauses A and B will match argunent @True@ so we have an overlap. Yet, we
cannot detect this unless we unfold the @fromInteger@ function. So @eqPmLit@
from deSugar/PmExpr.hs returns @Nothing@ in this case. This complexes things a
lot. Consider the following (similar to test ds022 in deSugar/should_compile):

  f l1 l2 = ...      -- clause A
  f l3 l4 = ...      -- clause B
  f l1 l2 = ...      -- clause C

Assuming that the @from@ function is side-effect-free (and total), clauses C
and D are redundant, independently of the implementation of @from@:

  l1 == l2     ===>    from l1 == from l2
  l1 /= l2     =/=>    from l1 /= from l2

Now consider what we should generate for @f@ (covered and uncovered only):

U0 = { [x y |> {}] }

Clause A: l1 l2
-------------------------------------------------------------------------------
CA = { [l1 l2 |> { x ~ l1, y ~ l2 }] }

UA = { [x  y  |> { False ~ (x ~ l1) }]
     , [l1 y  |> { x ~ l1, False ~ (y ~ l2) }] }

Clause B: l3 l4
-------------------------------------------------------------------------------

CB = { [l3 l4 |> { False ~ (x ~ l1), x ~ l3, y ~ l4}]
         ..reduces to: False ~ (l3 ~ l1), y ~ l4

     , [l1 l4 |> { x ~ l1, False ~ (y ~ l2), l3 ~ z, z ~ l1, y ~ l4}] }
         ..reduces to: x ~ l1, False ~ (l4 ~ l2), l1 ~ l3

UB = { [x  y  |> { False ~ (x ~ l1), False ~ (x ~ l3) }]
     , [l3 y  |> { False ~ (x ~ l1), x ~ l3, False ~ (y ~ l4) }]
         ..reduces to: False ~ (l1 ~ l3), False ~ (y ~ l4)
     , [l1 y  |> { x ~ l1, False ~ (y ~ l2), False ~ (l1 ~ l3) }]
     , [l1 y  |> { x ~ l1, False ~ (y ~ l2), l1 ~ l3, False ~ (y ~ l4) }] }

Clause C: l1 l2
-------------------------------------------------------------------------------

CC = { [l1 l2 |> { False ~ (x ~ l1), False ~ (x ~ l3), x ~ l1, y ~ l2 }]
         ..reduces to: False ~ (l1 ~ l1), False ~ (l1 ~ l3), x ~ l1
                       False ~ True, False ~ (l1 ~ l3), x ~ l1
                       INCONSISTENT
     , [l3 l2 |> { False ~ (l1 ~ l3), False ~ (y ~ l4), l1 ~ l3, y ~ l2 }]
         ..reduces to: False ~ (l1 ~ l3), False ~ (l2 ~ l4), l1 ~ l3
     , [l1 l2 |> { x ~ l1, False ~ (y ~ l2), False ~ (l1 ~ l3), y ~ l2 }]
         ..reduces to: x ~ l1, False ~ (l2 ~ l2), False ~ (l1 ~ l3)
                       x ~ l1, False ~ True, False ~ (l1 ~ l3)
                       INCONSISTENT
     , [l1 l2 |> { x ~ l1,False ~ (y ~ l2),l1 ~ l3,False ~ (y ~ l4),y ~ l2 }] }
         ..reduces to: x ~ l1, False ~ (l2 ~ l2), l1 ~ l3, False ~ (l2 ~ l4)
                       x ~ l1, False ~ True, l1 ~ l3, False ~ (l2 ~ l4)
                       INCONSISTENT
-------------------------------------------------------------------------------

PROBLEM 1:
~~~~~~~~~~
Now the first problem shows itself: Our basic unification-based term oracle
cannot detect that constraint:

    False ~ (l1 ~ l3), False ~ (l2 ~ l4), l1 ~ l3

is inconsistent. That's what function @tryLitEqs@ (in comments) tries to do:
use the equality l1 ~ l3 to replace False ~ (l1 ~ l3) with False ~ (l1 ~ l1)
and expose the inconsistency.

PROBLEM 2:
~~~~~~~~~~
Let's turn back to UB and assume we had only clauses A and B. UB is as follows:

  UB = { [x  y  |> { False ~ (x ~ l1), False ~ (x ~ l3) }]
       , [l3 y  |> { False ~ (l1 ~ l3), False ~ (y ~ l4) }]
       , [l1 y  |> { x ~ l1, False ~ (y ~ l2), False ~ (l1 ~ l3) }]
       , [l1 y  |> { x ~ l1, False ~ (y ~ l2), l1 ~ l3, False ~ (y ~ l4) }] }

So we would get:

    Pattern match(es) are non-exhaustive
    In an equation for f:
        Patterns not matched:
          x y    where x not one of {l1, l3}
          l3 y   where y not one of {l4}
          l1 y   where y not one of {l2}
          l1 y   where y not one of {l2, l4} -- (*)

What about the last warning? It holds UNDER THE ASSUMPTION that l1 == l2. It is
quite unintuitive for the user so at the moment we drop such cases (see
function @pruneVSABound@ in deSugar/Check.hs). I (gkaracha) would prefer to
issue a warning like:

    Pattern match(es) are non-exhaustive
    In an equation for f:
        Patterns not matched:
          ...
          l1 y   where y not one of {l2, l4}
                 under the assumption that l1 ~ l3

It may be more complex but I would prefer to play on the safe side and (safely)
issue all warnings and leave it up to the user to decide whether the assumption
holds or not.

At the moment, we use @containsEqLits@ and consider all constraints that
include literal equalities inconsistent. We could achieve the same by replacing
this clause of @eqPmLit@:

  eqPmLit (PmOLit b1 l1) (PmOLit b2 l2)
    | b1 == b2 && l1 == l2 = Just True
    | otherwise            = Nothing

with this:

  eqPmLit (PmOLit b1 l1) (PmOLit b2 l2)
    | b1 == b2 && l1 == l2 = Just True
    | otherwise            = Just False

which approximates on the unsafe side. Hopefully, literals always need a
catch-all case to be considered exhaustive so in practice it makes small
difference. I hate this but it gives the warnings the users are used to.
-}

{- Not Enabled at the moment

-- | Check whether overloaded literal constraints exist in the state and if
-- they can be used to detect further inconsistencies.
tryLitEqs :: TmState -> Maybe Bool
tryLitEqs tm_state@(stb,_) = do
  ans <- exploitLitEqs (Just tm_state)
  -- Three possible results:
  --   Nothing    : Inconsistency found.
  --   Just True  : Literal constraints exist but no inconsistency found.
  --   Just False : There are no literal constraints in the state.
  return (isJust $ exists isLitEq_mb stb)

-- | Exploit overloaded literal constraints
-- @lit1 ~ lit2@ to improve the term oracle's expressivity.
exploitLitEqs :: Maybe TmState -> Maybe TmState
exploitLitEqs tm_state = case tm_state of
  -- The oracle did not find any inconsistency. Try and exploit
  -- residual literal equalities for a more precise result.
  Just st@(standby, (unhandled, env)) ->
    case exists isLitEq_mb standby of
      -- Such an equality exists. This means that we are under the assumption
      -- that two overloaded literals reduce to the same value (for all we know
      -- they do). Replace the one with the other in the rest residual
      -- constraints and run the solver once more, looking for an inconsistency.
      Just ((l1, l2), rest) ->
        let new_env = Map.map (replaceLit l1 l2) env
            new_stb = map (replaceLitSimplifyComplexEq l1 l2) rest
        in  exploitLitEqs
              (foldlM solveComplexEq ([], (unhandled, new_env)) new_stb)
      -- We don't have anything. Just return what you had..
      Nothing -> Just st
  -- The oracle has already found an inconsistency.
  -- No need to search further.
  Nothing -> Nothing
  where
    replaceLitSimplifyComplexEq :: PmLit -> PmLit -> ComplexEq -> ComplexEq
    replaceLitSimplifyComplexEq l1 l2 (e1,e2) =
      simplifyComplexEq (replaceLit l1 l2 e1, replaceLit l1 l2 e2)

-- | Replace a literal with another in an expression
-- See Note [Undecidable Equality on Overloaded Literals]
replaceLit :: PmLit -> PmLit -> PmExpr -> PmExpr
replaceLit l1 l2 e = case e of
  PmExprVar {}   -> e
  PmExprCon c es -> PmExprCon c (map (replaceLit l1 l2) es)
  PmExprLit l    -> case eqPmLit l l1 of
                      Just True  -> PmExprLit l2
                      Just False -> e
                      Nothing    -> e
  PmExprEq e1 e2 -> PmExprEq (replaceLit l1 l2 e1) (replaceLit l1 l2 e2)
  PmExprOther {} -> e -- do nothing
-}

-- | Check whether the term oracle state
-- contains any equalities between literals.
containsEqLits :: TmState -> Bool
containsEqLits (stb, _) = isJust (exists isLitEq_mb stb)

exists :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
exists _ []     = Nothing
exists p (x:xs) = case p x of
  Just y  -> Just (y, xs)
  Nothing -> do
    (y, ys) <- exists p xs
    return (y, x:ys)

-- | Check whether a complex equality refers to literals only
isLitEq_mb :: ComplexEq -> Maybe (PmLit, PmLit)
isLitEq_mb (PmExprLit l1, PmExprLit l2) = Just (l1, l2)
isLitEq_mb _other_eq                    = Nothing
