{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>
-}

{-# LANGUAGE CPP, MultiWayIf #-}

-- | The term equality oracle. The main export of the module are the functions
-- 'tmOracle', 'solveOneEq' and 'addSolveRefutableAltCon'.
--
-- If you are looking for an oracle that can solve type-level constraints, look
-- at 'TcSimplify.tcCheckSatisfiability'.
module TmOracle (

        -- re-exported from PmExpr
        PmExpr(..), PmLit(..), PmAltCon(..), TmVarCt(..), TmVarCtEnv,
        PmRefutEnv, eqPmLit, isNotPmExprOther, lhsExprToPmExpr, hsExprToPmExpr,

        -- the term oracle
        tmOracle, TmState, initialTmState, wrapUpTmState, solveOneEq,
        extendSubst, canDiverge, isRigid,
        addSolveRefutableAltCon, lookupRefutableAltCons,

        -- misc.
        exprDeepLookup, pmLitType
    ) where

#include "HsVersions.h"

import GhcPrelude

import PmExpr

import Util
import Id
import Name
import Type
import HsLit
import TcHsSyn
import MonadUtils
import ListSetOps (insertNoDup, unionLists)
import Maybes
import Outputable
import NameEnv
import UniqFM
import UniqDFM

{-
%************************************************************************
%*                                                                      *
                      The term equality oracle
%*                                                                      *
%************************************************************************
-}

-- | Pretty much a @['TmVarCt']@ association list where the domain is 'Name'
-- instead of 'Id'. This is the type of 'tm_pos', where we store solutions for
-- rigid pattern match variables.
type TmVarCtEnv = NameEnv PmExpr

-- | An environment assigning shapes to variables that immediately lead to a
-- refutation. So, if this maps @x :-> [3]@, then trying to solve a 'TmVarCt'
-- like @x ~ 3@ immediately leads to a contradiction.
-- Determinism is important since we use this for warning messages in
-- 'PmPpr.pprUncovered'. We don't do the same for 'TmVarCtEnv', so that is a plain
-- 'NameEnv'.
--
-- See also Note [Refutable shapes] in TmOracle.
type PmRefutEnv = DNameEnv [PmAltCon]

-- | The state of the term oracle. Tracks all term-level facts of the form "x is
-- @True@" ('tm_pos') and "x is not @5@" ('tm_neg').
--
-- Subject to Note [The Pos/Neg invariant].
data TmState = TmS
  { tm_pos :: !TmVarCtEnv
  -- ^ A substitution with solutions we extend with every step and return as a
  -- result. The substitution is in /triangular form/: It might map @x@ to @y@
  -- where @y@ itself occurs in the domain of 'tm_pos', rendering lookup
  -- non-idempotent. This means that 'varDeepLookup' potentially has to walk
  -- along a chain of var-to-var mappings until we find the solution but has the
  -- advantage that when we update the solution for @y@ above, we automatically
  -- update the solution for @x@ in a union-find-like fashion.
  -- Invariant: Only maps to other variables ('PmExprVar') or to WHNFs
  -- ('PmExprLit', 'PmExprCon'). Ergo, never maps to a 'PmExprOther'.
  , tm_neg :: !PmRefutEnv
  -- ^ Maps each variable @x@ to a list of 'PmAltCon's that @x@ definitely
  -- cannot match. Example, @x :-> [3, 4]@ means that @x@ cannot match a literal
  -- 3 or 4. Should we later solve @x@ to a variable @y@
  -- ('extendSubstAndSolve'), we merge the refutable shapes of @x@ into those of
  -- @y@. See also Note [The Pos/Neg invariant].
  }

{- Note [The Pos/Neg invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant: In any 'TmState', The domains of 'tm_pos' and 'tm_neg' are disjoint.

For example, it would make no sense to say both
    tm_pos = [...x :-> 3 ...]
    tm_neg = [...x :-> [4,42]... ]
The positive information is strictly more informative than the negative.

Suppose we are adding the (positive) fact @x :-> e@ to 'tm_pos'. Then we must
delete any binding for @x@ from 'tm_neg', to uphold the invariant.

But there is more! Suppose we are adding @x :-> y@ to 'tm_pos', and 'tm_neg'
contains @x :-> cs, y :-> ds@. Then we want to update 'tm_neg' to
@y :-> (cs ++ ds)@, to make use of the negative information we have about @x@.
-}

instance Outputable TmState where
  ppr state = braces (fsep (punctuate comma (pos ++ neg)))
    where
      pos   = map pos_eq (nonDetUFMToList (tm_pos state))
      neg   = map neg_eq (udfmToList (tm_neg state))
      pos_eq (l, r) = ppr l <+> char '~' <+> ppr r
      neg_eq (l, r) = ppr l <+> char '≁' <+> ppr r

-- | Initial state of the oracle.
initialTmState :: TmState
initialTmState = TmS emptyNameEnv emptyDNameEnv

-- | Wrap up the term oracle's state once solving is complete. Return the
-- flattened 'tm_pos' and 'tm_neg'.
wrapUpTmState :: TmState -> (TmVarCtEnv, PmRefutEnv)
wrapUpTmState solver_state
  = (flattenTmVarCtEnv (tm_pos solver_state), tm_neg solver_state)

-- | Flatten the triangular subsitution.
flattenTmVarCtEnv :: TmVarCtEnv -> TmVarCtEnv
flattenTmVarCtEnv env = mapNameEnv (exprDeepLookup env) env

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Name -> TmState -> Bool
canDiverge x TmS{ tm_pos = pos, tm_neg = neg }
  -- If the variable seems not evaluated, there is a possibility for
  -- constraint x ~ BOT to be satisfiable. That's the case when we haven't found
  -- a solution (i.e. some equivalent literal or constructor) for it yet.
  | (_, PmExprVar y) <- varDeepLookup pos x -- seems not forced
  -- Even if we don't have a solution yet, it might be involved in a negative
  -- constraint, in which case we must already have evaluated it earlier.
  , Nothing <- lookupDNameEnv neg y
  = True
  -- Variable x is already in WHNF or we know some refutable shape, so the
  -- constraint is non-satisfiable
  | otherwise = False

-- | Check whether the equality @x ~ e@ leads to a refutation. Make sure that
-- @x@ and @e@ are completely substituted before!
isRefutable :: Name -> PmExpr -> PmRefutEnv -> Bool
isRefutable x e env
  = fromMaybe False $ elem <$> exprToAlt e <*> lookupDNameEnv env x

-- | Solve an equality (top-level).
solveOneEq :: TmState -> TmVarCt -> Maybe TmState
solveOneEq solver_env (TVC x e) = unify solver_env (PmExprVar (idName x), e)

exprToAlt :: PmExpr -> Maybe PmAltCon
exprToAlt (PmExprLit l)    = Just (PmAltLit l)
exprToAlt _                = Nothing

-- | Record that a particular 'Id' can't take the shape of a 'PmAltCon' in the
-- 'TmState' and return @Nothing@ if that leads to a contradiction.
addSolveRefutableAltCon :: TmState -> Id -> PmAltCon -> Maybe TmState
addSolveRefutableAltCon original@TmS{ tm_pos = pos, tm_neg = neg } x nalt
  = case exprToAlt e of
      -- We have to take care to preserve Note [The Pos/Neg invariant]
      Nothing         -> Just extended -- Not solved yet
      Just alt                         -- We have a solution
        | alt == nalt -> Nothing       -- ... which is contradictory
        | otherwise   -> Just original -- ... which is compatible, rendering the
  where                                --     refutation redundant
    (y, e) = varDeepLookup pos (idName x)
    extended = original { tm_neg = neg' }
    neg' = alterDNameEnv (delNulls (insertNoDup nalt)) neg y

-- | When updating 'tm_neg', we want to delete any 'null' entries. This adapter
-- intends to provide a suitable interface for 'alterDNameEnv'.
delNulls :: ([a] -> [a]) -> Maybe [a] -> Maybe [a]
delNulls f mb_entry
  | ret@(_:_) <- f (fromMaybe [] mb_entry) = Just ret
  | otherwise                              = Nothing

-- | Return all 'PmAltCon' shapes that are impossible for 'Id' to take, i.e.
-- would immediately lead to a refutation by the term oracle.
lookupRefutableAltCons :: Id -> TmState -> [PmAltCon]
lookupRefutableAltCons x TmS { tm_neg = neg }
  = fromMaybe [] (lookupDNameEnv neg (idName x))

-- | Is the given variable /rigid/ (i.e., we have a solution for it) or
-- /flexible/ (i.e., no solution)? Returns the solution if /rigid/. A
-- semantically helpful alias for 'lookupNameEnv'.
isRigid :: TmState -> Name -> Maybe PmExpr
isRigid TmS{ tm_pos = pos } x = lookupNameEnv pos x

-- | @isFlexible tms = isNothing . 'isRigid' tms@
isFlexible :: TmState -> Name -> Bool
isFlexible tms = isNothing . isRigid tms

-- | Try to unify two 'PmExpr's and record the gained knowledge in the
-- 'TmState'.
--
-- Returns @Nothing@ when there's a contradiction. Returns @Just tms@
-- when the constraint was compatible with prior facts, in which case @tms@ has
-- integrated the knowledge from the equality constraint.
unify :: TmState -> (PmExpr, PmExpr) -> Maybe TmState
unify tms eq@(e1, e2) = case eq of
  -- We cannot do a thing about these cases
  (PmExprOther _,_)            -> boring
  (_,PmExprOther _)            -> boring

  (PmExprLit l1, PmExprLit l2) -> case eqPmLit l1 l2 of
    -- See Note [Undecidable Equality for Overloaded Literals]
    True  -> boring
    False -> unsat

  (PmExprCon c1 ts1, PmExprCon c2 ts2)
    | c1 == c2  -> foldlM unify tms (zip ts1 ts2)
    | otherwise -> unsat

  (PmExprVar x, PmExprVar y)
    | x == y    -> boring

  -- It's important to handle both rigid cases first, otherwise we get cyclic
  -- substitutions. Cf. 'extendSubstAndSolve' and
  -- @testsuite/tests/pmcheck/should_compile/CyclicSubst.hs@.
  (PmExprVar x, _)
    | Just e1' <- isRigid tms x -> unify tms (e1', e2)
  (_, PmExprVar y)
    | Just e2' <- isRigid tms y -> unify tms (e1, e2')
  (PmExprVar x, PmExprVar y)    -> Just (equate x y tms)
  (PmExprVar x, _)              -> trySolve x e2 tms
  (_, PmExprVar y)              -> trySolve y e1 tms

  _ -> WARN( True, text "unify: Catch all" <+> ppr eq)
       boring -- I HATE CATCH-ALLS
  where
    boring    = Just tms
    unsat     = Nothing

-- | Merges the equivalence classes of @x@ and @y@ by extending the substitution
-- with @x :-> y@.
-- Preconditions: @x /= y@ and both @x@ and @y@ are flexible (cf.
-- 'isFlexible'/'isRigid').
equate :: Name -> Name -> TmState -> TmState
equate x y tms@TmS{ tm_pos = pos, tm_neg = neg }
  = ASSERT( x /= y )
    ASSERT( isFlexible tms x )
    ASSERT( isFlexible tms y )
    tms'
  where
    pos' = extendNameEnv pos x (PmExprVar y)
    -- Be careful to uphold Note [The Pos/Neg invariant] by merging the refuts
    -- of x into those of y
    nalts = fromMaybe [] (lookupDNameEnv neg x)
    neg'  = alterDNameEnv (delNulls (unionLists nalts)) neg y
              `delFromDNameEnv` x
    tms'  = TmS { tm_pos = pos', tm_neg = neg' }

-- | Extend the substitution with a mapping @x: -> e@ if compatible with
-- refutable shapes of @x@ and its solution, reject (@Nothing@) otherwise.
--
-- Precondition: @x@ is flexible (cf. 'isFlexible'/'isRigid').
-- Precondition: @e@ is a 'PmExprCon' or 'PmExprLit'
trySolve:: Name -> PmExpr -> TmState -> Maybe TmState
trySolve x e _tms@TmS{ tm_pos = pos, tm_neg = neg }
  | ASSERT( isFlexible _tms x )
    ASSERT( _is_whnf e )
    isRefutable x e neg
  = Nothing
  | otherwise
  = Just (TmS (extendNameEnv pos x e) (delFromDNameEnv neg x))
  where
    _is_whnf PmExprCon{} = True
    _is_whnf PmExprLit{} = True
    _is_whnf _           = False

-- | When we know that a variable is fresh, we do not actually have to
-- check whether anything changes, we know that nothing does. Hence,
-- @extendSubst@ simply extends the substitution, unlike what
-- 'extendSubstAndSolve' does.
extendSubst :: Id -> PmExpr -> TmState -> TmState
extendSubst y e solver_state@TmS{ tm_pos = pos }
  | isNotPmExprOther simpl_e
  = solver_state { tm_pos = extendNameEnv pos x simpl_e }
  | otherwise = solver_state
  where
    x = idName y
    simpl_e = exprDeepLookup pos e

-- | Apply an (un-flattened) substitution to a variable and return its
-- representative in the triangular substitution @env@ and the completely
-- substituted expression. The latter may just be the representative wrapped
-- with 'PmExprVar' if we haven't found a solution for it yet.
varDeepLookup :: TmVarCtEnv -> Name -> (Name, PmExpr)
varDeepLookup env x = case lookupNameEnv env x of
  Just (PmExprVar y) -> varDeepLookup env y
  Just e             -> (x, exprDeepLookup env e) -- go deeper
  Nothing            -> (x, PmExprVar x)          -- terminal
{-# INLINE varDeepLookup #-}

-- | Apply an (un-flattened) substitution to an expression.
exprDeepLookup :: TmVarCtEnv -> PmExpr -> PmExpr
exprDeepLookup env (PmExprVar x)    = snd (varDeepLookup env x)
exprDeepLookup env (PmExprCon c es) = PmExprCon c (map (exprDeepLookup env) es)
exprDeepLookup _   other_expr       = other_expr -- PmExprLit, PmExprOther

-- | External interface to the term oracle.
tmOracle :: TmState -> [TmVarCt] -> Maybe TmState
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

This will result in the following initial matching problem:

    PatVec: x     (0 <- x)
    ValVec: $tm_y

Where the first line is the pattern vector and the second line is the value
vector abstraction. When we handle the first pattern guard in Check, it will be
desugared to a match of the form

    PatVec: x     0
    ValVec: $tm_y x

In LitVar, this will split the value vector abstraction for `x` into a positive
`PmLit 0` and a negative `PmLit x [0]` value abstraction. While the former is
immediately matched against the pattern vector, the latter (vector value
abstraction `~[0] $tm_y`) is completely uncovered by the clause.

`pmcheck` proceeds by *discarding* the the value vector abstraction involving
the guard to accomodate for the desugaring. But this also discards the valuable
information that `x` certainly is not the literal 0! Consequently, we wouldn't
be able to report the second clause as redundant.

That's a typical example of why we need the term oracle, and in this specific
case, the ability to encode that `x` certainly is not the literal 0. Now the
term oracle can immediately refute the constraint `x ~ 0` generated by the
second clause and report the clause as redundant. After the third clause, the
set of such *refutable* literals is again extended to `[0, 1]`.

In general, we want to store a set of refutable shapes (`PmAltCon`) for each
variable. That's the purpose of the `PmRefutEnv`. `addSolveRefutableAltCon` will
add such a refutable mapping to the `PmRefutEnv` in the term oracles state and
check if causes any immediate contradiction. Whenever we record a solution in
the substitution via `extendSubstAndSolve`, the refutable environment is checked
for any matching refutable `PmAltCon`.
-}
