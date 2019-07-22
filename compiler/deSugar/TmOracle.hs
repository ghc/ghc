{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>
-}

{-# LANGUAGE CPP, LambdaCase, PatternSynonyms, ViewPatterns #-}

-- | The term equality oracle. The main export of the module are the functions
-- 'tmOracle', 'solveOneEq' and 'tryAddRefutableAltCon'.
--
-- If you are looking for an oracle that can solve type-level constraints, look
-- at 'TcSimplify.tcCheckSatisfiability'.
module TmOracle (

        -- the term oracle
        tmOracle, TmState, initialTmState,
        solveOneEq, extendSubst, canDiverge,
        tryAddRefutableAltCon, suggestPossibleConLike,

        -- misc.
        wrapUpRefutableShapes, exprDeepLookup
    ) where

#include "HsVersions.h"

import GhcPrelude

import PmExpr

import BasicTypes
import Util
import Id
import Name
import NameEnv
import MonadUtils
import ListSetOps (unionLists)
import Maybes
import Outputable
import ConLike
import IncompleteMatches
import Type
import DsMonad

{-
%************************************************************************
%*                                                                      *
                      The term equality oracle
%*                                                                      *
%************************************************************************
-}
{-
-- | Pretty much a @['TmVarCt']@ association list where the domain is 'Name'
-- instead of 'Id'. This is the type of 'tm_pos', where we store solutions for
-- rigid pattern match variables.
type TmVarCtEnv = NameEnv PmExpr

-- | An environment assigning shapes to variables that immediately lead to a
-- refutation. So, if this maps @x :-> [Just]@, then trying to solve a
-- 'TmVarCt' like @x ~ Just False@ immediately leads to a contradiction.
-- Additionally, this stores the 'Type' from which to draw 'ConLike's from.
--
-- Determinism is important since we use this for warning messages in
-- 'PmPpr.pprUncovered'. We don't do the same for 'TmVarCtEnv', so that is a plain
-- 'NameEnv'.
--
-- See also Note [Refutable shapes] in TmOracle.
type PmRefutEnv = DNameEnv [PmAltCon]
-}
{-
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
  -- ('PmExprCon'). Ergo, never maps to a 'PmExprOther'.
  , tm_neg :: !PmRefutEnv
  -- ^ Maps each variable @x@ to a list of 'PmAltCon's that @x@ definitely
  -- cannot match. Example, assuming
  --
  -- @
  --     data T = Leaf Int | Branch T T | Node Int T
  -- @
  --
  -- then @x :-> [Leaf, Node]@ means that @x@ cannot match a @Leaf@ or @Node@,
  -- and hence can only match @Branch@. Should we later 'equate' @x@ to a
  -- variable @y@, we merge the refutable shapes of @x@ into those of @y@. See
  -- also Note [The Pos/Neg invariant].
  }
-}

newtype TmState = TS (DNameEnv VarInfo)

data VarInfo
  = VI
  { vi_pos :: !PossibleShape
  , vi_neg :: ![PmAltCon]
  }

data PossibleShape
  = Rigid !PmExpr
  | CompleteSets !IncompleteMatches
  | NoInfoYet

{- Note [The Pos/Neg invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant: In any 'TmState', whenever there is @x ~ C@ in 'tm_pos',
an entry @x :-> cs@ in 'tm_neg' may only have incomparable 'PmAltCons' according
to 'decEqPmAltCons'.

For example, it would make no sense to say both
    tm_pos = [...x :-> 3...]
    tm_neg = [...x :-> [4,42]...]
The positive information is strictly more informative than the negative.
On the other hand
    tm_pos = [...x :-> I# y...]
    tm_neg = [...x :-> [4]...]
We want to know that @x@ is certainly not the literal 4 when we know it is a
@I#@. Notice that @PmAltLit 4@ and @PmAltConLike I#@ are incomparable. In
general, we consider every binding in 'tm_neg' informative when the equality
relation to the solution is undecidable ('decEqPmAltCons').

Now, suppose we are adding the (positive) fact @x :-> e@ to 'tm_pos'. Then we
must delete any comparable negative facts (after considering them for
refutation) for @x@ from 'tm_neg', to uphold the invariant.

But there is more! Suppose we are adding @x :-> y@ to 'tm_pos', and 'tm_neg'
contains @x :-> cs, y :-> ds@. Then we want to update 'tm_neg' to
@y :-> (cs ++ ds)@, to make use of the negative information we have about @x@,
while we can *completely* discard the entry for @x@ in 'tm_neg'.
-}

-- | Not user-facing.
instance Outputable TmState where
  ppr (TS state) = ppr state

-- | Not user-facing.
instance Outputable VarInfo where
  ppr (VI pos neg) = if null neg then pos_pp else braces (neg_pp <> char ',' <+> pos_pp)
    where
      neg_pp = char '¬' <> ppr neg
      pos_pp = ppr pos

-- | Not user-facing.
instance Outputable PossibleShape where
  ppr NoInfoYet = char '?'
  ppr (Rigid sol) = ppr sol
  ppr (CompleteSets sets) = ppr sets

emptyVarInfo :: VarInfo
emptyVarInfo = VI NoInfoYet []

-- | Initial state of the oracle.
initialTmState :: TmState
initialTmState = TS emptyDNameEnv

lookupVarInfo :: TmState -> Name -> (Name, VarInfo)
lookupVarInfo ts@(TS env) x = case lookupDNameEnv env x of
  Just (VI (Rigid (PmExprVar y)) _) -> lookupVarInfo ts y
  mb_vi                             -> (x, fromMaybe emptyVarInfo mb_vi)

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Name -> TmState -> Bool
canDiverge x ts
  -- If the variable seems not evaluated, there is a possibility for
  -- constraint x ~ BOT to be satisfiable. That's the case when we haven't found
  -- a solution (i.e. some equivalent literal or constructor) for it yet.
  -- Even if we don't have a solution yet, it might be involved in a negative
  -- constraint, in which case we must already have evaluated it earlier.
  | VI pos [] <- snd (lookupVarInfo ts x), not_solved_yet pos
  = True
  -- Variable x is already in WHNF or we know some refutable shape, so the
  -- constraint is non-satisfiable
  | otherwise = False
  where
    not_solved_yet (Rigid PmExprCon{}) = False
    not_solved_yet _                   = True

-- | Check whether the equality @x ~ e@ leads to a refutation. Make sure that
-- @x@ and @e@ are completely substituted before!
isRefutable :: Name -> PmExpr -> TmState -> Bool
isRefutable x e ts = fromMaybe False $ do
  alt <- exprToAlt e
  let VI _ ncons = snd (lookupVarInfo ts x)
  pure (notNull (filter ((== Just True) . decEqPmAltCon alt) ncons))

-- | Solve an equality (top-level).
solveOneEq :: TmState -> TmVarCt -> Maybe TmState
solveOneEq solver_env (TVC x e) = unify solver_env (PmExprVar (idName x), e)

exprToAlt :: PmExpr -> Maybe PmAltCon
exprToAlt (PmExprCon c _) = Just c
exprToAlt _               = Nothing

-- This is the only actual 'DsM' side-effect in the entire module
initIncompleteMatches :: Type -> PossibleShape -> DsM PossibleShape
initIncompleteMatches ty NoInfoYet = initIM ty >>= \case
  Nothing -> pure NoInfoYet
  Just im -> pure (CompleteSets im)
initIncompleteMatches _  pos       = pure pos

-- | Record that a particular 'Id' can't take the shape of a 'PmAltCon' in the
-- 'TmState' and return @Nothing@ if that leads to a contradiction.
tryAddRefutableAltCon :: TmState -> Name -> Type -> PmAltCon -> DsM (Maybe TmState)
tryAddRefutableAltCon ts@(TS env) x ty nalt = do
  pos' <- initIncompleteMatches ty pos
  pure (TS . extendDNameEnv env y <$> go pos')
  where
    (y, VI pos neg) = lookupVarInfo ts x
    neg' = combineRefutEntries neg [nalt]

    go NoInfoYet = Just (VI NoInfoYet neg')
    go pos@(CompleteSets im) =
      case nalt of
        PmAltConLike cl
          | let im' = markMatchedIM cl im, isJust (unmatchedConLikeIM im')
          -> Just (VI (CompleteSets im') neg')
          | otherwise
          -> Nothing
        _ -> Just (VI pos neg')
    go pos@(Rigid (exprToAlt -> Just alt)) =
      -- We have to take care to preserve Note [The Pos/Neg invariant]
      case decEqPmAltCon alt nalt of      -- We have a solution
        Just True  -> Nothing             -- ... that is contradictory
        Just False -> Just (VI pos neg)   -- ... that is compatible, rendering
                                          --     the refutation redundant
        Nothing    -> Just (VI pos neg')  -- ... which is incomparable, so might
                                          --     refute later
    go pos = pure (VI pos neg')

suggestPossibleConLike :: TmState -> Name -> Type -> DsM (Satisfiability TmState ConLike)
suggestPossibleConLike ts@(TS env) x ty = do
  let (y, VI pos neg) = lookupVarInfo ts x
  pos' <- initIncompleteMatches ty pos
  let ts' = TS (extendDNameEnv env y (VI pos' neg))
  case pos' of
    CompleteSets im -> case unmatchedConLikeIM im of
      Nothing -> pure Unsatisfiable
      Just cl -> pure (Satisfiable ts' cl)
    Rigid (exprToAlt -> Just (PmAltConLike cl))
      -> pure (Satisfiable ts' cl)
    _ -> pure (PossiblySatisfiable ts')

-- | Combines two entries in a 'PmRefutEnv' by merging the set of refutable
-- 'PmAltCon's.
combineRefutEntries :: [PmAltCon] -> [PmAltCon] -> [PmAltCon]
combineRefutEntries old_ncons new_ncons = unionLists old_ncons new_ncons

-- | Is the given variable /rigid/ (i.e., we have a solution for it) or
-- /flexible/ (i.e., no solution)? Returns the solution if /rigid/. A
-- semantically helpful alias for 'lookupNameEnv'.
isRigid :: TmState -> Name -> Maybe PmExpr
isRigid (TS env) x = do
  VI pos _ <- lookupDNameEnv env x
  case pos of
    Rigid e -> Just e
    _       -> Nothing

-- | @isFlexible tms = isNothing . 'isRigid' tms@
isFlexible :: TmState -> Name -> Bool
isFlexible ts = isNothing . isRigid ts

-- | Try to unify two 'PmExpr's and record the gained knowledge in the
-- 'TmState'.
--
-- Returns @Nothing@ when there's a contradiction. Returns @Just ts@
-- when the constraint was compatible with prior facts, in which case @ts@ has
-- integrated the knowledge from the equality constraint.
unify :: TmState -> (PmExpr, PmExpr) -> Maybe TmState
unify ts eq@(e1, e2) = case eq of
  -- We cannot do a thing about these cases
  (PmExprOther _,_)            -> boring
  (_,PmExprOther _)            -> boring

  (PmExprCon c1 ts1, PmExprCon c2 ts2) -> case decEqPmAltCon c1 c2 of
    -- See Note [Undecidable Equality for PmAltCons]
    Just True -> foldlM unify ts (zip ts1 ts2)
    Just False -> unsat
    Nothing -> boring

  (PmExprVar x, PmExprVar y)
    | x == y    -> boring

  -- It's important to handle both rigid cases before the flexible ones,
  -- otherwise we get cyclic substitutions. Cf. 'extendSubstAndSolve' and
  -- @testsuite/tests/pmcheck/should_compile/CyclicSubst.hs@.
  (PmExprVar x, _)
    | isRefutable x e2 ts -> unsat
  (_, PmExprVar y)
    | isRefutable y e1 ts -> unsat
  (PmExprVar x, _)
    | Just e1' <- isRigid ts x     -> unify ts (e1', e2)
  (_, PmExprVar y)
    | Just e2' <- isRigid ts y     -> unify ts (e1, e2')
  (PmExprVar x, PmExprVar y)       -> Just (equate x y ts)
  (PmExprVar x, PmExprCon c args)  -> trySolve x c args ts
  (PmExprCon c args, PmExprVar y)  -> trySolve y c args ts
  where
    boring    = Just ts
    unsat     = Nothing

-- | Merges the equivalence classes of @x@ and @y@ by extending the substitution
-- with @x :-> y@.
-- Preconditions: @x /= y@ and both @x@ and @y@ are flexible (cf.
-- 'isFlexible'/'isRigid').
equate :: Name -> Name -> TmState -> TmState
equate x y ts@(TS env)
  = ASSERT( x /= y )
    ASSERT( isFlexible ts x )
    ASSERT( isFlexible ts y )
    ts'
  where
    VI _     neg_x = snd (lookupVarInfo ts x)
    VI pos_y neg_y = snd (lookupVarInfo ts y)
    vi_x'          = VI (Rigid (PmExprVar y)) []
    -- Be careful to uphold Note [The Pos/Neg invariant] by merging the refuts
    -- of x into those of y
    -- We could compute the intersection of CompleteSets for pos_x and pos_y
    -- here. Should we? We merge the negs, after all. It's also not so clear
    -- how to merge different COMPLETE sets.
    vi_y'          = VI pos_y (combineRefutEntries neg_x neg_y)
    ts'           = TS (extendDNameEnv (extendDNameEnv env x vi_x') y vi_y')

-- | @trySolve x alt args ts@ extends the substitution with a mapping @x: ->
-- PmExprCon alt args@ if compatible with refutable shapes of @x@ and its
-- solution, reject (@Nothing@) otherwise.
--
-- Precondition: @x@ is flexible (cf. 'isFlexible'/'isRigid').
trySolve:: Name -> PmAltCon -> [PmExpr] -> TmState -> Maybe TmState
trySolve x alt args ts@(TS env)
  | ASSERT( isFlexible ts x )
    isRefutable x e ts
  = Nothing
  | otherwise
  = Just ts'
  where
    e = PmExprCon alt args
    VI _ neg = snd (lookupVarInfo ts x)
    -- Uphold Note [The Pos/Neg invariant]
    vi' = VI (Rigid e) (filter ((== Nothing) . decEqPmAltCon alt) neg)
    ts' = TS (extendDNameEnv env x vi')

-- | When we know that a variable is fresh, we do not actually have to
-- check whether anything changes, we know that nothing does. Hence,
-- @extendSubst@ simply extends the substitution, unlike what
-- 'extendSubstAndSolve' does.
extendSubst :: Id -> PmExpr -> TmState -> TmState
extendSubst y e ts@(TS env)
  | isNotPmExprOther e
  = TS (extendDNameEnv env (idName y) (VI (Rigid e) []))
  | otherwise
  = ts

-- | Apply an (un-flattened) substitution to an expression.
exprDeepLookup :: TmState -> PmExpr -> PmExpr
exprDeepLookup ts (PmExprCon c es) = PmExprCon c (map (exprDeepLookup ts) es)
exprDeepLookup ts (PmExprVar x)
  | Rigid e <- vi_pos (snd (lookupVarInfo ts x))
  = exprDeepLookup ts e
exprDeepLookup _   e               = e

wrapUpRefutableShapes :: TmState -> DNameEnv [PmAltCon]
wrapUpRefutableShapes (TS env) = mapDNameEnv vi_neg env

-- | External interface to the term oracle.
tmOracle :: TmState -> [TmVarCt] -> Maybe TmState
tmOracle tm_state eqs = foldlM solveOneEq tm_state eqs

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
variable. That's the purpose of the `PmRefutEnv`. This extends to
`ConLike`s, where all value arguments are universally quantified implicitly.
So, if the `PmRefutEnv` contains an entry for `x` with `Just [Bool]`, then this
corresponds to the fact that `forall y. x ≁ Just @Bool y`.

`tryAddRefutableAltCon` will add such a refutable mapping to the `PmRefutEnv`
in the term oracles state and check if it causes any immediate contradiction.
Whenever we record a solution in the substitution via `extendSubstAndSolve`, the
refutable environment is checked for any matching refutable `PmAltCon`.

Note that `PmAltConLike` carries a list of type arguments. This purely for the
purpose of being able to reconstruct all other constructors of the matching
group the `ConLike` is part of through calling `allCompleteMatches` in Check.
-}
