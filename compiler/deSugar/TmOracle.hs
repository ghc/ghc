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
        PmExpr(..), PmLit(..), PmAltCon(..), SimpleEq, ComplexEq, PmVarEnv,
        PmRefutEnv, eqPmLit, pmExprToAlt, isNotPmExprOther, lhsExprToPmExpr,
        hsExprToPmExpr,

        -- the term oracle
        tmOracle, TmState, initialTmState, wrapUpTmState, solveOneEq, extendSubst, canDiverge,
        addSolveRefutableAltCon, lookupRefutableAltCons,

        -- misc.
        toComplex, exprDeepLookup, pmLitType, flattenPmVarEnv
    ) where

#include "HsVersions.h"

import GhcPrelude

import PmExpr

import Id
import Name
import NameEnv
import UniqFM
import UniqDFM
import Type
import HsLit
import TcHsSyn
import MonadUtils
import ListSetOps (insertNoDup, unionLists)
import Util
import Maybes
import Outputable

{-
%************************************************************************
%*                                                                      *
                      The term equality oracle
%*                                                                      *
%************************************************************************
-}

-- | The type of substitutions.
type PmVarEnv = NameEnv PmExpr

-- | An environment assigning shapes to variables that immediately lead to a
-- refutation. So, if this maps @x :-> [Just]@, then trying to solve a
-- 'ComplexEq' like @x ~ Just False@ immediately leads to a contradiction.
-- Determinism is important since we use this for warning messages in
-- 'PmPpr.pprUncovered'. We don't do the same for 'PmVarEnv', so that is a plain
-- 'NameEnv'.
--
-- See also Note [Refutable shapes] in TmOracle.
type PmRefutEnv = DNameEnv [PmAltCon]

-- | The state of the term oracle. Tracks all term-level facts we know,
-- giving special treatment to constraints of the form "x is @True@" ('tm_pos')
-- and "x is not @5@" ('tm_neg').
data TmState = TmS
  { tm_facts   :: ![ComplexEq]
  -- ^ Complex equalities we may assume to hold. We have not (yet) brought them
  -- into a form leading to a contradiction or a 'SimpleEq'. Otherwise, we would
  -- store the 'SimpleEq' as a solution in the 'tm_pos' env, where it could be
  -- used to simplify other equations. All 'ComplexEq's are fully substituted
  -- according to (i.e., fixed-points under) 'tm_pos'.
  , tm_pos     :: !PmVarEnv
  -- ^ A substitution with solutions we extend with every step and return as a
  -- result. Think of it as any 'ComplexEq' from 'tm_facts' we managed to
  -- bring into the form of a 'SimpleEq'.
  -- Contrary to 'tm_facts', the substitution is in /triangular form/: It might
  -- map @x@ to @y@ where @y@ itself occurs in the domain of 'tm_pos', rendering
  -- lookup non-idempotent. This means that 'varDeepLookup' potentially has to
  -- walk along a chain of var-to-var mappings until we find the solution but
  -- has the advantage that when we update the solution for @y@ above, we
  -- automatically update the solution for @x@ in a union-find-like fashion.
  , tm_neg     :: !PmRefutEnv
  -- ^ maps each variable @x@ to a list of 'PmAltCon's that @x@ definitely
  -- cannot match. Example, assuming
  --
  -- @
  --     data T = Leaf Int | Branch T T | Node Int T
  -- @
  --
  -- then @x :-> [Leaf, Node]@ means that @x@ cannot match a @Leaf@ or @Node@,
  -- and hence can only match @Branch@. Should we later solve @x@ to a variable
  -- @y@ ('extendSubstAndSolve'), we merge the refutable shapes of @x@ into
  -- those of @y@.
  }

instance Outputable TmState where
  ppr state = braces (fsep (punctuate comma (facts ++ pos ++ neg)))
    where
      facts = map pos_eq (tm_facts state)
      pos   = map pos_eq (nonDetUFMToList (tm_pos state))
      neg   = map neg_eq (udfmToList (tm_neg state))
      pos_eq (l, r) = ppr l <+> char '~' <+> ppr r
      neg_eq (l, r) = ppr l <+> char '≁' <+> ppr r

-- | Initial state of the oracle.
initialTmState :: TmState
initialTmState = TmS [] emptyNameEnv emptyDNameEnv

-- | Wrap up the term oracle's state once solving is complete. Drop any
-- information about non-simple constraints and flatten (height 1) the
-- substitution.
wrapUpTmState :: TmState -> (PmVarEnv, PmRefutEnv)
wrapUpTmState solver_state
  = (flattenPmVarEnv (tm_pos solver_state), tm_neg solver_state)

-- | Flatten the triangular subsitution.
flattenPmVarEnv :: PmVarEnv -> PmVarEnv
flattenPmVarEnv env = mapNameEnv (exprDeepLookup env) env

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Name -> TmState -> Bool
canDiverge x TmS{ tm_pos = pos, tm_facts = facts }
  -- If the variable seems not evaluated, there is a possibility for
  -- constraint x ~ BOT to be satisfiable. That's the case when we haven't found
  -- a solution (i.e. some equivalent literal or constructor) for it yet.
  | (_, PmExprVar y) <- varDeepLookup pos x -- seems not forced
  -- If it is involved (directly or indirectly) in any equality in the
  -- worklist, we can assume that it is already indirectly evaluated,
  -- as a side-effect of equality checking. If not, then we can assume
  -- that the constraint is satisfiable.
  = not $ any (isForcedByEq x) facts || any (isForcedByEq y) facts
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
  = fromMaybe False $ elem <$> pmExprToAlt e <*> lookupDNameEnv env x

-- | Solve a complex equality (top-level).
solveOneEq :: TmState -> ComplexEq -> Maybe TmState
solveOneEq solver_env@TmS{ tm_pos = pos } complex
  = solveComplexEq solver_env       -- do the actual *merging* with existing state
  $ applySubstComplexEq pos complex -- replace everything we already know

-- | Record that a particular 'Id' can't take the shape of a 'PmAltCon' in the
-- 'TmState' and return @Nothing@ if that leads to a contradiction.
addSolveRefutableAltCon :: TmState -> Id -> PmAltCon -> Maybe TmState
addSolveRefutableAltCon original@TmS{ tm_pos = pos, tm_neg = neg } x nalt
  = case pmExprToAlt e of
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

-- | Solve a complex equality.
-- Nothing => definitely unsatisfiable
-- Just tms => I have added the complex equality and added
--             it to the tmstate; the result may or may not be
--             satisfiable
solveComplexEq :: TmState -> ComplexEq -> Maybe TmState
solveComplexEq solver_state eq@(e1, e2) = {-pprTraceWith "solveComplexEq" (\mb_sat -> ppr eq $$ ppr mb_sat) $-} case eq of
  -- We cannot do a thing about these cases
  (PmExprOther _,_)            -> Just solver_state
  (_,PmExprOther _)            -> Just solver_state

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
       Just solver_state -- I HATE CATCH-ALLS

-- | Extend the substitution and solve the (possibly updated) constraints.
extendSubstAndSolve :: Name -> PmExpr -> TmState -> Maybe TmState
extendSubstAndSolve x e TmS{ tm_facts = facts, tm_pos = pos, tm_neg = neg }
  | isRefutable x e' neg -- NB: e'
  = Nothing
  | otherwise
  = foldlM solveComplexEq new_incr_state changed
  where
    -- Apply the substitution to the worklist and partition them to the ones
    -- that had some progress and the rest. Then, recurse over the ones that
    -- had some progress. Careful about performance:
    -- See Note [Representation of Term Equalities] in deSugar/Check.hs
    (changed, unchanged) = partitionWith (substComplexEq x e) facts
    new_pos              = extendNameEnv pos x e
    -- When e is a @PmExprVar y@, we have to check @y@'s solution for
    -- refutability instead. Afterwards, we have to merge @x@'s refutable shapes
    -- to @y@'s. Actually, e == e' because it has been fully substituted before,
    -- but better be safe.
    (y, e')              = varDeepLookup new_pos x
    new_neg | x == y     = neg
            | otherwise  = case lookupDNameEnv neg x of
                             Nothing -> neg
                             Just nalts ->
                               alterDNameEnv (delNulls (unionLists nalts)) neg y
                                 `delFromDNameEnv` x
    new_incr_state       = TmS unchanged new_pos new_neg

-- | When we know that a variable is fresh, we do not actually have to
-- check whether anything changes, we know that nothing does. Hence,
-- `extendSubst` simply extends the substitution, unlike what
-- `extendSubstAndSolve` does.
extendSubst :: Id -> PmExpr -> TmState -> TmState
extendSubst y e solver_state@TmS{ tm_pos = pos }
  | isNotPmExprOther simpl_e
  = solver_state { tm_pos = extendNameEnv pos x simpl_e }
  | otherwise = solver_state
  where
    x = idName y
    simpl_e = exprDeepLookup pos e

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

`addSolveRefutableAltCon` will add such a refutable mapping to the `PmRefutEnv`
in the term oracles state and check if causes any immediate contradiction.
Whenever we record a solution in the substitution via `extendSubstAndSolve`, the
refutable environment is checked for any matching refutable `PmAltCon`.

Note that `PmAltConLike` carries a list of type arguments. This purely for the
purpose of being able to reconstruct all other constructors of the matching
group the `ConLike` is part of through calling `allCompleteMatches` in Check.
-}