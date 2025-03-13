{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Types.Var.Env (
        -- * Var, Id and TyVar environments (maps)
        VarEnv, IdEnv, TyVarEnv, CoVarEnv, TyCoVarEnv,

        -- ** Manipulating these environments
        emptyVarEnv, unitVarEnv, mkVarEnv, mkVarEnv_Directly,
        elemVarEnv, disjointVarEnv, anyVarEnv,
        extendVarEnv, extendVarEnv_C, extendVarEnv_Acc,
        extendVarEnvList,
        plusVarEnv, plusVarEnv_C, plusVarEnv_CD, plusMaybeVarEnv_C,
        plusVarEnvList, alterVarEnv,
        delVarEnvList, delVarEnv,
        minusVarEnv,
        lookupVarEnv, lookupVarEnv_NF, lookupWithDefaultVarEnv,
        lookupVarEnv_Directly,
        mapVarEnv, zipVarEnv,
        modifyVarEnv, modifyVarEnv_Directly,
        isEmptyVarEnv,
        elemVarEnvByKey,
        filterVarEnv, restrictVarEnv,
        partitionVarEnv, varEnvDomain,
        nonDetStrictFoldVarEnv_Directly,

        -- * Deterministic Var environments (maps)
        DVarEnv, DIdEnv, DTyVarEnv,

        -- ** Manipulating these environments
        emptyDVarEnv, mkDVarEnv,
        dVarEnvElts,
        extendDVarEnv, extendDVarEnv_C,
        extendDVarEnvList,
        lookupDVarEnv, elemDVarEnv,
        isEmptyDVarEnv, foldDVarEnv, nonDetStrictFoldDVarEnv,
        mapDVarEnv, filterDVarEnv,
        modifyDVarEnv,
        alterDVarEnv,
        plusDVarEnv, plusDVarEnv_C,
        unitDVarEnv,
        delDVarEnv,
        delDVarEnvList,
        minusDVarEnv,
        partitionDVarEnv,
        anyDVarEnv,

        -- * The InScopeSet type
        InScopeSet(..),

        -- ** Operations on InScopeSets
        emptyInScopeSet, mkInScopeSet, mkInScopeSetList, delInScopeSet,
        extendInScopeSet, extendInScopeSetList, extendInScopeSetSet,
        getInScopeVars, lookupInScope, lookupInScope_Directly,
        unionInScope, elemInScopeSet, uniqAway,
        varSetInScope,
        unsafeGetFreshLocalUnique,

        -- * The RnEnv2 type
        RnEnv2,

        -- ** Operations on RnEnv2s
        mkRnEnv2, rnBndr2, rnBndrs2, rnBndr2_var,
        rnOccL, rnOccR, inRnEnvL, inRnEnvR,  anyInRnEnvR,
        rnOccL_maybe, rnOccR_maybe,
        rnBndrL, rnBndrR, nukeRnEnvL, nukeRnEnvR, rnSwap,
        delBndrL, delBndrR, delBndrsL, delBndrsR,
        extendRnInScopeSetList,
        rnEtaL, rnEtaR,
        rnInScope, rnInScopeSet, lookupRnInScope,
        rnEnvL, rnEnvR,

        -- * TidyEnv and its operation
        TidyEnv,
        emptyTidyEnv, mkEmptyTidyEnv, delTidyEnvList,
        mapMaybeDVarEnv
    ) where

import GHC.Prelude
import qualified GHC.Data.Word64Map.Strict as Word64Map -- TODO: Move this to UniqFM

import GHC.Types.Name.Occurrence
import GHC.Types.Name
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Data.Graph.UnVar   -- UnVarSet
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Types.Unique
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.Maybe
import GHC.Utils.Outputable

{-
************************************************************************
*                                                                      *
                In-scope sets
*                                                                      *
************************************************************************
-}

-- | A set of variables that are in scope at some point.
--
-- Note that this is a /superset/ of the variables that are currently in scope.
-- See Note [The InScopeSet invariant].
--
-- "Secrets of the Glasgow Haskell Compiler inliner" Section 3.2 provides
-- the motivation for this abstraction.
newtype InScopeSet = InScope VarSet
        -- Note [Lookups in in-scope set]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- We store a VarSet here, but we use this for lookups rather than just
        -- membership tests. Typically the InScopeSet contains the canonical
        -- version of the variable (e.g. with an informative unfolding), so this
        -- lookup is useful (see, for instance, Note [In-scope set as a
        -- substitution]).

        -- Note [The InScopeSet invariant]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- The InScopeSet must include every in-scope variable, but it may also
        -- include other variables.

        -- Its principal purpose is to provide a set of variables to be avoided
        -- when creating a fresh identifier (fresh in the sense that it does not
        -- "shadow" any in-scope binding). To do this we simply have to find one that
        -- does not appear in the InScopeSet. This is done by the key function
        -- GHC.Types.Var.Env.uniqAway.

        -- See "Secrets of the Glasgow Haskell Compiler inliner" Section 3.2
        -- for more detailed motivation. #20419 has further discussion.


instance Outputable InScopeSet where
  ppr (InScope s) =
    text "InScope" <+>
    braces (fsep (map (ppr . Var.varName) (nonDetEltsUniqSet s)))
                      -- It's OK to use nonDetEltsUniqSet here because it's
                      -- only for pretty printing
                      -- In-scope sets get big, and with -dppr-debug
                      -- the output is overwhelming

emptyInScopeSet :: InScopeSet
emptyInScopeSet = InScope emptyVarSet

getInScopeVars ::  InScopeSet -> VarSet
getInScopeVars (InScope vs) = vs

mkInScopeSet :: VarSet -> InScopeSet
mkInScopeSet in_scope = InScope in_scope

mkInScopeSetList :: [Var] -> InScopeSet
mkInScopeSetList vs = InScope (mkVarSet vs)

extendInScopeSet :: InScopeSet -> Var -> InScopeSet
extendInScopeSet (InScope in_scope) v
   = InScope (extendVarSet in_scope v)

extendInScopeSetList :: InScopeSet -> [Var] -> InScopeSet
extendInScopeSetList (InScope in_scope) vs
   = InScope $ foldl' extendVarSet in_scope vs

extendInScopeSetSet :: InScopeSet -> VarSet -> InScopeSet
extendInScopeSetSet (InScope in_scope) vs
   = InScope (in_scope `unionVarSet` vs)

delInScopeSet :: InScopeSet -> Var -> InScopeSet
delInScopeSet (InScope in_scope) v = InScope (in_scope `delVarSet` v)

elemInScopeSet :: Var -> InScopeSet -> Bool
elemInScopeSet v (InScope in_scope) = v `elemVarSet` in_scope

-- | Look up a variable the 'InScopeSet'.  This lets you map from
-- the variable's identity (unique) to its full value.
lookupInScope :: InScopeSet -> Var -> Maybe Var
lookupInScope (InScope in_scope) v  = lookupVarSet in_scope v

lookupInScope_Directly :: InScopeSet -> Unique -> Maybe Var
lookupInScope_Directly (InScope in_scope) uniq
  = lookupVarSet_Directly in_scope uniq

unionInScope :: InScopeSet -> InScopeSet -> InScopeSet
unionInScope (InScope s1) (InScope s2)
  = InScope (s1 `unionVarSet` s2)

varSetInScope :: VarSet -> InScopeSet -> Bool
varSetInScope vars (InScope s1) = vars `subVarSet` s1

{-
Note [Local uniques]
~~~~~~~~~~~~~~~~~~~~
Sometimes one must create conjure up a unique which is unique in a particular
context (but not necessarily globally unique). For instance, one might need to
create a fresh local identifier which does not shadow any of the locally
in-scope variables.  For this we purpose we provide 'uniqAway'.

'uniqAway' is implemented in terms of the 'unsafeGetFreshLocalUnique'
operation, which generates an unclaimed 'Unique' from an 'InScopeSet'. To
ensure that we do not conflict with uniques allocated by future allocations
from 'UniqSupply's, Uniques generated by 'unsafeGetFreshLocalUnique' are
allocated into a dedicated region of the unique space (namely the X tag).

Note that one must be quite carefully when using uniques generated in this way
since they are only locally unique. In particular, two successive calls to
'uniqAway' on the same 'InScopeSet' will produce the same unique.
 -}

-- | @uniqAway in_scope v@ finds a unique that is not used in the
-- in-scope set, and gives that to v. See Note [Local uniques] and
-- Note [The InScopeSet invariant].
uniqAway :: InScopeSet -> Var -> Var
-- It starts with v's current unique, of course, in the hope that it won't
-- have to change, and thereafter uses the successor to the last derived unique
-- found in the in-scope set.
uniqAway in_scope var
  | var `elemInScopeSet` in_scope = uniqAway' in_scope var      -- Make a new one
  | otherwise                     = var                         -- Nothing to do

uniqAway' :: InScopeSet -> Var -> Var
-- This one *always* makes up a new variable
uniqAway' in_scope var
  = setVarUnique var (unsafeGetFreshLocalUnique in_scope)

-- | @unsafeGetFreshUnique in_scope@ finds a unique that is not in-scope in the
-- given 'InScopeSet'. This must be used very carefully since one can very easily
-- introduce non-unique 'Unique's this way. See Note [Local uniques].
unsafeGetFreshLocalUnique :: InScopeSet -> Unique
unsafeGetFreshLocalUnique (InScope set)
  | Just (uniq,_) <- Word64Map.lookupLT (getKey maxLocalUnique) (ufmToIntMap $ getUniqSet set)
  , let uniq' = mkLocalUnique uniq
  , not $ uniq' `ltUnique` minLocalUnique
  = incrUnique uniq'

  | otherwise
  = minLocalUnique

{-
************************************************************************
*                                                                      *
                Dual renaming
*                                                                      *
************************************************************************
-}

-- | Rename Environment 2
--
-- When we are comparing (or matching) types or terms, we are faced with
-- \"going under\" corresponding binders.  E.g. when comparing:
--
-- > \x. e1     ~   \y. e2
--
-- Basically we want to rename [@x@ -> @y@] or [@y@ -> @x@], but there are lots of
-- things we must be careful of.  In particular, @x@ might be free in @e2@, or
-- y in @e1@.  So the idea is that we come up with a fresh binder that is free
-- in neither, and rename @x@ and @y@ respectively.  That means we must maintain:
--
-- 1. A renaming for the left-hand expression
--
-- 2. A renaming for the right-hand expressions
--
-- 3. An in-scope set
--
-- Furthermore, when matching, we want to be able to have an 'occurs check',
-- to prevent:
--
-- > \x. f   ~   \y. y
--
-- matching with [@f@ -> @y@].  So for each expression we want to know that set of
-- locally-bound variables. That is precisely the domain of the mappings 1.
-- and 2., but we must ensure that we always extend the mappings as we go in.
--
-- All of this information is bundled up in the 'RnEnv2'
data RnEnv2
  = RV2 { envL     :: VarEnv Var        -- Renaming for Left term
        , envR     :: VarEnv Var        -- Renaming for Right term
        , in_scope :: InScopeSet }      -- In scope in left or right terms

-- The renamings envL and envR are *guaranteed* to contain a binding
-- for every variable bound as we go into the term, even if it is not
-- renamed.  That way we can ask what variables are locally bound
-- (inRnEnvL, inRnEnvR)

mkRnEnv2 :: InScopeSet -> RnEnv2
mkRnEnv2 vars = RV2     { envL     = emptyVarEnv
                        , envR     = emptyVarEnv
                        , in_scope = vars }

extendRnInScopeSetList :: RnEnv2 -> [Var] -> RnEnv2
extendRnInScopeSetList env vs
  | null vs   = env
  | otherwise = env { in_scope = extendInScopeSetList (in_scope env) vs }

rnInScope :: Var -> RnEnv2 -> Bool
rnInScope x env = x `elemInScopeSet` in_scope env

rnInScopeSet :: RnEnv2 -> InScopeSet
rnInScopeSet = in_scope

-- | Retrieve the left mapping
rnEnvL :: RnEnv2 -> VarEnv Var
rnEnvL = envL

-- | Retrieve the right mapping
rnEnvR :: RnEnv2 -> VarEnv Var
rnEnvR = envR

rnBndrs2 :: RnEnv2 -> [Var] -> [Var] -> RnEnv2
-- ^ Applies 'rnBndr2' to several variables: the two variable lists must be of equal length
rnBndrs2 env bsL bsR = foldl2 rnBndr2 env bsL bsR

rnBndr2 :: RnEnv2 -> Var -> Var -> RnEnv2
-- ^ @rnBndr2 env bL bR@ goes under a binder @bL@ in the Left term,
--                       and binder @bR@ in the Right term.
-- It finds a new binder, @new_b@,
-- and returns an environment mapping @bL -> new_b@ and @bR -> new_b@
rnBndr2 env bL bR = fst $ rnBndr2_var env bL bR

rnBndr2_var :: RnEnv2 -> Var -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndr2' but returns the new variable as well as the
-- new environment.
-- Postcondition: the type of the returned Var is that of bR
rnBndr2_var (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL bR
  = (RV2 { envL     = extendVarEnv envL bL new_b   -- See Note
         , envR     = extendVarEnv envR bR new_b   -- [Rebinding]
         , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
        -- Find a new binder not in scope in either term
        -- To avoid calling `uniqAway`, we try bL's Unique
        -- But we always return a Var whose type is that of bR
    new_b | not (bR `elemInScopeSet` in_scope) = bR
          | not (bL `elemInScopeSet` in_scope) = bR `setVarUnique` varUnique bL
          | otherwise                          = uniqAway' in_scope bR

        -- Note [Rebinding]
        -- ~~~~~~~~~~~~~~~~
        -- If the new var is the same as the old one, note that
        -- the extendVarEnv *replaces* any current renaming
        -- E.g.   (\x. \x. ...)  ~  (\y. \z. ...)
        --
        --                        envL    envR          in_scope
        --   Inside \x  \y      { [x->y], [y->y],       {y} }
        --          \x  \z      { [x->z], [y->y, z->z], {y,z} }
        --          The envL binding [x->y] is replaced by [x->z]

rnBndrL :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndr2' but used when there's a binder on the left
-- side only.
rnBndrL (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL
  = (RV2 { envL     = extendVarEnv envL bL new_b
         , envR     = envR
         , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bL

rnBndrR :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndr2' but used when there's a binder on the right
-- side only.
rnBndrR (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bR
  = (RV2 { envR     = extendVarEnv envR bR new_b
         , envL     = envL
         , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bR

rnEtaL :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndrL' but used for eta expansion
-- See Note [Eta expansion]
rnEtaL (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL
  = (RV2 { envL     = extendVarEnv envL bL new_b
         , envR     = extendVarEnv envR new_b new_b     -- Note [Eta expansion]
         , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bL

rnEtaR :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndr2' but used for eta expansion
-- See Note [Eta expansion]
rnEtaR (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bR
  = (RV2 { envL     = extendVarEnv envL new_b new_b     -- Note [Eta expansion]
         , envR     = extendVarEnv envR bR new_b
         , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bR

delBndrL, delBndrR :: RnEnv2 -> Var -> RnEnv2
delBndrL rn@(RV2 { envL = env, in_scope = in_scope }) v
  = rn { envL = env `delVarEnv` v, in_scope = in_scope `extendInScopeSet` v }
delBndrR rn@(RV2 { envR = env, in_scope = in_scope }) v
  = rn { envR = env `delVarEnv` v, in_scope = in_scope `extendInScopeSet` v }

delBndrsL, delBndrsR :: RnEnv2 -> [Var] -> RnEnv2
delBndrsL rn@(RV2 { envL = env, in_scope = in_scope }) v
  = rn { envL = env `delVarEnvList` v, in_scope = in_scope `extendInScopeSetList` v }
delBndrsR rn@(RV2 { envR = env, in_scope = in_scope }) v
  = rn { envR = env `delVarEnvList` v, in_scope = in_scope `extendInScopeSetList` v }

rnOccL, rnOccR :: RnEnv2 -> Var -> Var
-- ^ Look up the renaming of an occurrence in the left or right term
rnOccL (RV2 { envL = env }) v = lookupVarEnv env v `orElse` v
rnOccR (RV2 { envR = env }) v = lookupVarEnv env v `orElse` v

rnOccL_maybe, rnOccR_maybe :: RnEnv2 -> Var -> Maybe Var
-- ^ Look up the renaming of an occurrence in the left or right term
rnOccL_maybe (RV2 { envL = env }) v = lookupVarEnv env v
rnOccR_maybe (RV2 { envR = env }) v = lookupVarEnv env v

inRnEnvL, inRnEnvR :: RnEnv2 -> Var -> Bool
-- ^ Tells whether a variable is locally bound
inRnEnvL (RV2 { envL = env }) v = v `elemVarEnv` env
inRnEnvR (RV2 { envR = env }) v = v `elemVarEnv` env

-- | `anyInRnEnvR env set` == `any (inRnEnvR rn_env) (toList set)`
-- but lazy in the second argument if the right side of the env is empty.
anyInRnEnvR :: RnEnv2 -> VarSet -> Bool
anyInRnEnvR (RV2 { envR = env }) vs
  -- Avoid allocating the predicate if we deal with an empty env.
  | isEmptyVarEnv env = False
  | otherwise         = anyVarSet (`elemVarEnv` env) vs

lookupRnInScope :: RnEnv2 -> Var -> Var
lookupRnInScope env v = lookupInScope (in_scope env) v `orElse` v

nukeRnEnvL, nukeRnEnvR :: RnEnv2 -> RnEnv2
-- ^ Wipe the left or right side renaming
nukeRnEnvL env = env { envL = emptyVarEnv }
nukeRnEnvR env = env { envR = emptyVarEnv }

rnSwap :: RnEnv2 -> RnEnv2
-- ^ swap the meaning of left and right
rnSwap (RV2 { envL = envL, envR = envR, in_scope = in_scope })
  = RV2 { envL = envR, envR = envL, in_scope = in_scope }

{-
Note [Eta expansion]
~~~~~~~~~~~~~~~~~~~~
When matching
     (\x.M) ~ N
we rename x to x' with, where x' is not in scope in
either term.  Then we want to behave as if we'd seen
     (\x'.M) ~ (\x'.N x')
Since x' isn't in scope in N, the form (\x'. N x') doesn't
capture any variables in N.  But we must nevertheless extend
the envR with a binding [x' -> x'], to support the occurs check.
For example, if we don't do this, we can get silly matches like
        forall a.  (\y.a)  ~   v
succeeding with [a -> v y], which is bogus of course.


************************************************************************
*                                                                      *
                Tidying
*                                                                      *
************************************************************************
-}

-- | Tidy Environment
--
-- When tidying up print names, we keep a mapping of in-scope occ-names
-- (the 'TidyOccEnv') and a Var-to-Var of the current renamings
type TidyEnv = (TidyOccEnv, VarEnv Var)

emptyTidyEnv :: TidyEnv
emptyTidyEnv = (emptyTidyOccEnv, emptyVarEnv)

mkEmptyTidyEnv :: TidyOccEnv -> TidyEnv
mkEmptyTidyEnv occ_env = (occ_env, emptyVarEnv)

delTidyEnvList :: TidyEnv -> [Var] -> TidyEnv
delTidyEnvList (occ_env, var_env) vs = (occ_env', var_env')
  where
    occ_env' = occ_env `delTidyOccEnvList` map getOccName vs
    var_env' = var_env `delVarEnvList` vs

{-
************************************************************************
*                                                                      *
   VarEnv
*                                                                      *
************************************************************************
-}

-- We would like this to be `UniqFM Var elt`
-- but the code uses various key types.
-- So for now make it explicitly untyped

-- | Variable Environment
type VarEnv elt     = UniqFM Var elt

-- | Identifier Environment
type IdEnv elt      = UniqFM Id elt

-- | Type Variable Environment
type TyVarEnv elt   = UniqFM Var elt

-- | Type or Coercion Variable Environment
type TyCoVarEnv elt = UniqFM TyCoVar elt

-- | Coercion Variable Environment
type CoVarEnv elt   = UniqFM CoVar elt

emptyVarEnv       :: VarEnv a
mkVarEnv          :: [(Var, a)] -> VarEnv a
mkVarEnv_Directly :: [(Unique, a)] -> VarEnv a
zipVarEnv         :: [Var] -> [a] -> VarEnv a
unitVarEnv        :: Var -> a -> VarEnv a
alterVarEnv       :: (Maybe a -> Maybe a) -> VarEnv a -> Var -> VarEnv a
extendVarEnv      :: VarEnv a -> Var -> a -> VarEnv a
extendVarEnv_C    :: (a->a->a) -> VarEnv a -> Var -> a -> VarEnv a
extendVarEnv_Acc  :: (a->b->b) -> (a->b) -> VarEnv b -> Var -> a -> VarEnv b
plusVarEnv        :: VarEnv a -> VarEnv a -> VarEnv a
plusVarEnvList    :: [VarEnv a] -> VarEnv a
extendVarEnvList  :: VarEnv a -> [(Var, a)] -> VarEnv a
varEnvDomain      :: VarEnv elt -> UnVarSet

partitionVarEnv   :: (a -> Bool) -> VarEnv a -> (VarEnv a, VarEnv a)
-- | Only keep variables contained in the VarSet
restrictVarEnv    :: VarEnv a -> VarSet -> VarEnv a
delVarEnvList     :: Foldable f => VarEnv a -> f Var -> VarEnv a
delVarEnv         :: VarEnv a -> Var -> VarEnv a
minusVarEnv       :: VarEnv a -> VarEnv b -> VarEnv a
plusVarEnv_C      :: (a -> a -> a) -> VarEnv a -> VarEnv a -> VarEnv a
plusVarEnv_CD     :: (a -> a -> a) -> VarEnv a -> a -> VarEnv a -> a -> VarEnv a
plusMaybeVarEnv_C :: (a -> a -> Maybe a) -> VarEnv a -> VarEnv a -> VarEnv a
mapVarEnv         :: (a -> b) -> VarEnv a -> VarEnv b
modifyVarEnv      :: (a -> a) -> VarEnv a -> Var -> VarEnv a

isEmptyVarEnv     :: VarEnv a -> Bool
lookupVarEnv      :: VarEnv a -> Var -> Maybe a
lookupVarEnv_Directly :: VarEnv a -> Unique -> Maybe a
filterVarEnv      :: (a -> Bool) -> VarEnv a -> VarEnv a
anyVarEnv         :: (elt -> Bool) -> UniqFM key elt -> Bool
lookupVarEnv_NF   :: VarEnv a -> Var -> a
lookupWithDefaultVarEnv :: VarEnv a -> a -> Var -> a
elemVarEnv        :: Var -> VarEnv a -> Bool
elemVarEnvByKey   :: Unique -> VarEnv a -> Bool
disjointVarEnv    :: VarEnv a -> VarEnv a -> Bool
nonDetStrictFoldVarEnv_Directly :: (Unique -> a -> r -> r) -> r -> VarEnv a -> r

elemVarEnv       = elemUFM
elemVarEnvByKey  = elemUFM_Directly
disjointVarEnv   = disjointUFM
alterVarEnv      = alterUFM
extendVarEnv     = addToUFM
extendVarEnv_C   = addToUFM_C
extendVarEnv_Acc = addToUFM_Acc
extendVarEnvList = addListToUFM
plusVarEnv_C     = plusUFM_C
plusVarEnv_CD    = plusUFM_CD
plusMaybeVarEnv_C = plusMaybeUFM_C
delVarEnvList    = delListFromUFM
-- INLINE due to polymorphism
{-# INLINE delVarEnvList #-}
delVarEnv        = delFromUFM
minusVarEnv      = minusUFM
plusVarEnv       = plusUFM
plusVarEnvList   = plusUFMList
-- lookupVarEnv is very hot (in part due to being called by substTyVar),
-- if it's not inlined than the mere allocation of the Just constructor causes
-- perf benchmarks to regress by 2% in some cases. See #21159, !7638 and containers#821
-- for some more explanation about what exactly went wrong.
{-# INLINE lookupVarEnv #-}
lookupVarEnv     = lookupUFM
lookupVarEnv_Directly = lookupUFM_Directly
filterVarEnv     = filterUFM
anyVarEnv        = anyUFM
lookupWithDefaultVarEnv = lookupWithDefaultUFM
mapVarEnv        = mapUFM
mkVarEnv         = listToUFM
mkVarEnv_Directly= listToUFM_Directly
emptyVarEnv      = emptyUFM
unitVarEnv       = unitUFM
isEmptyVarEnv    = isNullUFM
partitionVarEnv  = partitionUFM
varEnvDomain     = domUFMUnVarSet
nonDetStrictFoldVarEnv_Directly = nonDetStrictFoldUFM_Directly


restrictVarEnv env vs = filterUFM_Directly keep env
  where
    keep u _ = u `elemVarSetByKey` vs

zipVarEnv tyvars tys   = mkVarEnv (zipEqual tyvars tys)
lookupVarEnv_NF env id = case lookupVarEnv env id of
                         Just xx -> xx
                         Nothing -> panic "lookupVarEnv_NF: Nothing"


{-
@modifyVarEnv@: Look up a thing in the VarEnv,
then mash it with the modify function, and put it back.
-}

modifyVarEnv mangle_fn env key
  = case (lookupVarEnv env key) of
      Nothing -> env
      Just xx -> extendVarEnv env key (mangle_fn xx)

modifyVarEnv_Directly :: (a -> a) -> UniqFM key a -> Unique -> UniqFM key a
modifyVarEnv_Directly mangle_fn env key
  = case (lookupUFM_Directly env key) of
      Nothing -> env
      Just xx -> addToUFM_Directly env key (mangle_fn xx)

{-
************************************************************************
*                                                                      *
   Deterministic VarEnv (DVarEnv)
*                                                                      *
************************************************************************
-}
-- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for explanation why we need
-- DVarEnv.

-- | Deterministic Variable Environment
type DVarEnv elt = UniqDFM Var elt

-- | Deterministic Identifier Environment
-- Sadly not always indexed by Id, but it is in the common case.
type DIdEnv elt = UniqDFM Var elt

-- | Deterministic Type Variable Environment
type DTyVarEnv elt = UniqDFM TyVar elt

emptyDVarEnv :: DVarEnv a
emptyDVarEnv = emptyUDFM

dVarEnvElts :: DVarEnv a -> [a]
dVarEnvElts = eltsUDFM

mkDVarEnv :: [(Var, a)] -> DVarEnv a
mkDVarEnv = listToUDFM

extendDVarEnv :: DVarEnv a -> Var -> a -> DVarEnv a
extendDVarEnv = addToUDFM

minusDVarEnv :: DVarEnv a -> DVarEnv a' -> DVarEnv a
minusDVarEnv = minusUDFM

lookupDVarEnv :: DVarEnv a -> Var -> Maybe a
lookupDVarEnv = lookupUDFM

foldDVarEnv :: (a -> b -> b) -> b -> DVarEnv a -> b
foldDVarEnv = foldUDFM

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetStrictFoldDVarEnv :: (a -> b -> b) -> b -> DVarEnv a -> b
nonDetStrictFoldDVarEnv = nonDetStrictFoldUDFM

mapDVarEnv :: (a -> b) -> DVarEnv a -> DVarEnv b
mapDVarEnv = mapUDFM

filterDVarEnv      :: (a -> Bool) -> DVarEnv a -> DVarEnv a
filterDVarEnv = filterUDFM

mapMaybeDVarEnv :: (a -> Maybe b) -> DVarEnv a -> DVarEnv b
mapMaybeDVarEnv f = mapMaybeUDFM f

alterDVarEnv :: (Maybe a -> Maybe a) -> DVarEnv a -> Var -> DVarEnv a
alterDVarEnv = alterUDFM

plusDVarEnv :: DVarEnv a -> DVarEnv a -> DVarEnv a
plusDVarEnv = plusUDFM

plusDVarEnv_C :: (a -> a -> a) -> DVarEnv a -> DVarEnv a -> DVarEnv a
plusDVarEnv_C = plusUDFM_C

unitDVarEnv :: Var -> a -> DVarEnv a
unitDVarEnv = unitUDFM

delDVarEnv :: DVarEnv a -> Var -> DVarEnv a
delDVarEnv = delFromUDFM

delDVarEnvList :: DVarEnv a -> [Var] -> DVarEnv a
delDVarEnvList = delListFromUDFM

isEmptyDVarEnv :: DVarEnv a -> Bool
isEmptyDVarEnv = isNullUDFM

elemDVarEnv :: Var -> DVarEnv a -> Bool
elemDVarEnv = elemUDFM

extendDVarEnv_C :: (a -> a -> a) -> DVarEnv a -> Var -> a -> DVarEnv a
extendDVarEnv_C = addToUDFM_C

modifyDVarEnv :: (a -> a) -> DVarEnv a -> Var -> DVarEnv a
modifyDVarEnv mangle_fn env key
  = case (lookupDVarEnv env key) of
      Nothing -> env
      Just xx -> extendDVarEnv env key (mangle_fn xx)

partitionDVarEnv :: (a -> Bool) -> DVarEnv a -> (DVarEnv a, DVarEnv a)
partitionDVarEnv = partitionUDFM

extendDVarEnvList :: DVarEnv a -> [(Var, a)] -> DVarEnv a
extendDVarEnvList = addListToUDFM

anyDVarEnv :: (a -> Bool) -> DVarEnv a -> Bool
anyDVarEnv = anyUDFM
