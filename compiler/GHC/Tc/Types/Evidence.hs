-- (c) The University of Glasgow 2006

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Tc.Types.Evidence (

  -- * HsWrapper
  HsWrapper(..),
  (<.>), mkWpTyApps, mkWpEvApps, mkWpEvVarApps, mkWpTyLams,
  mkWpLams, mkWpLet, mkWpFun, mkWpCastN, mkWpCastR,
  collectHsWrapBinders,
  idHsWrapper, isIdHsWrapper,
  pprHsWrapper, hsWrapDictBinders,

  -- * Evidence bindings
  TcEvBinds(..), EvBindsVar(..),
  EvBindMap(..), emptyEvBindMap, extendEvBinds,
  lookupEvBind, evBindMapBinds,
  foldEvBindMap, nonDetStrictFoldEvBindMap,
  filterEvBindMap,
  isEmptyEvBindMap,
  evBindMapToVarSet,
  varSetMinusEvBindMap,
  EvBind(..), emptyTcEvBinds, isEmptyTcEvBinds, mkGivenEvBind, mkWantedEvBind,
  evBindVar, isCoEvBindsVar,

  -- * EvTerm (already a CoreExpr)
  EvTerm(..), EvExpr,
  evId, evCoercion, evCast, evDFunApp,  evDataConApp, evSelector,
  mkEvCast, evVarsOfTerm, mkEvScSelectors, evTypeable, findNeededEvVars,

  evTermCoercion, evTermCoercion_maybe,
  EvCallStack(..),
  EvTypeable(..),

  -- * HoleExprRef
  HoleExprRef(..),

  -- * TcCoercion
  TcCoercion, TcCoercionR, TcCoercionN, TcCoercionP, CoercionHole,
  TcMCoercion, TcMCoercionN, TcMCoercionR,
  Role(..), LeftOrRight(..), pickLR,
  mkTcReflCo, mkTcNomReflCo, mkTcRepReflCo,
  mkTcTyConAppCo, mkTcAppCo, mkTcFunCo,
  mkTcAxInstCo, mkTcUnbranchedAxInstCo, mkTcForAllCo, mkTcForAllCos,
  mkTcSymCo, mkTcSymMCo,
  mkTcTransCo,
  mkTcNthCo, mkTcLRCo, mkTcSubCo, maybeTcSymCo,
  maybeTcSubCo, tcDowngradeRole,
  mkTcAxiomRuleCo, mkTcGReflRightCo, mkTcGReflRightMCo, mkTcGReflLeftCo, mkTcGReflLeftMCo,
  mkTcPhantomCo,
  mkTcCoherenceLeftCo,
  mkTcCoherenceRightCo,
  mkTcKindCo,
  tcCoercionKind,
  mkTcCoVarCo,
  mkTcFamilyTyConAppCo,
  isTcReflCo, isTcReflexiveCo,
  tcCoercionRole,
  unwrapIP, wrapIP,

  -- * QuoteWrapper
  QuoteWrapper(..), applyQuoteWrapper, quoteWrapperTyVarTy
  ) where

import GHC.Prelude

import GHC.Types.Unique.DFM
import GHC.Types.Unique.FM
import GHC.Types.Var
import GHC.Core.Coercion.Axiom
import GHC.Core.Coercion
import GHC.Core.Ppr ()   -- Instance OutputableBndr TyVar
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.DataCon ( DataCon, dataConWrapId )
import GHC.Builtin.Names
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Core.Predicate
import GHC.Data.Pair
import GHC.Types.Basic

import GHC.Core
import GHC.Core.Class (Class, classSCSelId )
import GHC.Core.FVs   ( exprSomeFreeVars )

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable

import GHC.Data.Bag
import GHC.Data.FastString

import qualified Data.Data as Data
import GHC.Types.SrcLoc
import Data.IORef( IORef )
import GHC.Types.Unique.Set
import GHC.Core.Multiplicity

import qualified Data.Semigroup as S

{-
Note [TcCoercions]
~~~~~~~~~~~~~~~~~~
| TcCoercions are a hack used by the typechecker. Normally,
Coercions have free variables of type (a ~# b): we call these
CoVars. However, the type checker passes around equality evidence
(boxed up) at type (a ~ b).

An TcCoercion is simply a Coercion whose free variables have may be either
boxed or unboxed. After we are done with typechecking the desugarer finds the
boxed free variables, unboxes them, and creates a resulting real Coercion with
kosher free variables.

-}

type TcCoercion  = Coercion
type TcCoercionN = CoercionN    -- A Nominal          coercion ~N
type TcCoercionR = CoercionR    -- A Representational coercion ~R
type TcCoercionP = CoercionP    -- a phantom coercion
type TcMCoercion  = MCoercion
type TcMCoercionN = MCoercionN  -- nominal
type TcMCoercionR = MCoercionR  -- representational

mkTcReflCo             :: Role -> TcType -> TcCoercion
mkTcSymCo              :: TcCoercion -> TcCoercion
mkTcSymMCo             :: TcMCoercion -> TcMCoercion
mkTcTransCo            :: TcCoercion -> TcCoercion -> TcCoercion
mkTcNomReflCo          :: TcType -> TcCoercionN
mkTcRepReflCo          :: TcType -> TcCoercionR
mkTcTyConAppCo         :: Role -> TyCon -> [TcCoercion] -> TcCoercion
mkTcAppCo              :: TcCoercion -> TcCoercionN -> TcCoercion
mkTcFunCo              :: Role -> TcCoercion -> TcCoercion -> TcCoercion -> TcCoercion
mkTcAxInstCo           :: Role -> CoAxiom br -> BranchIndex
                       -> [TcType] -> [TcCoercion] -> TcCoercion
mkTcUnbranchedAxInstCo :: CoAxiom Unbranched -> [TcType]
                       -> [TcCoercion] -> TcCoercionR
mkTcForAllCo           :: TyVar -> TcCoercionN -> TcCoercion -> TcCoercion
mkTcForAllCos          :: [(TyVar, TcCoercionN)] -> TcCoercion -> TcCoercion
mkTcNthCo              :: Role -> Int -> TcCoercion -> TcCoercion
mkTcLRCo               :: LeftOrRight -> TcCoercion -> TcCoercion
mkTcSubCo              :: HasDebugCallStack => TcCoercionN -> TcCoercionR
tcDowngradeRole        :: Role -> Role -> TcCoercion -> TcCoercion
mkTcAxiomRuleCo        :: CoAxiomRule -> [TcCoercion] -> TcCoercionR
mkTcGReflRightCo       :: Role -> TcType -> TcCoercionN -> TcCoercion
mkTcGReflRightMCo      :: Role -> TcType -> TcMCoercionN -> TcCoercion
mkTcGReflLeftCo        :: Role -> TcType -> TcCoercionN -> TcCoercion
mkTcGReflLeftMCo       :: Role -> TcType -> TcMCoercionN -> TcCoercion
mkTcCoherenceLeftCo    :: Role -> TcType -> TcCoercionN
                       -> TcCoercion -> TcCoercion
mkTcCoherenceRightCo   :: Role -> TcType -> TcCoercionN
                       -> TcCoercion -> TcCoercion
mkTcPhantomCo          :: TcCoercionN -> TcType -> TcType -> TcCoercionP
mkTcKindCo             :: TcCoercion -> TcCoercionN
mkTcCoVarCo            :: CoVar -> TcCoercion
mkTcFamilyTyConAppCo   :: TyCon -> [TcCoercionN] -> TcCoercionN

tcCoercionKind         :: TcCoercion -> Pair TcType
tcCoercionRole         :: TcCoercion -> Role
isTcReflCo             :: TcCoercion -> Bool

-- | This version does a slow check, calculating the related types and seeing
-- if they are equal.
isTcReflexiveCo        :: TcCoercion -> Bool

mkTcReflCo             = mkReflCo
mkTcSymCo              = mkSymCo
mkTcSymMCo             = mkSymMCo
mkTcTransCo            = mkTransCo
mkTcNomReflCo          = mkNomReflCo
mkTcRepReflCo          = mkRepReflCo
mkTcTyConAppCo         = mkTyConAppCo
mkTcAppCo              = mkAppCo
mkTcFunCo              = mkFunCo
mkTcAxInstCo           = mkAxInstCo
mkTcUnbranchedAxInstCo = mkUnbranchedAxInstCo Representational
mkTcForAllCo           = mkForAllCo
mkTcForAllCos          = mkForAllCos
mkTcNthCo              = mkNthCo
mkTcLRCo               = mkLRCo
mkTcSubCo              = mkSubCo
tcDowngradeRole        = downgradeRole
mkTcAxiomRuleCo        = mkAxiomRuleCo
mkTcGReflRightCo       = mkGReflRightCo
mkTcGReflRightMCo      = mkGReflRightMCo
mkTcGReflLeftCo        = mkGReflLeftCo
mkTcGReflLeftMCo       = mkGReflLeftMCo
mkTcCoherenceLeftCo    = mkCoherenceLeftCo
mkTcCoherenceRightCo   = mkCoherenceRightCo
mkTcPhantomCo          = mkPhantomCo
mkTcKindCo             = mkKindCo
mkTcCoVarCo            = mkCoVarCo
mkTcFamilyTyConAppCo   = mkFamilyTyConAppCo

tcCoercionKind         = coercionKind
tcCoercionRole         = coercionRole
isTcReflCo             = isReflCo
isTcReflexiveCo        = isReflexiveCo

-- | If the EqRel is ReprEq, makes a SubCo; otherwise, does nothing.
-- Note that the input coercion should always be nominal.
maybeTcSubCo :: HasDebugCallStack => EqRel -> TcCoercionN -> TcCoercion
maybeTcSubCo NomEq  = id
maybeTcSubCo ReprEq = mkTcSubCo

-- | If a 'SwapFlag' is 'IsSwapped', flip the orientation of a coercion
maybeTcSymCo :: SwapFlag -> TcCoercion -> TcCoercion
maybeTcSymCo IsSwapped  co = mkTcSymCo co
maybeTcSymCo NotSwapped co = co

{-
%************************************************************************
%*                                                                      *
                  HsWrapper
*                                                                      *
************************************************************************
-}

data HsWrapper
  = WpHole                      -- The identity coercion

  | WpCompose HsWrapper HsWrapper
       -- (wrap1 `WpCompose` wrap2)[e] = wrap1[ wrap2[ e ]]
       --
       -- Hence  (\a. []) `WpCompose` (\b. []) = (\a b. [])
       -- But    ([] a)   `WpCompose` ([] b)   = ([] b a)

  | WpFun HsWrapper HsWrapper (Scaled TcTypeFRR)
       -- (WpFun wrap1 wrap2 (w, t1))[e] = \(x:_w t1). wrap2[ e wrap1[x] ]
       -- So note that if  wrap1 :: exp_arg <= act_arg
       --                  wrap2 :: act_res <= exp_res
       --           then   WpFun wrap1 wrap2 : (act_arg -> arg_res) <= (exp_arg -> exp_res)
       -- This isn't the same as for mkFunCo, but it has to be this way
       -- because we can't use 'sym' to flip around these HsWrappers
       -- The TcType is the "from" type of the first wrapper
       --
       -- Use 'mkWpFun' to construct such a wrapper.

  | WpCast TcCoercionR        -- A cast:  [] `cast` co
                              -- Guaranteed not the identity coercion
                              -- At role Representational

        -- Evidence abstraction and application
        -- (both dictionaries and coercions)
  | WpEvLam EvVar               -- \d. []       the 'd' is an evidence variable
  | WpEvApp EvTerm              -- [] d         the 'd' is evidence for a constraint
        -- Kind and Type abstraction and application
  | WpTyLam TyVar       -- \a. []  the 'a' is a type/kind variable (not coercion var)
  | WpTyApp KindOrType  -- [] t    the 't' is a type (not coercion)


  | WpLet TcEvBinds             -- Non-empty (or possibly non-empty) evidence bindings,
                                -- so that the identity coercion is always exactly WpHole

  | WpMultCoercion Coercion     -- Require that a Coercion be reflexive; otherwise,
                                -- error in the desugarer. See GHC.Tc.Utils.Unify
                                -- Note [Wrapper returned from tcSubMult]
  deriving Data.Data

-- | The Semigroup instance is a bit fishy, since @WpCompose@, as a data
-- constructor, is "syntactic" and not associative. Concretely, if @a@, @b@,
-- and @c@ aren't @WpHole@:
--
-- > (a <> b) <> c ?= a <> (b <> c)
--
-- ==>
--
-- > (a `WpCompose` b) `WpCompose` c /= @ a `WpCompose` (b `WpCompose` c)
--
-- However these two associations are are "semantically equal" in the sense
-- that they produce equal functions when passed to
-- @GHC.HsToCore.Binds.dsHsWrapper@.
instance S.Semigroup HsWrapper where
  (<>) = (<.>)

instance Monoid HsWrapper where
  mempty = WpHole

(<.>) :: HsWrapper -> HsWrapper -> HsWrapper
WpHole <.> c = c
c <.> WpHole = c
c1 <.> c2    = c1 `WpCompose` c2

-- | Smart constructor to create a 'WpFun' 'HsWrapper'.
--
-- PRECONDITION: the "from" type of the first wrapper must have a syntactically
-- fixed RuntimeRep (see Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete).
mkWpFun :: HsWrapper -> HsWrapper
        -> Scaled TcTypeFRR -- ^ the "from" type of the first wrapper
                            -- MUST have a fixed RuntimeRep
        -> TcType           -- ^ either type of the second wrapper (used only when the
                            -- second wrapper is the identity)
        -> HsWrapper
  -- NB: we can't check that the argument type has a fixed RuntimeRep with an assertion,
  -- because of [Wrinkle: Typed Template Haskell] in Note [hasFixedRuntimeRep]
  -- in GHC.Tc.Utils.Concrete.
mkWpFun WpHole       WpHole       _             _  = WpHole
mkWpFun WpHole       (WpCast co2) (Scaled w t1) _  = WpCast (mkTcFunCo Representational (multToCo w) (mkTcRepReflCo t1) co2)
mkWpFun (WpCast co1) WpHole       (Scaled w _)  t2 = WpCast (mkTcFunCo Representational (multToCo w) (mkTcSymCo co1) (mkTcRepReflCo t2))
mkWpFun (WpCast co1) (WpCast co2) (Scaled w _)  _  = WpCast (mkTcFunCo Representational (multToCo w) (mkTcSymCo co1) co2)
mkWpFun co1          co2          t1            _  = WpFun co1 co2 t1

mkWpCastR :: TcCoercionR -> HsWrapper
mkWpCastR co
  | isTcReflCo co = WpHole
  | otherwise     = assertPpr (tcCoercionRole co == Representational) (ppr co) $
                    WpCast co

mkWpCastN :: TcCoercionN -> HsWrapper
mkWpCastN co
  | isTcReflCo co = WpHole
  | otherwise     = assertPpr (tcCoercionRole co == Nominal) (ppr co) $
                    WpCast (mkTcSubCo co)
    -- The mkTcSubCo converts Nominal to Representational

mkWpTyApps :: [Type] -> HsWrapper
mkWpTyApps tys = mk_co_app_fn WpTyApp tys

mkWpEvApps :: [EvTerm] -> HsWrapper
mkWpEvApps args = mk_co_app_fn WpEvApp args

mkWpEvVarApps :: [EvVar] -> HsWrapper
mkWpEvVarApps vs = mk_co_app_fn WpEvApp (map (EvExpr . evId) vs)

mkWpTyLams :: [TyVar] -> HsWrapper
mkWpTyLams ids = mk_co_lam_fn WpTyLam ids

mkWpLams :: [Var] -> HsWrapper
mkWpLams ids = mk_co_lam_fn WpEvLam ids

mkWpLet :: TcEvBinds -> HsWrapper
-- This no-op is a quite a common case
mkWpLet (EvBinds b) | isEmptyBag b = WpHole
mkWpLet ev_binds                   = WpLet ev_binds

mk_co_lam_fn :: (a -> HsWrapper) -> [a] -> HsWrapper
mk_co_lam_fn f as = foldr (\x wrap -> f x <.> wrap) WpHole as

mk_co_app_fn :: (a -> HsWrapper) -> [a] -> HsWrapper
-- For applications, the *first* argument must
-- come *last* in the composition sequence
mk_co_app_fn f as = foldr (\x wrap -> wrap <.> f x) WpHole as

idHsWrapper :: HsWrapper
idHsWrapper = WpHole

isIdHsWrapper :: HsWrapper -> Bool
isIdHsWrapper WpHole = True
isIdHsWrapper _      = False

hsWrapDictBinders :: HsWrapper -> Bag DictId
-- ^ Identifies the /lambda-bound/ dictionaries of an 'HsWrapper'. This is used
-- (only) to allow the pattern-match overlap checker to know what Given
-- dictionaries are in scope.
--
-- We specifically do not collect dictionaries bound in a 'WpLet'. These are
-- either superclasses of lambda-bound ones, or (extremely numerous) results of
-- binding Wanted dictionaries.  We definitely don't want all those cluttering
-- up the Given dictionaries for pattern-match overlap checking!
hsWrapDictBinders wrap = go wrap
 where
   go (WpEvLam dict_id)   = unitBag dict_id
   go (w1 `WpCompose` w2) = go w1 `unionBags` go w2
   go (WpFun _ w _)       = go w
   go WpHole              = emptyBag
   go (WpCast  {})        = emptyBag
   go (WpEvApp {})        = emptyBag
   go (WpTyLam {})        = emptyBag
   go (WpTyApp {})        = emptyBag
   go (WpLet   {})        = emptyBag
   go (WpMultCoercion {}) = emptyBag

collectHsWrapBinders :: HsWrapper -> ([Var], HsWrapper)
-- Collect the outer lambda binders of a HsWrapper,
-- stopping as soon as you get to a non-lambda binder
collectHsWrapBinders wrap = go wrap []
  where
    -- go w ws = collectHsWrapBinders (w <.> w1 <.> ... <.> wn)
    go :: HsWrapper -> [HsWrapper] -> ([Var], HsWrapper)
    go (WpEvLam v)       wraps = add_lam v (gos wraps)
    go (WpTyLam v)       wraps = add_lam v (gos wraps)
    go (WpCompose w1 w2) wraps = go w1 (w2:wraps)
    go wrap              wraps = ([], foldl' (<.>) wrap wraps)

    gos []     = ([], WpHole)
    gos (w:ws) = go w ws

    add_lam v (vs,w) = (v:vs, w)

{-
************************************************************************
*                                                                      *
                  Evidence bindings
*                                                                      *
************************************************************************
-}

data TcEvBinds
  = TcEvBinds           -- Mutable evidence bindings
       EvBindsVar       -- Mutable because they are updated "later"
                        --    when an implication constraint is solved

  | EvBinds             -- Immutable after zonking
       (Bag EvBind)

data EvBindsVar
  = EvBindsVar {
      ebv_uniq :: Unique,
         -- The Unique is for debug printing only

      ebv_binds :: IORef EvBindMap,
      -- The main payload: the value-level evidence bindings
      --     (dictionaries etc)
      -- Some Given, some Wanted

      ebv_tcvs :: IORef CoVarSet
      -- The free Given coercion vars needed by Wanted coercions that
      -- are solved by filling in their HoleDest in-place. Since they
      -- don't appear in ebv_binds, we keep track of their free
      -- variables so that we can report unused given constraints
      -- See Note [Tracking redundant constraints] in GHC.Tc.Solver
    }

  | CoEvBindsVar {  -- See Note [Coercion evidence only]

      -- See above for comments on ebv_uniq, ebv_tcvs
      ebv_uniq :: Unique,
      ebv_tcvs :: IORef CoVarSet
    }

instance Data.Data TcEvBinds where
  -- Placeholder; we can't travers into TcEvBinds
  toConstr _   = abstractConstr "TcEvBinds"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = Data.mkNoRepType "TcEvBinds"

{- Note [Coercion evidence only]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Class constraints etc give rise to /term/ bindings for evidence, and
we have nowhere to put term bindings in /types/.  So in some places we
use CoEvBindsVar (see newCoTcEvBinds) to signal that no term-level
evidence bindings are allowed.  Notebly ():

  - Places in types where we are solving kind constraints (all of which
    are equalities); see solveEqualities

  - When unifying forall-types
-}

isCoEvBindsVar :: EvBindsVar -> Bool
isCoEvBindsVar (CoEvBindsVar {}) = True
isCoEvBindsVar (EvBindsVar {})   = False

-----------------
newtype EvBindMap
  = EvBindMap {
       ev_bind_varenv :: DVarEnv EvBind
    }       -- Map from evidence variables to evidence terms
            -- We use @DVarEnv@ here to get deterministic ordering when we
            -- turn it into a Bag.
            -- If we don't do that, when we generate let bindings for
            -- dictionaries in dsTcEvBinds they will be generated in random
            -- order.
            --
            -- For example:
            --
            -- let $dEq = GHC.Classes.$fEqInt in
            -- let $$dNum = GHC.Num.$fNumInt in ...
            --
            -- vs
            --
            -- let $dNum = GHC.Num.$fNumInt in
            -- let $dEq = GHC.Classes.$fEqInt in ...
            --
            -- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for explanation why
            -- @UniqFM@ can lead to nondeterministic order.

emptyEvBindMap :: EvBindMap
emptyEvBindMap = EvBindMap { ev_bind_varenv = emptyDVarEnv }

extendEvBinds :: EvBindMap -> EvBind -> EvBindMap
extendEvBinds bs ev_bind
  = EvBindMap { ev_bind_varenv = extendDVarEnv (ev_bind_varenv bs)
                                               (eb_lhs ev_bind)
                                               ev_bind }

isEmptyEvBindMap :: EvBindMap -> Bool
isEmptyEvBindMap (EvBindMap m) = isEmptyDVarEnv m

lookupEvBind :: EvBindMap -> EvVar -> Maybe EvBind
lookupEvBind bs = lookupDVarEnv (ev_bind_varenv bs)

evBindMapBinds :: EvBindMap -> Bag EvBind
evBindMapBinds = foldEvBindMap consBag emptyBag

foldEvBindMap :: (EvBind -> a -> a) -> a -> EvBindMap -> a
foldEvBindMap k z bs = foldDVarEnv k z (ev_bind_varenv bs)

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetStrictFoldEvBindMap :: (EvBind -> a -> a) -> a -> EvBindMap -> a
nonDetStrictFoldEvBindMap k z bs = nonDetStrictFoldDVarEnv k z (ev_bind_varenv bs)

filterEvBindMap :: (EvBind -> Bool) -> EvBindMap -> EvBindMap
filterEvBindMap k (EvBindMap { ev_bind_varenv = env })
  = EvBindMap { ev_bind_varenv = filterDVarEnv k env }

evBindMapToVarSet :: EvBindMap -> VarSet
evBindMapToVarSet (EvBindMap dve) = unsafeUFMToUniqSet (mapUFM evBindVar (udfmToUfm dve))

varSetMinusEvBindMap :: VarSet -> EvBindMap -> VarSet
varSetMinusEvBindMap vs (EvBindMap dve) = vs `uniqSetMinusUDFM` dve

instance Outputable EvBindMap where
  ppr (EvBindMap m) = ppr m

-----------------
-- All evidence is bound by EvBinds; no side effects
data EvBind
  = EvBind { eb_lhs      :: EvVar
           , eb_rhs      :: EvTerm
           , eb_is_given :: Bool  -- True <=> given
                 -- See Note [Tracking redundant constraints] in GHC.Tc.Solver
    }

evBindVar :: EvBind -> EvVar
evBindVar = eb_lhs

mkWantedEvBind :: EvVar -> EvTerm -> EvBind
mkWantedEvBind ev tm = EvBind { eb_is_given = False, eb_lhs = ev, eb_rhs = tm }

-- EvTypeable are never given, so we can work with EvExpr here instead of EvTerm
mkGivenEvBind :: EvVar -> EvTerm -> EvBind
mkGivenEvBind ev tm = EvBind { eb_is_given = True, eb_lhs = ev, eb_rhs = tm }


-- An EvTerm is, conceptually, a CoreExpr that implements the constraint.
-- Unfortunately, we cannot just do
--   type EvTerm  = CoreExpr
-- Because of staging problems issues around EvTypeable
data EvTerm
  = EvExpr EvExpr

  | EvTypeable Type EvTypeable   -- Dictionary for (Typeable ty)

  | EvFun     -- /\as \ds. let binds in v
      { et_tvs   :: [TyVar]
      , et_given :: [EvVar]
      , et_binds :: TcEvBinds -- This field is why we need an EvFun
                              -- constructor, and can't just use EvExpr
      , et_body  :: EvVar }

  deriving Data.Data

type EvExpr = CoreExpr

-- An EvTerm is (usually) constructed by any of the constructors here
-- and those more complicates ones who were moved to module GHC.Tc.Types.EvTerm

-- | Any sort of evidence Id, including coercions
evId ::  EvId -> EvExpr
evId = Var

-- coercion bindings
-- See Note [Coercion evidence terms]
evCoercion :: TcCoercion -> EvTerm
evCoercion co = EvExpr (Coercion co)

-- | d |> co
evCast :: EvExpr -> TcCoercion -> EvTerm
evCast et tc | isReflCo tc = EvExpr et
             | otherwise   = EvExpr (Cast et tc)

-- Dictionary instance application
evDFunApp :: DFunId -> [Type] -> [EvExpr] -> EvTerm
evDFunApp df tys ets = EvExpr $ Var df `mkTyApps` tys `mkApps` ets

evDataConApp :: DataCon -> [Type] -> [EvExpr] -> EvTerm
evDataConApp dc tys ets = evDFunApp (dataConWrapId dc) tys ets

-- Selector id plus the types at which it
-- should be instantiated, used for HasField
-- dictionaries; see Note [HasField instances]
-- in TcInterface
evSelector :: Id -> [Type] -> [EvExpr] -> EvExpr
evSelector sel_id tys tms = Var sel_id `mkTyApps` tys `mkApps` tms

-- Dictionary for (Typeable ty)
evTypeable :: Type -> EvTypeable -> EvTerm
evTypeable = EvTypeable

-- | Instructions on how to make a 'Typeable' dictionary.
-- See Note [Typeable evidence terms]
data EvTypeable
  = EvTypeableTyCon TyCon [EvTerm]
    -- ^ Dictionary for @Typeable T@ where @T@ is a type constructor with all of
    -- its kind variables saturated. The @[EvTerm]@ is @Typeable@ evidence for
    -- the applied kinds..

  | EvTypeableTyApp EvTerm EvTerm
    -- ^ Dictionary for @Typeable (s t)@,
    -- given a dictionaries for @s@ and @t@.

  | EvTypeableTrFun EvTerm EvTerm EvTerm
    -- ^ Dictionary for @Typeable (s % w -> t)@,
    -- given a dictionaries for @w@, @s@, and @t@.

  | EvTypeableTyLit EvTerm
    -- ^ Dictionary for a type literal,
    -- e.g. @Typeable "foo"@ or @Typeable 3@
    -- The 'EvTerm' is evidence of, e.g., @KnownNat 3@
    -- (see #10348)
  deriving Data.Data

-- | Evidence for @CallStack@ implicit parameters.
data EvCallStack
  -- See Note [Overview of implicit CallStacks]
  = EvCsEmpty
  | EvCsPushCall
        FastString   -- Usually the name of the function being called
                     --   but can also be "the literal 42"
                     --   or "an if-then-else expression", etc
        RealSrcSpan  -- Location of the call
        EvExpr       -- Rest of the stack
    -- ^ @EvCsPushCall origin loc stk@ represents a call from @origin@,
    --  occurring at @loc@, in a calling context @stk@.
  deriving Data.Data

{-
************************************************************************
*                                                                      *
         Evidence for holes
*                                                                      *
************************************************************************
-}

-- | Where to store evidence for expression holes
-- See Note [Holes] in GHC.Tc.Types.Constraint
data HoleExprRef = HER (IORef EvTerm)   -- ^ where to write the erroring expression
                       TcType           -- ^ expected type of that expression
                       Unique           -- ^ for debug output only

instance Outputable HoleExprRef where
  ppr (HER _ _ u) = ppr u

instance Data.Data HoleExprRef where
  -- Placeholder; we can't traverse into HoleExprRef
  toConstr _   = abstractConstr "HoleExprRef"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = Data.mkNoRepType "HoleExprRef"

{-
Note [Typeable evidence terms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The EvTypeable data type looks isomorphic to Type, but the EvTerms
inside can be EvIds.  Eg
    f :: forall a. Typeable a => a -> TypeRep
    f x = typeRep (undefined :: Proxy [a])
Here for the (Typeable [a]) dictionary passed to typeRep we make
evidence
    dl :: Typeable [a] = EvTypeable [a]
                            (EvTypeableTyApp (EvTypeableTyCon []) (EvId d))
where
    d :: Typable a
is the lambda-bound dictionary passed into f.

Note [Coercion evidence terms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "coercion evidence term" takes one of these forms
   co_tm ::= EvId v           where v :: t1 ~# t2
           | EvCoercion co
           | EvCast co_tm co

We do quite often need to get a TcCoercion from an EvTerm; see
'evTermCoercion'.

INVARIANT: The evidence for any constraint with type (t1 ~# t2) is
a coercion evidence term.  Consider for example
    [G] d :: F Int a
If we have
    ax7 a :: F Int a ~ (a ~ Bool)
then we do NOT generate the constraint
    [G] (d |> ax7 a) :: a ~ Bool
because that does not satisfy the invariant (d is not a coercion variable).
Instead we make a binding
    g1 :: a~Bool = g |> ax7 a
and the constraint
    [G] g1 :: a~Bool
See #7238 and Note [Bind new Givens immediately] in GHC.Tc.Types.Constraint

Note [EvBinds/EvTerm]
~~~~~~~~~~~~~~~~~~~~~
How evidence is created and updated. Bindings for dictionaries,
and coercions and implicit parameters are carried around in TcEvBinds
which during constraint generation and simplification is always of the
form (TcEvBinds ref). After constraint simplification is finished it
will be transformed to t an (EvBinds ev_bag).

Evidence for coercions *SHOULD* be filled in using the TcEvBinds
However, all EvVars that correspond to *wanted* coercion terms in
an EvBind must be mutable variables so that they can be readily
inlined (by zonking) after constraint simplification is finished.

Conclusion: a new wanted coercion variable should be made mutable.
[Notice though that evidence variables that bind coercion terms
 from super classes will be "given" and hence rigid]


Note [Overview of implicit CallStacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See https://gitlab.haskell.org/ghc/ghc/wikis/explicit-call-stack/implicit-locations)

The goal of CallStack evidence terms is to reify locations
in the program source as runtime values, without any support
from the RTS. We accomplish this by assigning a special meaning
to constraints of type GHC.Stack.Types.HasCallStack, an alias

  type HasCallStack = (?callStack :: CallStack)

Implicit parameters of type GHC.Stack.Types.CallStack (the name is not
important) are solved in three steps:

1. Explicit, user-written occurrences of `?stk :: CallStack`
   which have IPOccOrigin, are solved directly from the given IP,
   just like a regular IP; see GHC.Tc.Solver.Interact.interactDict.

   For example, the occurrence of `?stk` in

     error :: (?stk :: CallStack) => String -> a
     error s = raise (ErrorCall (s ++ prettyCallStack ?stk))

   will be solved for the `?stk` in `error`s context as before.

2. In a function call, instead of simply passing the given IP, we first
   append the current call-site to it. For example, consider a
   call to the callstack-aware `error` above.

     foo :: (?stk :: CallStack) => a
     foo = error "undefined!"

   Here we want to take the given `?stk` and append the current
   call-site, before passing it to `error`. In essence, we want to
   rewrite `foo "undefined!"` to

     let ?stk = pushCallStack <foo's location> ?stk
     in foo "undefined!"

   We achieve this as follows:

   * At a call of foo :: (?stk :: CallStack) => blah
     we emit a Wanted
        [W] d1 : IP "stk" CallStack
     with CtOrigin = OccurrenceOf "foo"

   * We /solve/ this constraint, in GHC.Tc.Solver.Canonical.canClassNC
     by emitting a NEW Wanted
        [W] d2 :: IP "stk" CallStack
     with CtOrigin = IPOccOrigin

     and solve d1 = EvCsPushCall "foo" <foo's location> (EvId d1)

   * The new Wanted, for `d2` will be solved per rule (1), ie as a regular IP.

3. We use the predicate isPushCallStackOrigin to identify whether we
   want to do (1) solve directly, or (2) push and then solve directly.
   Key point (see #19918): the CtOrigin where we want to push an item on the
   call stack can include IfThenElseOrigin etc, when RebindableSyntax is
   involved.  See the defn of fun_orig in GHC.Tc.Gen.App.tcInstFun; it is
   this CtOrigin that is pinned on the constraints generated by functions
   in the "expansion" for rebindable syntax. c.f. GHC.Rename.Expr
   Note [Handling overloaded and rebindable constructs]

4. We default any insoluble CallStacks to the empty CallStack. Suppose
   `undefined` did not request a CallStack, ie

     undefinedNoStk :: a
     undefinedNoStk = error "undefined!"

   Under the usual IP rules, the new wanted from rule (2) would be
   insoluble as there's no given IP from which to solve it, so we
   would get an "unbound implicit parameter" error.

   We don't ever want to emit an insoluble CallStack IP, so we add a
   defaulting pass to default any remaining wanted CallStacks to the
   empty CallStack with the evidence term

     EvCsEmpty

   (see GHC.Tc.Solver.simplifyTopWanteds and GHC.Tc.Solver.defaultCallStacks)

This provides a lightweight mechanism for building up call-stacks
explicitly, but is notably limited by the fact that the stack will
stop at the first function whose type does not include a CallStack IP.
For example, using the above definition of `undefined`:

  head :: [a] -> a
  head []    = undefined
  head (x:_) = x

  g = head []

the resulting CallStack will include the call to `undefined` in `head`
and the call to `error` in `undefined`, but *not* the call to `head`
in `g`, because `head` did not explicitly request a CallStack.


Important Details:
- GHC should NEVER report an insoluble CallStack constraint.

- GHC should NEVER infer a CallStack constraint unless one was requested
  with a partial type signature (See GHC.Tc.Solver..pickQuantifiablePreds).

- A CallStack (defined in GHC.Stack.Types) is a [(String, SrcLoc)],
  where the String is the name of the binder that is used at the
  SrcLoc. SrcLoc is also defined in GHC.Stack.Types and contains the
  package/module/file name, as well as the full source-span. Both
  CallStack and SrcLoc are kept abstract so only GHC can construct new
  values.

- We will automatically solve any wanted CallStack regardless of the
  name of the IP, i.e.

    f = show (?stk :: CallStack)
    g = show (?loc :: CallStack)

  are both valid. However, we will only push new SrcLocs onto existing
  CallStacks when the IP names match, e.g. in

    head :: (?loc :: CallStack) => [a] -> a
    head [] = error (show (?stk :: CallStack))

  the printed CallStack will NOT include head's call-site. This reflects the
  standard scoping rules of implicit-parameters.

- An EvCallStack term desugars to a CoreExpr of type `IP "some str" CallStack`.
  The desugarer will need to unwrap the IP newtype before pushing a new
  call-site onto a given stack (See GHC.HsToCore.Binds.dsEvCallStack)

- When we emit a new wanted CallStack from rule (2) we set its origin to
  `IPOccOrigin ip_name` instead of the original `OccurrenceOf func`
  (see GHC.Tc.Solver.Interact.interactDict).

  This is a bit shady, but is how we ensure that the new wanted is
  solved like a regular IP.

-}

mkEvCast :: EvExpr -> TcCoercion -> EvTerm
mkEvCast ev lco
  | assertPpr (tcCoercionRole lco == Representational)
              (vcat [text "Coercion of wrong role passed to mkEvCast:", ppr ev, ppr lco]) $
    isTcReflCo lco = EvExpr ev
  | otherwise      = evCast ev lco


mkEvScSelectors         -- Assume   class (..., D ty, ...) => C a b
  :: Class -> [TcType]  -- C ty1 ty2
  -> [(TcPredType,      -- D ty[ty1/a,ty2/b]
       EvExpr)          -- :: C ty1 ty2 -> D ty[ty1/a,ty2/b]
     ]
mkEvScSelectors cls tys
   = zipWith mk_pr (immSuperClasses cls tys) [0..]
  where
    mk_pr pred i = (pred, Var sc_sel_id `mkTyApps` tys)
      where
        sc_sel_id  = classSCSelId cls i -- Zero-indexed

emptyTcEvBinds :: TcEvBinds
emptyTcEvBinds = EvBinds emptyBag

isEmptyTcEvBinds :: TcEvBinds -> Bool
isEmptyTcEvBinds (EvBinds b)    = isEmptyBag b
isEmptyTcEvBinds (TcEvBinds {}) = panic "isEmptyTcEvBinds"

evTermCoercion_maybe :: EvTerm -> Maybe TcCoercion
-- Applied only to EvTerms of type (s~t)
-- See Note [Coercion evidence terms]
evTermCoercion_maybe ev_term
  | EvExpr e <- ev_term = go e
  | otherwise           = Nothing
  where
    go :: EvExpr -> Maybe TcCoercion
    go (Var v)       = return (mkCoVarCo v)
    go (Coercion co) = return co
    go (Cast tm co)  = do { co' <- go tm
                          ; return (mkCoCast co' co) }
    go _             = Nothing

evTermCoercion :: EvTerm -> TcCoercion
evTermCoercion tm = case evTermCoercion_maybe tm of
                      Just co -> co
                      Nothing -> pprPanic "evTermCoercion" (ppr tm)


{- *********************************************************************
*                                                                      *
                  Free variables
*                                                                      *
********************************************************************* -}

findNeededEvVars :: EvBindMap -> VarSet -> VarSet
-- Find all the Given evidence needed by seeds,
-- looking transitively through binds
findNeededEvVars ev_binds seeds
  = transCloVarSet also_needs seeds
  where
   also_needs :: VarSet -> VarSet
   also_needs needs = nonDetStrictFoldUniqSet add emptyVarSet needs
     -- It's OK to use a non-deterministic fold here because we immediately
     -- forget about the ordering by creating a set

   add :: Var -> VarSet -> VarSet
   add v needs
     | Just ev_bind <- lookupEvBind ev_binds v
     , EvBind { eb_is_given = is_given, eb_rhs = rhs } <- ev_bind
     , is_given
     = evVarsOfTerm rhs `unionVarSet` needs
     | otherwise
     = needs

evVarsOfTerm :: EvTerm -> VarSet
evVarsOfTerm (EvExpr e)         = exprSomeFreeVars isEvVar e
evVarsOfTerm (EvTypeable _ ev)  = evVarsOfTypeable ev
evVarsOfTerm (EvFun {})         = emptyVarSet -- See Note [Free vars of EvFun]

evVarsOfTerms :: [EvTerm] -> VarSet
evVarsOfTerms = mapUnionVarSet evVarsOfTerm

evVarsOfTypeable :: EvTypeable -> VarSet
evVarsOfTypeable ev =
  case ev of
    EvTypeableTyCon _ e      -> mapUnionVarSet evVarsOfTerm e
    EvTypeableTyApp e1 e2    -> evVarsOfTerms [e1,e2]
    EvTypeableTrFun em e1 e2 -> evVarsOfTerms [em,e1,e2]
    EvTypeableTyLit e        -> evVarsOfTerm e


{- Note [Free vars of EvFun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Finding the free vars of an EvFun is made tricky by the fact the
bindings et_binds may be a mutable variable.  Fortunately, we
can just squeeze by.  Here's how.

* evVarsOfTerm is used only by GHC.Tc.Solver.neededEvVars.
* Each EvBindsVar in an et_binds field of an EvFun is /also/ in the
  ic_binds field of an Implication
* So we can track usage via the processing for that implication,
  (see Note [Tracking redundant constraints] in GHC.Tc.Solver).
  We can ignore usage from the EvFun altogether.

************************************************************************
*                                                                      *
                  Pretty printing
*                                                                      *
************************************************************************
-}

instance Outputable HsWrapper where
  ppr co_fn = pprHsWrapper co_fn (no_parens (text "<>"))

pprHsWrapper :: HsWrapper -> (Bool -> SDoc) -> SDoc
-- With -fprint-typechecker-elaboration, print the wrapper
--   otherwise just print what's inside
-- The pp_thing_inside function takes Bool to say whether
--    it's in a position that needs parens for a non-atomic thing
pprHsWrapper wrap pp_thing_inside
  = sdocOption sdocPrintTypecheckerElaboration $ \case
      True  -> help pp_thing_inside wrap False
      False -> pp_thing_inside False
  where
    help :: (Bool -> SDoc) -> HsWrapper -> Bool -> SDoc
    -- True  <=> appears in function application position
    -- False <=> appears as body of let or lambda
    help it WpHole             = it
    help it (WpCompose f1 f2)  = help (help it f2) f1
    help it (WpFun f1 f2 (Scaled w t1)) = add_parens $ text "\\(x" <> dcolon <> brackets (ppr w) <> ppr t1 <> text ")." <+>
                                            help (\_ -> it True <+> help (\_ -> text "x") f1 True) f2 False
    help it (WpCast co)   = add_parens $ sep [it False, nest 2 (text "|>"
                                              <+> pprParendCo co)]
    help it (WpEvApp id)  = no_parens  $ sep [it True, nest 2 (ppr id)]
    help it (WpTyApp ty)  = no_parens  $ sep [it True, text "@" <> pprParendType ty]
    help it (WpEvLam id)  = add_parens $ sep [ text "\\" <> pprLamBndr id <> dot, it False]
    help it (WpTyLam tv)  = add_parens $ sep [text "/\\" <> pprLamBndr tv <> dot, it False]
    help it (WpLet binds) = add_parens $ sep [text "let" <+> braces (ppr binds), it False]
    help it (WpMultCoercion co)   = add_parens $ sep [it False, nest 2 (text "<multiplicity coercion>"
                                              <+> pprParendCo co)]

pprLamBndr :: Id -> SDoc
pprLamBndr v = pprBndr LambdaBind v

add_parens, no_parens :: SDoc -> Bool -> SDoc
add_parens d True  = parens d
add_parens d False = d
no_parens d _ = d

instance Outputable TcEvBinds where
  ppr (TcEvBinds v) = ppr v
  ppr (EvBinds bs)  = text "EvBinds" <> braces (vcat (map ppr (bagToList bs)))

instance Outputable EvBindsVar where
  ppr (EvBindsVar { ebv_uniq = u })
     = text "EvBindsVar" <> angleBrackets (ppr u)
  ppr (CoEvBindsVar { ebv_uniq = u })
     = text "CoEvBindsVar" <> angleBrackets (ppr u)

instance Uniquable EvBindsVar where
  getUnique = ebv_uniq

instance Outputable EvBind where
  ppr (EvBind { eb_lhs = v, eb_rhs = e, eb_is_given = is_given })
     = sep [ pp_gw <+> ppr v
           , nest 2 $ equals <+> ppr e ]
     where
       pp_gw = brackets (if is_given then char 'G' else char 'W')
   -- We cheat a bit and pretend EqVars are CoVars for the purposes of pretty printing

instance Outputable EvTerm where
  ppr (EvExpr e)         = ppr e
  ppr (EvTypeable ty ev) = ppr ev <+> dcolon <+> text "Typeable" <+> ppr ty
  ppr (EvFun { et_tvs = tvs, et_given = gs, et_binds = bs, et_body = w })
      = hang (text "\\" <+> sep (map pprLamBndr (tvs ++ gs)) <+> arrow)
           2 (ppr bs $$ ppr w)   -- Not very pretty

instance Outputable EvCallStack where
  ppr EvCsEmpty
    = text "[]"
  ppr (EvCsPushCall orig loc tm)
    = ppr (orig,loc) <+> text ":" <+> ppr tm

instance Outputable EvTypeable where
  ppr (EvTypeableTyCon ts _)  = text "TyCon" <+> ppr ts
  ppr (EvTypeableTyApp t1 t2) = parens (ppr t1 <+> ppr t2)
  ppr (EvTypeableTrFun tm t1 t2) = parens (ppr t1 <+> mulArrow (ppr tm) <+> ppr t2)
  ppr (EvTypeableTyLit t1)    = text "TyLit" <> ppr t1


----------------------------------------------------------------------
-- Helper functions for dealing with IP newtype-dictionaries
----------------------------------------------------------------------

-- | Create a 'Coercion' that unwraps an implicit-parameter
-- dictionary to expose the underlying value.
-- We expect the 'Type' to have the form `IP sym ty`,
-- and return a 'Coercion' `co :: IP sym ty ~ ty`
unwrapIP :: Type -> CoercionR
unwrapIP ty =
  case unwrapNewTyCon_maybe tc of
    Just (_,_,ax) -> mkUnbranchedAxInstCo Representational ax tys []
    Nothing       -> pprPanic "unwrapIP" $
                       text "The dictionary for" <+> quotes (ppr tc)
                         <+> text "is not a newtype!"
  where
  (tc, tys) = splitTyConApp ty

-- | Create a 'Coercion' that wraps a value in an implicit-parameter
-- dictionary. See 'unwrapIP'.
wrapIP :: Type -> CoercionR
wrapIP ty = mkSymCo (unwrapIP ty)

----------------------------------------------------------------------
-- A datatype used to pass information when desugaring quotations
----------------------------------------------------------------------

-- We have to pass a `EvVar` and `Type` into `dsBracket` so that the
-- correct evidence and types are applied to all the TH combinators.
-- This data type bundles them up together with some convenience methods.
--
-- The EvVar is evidence for `Quote m`
-- The Type is a metavariable for `m`
--
data QuoteWrapper = QuoteWrapper EvVar Type deriving Data.Data

quoteWrapperTyVarTy :: QuoteWrapper -> Type
quoteWrapperTyVarTy (QuoteWrapper _ t) = t

-- | Convert the QuoteWrapper into a normal HsWrapper which can be used to
-- apply its contents.
applyQuoteWrapper :: QuoteWrapper -> HsWrapper
applyQuoteWrapper (QuoteWrapper ev_var m_var)
  = mkWpEvVarApps [ev_var] <.> mkWpTyApps [m_var]
