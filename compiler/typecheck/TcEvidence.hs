-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module TcEvidence (

  -- HsWrapper
  HsWrapper(..),
  (<.>), mkWpTyApps, mkWpEvApps, mkWpEvVarApps, mkWpTyLams,
  mkWpLams, mkWpLet, mkWpCastN, mkWpCastR, collectHsWrapBinders,
  mkWpFun, mkWpFuns, idHsWrapper, isIdHsWrapper, pprHsWrapper,

  -- Evidence bindings
  TcEvBinds(..), EvBindsVar(..),
  EvBindMap(..), emptyEvBindMap, extendEvBinds,
  lookupEvBind, evBindMapBinds, foldEvBindMap, isEmptyEvBindMap,
  EvBind(..), emptyTcEvBinds, isEmptyTcEvBinds, mkGivenEvBind, mkWantedEvBind,
  sccEvBinds, evBindVar,
  EvTerm(..), mkEvCast, evVarsOfTerm, mkEvScSelectors,
  EvLit(..), evTermCoercion,
  EvCallStack(..),
  EvTypeable(..),

  -- TcCoercion
  TcCoercion, TcCoercionR, TcCoercionN, TcCoercionP, CoercionHole,
  Role(..), LeftOrRight(..), pickLR,
  mkTcReflCo, mkTcNomReflCo, mkTcRepReflCo,
  mkTcTyConAppCo, mkTcAppCo, mkTcFunCo,
  mkTcAxInstCo, mkTcUnbranchedAxInstCo, mkTcForAllCo, mkTcForAllCos,
  mkTcSymCo, mkTcTransCo, mkTcNthCo, mkTcLRCo, mkTcSubCo, maybeTcSubCo,
  tcDowngradeRole,
  mkTcAxiomRuleCo, mkTcCoherenceLeftCo, mkTcCoherenceRightCo, mkTcPhantomCo,
  mkTcKindCo,
  tcCoercionKind, coVarsOfTcCo,
  mkTcCoVarCo,
  isTcReflCo, isTcReflexiveCo,
  tcCoercionRole,
  unwrapIP, wrapIP
  ) where
#include "HsVersions.h"

import Var
import CoAxiom
import Coercion
import PprCore ()   -- Instance OutputableBndr TyVar
import TcType
import Type
import TyCon
import Class( Class )
import PrelNames
import DynFlags   ( gopt, GeneralFlag(Opt_PrintTypecheckerElaboration) )
import VarEnv
import VarSet
import Name
import Pair

import Util
import Bag
import Digraph
import qualified Data.Data as Data
import Outputable
import FastString
import SrcLoc
import Data.IORef( IORef )
import UniqSet

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

mkTcReflCo             :: Role -> TcType -> TcCoercion
mkTcSymCo              :: TcCoercion -> TcCoercion
mkTcTransCo            :: TcCoercion -> TcCoercion -> TcCoercion
mkTcNomReflCo          :: TcType -> TcCoercionN
mkTcRepReflCo          :: TcType -> TcCoercionR
mkTcTyConAppCo         :: Role -> TyCon -> [TcCoercion] -> TcCoercion
mkTcAppCo              :: TcCoercion -> TcCoercionN -> TcCoercion
mkTcFunCo              :: Role -> TcCoercion -> TcCoercion -> TcCoercion
mkTcAxInstCo           :: Role -> CoAxiom br -> BranchIndex
                       -> [TcType] -> [TcCoercion] -> TcCoercion
mkTcUnbranchedAxInstCo :: CoAxiom Unbranched -> [TcType]
                       -> [TcCoercion] -> TcCoercionR
mkTcForAllCo           :: TyVar -> TcCoercionN -> TcCoercion -> TcCoercion
mkTcForAllCos          :: [(TyVar, TcCoercionN)] -> TcCoercion -> TcCoercion
mkTcNthCo              :: Int -> TcCoercion -> TcCoercion
mkTcLRCo               :: LeftOrRight -> TcCoercion -> TcCoercion
mkTcSubCo              :: TcCoercionN -> TcCoercionR
maybeTcSubCo           :: EqRel -> TcCoercion -> TcCoercion
tcDowngradeRole        :: Role -> Role -> TcCoercion -> TcCoercion
mkTcAxiomRuleCo        :: CoAxiomRule -> [TcCoercion] -> TcCoercionR
mkTcCoherenceLeftCo    :: TcCoercion -> TcCoercionN -> TcCoercion
mkTcCoherenceRightCo   :: TcCoercion -> TcCoercionN -> TcCoercion
mkTcPhantomCo          :: TcCoercionN -> TcType -> TcType -> TcCoercionP
mkTcKindCo             :: TcCoercion -> TcCoercionN
mkTcCoVarCo            :: CoVar -> TcCoercion

tcCoercionKind         :: TcCoercion -> Pair TcType
tcCoercionRole         :: TcCoercion -> Role
coVarsOfTcCo           :: TcCoercion -> TcTyCoVarSet
isTcReflCo             :: TcCoercion -> Bool

-- | This version does a slow check, calculating the related types and seeing
-- if they are equal.
isTcReflexiveCo        :: TcCoercion -> Bool

mkTcReflCo             = mkReflCo
mkTcSymCo              = mkSymCo
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
maybeTcSubCo           = maybeSubCo
tcDowngradeRole        = downgradeRole
mkTcAxiomRuleCo        = mkAxiomRuleCo
mkTcCoherenceLeftCo    = mkCoherenceLeftCo
mkTcCoherenceRightCo   = mkCoherenceRightCo
mkTcPhantomCo          = mkPhantomCo
mkTcKindCo             = mkKindCo
mkTcCoVarCo            = mkCoVarCo

tcCoercionKind         = coercionKind
tcCoercionRole         = coercionRole
coVarsOfTcCo           = coVarsOfCo
isTcReflCo             = isReflCo
isTcReflexiveCo        = isReflexiveCo

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

  | WpFun HsWrapper HsWrapper TcType SDoc
       -- (WpFun wrap1 wrap2 t1)[e] = \(x:t1). wrap2[ e wrap1[x] ]
       -- So note that if  wrap1 :: exp_arg <= act_arg
       --                  wrap2 :: act_res <= exp_res
       --           then   WpFun wrap1 wrap2 : (act_arg -> arg_res) <= (exp_arg -> exp_res)
       -- This isn't the same as for mkFunCo, but it has to be this way
       -- because we can't use 'sym' to flip around these HsWrappers
       -- The TcType is the "from" type of the first wrapper
       -- The SDoc explains the circumstances under which we have created this
       -- WpFun, in case we run afoul of levity polymorphism restrictions in
       -- the desugarer. See Note [Levity polymorphism checking] in DsMonad

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

-- Cannot derive Data instance because SDoc is not Data (it stores a function).
-- So we do it manually:
instance Data.Data HsWrapper where
  gfoldl _ z WpHole             = z WpHole
  gfoldl k z (WpCompose a1 a2)  = z WpCompose `k` a1 `k` a2
  gfoldl k z (WpFun a1 a2 a3 _) = z wpFunEmpty `k` a1 `k` a2 `k` a3
  gfoldl k z (WpCast a1)        = z WpCast `k` a1
  gfoldl k z (WpEvLam a1)       = z WpEvLam `k` a1
  gfoldl k z (WpEvApp a1)       = z WpEvApp `k` a1
  gfoldl k z (WpTyLam a1)       = z WpTyLam `k` a1
  gfoldl k z (WpTyApp a1)       = z WpTyApp `k` a1
  gfoldl k z (WpLet a1)         = z WpLet `k` a1

  gunfold k z c = case Data.constrIndex c of
                    1 -> z WpHole
                    2 -> k (k (z WpCompose))
                    3 -> k (k (k (z wpFunEmpty)))
                    4 -> k (z WpCast)
                    5 -> k (z WpEvLam)
                    6 -> k (z WpEvApp)
                    7 -> k (z WpTyLam)
                    8 -> k (z WpTyApp)
                    _ -> k (z WpLet)

  toConstr WpHole          = wpHole_constr
  toConstr (WpCompose _ _) = wpCompose_constr
  toConstr (WpFun _ _ _ _) = wpFun_constr
  toConstr (WpCast _)      = wpCast_constr
  toConstr (WpEvLam _)     = wpEvLam_constr
  toConstr (WpEvApp _)     = wpEvApp_constr
  toConstr (WpTyLam _)     = wpTyLam_constr
  toConstr (WpTyApp _)     = wpTyApp_constr
  toConstr (WpLet _)       = wpLet_constr

  dataTypeOf _ = hsWrapper_dataType

hsWrapper_dataType :: Data.DataType
hsWrapper_dataType
  = Data.mkDataType "HsWrapper"
      [ wpHole_constr, wpCompose_constr, wpFun_constr, wpCast_constr
      , wpEvLam_constr, wpEvApp_constr, wpTyLam_constr, wpTyApp_constr
      , wpLet_constr]

wpHole_constr, wpCompose_constr, wpFun_constr, wpCast_constr, wpEvLam_constr,
  wpEvApp_constr, wpTyLam_constr, wpTyApp_constr, wpLet_constr :: Data.Constr
wpHole_constr    = mkHsWrapperConstr "WpHole"
wpCompose_constr = mkHsWrapperConstr "WpCompose"
wpFun_constr     = mkHsWrapperConstr "WpFun"
wpCast_constr    = mkHsWrapperConstr "WpCast"
wpEvLam_constr   = mkHsWrapperConstr "WpEvLam"
wpEvApp_constr   = mkHsWrapperConstr "WpEvApp"
wpTyLam_constr   = mkHsWrapperConstr "WpTyLam"
wpTyApp_constr   = mkHsWrapperConstr "WpTyApp"
wpLet_constr     = mkHsWrapperConstr "WpLet"

mkHsWrapperConstr :: String -> Data.Constr
mkHsWrapperConstr name = Data.mkConstr hsWrapper_dataType name [] Data.Prefix

wpFunEmpty :: HsWrapper -> HsWrapper -> TcType -> HsWrapper
wpFunEmpty c1 c2 t1 = WpFun c1 c2 t1 empty

(<.>) :: HsWrapper -> HsWrapper -> HsWrapper
WpHole <.> c = c
c <.> WpHole = c
c1 <.> c2    = c1 `WpCompose` c2

mkWpFun :: HsWrapper -> HsWrapper
        -> TcType    -- the "from" type of the first wrapper
        -> TcType    -- either type of the second wrapper (used only when the
                     -- second wrapper is the identity)
        -> SDoc      -- what caused you to want a WpFun? Something like "When converting ..."
        -> HsWrapper
mkWpFun WpHole       WpHole       _  _  _ = WpHole
mkWpFun WpHole       (WpCast co2) t1 _  _ = WpCast (mkTcFunCo Representational (mkTcRepReflCo t1) co2)
mkWpFun (WpCast co1) WpHole       _  t2 _ = WpCast (mkTcFunCo Representational (mkTcSymCo co1) (mkTcRepReflCo t2))
mkWpFun (WpCast co1) (WpCast co2) _  _  _ = WpCast (mkTcFunCo Representational (mkTcSymCo co1) co2)
mkWpFun co1          co2          t1 _  d = WpFun co1 co2 t1 d

-- | @mkWpFuns [(ty1, wrap1), (ty2, wrap2)] ty_res wrap_res@,
-- where @wrap1 :: ty1 "->" ty1'@ and @wrap2 :: ty2 "->" ty2'@,
-- @wrap3 :: ty3 "->" ty3'@ and @ty_res@ is /either/ @ty3@ or @ty3'@,
-- gives a wrapper @(ty1' -> ty2' -> ty3) "->" (ty1 -> ty2 -> ty3')@.
-- Notice that the result wrapper goes the other way round to all
-- the others. This is a result of sub-typing contravariance.
-- The SDoc is a description of what you were doing when you called mkWpFuns.
mkWpFuns :: [(TcType, HsWrapper)] -> TcType -> HsWrapper -> SDoc -> HsWrapper
mkWpFuns args res_ty res_wrap doc = snd $ go args res_ty res_wrap
  where
    go [] res_ty res_wrap = (res_ty, res_wrap)
    go ((arg_ty, arg_wrap) : args) res_ty res_wrap
      = let (tail_ty, tail_wrap) = go args res_ty res_wrap in
        (arg_ty `mkFunTy` tail_ty, mkWpFun arg_wrap tail_wrap arg_ty tail_ty doc)

mkWpCastR :: TcCoercionR -> HsWrapper
mkWpCastR co
  | isTcReflCo co = WpHole
  | otherwise     = ASSERT2(tcCoercionRole co == Representational, ppr co)
                    WpCast co

mkWpCastN :: TcCoercionN -> HsWrapper
mkWpCastN co
  | isTcReflCo co = WpHole
  | otherwise     = ASSERT2(tcCoercionRole co == Nominal, ppr co)
                    WpCast (mkTcSubCo co)
    -- The mkTcSubCo converts Nominal to Representational

mkWpTyApps :: [Type] -> HsWrapper
mkWpTyApps tys = mk_co_app_fn WpTyApp tys

mkWpEvApps :: [EvTerm] -> HsWrapper
mkWpEvApps args = mk_co_app_fn WpEvApp args

mkWpEvVarApps :: [EvVar] -> HsWrapper
mkWpEvVarApps vs = mk_co_app_fn WpEvApp (map EvId vs)

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
    go wrap              wraps = ([], foldl (<.>) wrap wraps)

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

      ebv_tcvs :: IORef CoVarSet
      -- The free coercion vars of the (rhss of) the coercion bindings
      --
      -- Coercions don't actually have bindings
      -- because we plug them in-place (via a mutable
      -- variable); but we keep their free variables
      -- so that we can report unused given constraints
      -- See Note [Tracking redundant constraints] in TcSimplify
    }

instance Data.Data TcEvBinds where
  -- Placeholder; we can't travers into TcEvBinds
  toConstr _   = abstractConstr "TcEvBinds"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = Data.mkNoRepType "TcEvBinds"

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
            -- See Note [Deterministic UniqFM] in UniqDFM for explanation why
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

instance Outputable EvBindMap where
  ppr (EvBindMap m) = ppr m

-----------------
-- All evidence is bound by EvBinds; no side effects
data EvBind
  = EvBind { eb_lhs      :: EvVar
           , eb_rhs      :: EvTerm
           , eb_is_given :: Bool  -- True <=> given
                 -- See Note [Tracking redundant constraints] in TcSimplify
    }

evBindVar :: EvBind -> EvVar
evBindVar = eb_lhs

mkWantedEvBind :: EvVar -> EvTerm -> EvBind
mkWantedEvBind ev tm = EvBind { eb_is_given = False, eb_lhs = ev, eb_rhs = tm }


mkGivenEvBind :: EvVar -> EvTerm -> EvBind
mkGivenEvBind ev tm = EvBind { eb_is_given = True, eb_lhs = ev, eb_rhs = tm }

data EvTerm
  = EvId EvId                    -- Any sort of evidence Id, including coercions

  | EvCoercion TcCoercion        -- coercion bindings
                                 -- See Note [Coercion evidence terms]

  | EvCast EvTerm TcCoercionR    -- d |> co

  | EvDFunApp DFunId             -- Dictionary instance application
       [Type] [EvTerm]

  | EvDelayedError Type FastString  -- Used with Opt_DeferTypeErrors
                               -- See Note [Deferring coercion errors to runtime]
                               -- in TcSimplify

  | EvSuperClass EvTerm Int      -- n'th superclass. Used for both equalities and
                                 -- dictionaries, even though the former have no
                                 -- selector Id.  We count up from _0_

  | EvLit EvLit       -- Dictionary for KnownNat and KnownSymbol classes.
                      -- Note [KnownNat & KnownSymbol and EvLit]

  | EvCallStack EvCallStack      -- Dictionary for CallStack implicit parameters

  | EvTypeable Type EvTypeable   -- Dictionary for (Typeable ty)

  | EvSelector Id [Type] [EvTerm] -- Selector id plus the types at which it
                                  -- should be instantiated, used for HasField
                                  -- dictionaries; see Note [HasField instances]
                                  -- in TcInterface

  deriving Data.Data


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

  | EvTypeableTrFun EvTerm EvTerm
    -- ^ Dictionary for @Typeable (s -> t)@,
    -- given a dictionaries for @s@ and @t@.

  | EvTypeableTyLit EvTerm
    -- ^ Dictionary for a type literal,
    -- e.g. @Typeable "foo"@ or @Typeable 3@
    -- The 'EvTerm' is evidence of, e.g., @KnownNat 3@
    -- (see Trac #10348)
  deriving Data.Data

data EvLit
  = EvNum Integer
  | EvStr FastString
    deriving Data.Data

-- | Evidence for @CallStack@ implicit parameters.
data EvCallStack
  -- See Note [Overview of implicit CallStacks]
  = EvCsEmpty
  | EvCsPushCall Name RealSrcSpan EvTerm
    -- ^ @EvCsPushCall name loc stk@ represents a call to @name@, occurring at
    -- @loc@, in a calling context @stk@.
  deriving Data.Data

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
See Trac [7238] and Note [Bind new Givens immediately] in TcRnTypes

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


Note [KnownNat & KnownSymbol and EvLit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A part of the type-level literals implementation are the classes
"KnownNat" and "KnownSymbol", which provide a "smart" constructor for
defining singleton values.  Here is the key stuff from GHC.TypeLits

  class KnownNat (n :: Nat) where
    natSing :: SNat n

  newtype SNat (n :: Nat) = SNat Integer

Conceptually, this class has infinitely many instances:

  instance KnownNat 0       where natSing = SNat 0
  instance KnownNat 1       where natSing = SNat 1
  instance KnownNat 2       where natSing = SNat 2
  ...

In practice, we solve `KnownNat` predicates in the type-checker
(see typecheck/TcInteract.hs) because we can't have infinitely many instances.
The evidence (aka "dictionary") for `KnownNat` is of the form `EvLit (EvNum n)`.

We make the following assumptions about dictionaries in GHC:
  1. The "dictionary" for classes with a single method---like `KnownNat`---is
     a newtype for the type of the method, so using a evidence amounts
     to a coercion, and
  2. Newtypes use the same representation as their definition types.

So, the evidence for `KnownNat` is just a value of the representation type,
wrapped in two newtype constructors: one to make it into a `SNat` value,
and another to make it into a `KnownNat` dictionary.

Also note that `natSing` and `SNat` are never actually exposed from the
library---they are just an implementation detail.  Instead, users see
a more convenient function, defined in terms of `natSing`:

  natVal :: KnownNat n => proxy n -> Integer

The reason we don't use this directly in the class is that it is simpler
and more efficient to pass around an integer rather than an entier function,
especially when the `KnowNat` evidence is packaged up in an existential.

The story for kind `Symbol` is analogous:
  * class KnownSymbol
  * newtype SSymbol
  * Evidence: EvLit (EvStr n)


Note [Overview of implicit CallStacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See https://ghc.haskell.org/trac/ghc/wiki/ExplicitCallStack/ImplicitLocations)

The goal of CallStack evidence terms is to reify locations
in the program source as runtime values, without any support
from the RTS. We accomplish this by assigning a special meaning
to constraints of type GHC.Stack.Types.HasCallStack, an alias

  type HasCallStack = (?callStack :: CallStack)

Implicit parameters of type GHC.Stack.Types.CallStack (the name is not
important) are solved in three steps:

1. Occurrences of CallStack IPs are solved directly from the given IP,
   just like a regular IP. For example, the occurrence of `?stk` in

     error :: (?stk :: CallStack) => String -> a
     error s = raise (ErrorCall (s ++ prettyCallStack ?stk))

   will be solved for the `?stk` in `error`s context as before.

2. In a function call, instead of simply passing the given IP, we first
   append the current call-site to it. For example, consider a
   call to the callstack-aware `error` above.

     undefined :: (?stk :: CallStack) => a
     undefined = error "undefined!"

   Here we want to take the given `?stk` and append the current
   call-site, before passing it to `error`. In essence, we want to
   rewrite `error "undefined!"` to

     let ?stk = pushCallStack <error's location> ?stk
     in error "undefined!"

   We achieve this effect by emitting a NEW wanted

     [W] d :: IP "stk" CallStack

   from which we build the evidence term

     EvCsPushCall "error" <error's location> (EvId d)

   that we use to solve the call to `error`. The new wanted `d` will
   then be solved per rule (1), ie as a regular IP.

   (see TcInteract.interactDict)

3. We default any insoluble CallStacks to the empty CallStack. Suppose
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

   (see TcSimplify.simpl_top and TcSimplify.defaultCallStacks)

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
  with a partial type signature (See TcType.pickQuantifiablePreds).

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
  call-site onto a given stack (See DsBinds.dsEvCallStack)

- When we emit a new wanted CallStack from rule (2) we set its origin to
  `IPOccOrigin ip_name` instead of the original `OccurrenceOf func`
  (see TcInteract.interactDict).

  This is a bit shady, but is how we ensure that the new wanted is
  solved like a regular IP.

-}

mkEvCast :: EvTerm -> TcCoercion -> EvTerm
mkEvCast ev lco
  | ASSERT2(tcCoercionRole lco == Representational, (vcat [text "Coercion of wrong role passed to mkEvCast:", ppr ev, ppr lco]))
    isTcReflCo lco = ev
  | otherwise      = EvCast ev lco

mkEvScSelectors :: EvTerm -> Class -> [TcType] -> [(TcPredType, EvTerm)]
mkEvScSelectors ev cls tys
   = zipWith mk_pr (immSuperClasses cls tys) [0..]
  where
    mk_pr pred i = (pred, EvSuperClass ev i)

emptyTcEvBinds :: TcEvBinds
emptyTcEvBinds = EvBinds emptyBag

isEmptyTcEvBinds :: TcEvBinds -> Bool
isEmptyTcEvBinds (EvBinds b)    = isEmptyBag b
isEmptyTcEvBinds (TcEvBinds {}) = panic "isEmptyTcEvBinds"


evTermCoercion :: EvTerm -> TcCoercion
-- Applied only to EvTerms of type (s~t)
-- See Note [Coercion evidence terms]
evTermCoercion (EvId v)        = mkCoVarCo v
evTermCoercion (EvCoercion co) = co
evTermCoercion (EvCast tm co)  = mkCoCast (evTermCoercion tm) co
evTermCoercion tm = pprPanic "evTermCoercion" (ppr tm)

evVarsOfTerm :: EvTerm -> VarSet
evVarsOfTerm (EvId v)             = unitVarSet v
evVarsOfTerm (EvCoercion co)      = coVarsOfCo co
evVarsOfTerm (EvDFunApp _ _ evs)  = mapUnionVarSet evVarsOfTerm evs
evVarsOfTerm (EvSuperClass v _)   = evVarsOfTerm v
evVarsOfTerm (EvCast tm co)       = evVarsOfTerm tm `unionVarSet` coVarsOfCo co
evVarsOfTerm (EvDelayedError _ _) = emptyVarSet
evVarsOfTerm (EvLit _)            = emptyVarSet
evVarsOfTerm (EvCallStack cs)     = evVarsOfCallStack cs
evVarsOfTerm (EvTypeable _ ev)    = evVarsOfTypeable ev
evVarsOfTerm (EvSelector _ _ evs) = mapUnionVarSet evVarsOfTerm evs

evVarsOfTerms :: [EvTerm] -> VarSet
evVarsOfTerms = mapUnionVarSet evVarsOfTerm

-- | Do SCC analysis on a bag of 'EvBind's.
sccEvBinds :: Bag EvBind -> [SCC EvBind]
sccEvBinds bs = stronglyConnCompFromEdgedVerticesUniq edges
  where
    edges :: [ Node EvVar EvBind ]
    edges = foldrBag ((:) . mk_node) [] bs

    mk_node :: EvBind -> Node EvVar EvBind
    mk_node b@(EvBind { eb_lhs = var, eb_rhs = term })
      = DigraphNode b var (nonDetEltsUniqSet (evVarsOfTerm term `unionVarSet`
                                coVarsOfType (varType var)))
      -- It's OK to use nonDetEltsUniqSet here as stronglyConnCompFromEdgedVertices
      -- is still deterministic even if the edges are in nondeterministic order
      -- as explained in Note [Deterministic SCC] in Digraph.

evVarsOfCallStack :: EvCallStack -> VarSet
evVarsOfCallStack cs = case cs of
  EvCsEmpty -> emptyVarSet
  EvCsPushCall _ _ tm -> evVarsOfTerm tm

evVarsOfTypeable :: EvTypeable -> VarSet
evVarsOfTypeable ev =
  case ev of
    EvTypeableTyCon _ e   -> mapUnionVarSet evVarsOfTerm e
    EvTypeableTyApp e1 e2 -> evVarsOfTerms [e1,e2]
    EvTypeableTrFun e1 e2 -> evVarsOfTerms [e1,e2]
    EvTypeableTyLit e     -> evVarsOfTerm e

{-
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
  = sdocWithDynFlags $ \ dflags ->
    if gopt Opt_PrintTypecheckerElaboration dflags
    then help pp_thing_inside wrap False
    else pp_thing_inside False
  where
    help :: (Bool -> SDoc) -> HsWrapper -> Bool -> SDoc
    -- True  <=> appears in function application position
    -- False <=> appears as body of let or lambda
    help it WpHole             = it
    help it (WpCompose f1 f2)  = help (help it f2) f1
    help it (WpFun f1 f2 t1 _) = add_parens $ text "\\(x" <> dcolon <> ppr t1 <> text ")." <+>
                                              help (\_ -> it True <+> help (\_ -> text "x") f1 True) f2 False
    help it (WpCast co)   = add_parens $ sep [it False, nest 2 (text "|>"
                                              <+> pprParendCo co)]
    help it (WpEvApp id)  = no_parens  $ sep [it True, nest 2 (ppr id)]
    help it (WpTyApp ty)  = no_parens  $ sep [it True, text "@" <+> pprParendType ty]
    help it (WpEvLam id)  = add_parens $ sep [ text "\\" <> pp_bndr id, it False]
    help it (WpTyLam tv)  = add_parens $ sep [text "/\\" <> pp_bndr tv, it False]
    help it (WpLet binds) = add_parens $ sep [text "let" <+> braces (ppr binds), it False]

    pp_bndr v = pprBndr LambdaBind v <> dot

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

instance Uniquable EvBindsVar where
  getUnique (EvBindsVar { ebv_uniq = u }) = u

instance Outputable EvBind where
  ppr (EvBind { eb_lhs = v, eb_rhs = e, eb_is_given = is_given })
     = sep [ pp_gw <+> ppr v
           , nest 2 $ equals <+> ppr e ]
     where
       pp_gw = brackets (if is_given then char 'G' else char 'W')
   -- We cheat a bit and pretend EqVars are CoVars for the purposes of pretty printing

instance Outputable EvTerm where
  ppr (EvId v)              = ppr v
  ppr (EvCast v co)         = ppr v <+> (text "`cast`") <+> pprParendCo co
  ppr (EvCoercion co)       = text "CO" <+> ppr co
  ppr (EvSuperClass d n)    = text "sc" <> parens (ppr (d,n))
  ppr (EvDFunApp df tys ts) = ppr df <+> sep [ char '@' <> ppr tys, ppr ts ]
  ppr (EvLit l)             = ppr l
  ppr (EvCallStack cs)      = ppr cs
  ppr (EvDelayedError ty msg) =     text "error"
                                <+> sep [ char '@' <> ppr ty, ppr msg ]
  ppr (EvTypeable ty ev)      = ppr ev <+> dcolon <+> text "Typeable" <+> ppr ty
  ppr (EvSelector sel tys ts) = ppr sel <+> sep [ char '@' <> ppr tys, ppr ts]

instance Outputable EvLit where
  ppr (EvNum n) = integer n
  ppr (EvStr s) = text (show s)

instance Outputable EvCallStack where
  ppr EvCsEmpty
    = text "[]"
  ppr (EvCsPushCall name loc tm)
    = ppr (name,loc) <+> text ":" <+> ppr tm

instance Outputable EvTypeable where
  ppr (EvTypeableTyCon ts _)  = text "TyCon" <+> ppr ts
  ppr (EvTypeableTyApp t1 t2) = parens (ppr t1 <+> ppr t2)
  ppr (EvTypeableTrFun t1 t2) = parens (ppr t1 <+> arrow <+> ppr t2)
  ppr (EvTypeableTyLit t1)    = text "TyLit" <> ppr t1


----------------------------------------------------------------------
-- Helper functions for dealing with IP newtype-dictionaries
----------------------------------------------------------------------

-- | Create a 'Coercion' that unwraps an implicit-parameter or
-- overloaded-label dictionary to expose the underlying value. We
-- expect the 'Type' to have the form `IP sym ty` or `IsLabel sym ty`,
-- and return a 'Coercion' `co :: IP sym ty ~ ty` or
-- `co :: IsLabel sym ty ~ Proxy# sym -> ty`.  See also
-- Note [Type-checking overloaded labels] in TcExpr.
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
