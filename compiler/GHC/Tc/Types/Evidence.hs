-- (c) The University of Glasgow 2006


module GHC.Tc.Types.Evidence (

  -- * HsWrapper
  HsWrapper(..),
  (<.>), mkWpTyApps, mkWpEvApps, mkWpEvVarApps, mkWpTyLams, mkWpForAllCast,
  mkWpEvLams, mkWpLet, mkWpFun, mkWpCastN, mkWpCastR, mkWpEta, mkWpSubType,
  SubMultCo(..), mkSubMultFunCo, subMultCoRKind,
  collectHsWrapBinders,
  idHsWrapper, isIdHsWrapper,
  pprHsWrapper, hsWrapDictBinders,
  optSubTypeHsWrapper,

  -- * Evidence bindings
  TcEvBinds(..), EvBindsVar(..),
  EvBindMap(..), emptyEvBindMap, extendEvBinds, unionEvBindMap,
  lookupEvBind, evBindMapBinds,
  foldEvBindMap, nonDetStrictFoldEvBindMap,
  filterEvBindMap,
  isEmptyEvBindMap,
  evBindMapToVarSet,
  varSetMinusEvBindMap,
  EvBindInfo(..), EvBind(..), emptyTcEvBinds, isEmptyTcEvBinds, mkGivenEvBind, mkWantedEvBind,
  evBindVar, isCoEvBindsVar,

  -- * EvTerm (already a CoreExpr)
  EvTerm(..), EvExpr,
  evId, evCoercion, evCast, evCastE, evDFunApp,  evDictApp, evSelector, evDelayedError,
  mkEvScSelectors, evTypeable,
  evWrapIPE, evUnwrapIPE, evUnaryDictAppE,
  mkEvCast,
  nestedEvIdsOfTerm, evTermFVs,

  evTermCoercion, evTermCoercion_maybe,
  evExprCoercion, evExprCoercion_maybe,
  EvCallStack(..),
  EvTypeable(..),

  -- * HoleExprRef
  HoleExprRef(..),

  -- * TcCoercion
  TcCoercion, TcCoercionR, TcCoercionN, TcCoercionP, CoercionHole,
  TcMCoercion, TcMCoercionN, TcMCoercionR,
  Role(..), LeftOrRight(..), pickLR,
  maybeSymCo,

  -- * QuoteWrapper
  QuoteWrapper(..), applyQuoteWrapper, quoteWrapperTyVarTy
  ) where

import GHC.Prelude

import GHC.Tc.Utils.TcType

import GHC.Core
import GHC.Core.Coercion.Axiom
import GHC.Core.Coercion
import GHC.Core.Ppr ()   -- Instance OutputableBndr TyVar
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.TyCo.Rep (UnivCoProvenance(..))
import GHC.Core.TyCon
import GHC.Core.Make    ( mkWildCase, mkRuntimeErrorApp, tYPE_ERROR_ID )
import GHC.Core.Class   ( classTyCon )
import GHC.Core.DataCon ( dataConWrapId )
import GHC.Core.Class (Class, classSCSelId )
import GHC.Core.FVs
import GHC.Core.InstEnv ( CanonicalEvidence(..) )

import GHC.Types.Unique.DFM
import GHC.Types.Unique.FM
import GHC.Types.Name( isInternalName )
import GHC.Types.Var
import GHC.Types.Id( idType )
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic

import GHC.Builtin.Names
import GHC.Builtin.Types( unitTy )

import GHC.Utils.FV
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

type TcCoercion   = Coercion
type TcCoercionN  = CoercionN    -- A Nominal          coercion ~N
type TcCoercionR  = CoercionR    -- A Representational coercion ~R
type TcCoercionP  = CoercionP    -- a phantom coercion
type TcMCoercion  = MCoercion
type TcMCoercionN = MCoercionN  -- nominal
type TcMCoercionR = MCoercionR  -- representational


-- | If a 'SwapFlag' is 'IsSwapped', flip the orientation of a coercion
maybeSymCo :: SwapFlag -> TcCoercion -> TcCoercion
maybeSymCo IsSwapped  co = mkSymCo co
maybeSymCo NotSwapped co = co

{-
%************************************************************************
%*                                                                      *
                  HsWrapper
*                                                                      *
************************************************************************
-}

{- Note [Deep subsumption and WpSubType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When making DeepSubsumption checks, we may end up with hard-to-spot identity wrappers.
For example (#26349) suppose we have
    (forall a. Eq a => a->a) -> Int  <=   (forall a. Eq a => a->a) -> Int
The two types are equal so we should certainly get an identity wrapper.  But we'll get
tihs wrapper from `tcSubType`:
    WpFun (WpTyLam a <.> WpEvLam dg <.> WpLet (dw=dg) <.> WpEvApp dw <.> WpTyApp a)
          WpHole
That elaborate wrapper is really just a no-op, but it's far from obvious.  If we just
desugar (HsWrap f wp) straightforwardly we'll get
   \(g:forall a. Eq a => a -> a).
       f (/\a. \(dg:Eq a). let dw=dg in g a dw)

To recognise that as just `f`, we'd have to eta-reduce twice.  But eta-reduction
is not sound in general, so we'll end up retaining the lambdas.  Two bad results:

* Adding DeepSubsumption gratuitiously makes programs less efficient.

* When the subsumption is on the LHS of a rule, or in a SPECIALISE pragma, we
  may not be able to make a decent RULE at all, and will fail with "LHS of rule
  is too complicated to desugar" (#26255)

It'd be ideal to solve the problem at the source, by never generating those
gruesome wrappers in the first place, but we can't do that because:

* The WpTyLam and WpTyApp are introduced independently, not together, in `tcSubType`,
  so we can't easily cancel them out.   For example, even if we have
     forall a. t1  <=  forall a. t2
  there is no guarantee that these are the "same" a.  E.g.
     forall a b. a -> b -> b   <=   forall x y. y -> x -> x
  Similarly WpEvLam and WpEvApp

* We have not yet done constraint solving so we don't know what evidence will
  end up in those WpLet bindings.

TL;DR we must generate the wrapper and then optimise it way if it turns out
that it is a no-op.  Here's our solution:

(DSST1) Tag the wrappers generated from a subtype check with WpSubType. In normal
  wrappers the binders of a WpTyLam or WpEvLam can scope over the "hole" of the
  wrapper -- that is how we introduce type-lambdas and dictionary-lambda into the
  terms!  But in /subtype/ wrappers, these type/dictionary lambdas only scope over
  the WpTyApp and WpEvApp nodes in the /same/ wrapper.  That is what justifies us
  eta-reducing the type/dictionary lambdas.

  In short, (WpSubType wp) means the same as `wp`, but with the added promise that
  the binders in `wp` do not scope over the hole.

(DSST2) Avoid creating a WpSubType in the common WpHole case, using `mkWpSubType`.

(DSST3) When desugaring, try eta-reduction on the payload of a WpSubType.
  This is done in `GHC.HsToCore.Binds.dsHsWrapper` by the call to `optSubTypeHsWrapper`.

  We don't attempt to optimise HsWrappers /other than/ subtype wrappers. Why not?
  Because there aren't any useful optimsations we can do.  (We could collapse
  adjacent `WpCast`s perhaps, but that'll happen later automatically via `mkCast`.)

  TL;DR:
    * we /must/ optimise subtype-HsWrappers (that's the point of this Note!)
    * there is little point in attempting to optimise any other HsWrappers

Note [WpFun-FRR-INVARIANT]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Given
  wrap = WpFun wrap1 wrap2 sty1 ty2
  where:  wrap1 :: exp_arg ~~> act_arg
          wrap2 :: act_res ~~> exp_res
          wrap  :: (act_arg -> act_res) ~~> (exp_arg -> exp_res)
we have
  WpFun-FRR-INVARIANT:
      the input (exp_arg) and output (act_arg) types of `wrap1`
      both have a fixed runtime-rep

Reason: We desugar wrap[e] into
    \(x:exp_arg). wrap2[ e wrap1[x] ]
And then, because of Note [Representation polymorphism invariants]:

  * `exp_arg` must have a fixed runtime rep,
    so that lambda obeys the the FRR rules

  * `act_arg` must have a fixed runtime rep,
    so that the application (e wrap1[x]) obeys the FRR rules

Hence WpFun-FRR-INVARIANT.
-}

-- | 'HsWrapper's are produced by the typechecker to insert pieces of Core syntax.
--
-- For example:
--
--   - User writes: @id False@.
--     Typechecker elaborates to @id \@Bool False@ using 'WpTyApp'.
--   - User writes: @compare True False@.
--     Typechecker elaborates to @compare $dOrdBool True False@ using 'WpEvApp'.
--
-- A wrapper can be thought of as a function @CoreExpr -> CoreExpr@ which
-- transforms Core syntax by elaboration; we usually write this as
-- @expr ↦ wrap[expr]@. We often write a wrapper as an "expression with a hole",
-- e.g. @WpTyApp Int@ is @<hole> \@Int@.
--
-- NOTATION @~~>@: if @wrap[ e::t1 ] :: t2@, we write the type of @wrap@
-- as @wrap :: t1 ~~> t2@.
data HsWrapper
  -- | The identity 'HsWrapper' (similar to the 'Refl' coercion).
  = WpHole

  -- | @WpSubType wp@ is the same as @wp@, but with extra invariants.
  -- See Note [Deep subsumption and WpSubType] (DSST1)
  | WpSubType HsWrapper

  -- | @(wrap1 `WpCompose` wrap2)[e] = wrap1[ wrap2[ e ]]@
  --
  -- Hence  @(\\a. <hole>) `WpCompose` (\\b. <hole>) = (\\a b. <hole>)@
  -- But    @(<hole> a) `WpCompose` (<hole> b) = (<hole> b a)@
  --
  -- If @wrap1 :: t2 ~> t3@ and @wrap2 :: t1 ~~> t2@,
  -- then @(wrap1 `WpCompose` wrap2) :: t1 ~~> t3@
  | WpCompose HsWrapper HsWrapper

  -- | @(WpFun mult_co arg_wrap res_wrap t1 t2)[e] = \(x :: exp_arg). res_wrap[ e arg_wrap[x] ]@
  --
  -- INVARIANT: both input and output types of `arg_wrap` have a fixed runtime-rep
  --            See Note [WpFun-FRR-INVARIANT]
  --
  -- Typing rules: given:
  --  - @mult_co  :: act_mult ~ exp_mult@
  --  - @arg_wrap :: exp_arg ~~> act_arg@
  --  - @res_wrap :: act_res ~~> exp_res@
  -- then @WpFun mult_co arg_wrap res_wrap :: (act_arg %act_mult -> arg_res) ~~> (exp_arg %exp_mult -> exp_res)@.
  --
  -- This isn't the same as for @mkFunCo@, but it has to be this way
  -- because we can't use @mkSymCo@ to flip around these @HsWrapper@s.
  --
  -- NB: a 'WpFun' is always for type-like function arrows; no constraints.
  --
  -- Use 'mkWpFun' to construct such a wrapper.
  | WpFun
     { mult_co  :: SubMultCo
     , arg_wrap :: HsWrapper
     , res_wrap :: HsWrapper
     , arg_type :: TcTypeFRR
         -- ^ The "from" type of the argument wrapper
     , res_type :: TcType
         -- ^ Either the "from" or the "to" type of the result wrapper
     }

  -- | A cast:  @<hole> `cast` co@
  | WpCast TcCoercionR
    -- ^ Invariants:
    --
    --    - 'Representational' role.
    --    - Never the identity coercion.

  -- | Evidence abstraction @\d. <hole>@, where @d@ is an evidence variable
  | WpEvLam EvVar
  -- | @<hole> d, where @d@ is evidence for a constraint
  | WpEvApp EvTerm

  -- | Type abstraction @\a. <hole>@, where @a@ is a type/kind variable (not a coercion variable)
  | WpTyLam TyVar
  -- | Type application @<hole> t@, where @t@ is a type/kind (not a coercion)
  | WpTyApp KindOrType

  -- | Evidence bindings: @let binds in <hole>@.
  --
  -- The set of evidence bindings should be non-empty (or possibly non-empty),
  -- so that the identity 'HsWrapper' is always exactly 'WpHole'.
  | WpLet TcEvBinds

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
WpHole    <.> c         = c
c         <.> WpHole    = c
WpCast c1 <.> WpCast c2 = WpCast (c2 `mkTransCo` c1)
  -- If we can represent the HsWrapper as a cast, try to do so: this may avoid
  -- unnecessary eta-expansion (see 'mkWpFun').
  --
  -- NB: <.> behaves like function composition:
  --
  --   WpCast c1 <.> WpCast c2 :: coercionLKind c2 ~~> coercionRKind c1
  --
  -- This is thus the same as WpCast (c2 ; c1) and not WpCast (c1 ; c2).
c1        <.> c2        = c1 `WpCompose` c2

-- | A sub-multiplicity coercion.
--
-- Used only to get multiplicities to line up when typechecking partial
-- applications of data constructors, as per Note [Typechecking data constructors]
-- in GHC.Tc.Gen.Head.
--
--  See Note [Multiplicity in deep subsumption]
data SubMultCo
  -- | Equality of multiplicities
  = EqMultCo TcCoercionN
  -- | One is a submultiplicity of any other multiplicity.
  | OneSubMult Mult
  deriving Data.Data

instance Outputable SubMultCo where
  ppr (EqMultCo co) = ppr co
  ppr (OneSubMult w) = parens $ text "One ⊆" <+> ppr w

subMultCoRKind :: SubMultCo -> Mult
subMultCoRKind (EqMultCo co) = coercionRKind co
subMultCoRKind (OneSubMult w) = w

-- | Like 'mkFunCo2' except that it allows for sub-multiplicity instead of
-- a multiplicity coercion.
mkSubMultFunCo :: FunTyFlag -> FunTyFlag
               -> SubMultCo   -- ^ act_mult ~~> exp_mult
               -> TcCoercionR -- ^ act_arg  ~~> exp_arg
               -> TcCoercionR -- ^ act_res  ~~> exp_res
               -> TcCoercionR
mkSubMultFunCo act_af exp_af (EqMultCo w_co) arg_co res_co =
  mkFunCo2 Representational act_af exp_af
    w_co
    arg_co
    res_co
mkSubMultFunCo act_af exp_af (OneSubMult w2) arg_co res_co =
  mkUnivCo SubMultProv [] Representational
    (mkFunTy act_af OneTy act_arg act_res)
    (mkFunTy exp_af w2    act_arg act_res)
    `mkTransCo`
  mkFunCo2 Representational act_af exp_af
    (mkNomReflCo w2)
    arg_co
    res_co
  where
    act_arg = coercionLKind arg_co
    act_res = coercionLKind res_co

-- | Smart constructor to create a 'WpFun' 'HsWrapper', which avoids introducing
-- a lambda abstraction if the two supplied wrappers are either identities or
-- casts.
--
-- PRECONDITION: same as Note [WpFun-FRR-INVARIANT]
mkWpFun :: HsWrapper -> HsWrapper
        -> (SubMultCo, TcTypeFRR) -- ^ the "from" type of the first wrapper
        -> TcType           -- ^ Either "from" type or "to" type of the second wrapper
                            --   (used only when the second wrapper is the identity)
        -> HsWrapper
-- Unfortunately, we can't check PRECONDITION with an assertion here, because of
-- [Wrinkle: Typed Template Haskell] in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
mkWpFun w1 w2 (wp_mult, t1) t2
  = case (w1,w2) of
      (WpHole,     WpHole)
        -> mkWpCastR (mk_wp_fun_co (mkRepReflCo t1) (mkRepReflCo t2))
      (WpHole,     WpCast co2)
        -> WpCast    (mk_wp_fun_co (mkRepReflCo t1) co2)
      (WpCast co1, WpHole)
        -> WpCast    (mk_wp_fun_co (mkSymCo co1)    (mkRepReflCo t2))
      (WpCast co1, WpCast co2)
        -> WpCast    (mk_wp_fun_co (mkSymCo co1)    co2)
      (_,          _)
        -> WpFun { mult_co = wp_mult
                 , arg_wrap = w1, res_wrap = w2
                 , arg_type = t1, res_type = t2 }
  where
    mk_wp_fun_co = mkSubMultFunCo FTF_T_T FTF_T_T wp_mult
      -- FTF_T_T: WpFun is always (->)

mkWpSubType :: HsWrapper -> HsWrapper
-- See (DSST2) in Note [Deep subsumption and WpSubType]
mkWpSubType WpHole      = WpHole
mkWpSubType (WpCast co) = WpCast co
mkWpSubType w           = WpSubType w

mkWpEta :: Type -> [Id] -> HsWrapper -> HsWrapper
-- (mkWpEta [x1, x2] wrap) [e]
--   = \x1. \x2.  wrap[e x1 x2]
-- Just generates a bunch of WpFuns
-- The incoming type is the type of the entire expression
mkWpEta orig_fun_ty xs wrap = go orig_fun_ty xs
  where
    go _      []       = wrap
    go fun_ty (id:ids) =
      WpFun
        { mult_co  = EqMultCo $ mkNomReflCo (idMult id)
        , arg_wrap = idHsWrapper
        , res_wrap = go res_ty ids
        , arg_type = idType id
        , res_type = res_ty
        }
      where
        res_ty = funResultTy fun_ty

mkWpCastR :: TcCoercionR -> HsWrapper
mkWpCastR co
  | isReflCo co = WpHole
  | otherwise   = assertPpr (coercionRole co == Representational) (ppr co) $
                  WpCast co

mkWpCastN :: TcCoercionN -> HsWrapper
mkWpCastN co
  | isReflCo co = WpHole
  | otherwise   = assertPpr (coercionRole co == Nominal) (ppr co) $
                  WpCast (mkSubCo co)
    -- The mkTcSubCo converts Nominal to Representational

mkWpTyApps :: [Type] -> HsWrapper
mkWpTyApps tys = mk_co_app_fn WpTyApp tys

mkWpEvApps :: [EvTerm] -> HsWrapper
mkWpEvApps args = mk_co_app_fn WpEvApp args

mkWpEvVarApps :: [EvVar] -> HsWrapper
mkWpEvVarApps vs = mk_co_app_fn WpEvApp (map (EvExpr . evId) vs)

mkWpTyLams :: [TyVar] -> HsWrapper
mkWpTyLams ids = mk_co_lam_fn WpTyLam ids

-- mkWpForAllCast [tv{vis}] constructs a cast
--   forall tv. res  ~R#   forall tv{vis} res`.
-- See Note [Required foralls in Core] in GHC.Core.TyCo.Rep
--
-- It's a no-op if all binders are invisible;
-- but in that case we refrain from calling it.
mkWpForAllCast :: [ForAllTyBinder] -> Type -> HsWrapper
mkWpForAllCast bndrs res_ty
  = mkWpCastR (mkForAllVisCos bndrs (mkRepReflCo res_ty))

mkWpEvLams :: [Var] -> HsWrapper
mkWpEvLams ids = mk_co_lam_fn WpEvLam ids

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
   go (WpFun {res_wrap})  = go res_wrap
   go WpHole              = emptyBag
   go (WpSubType {})      = emptyBag  -- See Note [Deep subsumption and WpSubType]
   go (WpCast  {})        = emptyBag
   go (WpEvApp {})        = emptyBag
   go (WpTyLam {})        = emptyBag
   go (WpTyApp {})        = emptyBag
   go (WpLet   {})        = emptyBag

collectHsWrapBinders :: HsWrapper -> ([Var], HsWrapper)
-- Collect the outer lambda binders of a HsWrapper,
-- stopping as soon as you get to a non-lambda binder
collectHsWrapBinders wrap = go wrap []
  where
    -- go w ws = collectHsWrapBinders (w <.> w1 <.> ... <.> wn)
    go :: HsWrapper -> [HsWrapper] -> ([Var], HsWrapper)
    go (WpEvLam v)       wraps = add_lam v (gos wraps)
    go (WpTyLam v)       wraps = add_lam v (gos wraps)
    go (WpSubType w)     wraps = go w wraps
    go (WpCompose w1 w2) wraps = go w1 (w2:wraps)
    go wrap              wraps = ([], foldl' (<.>) wrap wraps)

    gos []     = ([], WpHole)
    gos (w:ws) = go w ws

    add_lam v (vs,w) = (v:vs, w)


optSubTypeHsWrapper :: HsWrapper -> HsWrapper
-- This optimiser is used only on the payload of WpSubType
-- It finds cases where the entire wrapper is a no-op
-- See (DSST3) in Note [Deep subsumption and WpSubType]
optSubTypeHsWrapper wrap
  = opt wrap
  where
    opt :: HsWrapper -> HsWrapper
    opt w = foldr (<.>) WpHole (opt1 w [])

    opt1 :: HsWrapper -> [HsWrapper] -> [HsWrapper]
    -- opt1 w ws = w <.> (foldr <.> WpHole ws)
    -- INVARIANT: ws::[HsWrapper] is optimised
    opt1 WpHole                 ws = ws
    opt1 (WpSubType w)          ws = opt1 w ws
    opt1 (w1 `WpCompose` w2)    ws = opt1 w1 (opt1 w2 ws)
    opt1 (WpCast co)            ws = opt_co co ws
    opt1 (WpEvLam ev)           ws = opt_ev_lam ev ws
    opt1 (WpTyLam tv)           ws = opt_ty_lam tv ws
    opt1 (WpLet binds)          ws = pushWpLet binds ws
    opt1 w@(WpTyApp {})         ws = w : ws
    opt1 w@(WpEvApp {})         ws = w : ws
    opt1 (WpFun mult_co arg_wrap res_wrap arg_ty res_ty) ws
      = opt_fun mult_co arg_wrap res_wrap arg_ty res_ty ws

    -----------------
    -- (WpTyLam a <.> WpTyApp a <.> w) = w
    -- i.e.   /\a. <hole> a   -->  <hole>
    -- This is only valid if whatever fills the hole does not mention 'a'
    -- But that's guaranteed in subtype-wrappers;
    -- see (DSST1) in Note [Deep subsumption and WpSubType]
    opt_ty_lam tv (WpTyApp ty : ws)
      | Just tv' <- getTyVar_maybe ty
      , tv==tv'
      , all (tv `not_in`) ws
      = ws

    -- (WpTyLam a <.> WpCastCo co <.> w)
    --    = WpCast (ForAllCo a co) (WpTyLam <.> w)
    opt_ty_lam tv (WpCast co : ws)
      = opt_co (mkHomoForAllCo tv co) (opt_ty_lam tv ws)

    opt_ty_lam tv ws
      = WpTyLam tv : ws

    -----------------
    -- (WpEvLam ev <.> WpEvAp ev <.> w) = w
    -- Similar notes to WpTyLam
    opt_ev_lam ev (WpEvApp ev_tm : ws)
      | EvExpr (Var ev') <- ev_tm
      , ev == ev'
      , all (ev `not_in`) ws
      = ws

    -- (WpEvLam ev <.> WpCast co <.> w)
    --    = WpCast (FunCo ev co) (WpEvLam <.> w)
    opt_ev_lam ev (WpCast co : ws)
      = opt_co fun_co (opt_ev_lam ev ws)
      where
        fun_co = mkFunCo Representational FTF_C_T
                        (mkNomReflCo ManyTy)
                        (mkRepReflCo (idType ev))
                        co

    opt_ev_lam ev ws
      = WpEvLam ev : ws

    -----------------
    -- WpCast co <.> WpCast co' <.> ws = WpCast (co;co') ws
    opt_co co (WpCast co' : ws)     = opt_co (co `mkTransCo` co') ws
    opt_co co ws | isReflexiveCo co = ws
                 | otherwise        = WpCast co : ws

    ------------------
    opt_fun mult_co arg_wrap res_wrap ty1 ty2 ws
      = case mkWpFun (opt arg_wrap) (opt res_wrap) (mult_co, ty1) ty2 of
          WpHole    -> ws
          WpCast co -> opt_co co ws
          w         -> w : ws

    ------------------
    -- Tiresome check that the lambda-bound type/evidence variable that we
    -- want to eta-reduce isn't free in the rest of the wrapper
    not_in :: TyVar -> HsWrapper -> Bool
    not_in _  WpHole                   = True
    not_in v (WpCast co)               = not (anyFreeVarsOfCo (== v) co)
    not_in v (WpTyApp ty)              = not (anyFreeVarsOfType (== v) ty)
    not_in v (WpFun w_co w1 w2 _ _)    = not_in_submult v w_co && not_in v w1 && not_in v w2
    not_in v (WpSubType w)             = not_in v w
    not_in v (WpCompose w1 w2)         = not_in v w1 && not_in v w2
    not_in v (WpEvApp (EvExpr e))      = not (v `elemVarSet` exprFreeVars e)
    not_in _ (WpEvApp (EvTypeable {})) = False  -- Giving up; conservative
    not_in _ (WpEvApp (EvFun {}))      = False  -- Giving up; conservative
    not_in _ (WpTyLam {}) = False    -- Give  up; conservative
    not_in _ (WpEvLam {}) = False    -- Ditto
    not_in _ (WpLet {})   = False    -- Ditto

    not_in_submult :: TyVar -> SubMultCo -> Bool
    not_in_submult v = \case
      EqMultCo co -> not (anyFreeVarsOfCo (== v) co)
      OneSubMult w -> not (anyFreeVarsOfType (== v) w)



pushWpLet :: TcEvBinds -> [HsWrapper] -> [HsWrapper]
-- See if we can transform
--    WpLet binds <.> w1 <.> .. <.> wn   -->   w1' <.> .. <.> wn'
-- by substitution.
-- We do this just for the narrow case when
--   - the `binds` are all just v=w, variables only
--   - the wi are all WpTyApp, WpEvApp, or WpCast
-- This is just enough to get us the eta-reductions that we seek
pushWpLet tc_ev_binds ws
  = case tc_ev_binds of
      TcEvBinds {} -> pprPanic "pushWpLet" (ppr tc_ev_binds)
      EvBinds binds
        | isEmptyBag binds
        -> ws
        | Just env <- ev_bind_swizzle binds
        -> case go env ws of
              Just ws' -> ws'
              Nothing  -> bale_out
        | otherwise
        -> bale_out
  where
    bale_out = WpLet tc_ev_binds : ws

    go :: IdEnv Id -> [HsWrapper] -> Maybe [HsWrapper]
    go env (WpCast co  : ws) = do { ws' <- go env ws
                                  ; return (WpCast co  : ws') }
    go env (WpTyApp ty : ws) = do { ws' <- go env ws
                                  ; return (WpTyApp ty : ws') }
    go env (WpEvApp (EvExpr (Var v)) : ws)
       = do { v'  <- swizzle_id env v
            ; ws' <- go env ws
            ; return (WpEvApp (EvExpr (Var v')) : ws') }

    go _ ws = case ws of
                 []    -> Just []
                 (_:_) -> Nothing  -- Could not fully eliminate the WpLet

    swizzle_id :: IdEnv Id -> Id -> Maybe Id
    -- Nothing <=> ran out of fuel
    -- This is just belt and braces; we should never build bottom evidence
    swizzle_id env v = go 100 v
      where
        go :: Int -> EvId -> Maybe EvId
        go fuel v
          | fuel == 0                     = Nothing
          | Just v' <- lookupVarEnv env v = go (fuel-1) v'
          | otherwise                     = Just v

    ev_bind_swizzle :: Bag EvBind -> Maybe (IdEnv Id)
    -- Succeeds only if the bindings are all var-to-var bindings
    ev_bind_swizzle evbs = foldl' do_one (Just emptyVarEnv) evbs
      where
        do_one :: Maybe (IdEnv Id) -> EvBind -> Maybe (IdEnv Id)
        do_one Nothing _ = Nothing
        do_one (Just swizzle) (EvBind {eb_lhs = bndr, eb_rhs = rhs})
          = case rhs of
               EvExpr (Var v) -> Just (extendVarEnv swizzle bndr v)
               _              -> Nothing

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

      ebv_tcvs :: IORef [TcCoercion]
      -- When we solve a Wanted by filling in a CoercionHole, it is as
      -- if we were adding an evidence binding
      --       co_hole := coercion
      -- We keep all these RHS coercions in a list, alongside `ebv_binds`,
      --  so that we can report unused given constraints,
      --  in GHC.Tc.Solver.neededEvVars
      -- See Note [Tracking redundant constraints] in GHC.Tc.Solver
      -- Also: we garbage-collect unused bindings in `neededEvVars`,
      --       so this matters for correctness too.
    }

  | CoEvBindsVar {  -- See Note [Coercion evidence only]

      -- See above for comments on ebv_uniq, ebv_tcvs
      ebv_uniq :: Unique,
      ebv_tcvs :: IORef [TcCoercion]
    }

instance Data.Data TcEvBinds where
  -- Placeholder; we can't traverse into TcEvBinds
  toConstr _   = abstractConstr "TcEvBinds"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = Data.mkNoRepType "TcEvBinds"
instance Data.Data EvBind where
  -- Placeholder; we can't traverse into EvBind
  toConstr _   = abstractConstr "TcEvBind"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = Data.mkNoRepType "EvBind"

{- Note [Coercion evidence only]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Class constraints etc give rise to /term/ bindings for evidence, and
we have nowhere to put term bindings in /types/.  So in some places we
use CoEvBindsVar (see newCoTcEvBinds) to signal that no term-level
evidence bindings are allowed.  Notably ():

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

-- | Union two evidence binding maps
unionEvBindMap :: EvBindMap -> EvBindMap -> EvBindMap
unionEvBindMap (EvBindMap env1) (EvBindMap env2) =
  EvBindMap { ev_bind_varenv = plusDVarEnv env1 env2 }

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

data EvBindInfo
  = EvBindGiven { -- See Note [Tracking redundant constraints] in GHC.Tc.Solver
    }
  | EvBindWanted { ebi_canonical :: CanonicalEvidence -- See Note [Desugaring non-canonical evidence]
    }

-----------------
-- All evidence is bound by EvBinds; no side effects
data EvBind
  = EvBind { eb_lhs  :: EvVar
           , eb_rhs  :: EvTerm
           , eb_info :: EvBindInfo
    }

evBindVar :: EvBind -> EvVar
evBindVar = eb_lhs

mkWantedEvBind :: EvVar -> CanonicalEvidence -> EvTerm -> EvBind
mkWantedEvBind ev c tm = EvBind { eb_info = EvBindWanted c, eb_lhs = ev, eb_rhs = tm }

-- EvTypeable are never given, so we can work with EvExpr here instead of EvTerm
mkGivenEvBind :: EvVar -> EvTerm -> EvBind
mkGivenEvBind ev tm = EvBind { eb_info = EvBindGiven, eb_lhs = ev, eb_rhs = tm }


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
      , et_binds :: TcEvBinds  -- This field is why we need an EvFun
                               -- constructor, and can't just use EvExpr
      , et_body  :: EvVar }

  deriving Data.Data

type EvExpr = CoreExpr

-- | Any sort of evidence Id, including coercions
evId ::  EvId -> EvExpr
evId = Var

-- coercion bindings
-- See Note [Coercion evidence terms]
evCoercion :: TcCoercion -> EvTerm
evCoercion co = EvExpr (Coercion co)

{-# DEPRECATED mkEvCast "Please use evCast instead" #-}
-- We had gotten duplicate functions; let's get rid of mkEvCast in due course
mkEvCast :: EvExpr -> TcCoercion -> EvTerm
mkEvCast = evCast

evCast :: EvExpr -> TcCoercion -> EvTerm
evCast et tc = EvExpr (evCastE et tc)

-- | d |> co
evCastE :: EvExpr -> TcCoercion -> EvExpr
evCastE ee co
  | assertPpr (coercionRole co == Representational)
              (vcat [text "Coercion of wrong role passed to evCastE:", ppr ee, ppr co]) $
    isReflCo co = ee
  | otherwise   = Cast ee co

evDFunApp :: DFunId -> [Type] -> [EvExpr] -> EvTerm
-- Dictionary instance application, including when the "dictionary function"
-- is actually the data construtor for a dictionary
evDFunApp df tys ets = EvExpr (evDFunAppE df tys ets)

evDFunAppE :: DFunId -> [Type] -> [EvExpr] -> EvExpr
evDFunAppE df tys ets = Var df `mkTyApps` tys `mkApps` ets

evDictApp :: Class -> [Type] -> [EvExpr] -> EvTerm
evDictApp cls tys args = EvExpr (evDictAppE cls tys args)

evDictAppE :: Class -> [Type] -> [EvExpr] -> EvExpr
evDictAppE cls tys args
  = case tyConSingleDataCon_maybe (classTyCon cls) of
      Just dc -> evDFunAppE (dataConWrapId dc) tys args
      Nothing -> pprPanic "evDictApp" (ppr cls)

evUnaryDictAppE :: Class -> [Type] -> EvExpr -> EvExpr
-- See (UCM6) in Note [Unary class magic] in GHC.Core.TyCon
evUnaryDictAppE cls tys meth
  = evDictAppE cls tys [meth]

evWrapIPE :: PredType -> EvExpr -> EvExpr
-- Given  pred = IP s ty
  --      et_tm :: ty
-- Return an EvTerm of type (IP s ty)
evWrapIPE pred ev_tm
  = evUnaryDictAppE cls tys ev_tm
  where
    (cls, tys) = getClassPredTys pred

evUnwrapIPE :: PredType -> EvExpr -> EvExpr
-- Given  pred = IP s ty
  --      et_tm :: (IP s ty)
-- Return an EvTerm of type ty
evUnwrapIPE pred ev_tm
  = mkApps (Var ip_sel) (map Type tys ++ [ev_tm])
  where
    (ip_sel, tys) = decomposeIPPred pred


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
-- See Note [Holes in expressions] in GHC.Hs.Expr.
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
    d :: Typeable a
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

Implicit parameters of type GHC.Stack.Types.CallStack (the /name/ of the
implicit parameter is not important, see (CS5) below) are solved as follows:

1. Plan NORMAL. Explicit, user-written occurrences of `?stk :: CallStack`, which
   have IPOccOrigin, are solved directly from the given IP, just like any other
   implicit-parameter constraint; see GHC.Tc.Solver.Dict.tryInertDicts. We can
   solve it from a Given or from another Wanted, if the two have the same type.

   For example, the occurrence of `?stk` in

     error :: (?stk :: CallStack) => String -> a
     error s = raise (ErrorCall (s ++ prettyCallStack ?stk))

   will be solved for the `?stk` in `error`s context as before.

2. Plan PUSH.  A /function call/ with a CallStack constraint, such as
   a call to `foo` where
        foo :: (?stk :: CallStack) => a
   will give rise to a Wanted constraint
        [W] d :: (?stk :: CallStack)    CtOrigin = OccurrenceOf "foo"

   We do /not/ solve this constraint from Givens, or from other
   Wanteds.  Rather, have a built-in mechanism in that solves it thus:
        d := EvCsPushCall "foo" <details of call-site of `foo`> d2
        [W] d2 :: (?stk :: CallStack)    CtOrigin = IPOccOrigin

   That is, `d` is a call-stack that has the `foo` call-site pushed on top of
   `d2`, which can now be solved normally (as in (1) above).  This is done as follows:
     - In GHC.Tc.Solver.Dict.canDictCt we do the pushing.
     - We only look up canonical constraints in the inert set

3. For a CallStack constraint, we choose how to solve it based on its CtOrigin:

     * solve it normally (plan NORMAL above)
         - IPOccOrigin (discussed above)
         - GivenOrigin (see (CS1) below)

     * push an item on the stack and emit a new constraint (plan PUSH above)
         - OccurrenceOf "foo" (discused above)
         - anything else      (see (CS1) below)

   This choice is by the predicate isPushCallStackOrigin_maybe

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


Wrinkles

(CS1) Which CtOrigins should qualify for plan PUSH?  Certainly ones that arise
   from a function call like (f a b).

   But (see #19918) when RebindableSyntax is involved we can function call whose
   CtOrigin is somethign like `IfThenElseOrigin`. See the defn of fun_orig in
   GHC.Tc.Gen.App.tcInstFun; it is this CtOrigin that is pinned on the
   constraints generated by functions in the "expansion" for rebindable
   syntax. c.f. GHC.Rename.Expr Note [Handling overloaded and rebindable
   constructs].

   So isPushCallStackOrigin_maybe has a fall-through for "anything else", and
   assumes that we should adopt plan PUSH for it.

   However we should /not/ take this fall-through for Given constraints
   (#25675).  So isPushCallStackOrigin_maybe identifies Givens as plan NORMAL.

(CS2) GHC should NEVER report an insoluble CallStack constraint.

(CS3) GHC should NEVER infer a CallStack constraint unless one was requested
  with a partial type signature (See GHC.Tc.Solver..pickQuantifiablePreds).

(CS4) A CallStack (defined in GHC.Stack.Types) is a [(String, SrcLoc)],
  where the String is the name of the binder that is used at the
  SrcLoc. SrcLoc is also defined in GHC.Stack.Types and contains the
  package/module/file name, as well as the full source-span. Both
  CallStack and SrcLoc are kept abstract so only GHC can construct new
  values.

(CS5) We will automatically solve any wanted CallStack regardless of the
  /name/ of the IP, i.e.

    f = show (?stk :: CallStack)
    g = show (?loc :: CallStack)

  are both valid. However, we will only push new SrcLocs onto existing
  CallStacks when the IP names match, e.g. in

    head :: (?loc :: CallStack) => [a] -> a
    head [] = error (show (?stk :: CallStack))

  the printed CallStack will NOT include head's call-site. This reflects the
  standard scoping rules of implicit-parameters.

(CS6) An EvCallStack term desugars to a CoreExpr of type `IP "some str" CallStack`.
  The desugarer will need to unwrap the IP newtype before pushing a new
  call-site onto a given stack (See GHC.HsToCore.Binds.dsEvCallStack)

(CS7) When we emit a new wanted CallStack in plan PUSH we set its origin to
  `IPOccOrigin ip_name` instead of the original `OccurrenceOf func`
  (see GHC.Tc.Solver.Dict.tryInertDicts).

  This is a bit shady, but is how we ensure that the new wanted is
  solved like a regular IP.
-}

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

evExprCoercion_maybe :: EvExpr -> Maybe TcCoercion
-- Applied only to EvExprs of type (s~t)
-- See Note [Coercion evidence terms]
evExprCoercion_maybe (Var v)       = return (mkCoVarCo v)
evExprCoercion_maybe (Coercion co) = return co
evExprCoercion_maybe (Cast tm co)  = do { co' <- evExprCoercion_maybe tm
                                        ; return (mkCoCast co' co) }
evExprCoercion_maybe _             = Nothing

evExprCoercion :: EvExpr -> TcCoercion
evExprCoercion tm = case evExprCoercion_maybe tm of
                      Just co -> co
                      Nothing -> pprPanic "evExprCoercion" (ppr tm)

evTermCoercion_maybe :: EvTerm -> Maybe TcCoercion
-- Applied only to EvTerms of type (s~t)
-- See Note [Coercion evidence terms]
evTermCoercion_maybe ev_term
  | EvExpr e <- ev_term = evExprCoercion_maybe e
  | otherwise           = Nothing

evTermCoercion :: EvTerm -> TcCoercion
evTermCoercion tm = case evTermCoercion_maybe tm of
                      Just co -> co
                      Nothing -> pprPanic "evTermCoercion" (ppr tm)

-- Used with Opt_DeferTypeErrors
-- See Note [Deferring coercion errors to runtime]
-- in GHC.Tc.Solver
evDelayedError :: Type -> String -> EvTerm
evDelayedError ty msg
  = EvExpr $
    let fail_expr = mkRuntimeErrorApp tYPE_ERROR_ID unitTy msg
    in mkWildCase fail_expr (unrestricted unitTy) ty []
       -- See Note [Incompleteness and linearity] in GHC.HsToCore.Utils
       -- c.f. mkErrorAppDs in GHC.HsToCore.Utils

{- *********************************************************************
*                                                                      *
                  Free variables
*                                                                      *
********************************************************************* -}

isNestedEvId :: Var -> Bool
-- Just returns /nested/ free evidence variables; i.e ones with Internal Names
-- Top-level ones (DFuns, dictionary selectors and the like) don't count
-- Evidence variables are always Ids; do not pick TyVars
isNestedEvId v = isId v && isInternalName (varName v)

nestedEvIdsOfTerm :: EvTerm -> VarSet
-- Returns only EvIds satisfying relevantEvId
nestedEvIdsOfTerm tm = fvVarSet (filterFV isNestedEvId (evTermFVs tm))

evTermFVs :: EvTerm -> FV
evTermFVs (EvExpr e)         = exprFVs e
evTermFVs (EvTypeable _ ev)  = evFVsOfTypeable ev
evTermFVs (EvFun { et_tvs = tvs, et_given = given
                 , et_binds = tc_ev_binds, et_body = v })
  = case tc_ev_binds of
      TcEvBinds {}  -> emptyFV  -- See Note [Free vars of EvFun]
      EvBinds binds -> addBndrsFV bndrs fvs
        where
          fvs = foldr (unionFV . evTermFVs . eb_rhs) (unitFV v) binds
          bndrs = foldr ((:) . eb_lhs) (tvs ++ given) binds

evTermFVss :: [EvTerm] -> FV
evTermFVss = mapUnionFV evTermFVs

evFVsOfTypeable :: EvTypeable -> FV
evFVsOfTypeable ev =
  case ev of
    EvTypeableTyCon _ e      -> mapUnionFV evTermFVs e
    EvTypeableTyApp e1 e2    -> evTermFVss [e1,e2]
    EvTypeableTrFun em e1 e2 -> evTermFVss [em,e1,e2]
    EvTypeableTyLit e        -> evTermFVs e

{- Note [Free vars of EvFun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Finding the free vars of an EvFun is made tricky by the fact the
bindings et_binds may be a mutable variable.  Fortunately, we
can just squeeze by.  Here's how.

* /During/ typechecking, `evTermFVs` is used only by `GHC.Tc.Solver.neededEvVars`
  * Each EvBindsVar in an et_binds field of an EvFun is /also/ in the
    ic_binds field of an Implication
  * So we can track usage via the processing for that implication,
    (see Note [Tracking redundant constraints] in GHC.Tc.Solver).
    We can ignore usage from the EvFun altogether.

* /After/ typechecking `evTermFVs` is used by `GHC.Iface.Ext.Ast`, but by
  then it has been zonked so we can get at the bindings.
-}

{- *********************************************************************
*                                                                      *
                  Pretty printing
*                                                                      *
********************************************************************* -}

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
    help it (WpCompose w1 w2)  = help (help it w2) w1
    help it (WpSubType w)      = no_parens $ text "subtype" <> braces (help it w False)
    help it (WpFun w arg_wrap res_wrap t1 _t2)
      = add_parens $ text "\\(x" <> dcolon <> brackets (ppr $ subMultCoRKind w) <> ppr t1 <> text ")." <+>
                     help (\_ -> it True <+> help (\_ -> text "x") arg_wrap True) res_wrap False
    help it (WpCast co)   = add_parens $ sep [it False, nest 2 (text "|>"
                                              <+> pprParendCo co)]
    help it (WpEvApp id)  = no_parens  $ sep [it True, nest 2 (ppr id)]
    help it (WpTyApp ty)  = no_parens  $ sep [it True, text "@" <> pprParendType ty]
    help it (WpEvLam id)  = add_parens $ sep [ text "\\" <> pprLamBndr id <> dot, it False]
    help it (WpTyLam tv)  = add_parens $ sep [text "/\\" <> pprLamBndr tv <> dot, it False]
    help it (WpLet binds) = add_parens $ sep [text "let" <+> braces (ppr binds), it False]

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
  ppr (EvBind { eb_lhs = v, eb_rhs = e, eb_info = info })
     = sep [ pp_gw <+> ppr v
           , nest 2 $ equals <+> ppr e ]
      -- We cheat a bit and pretend EqVars are CoVars for the purposes of pretty printing
     where
       pp_gw = brackets $ case info of
           EvBindGiven{}  -> char 'G'
           EvBindWanted{} -> char 'W'

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
  ppr (EvTypeableTyCon ts _)     = text "TyCon" <+> ppr ts
  ppr (EvTypeableTyApp t1 t2)    = parens (ppr t1 <+> ppr t2)
  ppr (EvTypeableTyLit t1)       = text "TyLit" <> ppr t1
  ppr (EvTypeableTrFun tm t1 t2) = parens (ppr t1 <+> arr <+> ppr t2)
    where
      arr = pprArrowWithMultiplicity visArgTypeLike (Right (ppr tm))

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
