
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DisambiguateRecordFields #-}

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.Gen.Head
       ( HsExprArg(..), EValArg(..), TcPass(..)
       , AppCtxt(..), appCtxtLoc, insideExpansion
       , splitHsApps, rebuildHsApps
       , addArgWrap, isHsValArg
       , leadingValArgs, isVisibleArg, pprHsExprArgTc

       , tcInferAppHead, tcInferAppHead_maybe
       , tcInferId, tcCheckId, obviousSig
       , tyConOf, tyConOfET, fieldNotInType
       , nonBidirectionalErr

       , addHeadCtxt, addExprCtxt, addStmtCtxt, addFunResCtxt ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcExpr, tcCheckPolyExprNC, tcPolyLExprSig )
import {-# SOURCE #-} GHC.Tc.Gen.Splice( getUntypedSpliceBody )

import GHC.Prelude
import GHC.Hs
import GHC.Hs.Syn.Type

import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Bind( chooseInferredQuantifiers )
import GHC.Tc.Gen.Sig( tcUserTypeSig, tcInstSig )
import GHC.Tc.TyCl.PatSyn( patSynBuilderOcc )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic )
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family ( tcLookupDataFamInst )
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Core.UsageEnv      ( singleUsageUE )
import GHC.Tc.Errors.Types
import GHC.Tc.Solver          ( InferMode(..), simplifyInfer )
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Zonk.TcType


import GHC.Core.PatSyn( PatSyn )
import GHC.Core.ConLike( ConLike(..) )
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.Error

import GHC.Builtin.Types( multiplicityTy )
import GHC.Builtin.Names
import GHC.Builtin.Names.TH( liftStringName, liftName )

import GHC.Driver.DynFlags
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Data.Maybe
import Control.Monad
import GHC.Rename.Unbound (WhatLooking(WL_Anything))



{- *********************************************************************
*                                                                      *
              HsExprArg: auxiliary data type
*                                                                      *
********************************************************************* -}

{- Note [HsExprArg]
~~~~~~~~~~~~~~~~~~~
The data type HsExprArg :: TcPass -> Type
is a very local type, used only within this module and GHC.Tc.Gen.App

* It's really a zipper for an application chain
  See Note [Application chains and heads] in GHC.Tc.Gen.App for
  what an "application chain" is.

* It's a GHC-specific type, so using TTG only where necessary

* It is indexed by TcPass, meaning
  - HsExprArg TcpRn:
      The result of splitHsApps, which decomposes a HsExpr GhcRn

  - HsExprArg TcpInst:
      The result of tcInstFun, which instantiates the function type
      Adds EWrap nodes, the argument type in EValArg,
      and the kind-checked type in ETypeArg

  - HsExprArg TcpTc:
      The result of tcArg, which typechecks the value args
      In EValArg we now have a (LHsExpr GhcTc)

* rebuildPrefixApps is dual to splitHsApps, and zips an application
  back into a HsExpr

Note [EValArg]
~~~~~~~~~~~~~~
The data type EValArg is the payload of the EValArg constructor of
HsExprArg; i.e. a value argument of the application.  EValArg has two
forms:

* ValArg: payload is just the expression itself. Simple.

* ValArgQL: captures the results of applying quickLookArg to the
  argument in a ValArg.  When we later want to typecheck that argument
  we can just carry on from where quick-look left off.  The fields of
  ValArgQL exactly capture what is needed to complete the job.

Invariants:

1. With QL switched off, all arguments are ValArg; no ValArgQL

2. With QL switched on, tcInstFun converts some ValArgs to ValArgQL,
   under the conditions when quick-look should happen (eg the argument
   type is guarded) -- see quickLookArg

Note [splitHsApps]
~~~~~~~~~~~~~~~~~~
The key function
  splitHsApps :: HsExpr GhcRn -> (HsExpr GhcRn, HsExpr GhcRn, [HsExprArg 'TcpRn])
takes apart either an HsApp, or an infix OpApp, returning

* The "head" of the application, an expression that is often a variable;
  this is used for typechecking

* The "user head" or "error head" of the application, to be reported to the
  user in case of an error.  Example:
         (`op` e)
  expands (via ExpandedThingRn) to
         (rightSection op e)
  but we don't want to see 'rightSection' in error messages. So we keep the
  innermost un-expanded head as the "error head".

* A list of HsExprArg, the arguments
-}

data TcPass = TcpRn     -- Arguments decomposed
            | TcpInst   -- Function instantiated
            | TcpTc     -- Typechecked

data HsExprArg (p :: TcPass)
  = -- See Note [HsExprArg]
    EValArg  { eva_ctxt   :: AppCtxt
             , eva_arg    :: EValArg p
             , eva_arg_ty :: !(XEVAType p) }

  | ETypeArg { eva_ctxt  :: AppCtxt
             , eva_hs_ty :: LHsWcType GhcRn  -- The type arg
             , eva_ty    :: !(XETAType p) }  -- Kind-checked type arg

  | EPrag    AppCtxt
             (HsPragE (GhcPass (XPass p)))

  | EWrap    EWrap

data EWrap = EPar    AppCtxt
           | EExpand HsThingRn
           | EHsWrap HsWrapper

data EValArg (p :: TcPass) where  -- See Note [EValArg]
  ValArg   :: LHsExpr (GhcPass (XPass p))
           -> EValArg p

  ValArgQL :: { va_expr :: LHsExpr GhcRn        -- Original application
                                                -- For location and error msgs
              , va_fun  :: (HsExpr GhcTc, AppCtxt) -- Function of the application,
                                                   -- typechecked, plus its context
              , va_args :: [HsExprArg 'TcpInst] -- Args, instantiated
              , va_ty   :: TcRhoType }          -- Result type
           -> EValArg 'TcpInst  -- Only exists in TcpInst phase

data AppCtxt
  = VAExpansion
       HsThingRn
       SrcSpan
       SrcSpan

  | VACall
       (HsExpr GhcRn) Int  -- In the third argument of function f
       SrcSpan             -- The SrcSpan of the application (f e1 e2 e3)
                         --    noSrcSpan if outermost; see Note [AppCtxt]

{- Note [AppCtxt]
~~~~~~~~~~~~~~~~~
In a call (f e1 ... en), we pair up each argument with an AppCtxt. For
example, the AppCtxt for e3 allows us to say
    "In the third argument of `f`"
See splitHsApps.

To do this we must take a quick look into the expression to find the
function at the head (`f` in this case) and how many arguments it
has. That is what the funcion top_ctxt does.

If the function part is an expansion, we don't want to look further.
For example, with rebindable syntax the expression
    (if e1 then e2 else e3) e4 e5
might expand to
    (ifThenElse e1 e2 e3) e4 e5
For e4 we an AppCtxt that says "In the first argument of (if ...)",
not "In the fourth argument of ifThenElse".  So top_ctxt stops
at expansions.

The SrcSpan in an AppCtxt describes the whole call.  We initialise
it to noSrcSpan, because splitHsApps deals in HsExpr not LHsExpr, so
we don't have a span for the whole call; and we use that noSrcSpan in
GHC.Tc.Gen.App.tcInstFun (set_fun_ctxt) to avoid pushing "In the expression `f`"
a second time.
-}

appCtxtLoc :: AppCtxt -> SrcSpan
appCtxtLoc (VAExpansion _ l _) = l
appCtxtLoc (VACall _ _ l)    = l

insideExpansion :: AppCtxt -> Bool
insideExpansion (VAExpansion {}) = True
insideExpansion (VACall {})      = False -- but what if the VACall has a generated context?

instance Outputable AppCtxt where
  ppr (VAExpansion e l _) = text "VAExpansion" <+> ppr e <+> ppr l
  ppr (VACall f n l)    = text "VACall" <+> int n <+> ppr f  <+> ppr l

type family XPass p where
  XPass 'TcpRn   = 'Renamed
  XPass 'TcpInst = 'Renamed
  XPass 'TcpTc   = 'Typechecked

type family XETAType p where  -- Type arguments
  XETAType 'TcpRn = NoExtField
  XETAType _      = Type

type family XEVAType p where  -- Value arguments
  XEVAType 'TcpRn = NoExtField
  XEVAType _      = Scaled Type

mkEValArg :: AppCtxt -> LHsExpr GhcRn -> HsExprArg 'TcpRn
mkEValArg ctxt e = EValArg { eva_arg = ValArg e, eva_ctxt = ctxt
                           , eva_arg_ty = noExtField }

mkETypeArg :: AppCtxt -> LHsWcType GhcRn -> HsExprArg 'TcpRn
mkETypeArg ctxt hs_ty =
  ETypeArg { eva_ctxt = ctxt
           , eva_hs_ty = hs_ty
           , eva_ty = noExtField }

addArgWrap :: HsWrapper -> [HsExprArg p] -> [HsExprArg p]
addArgWrap wrap args
 | isIdHsWrapper wrap = args
 | otherwise          = EWrap (EHsWrap wrap) : args

splitHsApps :: HsExpr GhcRn
            -> TcM ( (HsExpr GhcRn, AppCtxt)  -- Head
                   , [HsExprArg 'TcpRn])      -- Args
-- See Note [splitHsApps].
--
-- This uses the TcM monad solely because we must run modFinalizers when looking
-- through HsUntypedSplices
-- (see Note [Looking through Template Haskell splices in splitHsApps]).
splitHsApps e = go e (top_ctxt 0 e) []
  where
    top_ctxt :: Int -> HsExpr GhcRn -> AppCtxt
    -- Always returns VACall fun n_val_args noSrcSpan
    -- to initialise the argument splitting in 'go'
    -- See Note [AppCtxt]
    top_ctxt n (HsPar _ fun)               = top_lctxt n fun
    top_ctxt n (HsPragE _ _ fun)           = top_lctxt n fun
    top_ctxt n (HsAppType _ fun _)         = top_lctxt (n+1) fun
    top_ctxt n (HsApp _ fun _)             = top_lctxt (n+1) fun
    top_ctxt n (XExpr (ExpandedThingRn o _))
      | OrigExpr fun <- o                  = VACall fun  n noSrcSpan
    top_ctxt n other_fun                   = VACall other_fun n noSrcSpan

    top_lctxt n (L _ fun) = top_ctxt n fun

    go :: HsExpr GhcRn -> AppCtxt -> [HsExprArg 'TcpRn]
       -> TcM ((HsExpr GhcRn, AppCtxt), [HsExprArg 'TcpRn])
    -- Modify the AppCtxt as we walk inwards, so it describes the next argument
    go (HsPar _ (L l fun))           ctxt args = go fun (set l ctxt) (EWrap (EPar ctxt)     : args)
    go (HsPragE _ p (L l fun))       ctxt args = go fun (set l ctxt) (EPrag      ctxt p     : args)
    go (HsAppType _ (L l fun) ty)    ctxt args = go fun (dec l ctxt) (mkETypeArg ctxt ty    : args)
    go (HsApp _ (L l fun) arg)       ctxt args = go fun (dec l ctxt) (mkEValArg  ctxt arg   : args)

    -- See Note [Looking through Template Haskell splices in splitHsApps]
    go e@(HsUntypedSplice splice_res splice) ctxt args
      = do { fun <- getUntypedSpliceBody splice_res
           ; go fun ctxt' (EWrap (EExpand (OrigExpr e)) : args) }
      where
        ctxt' :: AppCtxt
        ctxt' = case splice of
            HsUntypedSpliceExpr _ (L l _) -> set l ctxt -- l :: SrcAnn AnnListItem
            HsQuasiQuote _ _ (L l _)      -> set l ctxt -- l :: SrcAnn NoEpAnns

    -- See Note [Looking through ExpandedThingRn]
    go (XExpr (ExpandedThingRn o e)) ctxt args
      | isHsThingRnExpr o
      = go e (VAExpansion o (appCtxtLoc ctxt) (appCtxtLoc ctxt))
               (EWrap (EExpand o) : args)

      | OrigStmt (L _ stmt) <- o                -- so that we set `(>>)` as generated
      , BodyStmt{} <- stmt                      -- and get the right unused bind warnings
      = go e (VAExpansion o generatedSrcSpan generatedSrcSpan)
                                                -- See Part 3. in Note [Expanding HsDo with XXExprGhcRn]
               (EWrap (EExpand o) : args)       -- in `GHC.Tc.Gen.Do`


      | OrigPat (L loc _) <- o                              -- so that we set the compiler generated fail context
      = go e (VAExpansion o (locA loc) (locA loc))          -- to be originating from a failable pattern
                                                            -- See Part 1. Wrinkle 2. of
               (EWrap (EExpand o) : args)                   -- Note [Expanding HsDo with XXExprGhcRn]
                                                            -- in `GHC.Tc.Gen.Do`

      | otherwise
      = go e (VAExpansion o (appCtxtLoc ctxt) (appCtxtLoc ctxt))
               (EWrap (EExpand o) : args)

    -- See Note [Desugar OpApp in the typechecker]
    go e@(OpApp _ arg1 (L l op) arg2) _ args
      = pure ( (op, VACall op 0 (locA l))
             ,   mkEValArg (VACall op 1 generatedSrcSpan) arg1
               : mkEValArg (VACall op 2 generatedSrcSpan) arg2
               : EWrap (EExpand (OrigExpr e))
               : args )

    go e ctxt args = pure ((e,ctxt), args)

    set :: EpAnn ann -> AppCtxt -> AppCtxt
    set l (VACall f n _)          = VACall f n (locA l)
    set l (VAExpansion orig ol _) = VAExpansion orig ol (locA l)

    dec :: EpAnn ann -> AppCtxt -> AppCtxt
    dec l (VACall f n _)          = VACall f (n-1) (locA l)
    dec l (VAExpansion orig ol _) = VAExpansion orig ol (locA l)

-- | Rebuild an application: takes a type-checked application head
-- expression together with arguments in the form of typechecked 'HsExprArg's
-- and returns a typechecked application of the head to the arguments.
--
-- This performs a representation-polymorphism check to ensure that
-- representation-polymorphic unlifted newtypes have been eta-expanded.
--
-- See Note [Eta-expanding rep-poly unlifted newtypes].
rebuildHsApps :: HsExpr GhcTc
                      -- ^ the function being applied
              -> AppCtxt
              -> [HsExprArg 'TcpTc]
                      -- ^ the arguments to the function
              -> TcRhoType
                      -- ^ result type of the application
              -> TcM (HsExpr GhcTc)
rebuildHsApps fun ctxt args app_res_rho
  = do { rejectRepPolyNewtypes args app_res_rho fun
       ; return $ rebuild_hs_apps fun ctxt args }

-- | The worker function for 'rebuildHsApps': simply rebuilds
-- an application chain in which arguments are specified as
-- typechecked 'HsExprArg's.
rebuild_hs_apps :: HsExpr GhcTc
                      -- ^ the function being applied
              -> AppCtxt
              -> [HsExprArg 'TcpTc]
                      -- ^ the arguments to the function
              -> HsExpr GhcTc
rebuild_hs_apps fun _ [] = fun
rebuild_hs_apps fun ctxt (arg : args)
  = case arg of
      EValArg { eva_arg = ValArg arg, eva_ctxt = ctxt' }
        -> rebuild_hs_apps (HsApp noExtField lfun arg) ctxt' args
      ETypeArg { eva_hs_ty = hs_ty, eva_ty = ty, eva_ctxt = ctxt' }
        -> rebuild_hs_apps (HsAppType ty lfun hs_ty) ctxt' args
      EPrag ctxt' p
        -> rebuild_hs_apps (HsPragE noExtField p lfun) ctxt' args
      EWrap (EPar ctxt')
        -> rebuild_hs_apps (gHsPar lfun) ctxt' args
      EWrap (EExpand orig)
        | OrigExpr oe <- orig
        -> rebuild_hs_apps (mkExpandedExprTc oe fun) ctxt args
        | otherwise
        -> rebuild_hs_apps fun ctxt args
      EWrap (EHsWrap wrap)
        -> rebuild_hs_apps (mkHsWrap wrap fun) ctxt args
  where
    lfun = L (noAnnSrcSpan $ appCtxtLoc' ctxt) fun
    appCtxtLoc' (VAExpansion _ _ l) = l
    appCtxtLoc' v = appCtxtLoc v

{- Note [Representation-polymorphic Ids with no binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We cannot have representation-polymorphic or levity-polymorphic
function arguments. See Note [Representation polymorphism invariants]
in GHC.Core.  That is checked in 'GHC.Tc.Gen.App.tcInstFun', see the call
to 'matchActualFunTy', which performs the representation-polymorphism
check.

However, some special Ids have representation-polymorphic argument
types. These are all GHC built-ins or data constructors. They have no binding;
instead they have compulsory unfoldings. Specifically, these Ids are:

1. Some wired-in Ids, such as coerce, oneShot and unsafeCoerce# (which is only
   partly wired-in),
2. Representation-polymorphic primops, such as raise#.
3. Representation-polymorphic data constructors: unboxed tuples
   and unboxed sums.
4. Newtype constructors with `UnliftedNewtypes` which have
   a representation-polymorphic argument.

For (1) consider
  badId :: forall r (a :: TYPE r). a -> a
  badId = unsafeCoerce# @r @r @a @a

The (partly) wired-in function
  unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                   (a :: TYPE r1) (b :: TYPE r2).
                   a -> b
has a convenient but representation-polymorphic type. It has no
binding; instead it has a compulsory unfolding, after which we
would have
  badId = /\r /\(a :: TYPE r). \(x::a). ...body of unsafeCorece#...
And this is no good because of that rep-poly \(x::a).  So we want
to reject this.

On the other hand
  goodId :: forall (a :: Type). a -> a
  goodId = unsafeCoerce# @LiftedRep @LiftedRep @a @a

is absolutely fine, because after we inline the unfolding, the \(x::a)
is representation-monomorphic.

Test cases: T14561, RepPolyWrappedVar2.

For primops (2) and unboxed tuples/sums (3), the situation is similar;
they are eta-expanded in CorePrep to be saturated, and that eta-expansion
must not add a representation-polymorphic lambda.

Test cases: T14561b, RepPolyWrappedVar, UnliftedNewtypesCoerceFail.

The Note [Representation-polymorphism checking built-ins] explains how we handle
cases (1) (2) and (3).

For (4), consider a representation-polymorphic newtype with
UnliftedNewtypes:

  type Id :: forall r. TYPE r -> TYPE r
  newtype Id a where { MkId :: a }

  bad :: forall r (a :: TYPE r). a -> Id a
  bad = MkId @r @a             -- Want to reject

  good :: forall (a :: Type). a -> Id a
  good = MkId @LiftedRep @a   -- Want to accept

Test cases: T18481, UnliftedNewtypesLevityBinder

(4) is handled differently than (1) (2) and (3);
see Note [Eta-expanding rep-poly unlifted newtypes].

Note [Representation-polymorphism checking built-ins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some primops and wired-in functions are representation-polymorphic, but must
only be instantiated at particular, concrete representations.
There are three cases, all for `hasNoBinding` Ids:

* Wired-in Ids.  For example, `seq`
  is a wired-in Id, defined in GHC.Types.Id.Make.seqId, with this type:

  seq :: forall {r} a (b :: TYPE r). a -> b -> b

  It is more like a macro than a regular Id: it has /compulsory/ unfolding, so
  we inline it at every call site.  At those call sites we should instantiate
  `r` with a concrete RuntimeRep, so that the lambda has a concrete representation.
  So somehow the type checker has to ensure that `seq` is called with a concrete
  instantiation for `r`.

  NB: unsafeCoerce# is not quite wired-in (see Note [Wiring in unsafeCoerce#] in GHC.HsToCore),
  but it gets a similar treatment.

* PrimOps. Some representation-polymorphic primops must be called at a concrete
  type.  For example:

  catch# :: forall {r} {l} (k :: TYPE r) (w :: TYPE (BoxedRep l)).
              (State# RealWorld -> (# State# RealWorld, k #) )
           -> (w -> State# RealWorld -> (# State# RealWorld, k #) )
           -> State# RealWorld -> (# State# RealWorld, k #)

  This primop pushes a "catch frame" on the stack, which must "know"
  the return convention of `k`.  So `k` must be concrete, so we know
  what kind of catch-frame to push. (See #21868 for more details.

  So again we want to ensure that `r` is instantiated with a concrete RuntimeRep.

* Unboxed-tuple data constructors.  Consider the unboxed pair data constructor:

  (#,#) :: forall {r1} {r2} (a :: TYPE r1) (b :: TYPE r2). a -> b -> (# a, b #)

  Again, we need concrete `r1` and `r2`. For example, we want to reject

    f :: forall r (a :: TYPE r). a -> (# Int, a #)
    f = (#,#) 3

As pointed out in #21906; we see here that it is not enough to simply check
the representation of the argument types, as for example "k :: TYPE r" in the
type of catch# occurs in negative position but not directly as the type of
an argument.

NB: we specifically *DO NOT* handle representation-polymorphic unlifted newtypes
with this mechanism. See Note [Eta-expanding rep-poly unlifted newtypes] for an
overview of representation-polymorphism checks for those.

To achieve this goal, for these these three kinds of `hasNoBinding` functions:

* We identify the quantified variable `r` as a "concrete quantifier"

* When instantiating a concrete quantifier, such as `r`, at a call site, we
  instantiate with a ConcreteTv meta-tyvar, `r0[conc]`.
  See Note [ConcreteTv] in GHC.Tc.Utils.Concrete.

Now the type checker will ensure that `r0` is instantiated with a concrete
RuntimeRep.

Here are the moving parts:

* In the IdDetails of an Id, we record a mapping from type variable name
  to concreteness information, in the form of a ConcreteTvOrigin.
  See 'idDetailsConcreteTvs'.

  The ConcreteTvOrigin is used to determine which error message to show
  to the user if the type variable gets instantiated to a non-concrete type;
  this is slightly more granular than simply storing a set of type variable names.

* The domain of this NameEnv is the outer forall'd TyVars of that
  Id's type.  (A bit yukky because it means that alpha-renaming that type
  would be invalid.  But we never do that.)  So `seq` has
    Type:       forall {r} a (b :: TYPE r). a -> b -> b
    IdDetails:  RepPolyId [ r :-> ConcreteFRR (FixedRuntimeRepOrigin b (..)) ]

* When instantiating the type of an Id at a call site, at the call to
  GHC.Tc.Utils.Instantiate.instantiateSigma in GHC.Tc.Gen.App.tcInstFun,
  create ConcreteTv metavariables (instead of TauTvs) based on the
  ConcreteTyVars stored in the IdDetails of the Id.

Note that the /only/ place that one of these restricted rep-poly Ids can enter
typechecking is in `tcInferId`, and all the interesting cases then land
in `tcInstFun` where we take care to instantantiate those concrete
type variables correctly.

  Design alternative: in some ways, it would be more kosher for the concrete-ness
  to be stored in the /type/, thus  forall (r[conc] :: RuntimeRep). ty.
  But that pollutes Type for a very narrow use-case; so instead we adopt the
  more ad-hoc solution described above.

Examples:

  ok :: forall (a :: Type) (b :: Type). a -> b -> b
  ok = seq

  bad :: forall s (b :: TYPE s). Int -> b -> b
  bad x = seq x

    Here we will instantiate the RuntimeRep skolem variable r from the type
    of seq to a concrete metavariable rr[conc].
    For 'ok' we will unify rr := LiftedRep, and for 'bad' we will fail to
    solve rr[conc] ~# s[sk] and report a representation-polymorphism error to
    the user.

  type RR :: RuntimeRep
  type family RR where { RR = IntRep }

  tricky1, tricky2 :: forall (b :: TYPE RR). Int -> b -> b
  tricky1 = seq
  tricky2 = seq @RR

    'tricky1' proceeds as above: we instantiate r |-> rr[conc], get a Wanted
    rr[conc] ~# RR, which we solve by rewriting the type family.

    For 'tricky2', we again create a fresh ConcreteTv metavariable rr[conc],
    and we then proceed as if the user had written "seq @rr", but adding an
    additional [W] rr ~ RR to the constraint solving context.

[Wrinkle: VTA]

  We must also handle the case when the user has instantiated the type variables
  themselves, with a visible type application. We do this in GHC.Tc.Gen.App.tcVTA.

  For example:

    type F :: Type -> RuntimeRep
    type family F a where { F Bool = IntRep }

    foo = (# , #) @(F Bool) @FloatRep

  We want to accept "foo" even though "F Bool" is not a concrete RuntimeRep.
  We proceed as follows (see tcVTA):

    - create a fresh concrete metavariable kappa,
    - emit [W] F Bool ~ kappa[conc]
    - pretend the user wrote (#,#) @kappa.

  The solver will then unify kappa := IntRep, after rewriting the type family
  application on the LHS of the Wanted.

  Note that this is a bit of a corner case: only a few built-ins, such as
  unsafeCoerce# and unboxed tuples, have specified (not inferred) RuntimeRep
  quantified variables which can be instantiated by the user with a
  visible type application.
  For example,

    coerce :: forall {r :: RuntimeRep} (a :: TYPE r) (b :: TYPE r)
           .  Coercible a b => a -> b

  does not allow the RuntimeRep argument to be specified by a visible type
  application.

Note [Eta-expanding rep-poly unlifted newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Any occurrence of a newtype constructor must appear at a known representation.
If the newtype is applied to an argument, then we are done: by (I2) in
Note [Representation polymorphism invariants], the argument has a known
representation, and we are done. So we are left with the situation of an
unapplied newtype constructor. For example:

  type N :: TYPE r -> TYPE r
  newtype N a = MkN a

  ok :: N Int# -> N Int#
  ok = MkN

  bad :: forall r (a :: TYPE r). N (# Int, r #) -> N (# Int, r #)
  bad = MkN

The difficulty is that, unlike the situation described in
Note [Representation-polymorphism checking built-ins],
it is not necessarily the case that we simply need to check the instantiation
of a single variable. Consider for example:

  type RR :: Type -> Type -> RuntimeRep
  type family RR a b where ...

  type T :: forall a -> forall b -> TYPE (RR a b)
  type family T a b where ...

  type M :: forall a -> forall b -> TYPE (RR a b)
  newtype M a b = MkM (T a b)

Now, suppose we instantiate MkM, say with two types X, Y from the environment:

  foo :: T X Y -> M X Y
  foo = MkM @X @Y

we need to check that we can eta-expand MkM, for which we need to know the
representation of its argument, which is "RR X Y".

To do this, in "rejectRepPolyNewtypes", we perform a syntactic representation-
polymorphism check on the instantiated argument of the newtype, and reject
the definition if the representation isn't concrete (in the sense of Note [Concrete types]
in GHC.Tc.Utils.Concrete).

For example, we would accept "ok" above, as "IntRep" is a concrete RuntimeRep.
However, we would reject "foo", because "RR X Y" is not a concrete RuntimeRep.
If we wanted to accept "foo" (performing a PHASE 2 check (in the sense of
Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete), we would have to
significantly re-engineer unlifted newtypes in GHC. Currently, "MkM" has type:

  MkM :: forall a b. T a b %1 -> M a b

However, we should only be able to use MkM when we know the representation of
T a b (which is RR a b). This means that MkM should instead have type:

  MkM :: forall {must_be_conc} a b (co :: RR a b ~# must_be_conc)
      .  T a b |> GRefl Nominal (TYPE co) %1 -> M a b

where "must_be_conc" is a skolem type variable that must be instantiated to
a concrete type, just as in Note [Representation-polymorphism checking built-ins].
This means that any instantiation of "MkM", such as "MkM @X @Y" from "foo",
would create a fresh concrete metavariable "gamma[conc]" and emit a Wanted constraint

  [W] co :: RR X Y ~# gamma[conc]

However, this all seems like a lot of work for a feature that no one is asking for,
so we decided to keep the much simpler syntactic check. Note that one possible
advantage of this approach is that we should be able to stop skipping
representation-polymorphism checks in the output of the desugarer; see (C) in
Wrinkle [Representation-polymorphic lambdas] in Note [Typechecking data constructors].
-}

-- | Reject any unsaturated use of an unlifted newtype constructor
-- if the representation of its argument isn't known.
--
-- See Note [Eta-expanding rep-poly unlifted newtypes].
rejectRepPolyNewtypes :: [HsExprArg 'TcpTc]
                      -> TcRhoType
                      -> HsExpr GhcTc
                      -> TcM ()
rejectRepPolyNewtypes _applied_args app_res_rho fun = case fun of

  XExpr (ConLikeTc (RealDataCon con) _ _)
    -- Check that this is an unsaturated occurrence of a
    -- representation-polymorphic newtype constructor.
    | isNewDataCon con
    , not $ tcHasFixedRuntimeRep $ dataConTyCon con
    , Just (_rem_arg_af, _rem_arg_mult, rem_arg_ty, _nt_res_ty)
        <- splitFunTy_maybe app_res_rho
    -> do { let frr_ctxt = FRRRepPolyUnliftedNewtype con
          ; hasFixedRuntimeRep_syntactic frr_ctxt rem_arg_ty }

  _ -> return ()

isHsValArg :: HsExprArg id -> Bool
isHsValArg (EValArg {}) = True
isHsValArg _            = False

leadingValArgs :: [HsExprArg id] -> [EValArg id]
leadingValArgs []                        = []
leadingValArgs (arg@(EValArg {}) : args) = eva_arg arg : leadingValArgs args
leadingValArgs (EWrap {}    : args)      = leadingValArgs args
leadingValArgs (EPrag {}    : args)      = leadingValArgs args
leadingValArgs (ETypeArg {} : _)         = []

isValArg :: HsExprArg id -> Bool
isValArg (EValArg {}) = True
isValArg _            = False

isVisibleArg :: HsExprArg id -> Bool
isVisibleArg (EValArg {})  = True
isVisibleArg (ETypeArg {}) = True
isVisibleArg _             = False

instance OutputableBndrId (XPass p) => Outputable (HsExprArg p) where
  ppr (EValArg { eva_arg = arg })      = text "EValArg" <+> ppr arg
  ppr (EPrag _ p)                      = text "EPrag" <+> ppr p
  ppr (ETypeArg { eva_hs_ty = hs_ty }) = char '@' <> ppr hs_ty
  ppr (EWrap wrap)                     = ppr wrap

instance Outputable EWrap where
  ppr (EPar _)       = text "EPar"
  ppr (EHsWrap w)    = text "EHsWrap" <+> ppr w
  ppr (EExpand orig) = text "EExpand" <+> ppr orig

instance OutputableBndrId (XPass p) => Outputable (EValArg p) where
  ppr (ValArg e) = ppr e
  ppr (ValArgQL { va_fun = fun, va_args = args, va_ty = ty})
    = hang (text "ValArgQL" <+> ppr fun)
         2 (vcat [ ppr args, text "va_ty:" <+> ppr ty ])

pprHsExprArgTc :: HsExprArg 'TcpInst -> SDoc
pprHsExprArgTc (EValArg { eva_arg = tm, eva_arg_ty = ty })
  = text "EValArg" <+> hang (ppr tm) 2 (dcolon <+> ppr ty)
pprHsExprArgTc arg = ppr arg

{- Note [Desugar OpApp in the typechecker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Operator sections are desugared in the renamer; see GHC.Rename.Expr
Note [Handling overloaded and rebindable constructs].
But for reasons explained there, we rename OpApp to OpApp.  Then,
here in the typechecker, we desugar it to a use of ExpandedThingRn.
That makes it possible to typecheck something like
     e1 `f` e2
where
   f :: forall a. t1 -> forall b. t2 -> t3

Note [Looking through ExpandedThingRn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When creating an application chain in splitHsApps, we must deal with
     ExpandedThingRn f1 (f `HsApp` e1) `HsApp` e2 `HsApp` e3

as a single application chain `f e1 e2 e3`.  Otherwise stuff like overloaded
labels (#19154) won't work.

It's easy to achieve this: `splitHsApps` unwraps `ExpandedThingRn`.

In order to be able to more accurately reconstruct the original `SrcSpan`s
from the renamer in `rebuildHsApps`, we also have to track the `SrcSpan`
of the current application in `VAExpansion` when unwrapping `ExpandedThingRn`
in `splitHsApps`, just as we track it in a non-expanded expression.

Previously, `rebuildHsApps` substituted the location of the original
expression as given by `splitHsApps` for this. As a result, the application
head in expanded expressions, e.g. the call to `fromListN`, would either
have `noSrcSpan` set as its location post-typecheck, or get the location
of the original expression, depending on whether the `XExpr` given to
`splitHsApps` is in the outermost layer. The span it got in the renamer
would always be discarded, causing #23120.

Note [Looking through Template Haskell splices in splitHsApps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking an application, we must look through untyped TH splices in
order to typecheck examples like the one in #21077:

  data Foo = MkFoo () (forall a. a -> a)

  foo :: Foo
  foo = $([| MkFoo () |]) $ \x -> x

In principle, this is straightforward to accomplish. By the time we typecheck
`foo`, the renamer will have already run the splice, so all we have to do is
look at the expanded version of the splice in `splitHsApps`. See the
`HsUntypedSplice` case in `splitHsApps` for how this is accomplished.

There is one slight complication in that untyped TH splices also include
modFinalizers (see Note [Delaying modFinalizers in untyped splices] in
GHC.Rename.Splice), which must be run during typechecking. splitHsApps is a
convenient place to run the modFinalizers, so we do so there. This is the only
reason that `splitHsApps` uses the TcM monad.

`HsUntypedSplice` covers both ordinary TH splices, such as the example above,
as well as quasiquotes (see Note [Quasi-quote overview] in
Language.Haskell.Syntax.Expr). The `splitHsApps` case for `HsUntypedSplice`
handles both of these. This is easy to accomplish, since all the real work in
handling splices and quasiquotes has already been performed by the renamer by
the time we get to `splitHsApps`.

Wrinkle (UTS1):
  `tcExpr` has a separate case for `HsUntypedSplice`s that do /not/ occur at the
  head of an application. This is important to handle programs like this one:

    foo :: (forall a. a -> a) -> b -> b
    foo = $([| \g x -> g x |])

  Here, it is vital that we push the expected type inwards so that `g` gets the
  type `forall a. a -> a`, and the `tcExpr` case for `HsUntypedSplice` performs
  this pushing. Without it, we would instead infer `g` to have type `b -> b`,
  which isn't sufficiently general. Unfortunately, this does mean that there are
  two different places in the code where an `HsUntypedSplice`'s modFinalizers can
  be ran, depending on whether the splice appears at the head of an application
  or not.
-}

{- *********************************************************************
*                                                                      *
                 tcInferAppHead
*                                                                      *
********************************************************************* -}

tcInferAppHead :: (HsExpr GhcRn, AppCtxt)
               -> TcM (HsExpr GhcTc, TcSigmaType)
-- Infer type of the head of an application
--   i.e. the 'f' in (f e1 ... en)
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
-- We get back a /SigmaType/ because we have special cases for
--   * A bare identifier (just look it up)
--     This case also covers a record selector HsRecSel
--   * An expression with a type signature (e :: ty)
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
--
-- Note that [] and (,,) are both HsVar:
--   see Note [Empty lists] and [ExplicitTuple] in GHC.Hs.Expr
--
-- NB: 'e' cannot be HsApp, HsTyApp, HsPrag, HsPar, because those
--     cases are dealt with by splitHsApps.
--
-- See Note [tcApp: typechecking applications] in GHC.Tc.Gen.App
tcInferAppHead (fun,ctxt)
  = addHeadCtxt ctxt $
    do { mb_tc_fun <- tcInferAppHead_maybe fun
       ; case mb_tc_fun of
            Just (fun', fun_sigma) -> return (fun', fun_sigma)
            Nothing -> tcInfer (tcExpr fun) }

tcInferAppHead_maybe :: HsExpr GhcRn
                     -> TcM (Maybe (HsExpr GhcTc, TcSigmaType))
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
-- Returns Nothing for a complicated head
tcInferAppHead_maybe fun
  = case fun of
      HsVar _ (L _ nm)          -> Just <$> tcInferId nm
      HsRecSel _ f              -> Just <$> tcInferRecSelId f
      ExprWithTySig _ e hs_ty   -> Just <$> tcExprWithSig e hs_ty
      HsOverLit _ lit           -> Just <$> tcInferOverLit lit
      _                         -> return Nothing

addHeadCtxt :: AppCtxt -> TcM a -> TcM a
addHeadCtxt (VAExpansion (OrigStmt (L loc stmt)) _ _) thing_inside =
  do setSrcSpanA loc $
       addStmtCtxt stmt
         thing_inside
addHeadCtxt fun_ctxt thing_inside
  | not (isGoodSrcSpan fun_loc)   -- noSrcSpan => no arguments
  = thing_inside                  -- => context is already set
  | otherwise
  = setSrcSpan fun_loc $
    do case fun_ctxt of
         VAExpansion (OrigExpr orig) _ _ -> addExprCtxt orig thing_inside
         _                               -> thing_inside
  where
    fun_loc = appCtxtLoc fun_ctxt


{- *********************************************************************
*                                                                      *
                 Record selectors
*                                                                      *
********************************************************************* -}

tcInferRecSelId :: FieldOcc GhcRn
                -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferRecSelId (FieldOcc sel_name lbl)
   = do { sel_id <- tc_rec_sel_id
        ; let expr = HsRecSel noExtField (FieldOcc sel_id lbl)
        ; return (expr, idType sel_id)
        }
     where
       occ :: OccName
       occ = rdrNameOcc (unLoc lbl)

       tc_rec_sel_id :: TcM TcId
       -- Like tc_infer_id, but returns an Id not a HsExpr,
       -- so we can wrap it back up into a HsRecSel
       tc_rec_sel_id
         = do { thing <- tcLookup sel_name
              ; case thing of
                    ATcId { tct_id = id }
                      -> do { check_naughty occ id  -- See Note [Local record selectors]
                            ; check_local_id id
                            ; return id }

                    AGlobal (AnId id)
                      -> do { check_naughty occ id
                            ; return id }
                           -- A global cannot possibly be ill-staged
                           -- nor does it need the 'lifting' treatment
                           -- hence no checkTh stuff here

                    _ -> failWithTc $ TcRnExpectedValueId thing }

------------------------

-- A type signature on the argument of an ambiguous record selector or
-- the record expression in an update must be "obvious", i.e. the
-- outermost constructor ignoring parentheses.
obviousSig :: HsExpr GhcRn -> Maybe (LHsSigWcType GhcRn)
obviousSig (ExprWithTySig _ _ ty) = Just ty
obviousSig (HsPar _ p)            = obviousSig (unLoc p)
obviousSig (HsPragE _ _ p)        = obviousSig (unLoc p)
obviousSig _                      = Nothing

-- Extract the outermost TyCon of a type, if there is one; for
-- data families this is the representation tycon (because that's
-- where the fields live).
tyConOf :: FamInstEnvs -> TcSigmaType -> Maybe TyCon
tyConOf fam_inst_envs ty0
  = case tcSplitTyConApp_maybe ty of
      Just (tc, tys) -> Just (fstOf3 (tcLookupDataFamInst fam_inst_envs tc tys))
      Nothing        -> Nothing
  where
    (_, _, ty) = tcSplitSigmaTy ty0

-- Variant of tyConOf that works for ExpTypes
tyConOfET :: FamInstEnvs -> ExpRhoType -> Maybe TyCon
tyConOfET fam_inst_envs ty0 = tyConOf fam_inst_envs =<< checkingExpType_maybe ty0

fieldNotInType :: RecSelParent -> RdrName -> TcRnMessage
fieldNotInType p rdr
  = mkTcRnNotInScope rdr $
    UnknownSubordinate (text "field of type" <+> quotes (ppr p))


{- *********************************************************************
*                                                                      *
                Expressions with a type signature
                        expr :: type
*                                                                      *
********************************************************************* -}

tcExprWithSig :: LHsExpr GhcRn -> LHsSigWcType (NoGhcTc GhcRn)
              -> TcM (HsExpr GhcTc, TcSigmaType)
tcExprWithSig expr hs_ty
  = do { sig_info <- checkNoErrs $  -- Avoid error cascade
                     tcUserTypeSig loc hs_ty Nothing
       ; (expr', poly_ty) <- tcExprSig expr sig_info
       ; return (ExprWithTySig noExtField expr' hs_ty, poly_ty) }
  where
    loc = getLocA (dropWildCards hs_ty)

tcExprSig :: LHsExpr GhcRn -> TcIdSig -> TcM (LHsExpr GhcTc, TcSigmaType)
tcExprSig expr (TcCompleteSig sig)
   = do { expr' <- tcPolyLExprSig expr sig
        ; return (expr', idType (sig_bndr sig)) }

tcExprSig expr sig@(TcPartialSig (PSig { psig_name = name, psig_loc = loc }))
  = setSrcSpan loc $   -- Sets the location for the implication constraint
    do { (tclvl, wanted, (expr', sig_inst))
             <- pushLevelAndCaptureConstraints  $
                do { sig_inst <- tcInstSig sig
                   ; expr' <- tcExtendNameTyVarEnv (mapSnd binderVar $ sig_inst_skols sig_inst) $
                              tcExtendNameTyVarEnv (sig_inst_wcs   sig_inst) $
                              tcCheckPolyExprNC expr (sig_inst_tau sig_inst)
                   ; return (expr', sig_inst) }
       -- See Note [Partial expression signatures]
       ; let tau = sig_inst_tau sig_inst
             infer_mode | null (sig_inst_theta sig_inst)
                        , isNothing (sig_inst_wcx sig_inst)
                        = ApplyMR
                        | otherwise
                        = NoRestrictions
       ; ((qtvs, givens, ev_binds, _), residual)
           <- captureConstraints $ simplifyInfer tclvl infer_mode [sig_inst] [(name, tau)] wanted
       ; emitConstraints residual

       ; tau <- liftZonkM $ zonkTcType tau
       ; let inferred_theta = map evVarPred givens
             tau_tvs        = tyCoVarsOfType tau
       ; (binders, my_theta) <- chooseInferredQuantifiers residual inferred_theta
                                   tau_tvs qtvs (Just sig_inst)
       ; let inferred_sigma = mkInfSigmaTy qtvs inferred_theta tau
             my_sigma       = mkInvisForAllTys binders (mkPhiTy  my_theta tau)
       ; wrap <- if inferred_sigma `eqType` my_sigma -- NB: eqType ignores vis.
                 then return idHsWrapper  -- Fast path; also avoids complaint when we infer
                                          -- an ambiguous type and have AllowAmbiguousType
                                          -- e..g infer  x :: forall a. F a -> Int
                 else tcSubTypeSigma ExprSigOrigin (ExprSigCtxt NoRRC) inferred_sigma my_sigma

       ; traceTc "tcExpSig" (ppr qtvs $$ ppr givens $$ ppr inferred_sigma $$ ppr my_sigma)
       ; let poly_wrap = wrap
                         <.> mkWpTyLams qtvs
                         <.> mkWpEvLams givens
                         <.> mkWpLet  ev_binds
       ; return (mkLHsWrap poly_wrap expr', my_sigma) }


{- Note [Partial expression signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Partial type signatures on expressions are easy to get wrong.  But
here is a guiding principle
    e :: ty
should behave like
    let x :: ty
        x = e
    in x

So for partial signatures we apply the MR if no context is given.  So
   e :: IO _          apply the MR
   e :: _ => IO _     do not apply the MR
just like in GHC.Tc.Gen.Bind.decideGeneralisationPlan

This makes a difference (#11670):
   peek :: Ptr a -> IO CLong
   peek ptr = peekElemOff undefined 0 :: _
from (peekElemOff undefined 0) we get
          type: IO w
   constraints: Storable w

We must NOT try to generalise over 'w' because the signature specifies
no constraints so we'll complain about not being able to solve
Storable w.  Instead, don't generalise; then _ gets instantiated to
CLong, as it should.
-}


{- *********************************************************************
*                                                                      *
                 Overloaded literals
*                                                                      *
********************************************************************* -}

tcInferOverLit :: HsOverLit GhcRn -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferOverLit lit@(OverLit { ol_val = val
                            , ol_ext = OverLitRn { ol_rebindable = rebindable
                                                 , ol_from_fun = L loc from_name } })
  = -- Desugar "3" to (fromInteger (3 :: Integer))
    --   where fromInteger is gotten by looking up from_name, and
    --   the (3 :: Integer) is returned by mkOverLit
    -- Ditto the string literal "foo" to (fromString ("foo" :: String))
    do { hs_lit <- mkOverLit val
       ; from_id <- tcLookupId from_name
       ; (wrap1, from_ty) <- topInstantiate (LiteralOrigin lit) (idType from_id)
       ; let
           thing    = NameThing from_name
           mb_thing = Just thing
           herald   = ExpectedFunTyArg thing (HsLit noExtField hs_lit)
       ; (wrap2, sarg_ty, res_ty) <- matchActualFunTy herald mb_thing (1, from_ty) from_ty

       ; co <- unifyType mb_thing (hsLitType hs_lit) (scaledThing sarg_ty)
       -- See Note [Source locations for implicit function calls] in GHC.Iface.Ext.Ast
       ; let lit_expr = L (l2l loc) $ mkHsWrapCo co $
                        HsLit noExtField hs_lit
             from_expr = mkHsWrap (wrap2 <.> wrap1) $
                         HsVar noExtField (L loc from_id)
             witness = HsApp noExtField (L (l2l loc) from_expr) lit_expr
             lit' = lit { ol_ext = OverLitTc { ol_rebindable = rebindable
                                             , ol_witness = witness
                                             , ol_type = res_ty } }
       ; return (HsOverLit noExtField lit', res_ty) }

{- *********************************************************************
*                                                                      *
                 tcInferId, tcCheckId
*                                                                      *
********************************************************************* -}

tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTc)
tcCheckId name res_ty
  = do { (expr, actual_res_ty) <- tcInferId name
       ; traceTc "tcCheckId" (vcat [ppr name, ppr actual_res_ty, ppr res_ty])
       ; addFunResCtxt rn_fun [] actual_res_ty res_ty $
         tcWrapResultO (OccurrenceOf name) rn_fun expr actual_res_ty res_ty }
  where
    rn_fun = HsVar noExtField (noLocA name)

------------------------
tcInferId :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Look up an occurrence of an Id
-- Do not instantiate its type
tcInferId id_name
  | id_name `hasKey` assertIdKey
  = do { dflags <- getDynFlags
       ; if gopt Opt_IgnoreAsserts dflags
         then tc_infer_id id_name
         else tc_infer_assert id_name }

  | otherwise
  = do { (expr, ty) <- tc_infer_id id_name
       ; traceTc "tcInferId" (ppr id_name <+> dcolon <+> ppr ty)
       ; return (expr, ty) }

tc_infer_assert :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Deal with an occurrence of 'assert'
-- See Note [Adding the implicit parameter to 'assert']
tc_infer_assert assert_name
  = do { assert_error_id <- tcLookupId assertErrorName
       ; (wrap, id_rho) <- topInstantiate (OccurrenceOf assert_name)
                                          (idType assert_error_id)
       ; return (mkHsWrap wrap (HsVar noExtField (noLocA assert_error_id)), id_rho)
       }

tc_infer_id :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
tc_infer_id id_name
 = do { thing <- tcLookup id_name
      ; case thing of
             ATcId { tct_id = id }
               -> do { check_local_id id
                     ; return_id id }

             AGlobal (AnId id) -> return_id id
               -- A global cannot possibly be ill-staged
               -- nor does it need the 'lifting' treatment
               -- Hence no checkTh stuff here

             AGlobal (AConLike (RealDataCon con)) -> tcInferDataCon con
             AGlobal (AConLike (PatSynCon ps)) -> tcInferPatSyn id_name ps
             (tcTyThingTyCon_maybe -> Just tc) -> failIllegalTyCon WL_Anything tc -- TyCon or TcTyCon
             ATyVar name _ -> failIllegalTyVal name

             _ -> failWithTc $ TcRnExpectedValueId thing }
  where
    return_id id = return (HsVar noExtField (noLocA id), idType id)

{- Note [Suppress hints with RequiredTypeArguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a type variable is used at the term level, GHC assumes the user might
have made a typo and suggests a term variable with a similar name.

For example, if the user writes
  f (Proxy :: Proxy nap) (Proxy :: Proxy gap) = nap (+1) [1,2,3]
then GHC will helpfully suggest `map` instead of `nap`
  • Illegal term-level use of the type variable ‘nap’
  • Perhaps use ‘map’ (imported from Prelude)

Importantly, GHC does /not/ suggest `gap`, which is in scope.
Question: How does GHC know not to suggest `gap`?  After all, the edit distance
          between `map`, `nap`, and `gap` is equally short.
Answer: GHC takes the namespace into consideration. `gap` is a `tvName`, and GHC
        would only suggest a `varName` at the term level.

In other words, the current hint infrastructure assumes that the namespace of an
entity is a reliable indicator of its level
   term-level name <=> term-level entity
   type-level name <=> type-level entity

With RequiredTypeArguments, this assumption does not hold. Consider
  bad :: forall a b -> ...
  bad nap gap = nap

This use of `nap` on the RHS is illegal because `nap` stands for a type
variable. It cannot be returned as the result of a function. At the same time,
it is bound as a `varName`, i.e. in the term-level namespace.

Unless we suppress hints, GHC gets awfully confused
    • Illegal term-level use of the variable ‘nap’
    • Perhaps use one of these:
        ‘nap’ (line 2), ‘gap’ (line 2), ‘map’ (imported from Prelude)

GHC shouldn't suggest `gap`, which is also a type variable; using it would
result in the same error. And it especially shouldn't suggest using `nap`
instead of `nap`, which is absurd.

The proper solution is to overhaul the hint system to consider what a name
stands for instead of looking at its namespace alone. This is tracked in #24231.
As a temporary measure, we avoid those potentially misleading hints by
suppressing them entirely if RequiredTypeArguments is in effect.
-}

check_local_id :: Id -> TcM ()
check_local_id id
  = do { checkThLocalId id
       ; tcEmitBindingUsage $ singleUsageUE id }

check_naughty :: OccName -> TcId -> TcM ()
check_naughty lbl id
  | isNaughtyRecordSelector id = failWithTc (TcRnRecSelectorEscapedTyVar lbl)
  | otherwise                  = return ()

tcInferDataCon :: DataCon -> TcM (HsExpr GhcTc, TcSigmaType)
-- See Note [Typechecking data constructors]
tcInferDataCon con
  = do { let tvbs  = dataConUserTyVarBinders con
             tvs   = binderVars tvbs
             theta = dataConOtherTheta con
             args  = dataConOrigArgTys con
             res   = dataConOrigResTy con
             stupid_theta = dataConStupidTheta con

       ; scaled_arg_tys <- mapM linear_to_poly args

       ; let full_theta  = stupid_theta ++ theta
             all_arg_tys = map unrestricted full_theta ++ scaled_arg_tys
                -- We are building the type of the data con wrapper, so the
                -- type must precisely match the construction in
                -- GHC.Core.DataCon.dataConWrapperType.
                -- See Note [Instantiating stupid theta]
                -- in GHC.Core.DataCon.

       ; return ( XExpr (ConLikeTc (RealDataCon con) tvs all_arg_tys)
                , mkInvisForAllTys tvbs $ mkPhiTy full_theta $
                  mkScaledFunTys scaled_arg_tys res ) }
  where
    linear_to_poly :: Scaled Type -> TcM (Scaled Type)
    -- linear_to_poly implements point (3,4)
    -- of Note [Typechecking data constructors]
    linear_to_poly (Scaled OneTy ty) = do { mul_var <- newFlexiTyVarTy multiplicityTy
                                          ; return (Scaled mul_var ty) }
    linear_to_poly scaled_ty         = return scaled_ty

tcInferPatSyn :: Name -> PatSyn -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferPatSyn id_name ps
  = case patSynBuilderOcc ps of
       Just (expr,ty) -> return (expr,ty)
       Nothing        -> failWithTc (nonBidirectionalErr id_name)

nonBidirectionalErr :: Name -> TcRnMessage
nonBidirectionalErr = TcRnPatSynNotBidirectional

{- Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker transforms (assert e1 e2) to (assertError e1 e2).
This isn't really the Right Thing because there's no way to "undo"
if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.

Note [Typechecking data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As per Note [Polymorphisation of linear fields] in
GHC.Core.Multiplicity, linear fields of data constructors get a
polymorphic multiplicity when the data constructor is used as a term:

    Just :: forall {p} a. a %p -> Maybe a

So at an occurrence of a data constructor we do the following:

1. Typechecking, in tcInferDataCon.

  a. Get the original type of the constructor, say
     K :: forall (r :: RuntimeRep) (a :: TYPE r). a %1 -> T r a
     Note the %1: it is linear

  b. We are going to return a ConLikeTc, thus:
     XExpr (ConLikeTc K [r,a] [Scaled p a])
      :: forall (r :: RuntimeRep) (a :: TYPE r). a %p -> T r a
   where 'p' is a fresh multiplicity unification variable.

   To get the returned ConLikeTc, we allocate a fresh multiplicity
   variable for each linear argument, and store the type, scaled by
   the fresh multiplicity variable in the ConLikeTc; along with
   the type of the ConLikeTc. This is done by linear_to_poly.

   If the argument is not linear (perhaps explicitly declared as
   non-linear by the user), don't bother with this.

2. Desugaring, in dsConLike.

  a. The (ConLikeTc K [r,a] [Scaled p a]) is desugared to
     (/\r (a :: TYPE r). \(x %p :: a). K @r @a x)
   which has the desired type given in the previous bullet.

   The 'p' is the multiplicity unification variable, which
   will by now have been unified to something, or defaulted in
   `GHC.Tc.Zonk.Type.commitFlexi`. So it won't just be an
   (unbound) variable.

   So a saturated application (K e), where e::Int will desugar to
     (/\r (a :: TYPE r). ..etc..)
        @LiftedRep @Int e
   and all those lambdas will beta-reduce away in the simple optimiser
   (see Wrinkle [Representation-polymorphic lambdas] below).

   But for an /unsaturated/ application, such as `map (K @LiftedRep @Int) xs`,
   beta reduction will leave (\x %Many :: Int. K x), which is the type `map`
   expects whereas if we had just plain K, with its linear type, we'd
   get a type mismatch. That's why we do this funky desugaring.

Wrinkles

  [ConLikeTc arguments]

    Note that the [TcType] argument to ConLikeTc is strictly redundant; those are
    the type variables from the dataConUserTyVarBinders of the data constructor.
    Similarly in the [Scaled TcType] field of ConLikeTc, the types come directly
    from the data constructor.  The only bit that /isn't/ redundant is the
    fresh multiplicity variables!

    So an alternative would be to define ConLikeTc like this:
        | ConLikeTc [TcType]    -- Just the multiplicity variables
    But then the desugarer would need to repeat some of the work done here.
    So for now at least ConLikeTc records this strictly-redundant info.

  [Representation-polymorphic lambdas]

    The lambda expression we produce in (4) can have representation-polymorphic
    arguments, as indeed in (/\r (a :: TYPE r). \(x %p :: a). K @r @a x),
    we have a lambda-bound variable x :: (a :: TYPE r).
    This goes against the representation polymorphism invariants given in
    Note [Representation polymorphism invariants] in GHC.Core. The trick is that
    this this lambda will always be instantiated in a way that upholds the invariants.
    This is achieved as follows:

      A. Any arguments to such lambda abstractions are guaranteed to have
         a fixed runtime representation. This is enforced in 'tcApp' by
         'matchActualFunTy'.

      B. If there are fewer arguments than there are bound term variables,
         we will ensure that the appropriate type arguments are instantiated
         concretely, such as 'r' in

         ( /\r (a :: TYPE r). \ (x %p :: a). K @r @a x) @IntRep @Int#
           :: Int# -> T IntRep Int#

         See Note [Representation-polymorphic Ids with no binding] in GHC.Tc.Gen.Head.

      C. In the output of the desugarer in (4) above, we have a representation
         polymorphic lambda, which Lint would normally reject. So for that one
         pass, we switch off Lint's representation-polymorphism checks; see
         the `lf_check_fixed_rep` flag in `LintFlags`.
-}

{-
************************************************************************
*                                                                      *
                 Template Haskell checks
*                                                                      *
************************************************************************
-}

checkThLocalId :: Id -> TcM ()
-- The renamer has already done checkWellStaged,
--   in RnSplice.checkThLocalName, so don't repeat that here.
-- Here we just add constraints for cross-stage lifting
checkThLocalId id
  = do  { mb_local_use <- getStageAndBindLevel (idName id)
        ; case mb_local_use of
             Just (top_lvl, bind_lvl, use_stage)
                | thLevel use_stage > bind_lvl
                -> checkCrossStageLifting top_lvl id use_stage
             _  -> return ()   -- Not a locally-bound thing, or
                               -- no cross-stage link
    }

--------------------------------------
checkCrossStageLifting :: TopLevelFlag -> Id -> ThStage -> TcM ()
-- If we are inside typed brackets, and (use_lvl > bind_lvl)
-- we must check whether there's a cross-stage lift to do
-- Examples   \x -> [|| x ||]
--            [|| map ||]
--
-- This is similar to checkCrossStageLifting in GHC.Rename.Splice, but
-- this code is applied to *typed* brackets.

checkCrossStageLifting top_lvl id (Brack _ (TcPending ps_var lie_var q))
  | isTopLevel top_lvl
  = when (isExternalName id_name) (keepAlive id_name)
    -- See Note [Keeping things alive for Template Haskell] in GHC.Rename.Splice

  | otherwise
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [|| h x ||]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the splice proxy, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same splice proxy, but that doesn't
        -- matter, although it's a mite untidy.
    do  { let id_ty = idType id
        ; checkTc (isTauTy id_ty) $
          TcRnTHError $ TypedTHError $ SplicePolymorphicLocalVar id
               -- If x is polymorphic, its occurrence sites might
               -- have different instantiations, so we can't use plain
               -- 'x' as the splice proxy name.  I don't know how to
               -- solve this, and it's probably unimportant, so I'm
               -- just going to flag an error for now

        ; lift <- if isStringTy id_ty then
                     do { sid <- tcLookupId GHC.Builtin.Names.TH.liftStringName
                                     -- See Note [Lifting strings]
                        ; return (HsVar noExtField (noLocA sid)) }
                  else
                     setConstraintVar lie_var   $
                          -- Put the 'lift' constraint into the right LIE
                     newMethodFromName (OccurrenceOf id_name)
                                       GHC.Builtin.Names.TH.liftName
                                       [getRuntimeRep id_ty, id_ty]

                   -- Warning for implicit lift (#17804)
        ; addDetailedDiagnostic (TcRnImplicitLift $ idName id)

                   -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; let pending_splice = PendingTcSplice id_name
                                 (nlHsApp (mkLHsWrap (applyQuoteWrapper q) (noLocA lift))
                                          (nlHsVar id))
        ; writeMutVar ps_var (pending_splice : ps)

        ; return () }
  where
    id_name = idName id

checkCrossStageLifting _ _ _ = return ()

{-
Note [Lifting strings]
~~~~~~~~~~~~~~~~~~~~~~
If we see $(... [| s |] ...) where s::String, we don't want to
generate a mass of Cons (CharL 'x') (Cons (CharL 'y') ...)) etc.
So this conditional short-circuits the lifting mechanism to generate
(liftString "xy") in that case.  I didn't want to use overlapping instances
for the Lift class in TH.Syntax, because that can lead to overlapping-instance
errors in a polymorphic situation.

If this check fails (which isn't impossible) we get another chance; see
Note [Converting strings] in Convert.hs

Note [Local record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. GHC.Tc.TyCl.Utils.mkRecSelBinds.
-}


{- *********************************************************************
*                                                                      *
         Error reporting for function result mis-matches
*                                                                      *
********************************************************************* -}

addFunResCtxt :: HsExpr GhcRn -> [HsExprArg 'TcpRn]
              -> TcType -> ExpRhoType
              -> TcM a -> TcM a
-- When we have a mis-match in the return type of a function
-- try to give a helpful message about too many/few arguments
-- But not in generated code, where we don't want
-- to mention internal (rebindable syntax) function names
addFunResCtxt fun args fun_res_ty env_ty thing_inside
  = do { env_tv  <- newFlexiTyVarTy liftedTypeKind
       ; dumping <- doptM Opt_D_dump_tc_trace
       ; addLandmarkErrCtxtM (\env -> (env, ) <$> mk_msg dumping env_tv) thing_inside }
      -- NB: use a landmark error context, so that an empty context
      -- doesn't suppress some more useful context
  where
    mk_msg dumping env_tv
      = do { mb_env_ty <- readExpType_maybe env_ty
                     -- by the time the message is rendered, the ExpType
                     -- will be filled in (except if we're debugging)
           ; fun_res' <- zonkTcType fun_res_ty
           ; env'     <- case mb_env_ty of
                           Just env_ty -> zonkTcType env_ty
                           Nothing     -> do { massert dumping; return env_tv }
           ; let -- See Note [Splitting nested sigma types in mismatched
                 --           function types]
                 (_, _, fun_tau) = tcSplitNestedSigmaTys fun_res'
                 (_, _, env_tau) = tcSplitNestedSigmaTys env'
                     -- env_ty is an ExpRhoTy, but with simple subsumption it
                     -- is not deeply skolemised, so still use tcSplitNestedSigmaTys
                 (args_fun, res_fun) = tcSplitFunTys fun_tau
                 (args_env, res_env) = tcSplitFunTys env_tau
                 n_fun = length args_fun
                 n_env = length args_env
                 info  | -- Check for too few args
                         --  fun_tau = a -> b, res_tau = Int
                         n_fun > n_env
                       , not_fun res_env
                       = text "Probable cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too few arguments"

                       | -- Check for too many args
                         -- fun_tau = a -> Int,   res_tau = a -> b -> c -> d
                         -- The final guard suppresses the message when there
                         -- aren't enough args to drop; eg. the call is (f e1)
                         n_fun < n_env
                       , not_fun res_fun
                       , (n_fun + count isValArg args) >= n_env
                          -- Never suggest that a naked variable is
                                           -- applied to too many args!
                       = text "Possible cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too many arguments"

                       | otherwise
                       = Outputable.empty

           ; return info }

    not_fun ty   -- ty is definitely not an arrow type,
                 -- and cannot conceivably become one
      = case tcSplitTyConApp_maybe ty of
          Just (tc, _) -> isAlgTyCon tc
          Nothing      -> False

{-
Note [Splitting nested sigma types in mismatched function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When one applies a function to too few arguments, GHC tries to determine this
fact if possible so that it may give a helpful error message. It accomplishes
this by checking if the type of the applied function has more argument types
than supplied arguments.

Previously, GHC computed the number of argument types through tcSplitSigmaTy.
This is incorrect in the face of nested foralls, however!
This caused Ticket #13311, for instance:

  f :: forall a. (Monoid a) => Int -> forall b. (Monoid b) => Maybe a -> Maybe b

If one uses `f` like so:

  do { f; putChar 'a' }

Then tcSplitSigmaTy will decompose the type of `f` into:

  Tyvars: [a]
  Context: (Monoid a)
  Argument types: []
  Return type: Int -> forall b. Monoid b => Maybe a -> Maybe b

That is, it will conclude that there are *no* argument types, and since `f`
was given no arguments, it won't print a helpful error message. On the other
hand, tcSplitNestedSigmaTys correctly decomposes `f`'s type down to:

  Tyvars: [a, b]
  Context: (Monoid a, Monoid b)
  Argument types: [Int, Maybe a]
  Return type: Maybe b

So now GHC recognizes that `f` has one more argument type than it was actually
provided.

Notice that tcSplitNestedSigmaTys looks through function arrows too, regardless
of simple/deep subsumption.  Here we are concerned only whether there is a
mis-match in the number of value arguments.
-}


{- *********************************************************************
*                                                                      *
             Misc utility functions
*                                                                      *
********************************************************************* -}

addStmtCtxt :: ExprStmt GhcRn -> TcRn a -> TcRn a
addStmtCtxt stmt thing_inside
  = do let err_doc = pprStmtInCtxt (HsDoStmt (DoExpr Nothing)) stmt
       addErrCtxt err_doc thing_inside
  where
    pprStmtInCtxt :: HsStmtContextRn -> StmtLR GhcRn GhcRn (LHsExpr GhcRn) -> SDoc
    pprStmtInCtxt ctxt stmt
      = vcat [ hang (text "In a stmt of"
                     <+> pprAStmtContext ctxt <> colon) 2 (pprStmt stmt)
             ]

addExprCtxt :: HsExpr GhcRn -> TcRn a -> TcRn a
addExprCtxt e thing_inside
  = case e of
      XExpr (HsUnboundVarRn {}) -> thing_inside
      _ -> addErrCtxt (exprCtxt e) thing_inside
   -- The HsUnboundVarRn special case addresses situations like
   --    f x = _
   -- when we don't want to say "In the expression: _",
   -- because it is mentioned in the error message itself

exprCtxt :: HsExpr GhcRn -> SDoc
exprCtxt expr = hang (text "In the expression:") 2 (ppr (stripParensHsExpr expr))
