
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
       , countLeadingValArgs, isVisibleArg, pprHsExprArgTc
       , countVisAndInvisValArgs, countHsWrapperInvisArgs

       , tcInferAppHead, tcInferAppHead_maybe
       , tcInferId, tcCheckId
       , obviousSig
       , tyConOf, tyConOfET, fieldNotInType
       , nonBidirectionalErr

       , addHeadCtxt, addExprCtxt, addFunResCtxt ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcExpr, tcCheckMonoExprNC, tcCheckPolyExprNC )

import GHC.Prelude
import GHC.Hs

import GHC.Tc.Gen.HsType
import GHC.Rename.Unbound     ( unknownNameSuggestions, WhatLooking(..) )

import GHC.Tc.Gen.Bind( chooseInferredQuantifiers )
import GHC.Tc.Gen.Sig( tcUserTypeSig, tcInstSig, lhsSigWcTypeContextSpan )
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
import GHC.Hs.Syn.Type

import GHC.Core.PatSyn( PatSyn )
import GHC.Core.ConLike( ConLike(..) )
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type

import GHC.Types.Var( isInvisibleFunArg )
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

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import GHC.Data.Maybe
import Control.Monad



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
  expands (via HsExpanded) to
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
             , eva_at    :: !(LHsToken "@" GhcRn)
             , eva_hs_ty :: LHsWcType GhcRn  -- The type arg
             , eva_ty    :: !(XETAType p) }  -- Kind-checked type arg

  | EPrag    AppCtxt
             (HsPragE (GhcPass (XPass p)))

  | EWrap    EWrap

data EWrap = EPar    AppCtxt
           | EExpand (HsExpr GhcRn)
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
       (HsExpr GhcRn)    -- Inside an expansion of this expression
       SrcSpan           -- The SrcSpan of the expression
                         --    noSrcSpan if outermost; see Note [AppCtxt]

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
appCtxtLoc (VAExpansion _ l) = l
appCtxtLoc (VACall _ _ l)    = l

insideExpansion :: AppCtxt -> Bool
insideExpansion (VAExpansion {}) = True
insideExpansion (VACall {})      = False

instance Outputable AppCtxt where
  ppr (VAExpansion e _) = text "VAExpansion" <+> ppr e
  ppr (VACall f n _)    = text "VACall" <+> int n <+> ppr f

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

mkETypeArg :: AppCtxt -> LHsToken "@" GhcRn -> LHsWcType GhcRn -> HsExprArg 'TcpRn
mkETypeArg ctxt at hs_ty =
  ETypeArg { eva_ctxt = ctxt
           , eva_at = at, eva_hs_ty = hs_ty
           , eva_ty = noExtField }

addArgWrap :: HsWrapper -> [HsExprArg 'TcpInst] -> [HsExprArg 'TcpInst]
addArgWrap wrap args
 | isIdHsWrapper wrap = args
 | otherwise          = EWrap (EHsWrap wrap) : args

splitHsApps :: HsExpr GhcRn
            -> ( (HsExpr GhcRn, AppCtxt)  -- Head
               , [HsExprArg 'TcpRn])      -- Args
-- See Note [splitHsApps]
splitHsApps e = go e (top_ctxt 0 e) []
  where
    top_ctxt :: Int -> HsExpr GhcRn -> AppCtxt
    -- Always returns VACall fun n_val_args noSrcSpan
    -- to initialise the argument splitting in 'go'
    -- See Note [AppCtxt]
    top_ctxt n (HsPar _ _ fun _)           = top_lctxt n fun
    top_ctxt n (HsPragE _ _ fun)           = top_lctxt n fun
    top_ctxt n (HsAppType _ fun _ _)         = top_lctxt (n+1) fun
    top_ctxt n (HsApp _ fun _)             = top_lctxt (n+1) fun
    top_ctxt n (XExpr (HsExpanded orig _)) = VACall orig      n noSrcSpan
    top_ctxt n other_fun                   = VACall other_fun n noSrcSpan

    top_lctxt n (L _ fun) = top_ctxt n fun

    go :: HsExpr GhcRn -> AppCtxt -> [HsExprArg 'TcpRn]
       -> ((HsExpr GhcRn, AppCtxt), [HsExprArg 'TcpRn])
    -- Modify the AppCtxt as we walk inwards, so it describes the next argument
    go (HsPar _ _ (L l fun) _)       ctxt args = go fun (set l ctxt) (EWrap (EPar ctxt)     : args)
    go (HsPragE _ p (L l fun))       ctxt args = go fun (set l ctxt) (EPrag      ctxt p     : args)
    go (HsAppType _ (L l fun) at ty) ctxt args = go fun (dec l ctxt) (mkETypeArg ctxt at ty : args)
    go (HsApp _ (L l fun) arg)       ctxt args = go fun (dec l ctxt) (mkEValArg  ctxt arg   : args)

    -- See Note [Looking through HsExpanded]
    go (XExpr (HsExpanded orig fun)) ctxt args
      = go fun (VAExpansion orig (appCtxtLoc ctxt))
               (EWrap (EExpand orig) : args)

    -- See Note [Desugar OpApp in the typechecker]
    go e@(OpApp _ arg1 (L l op) arg2) _ args
      = ( (op, VACall op 0 (locA l))
        ,   mkEValArg (VACall op 1 generatedSrcSpan) arg1
          : mkEValArg (VACall op 2 generatedSrcSpan) arg2
          : EWrap (EExpand e)
          : args )

    go e ctxt args = ((e,ctxt), args)

    set :: SrcSpanAnnA -> AppCtxt -> AppCtxt
    set l (VACall f n _)        = VACall f n (locA l)
    set _ ctxt@(VAExpansion {}) = ctxt

    dec :: SrcSpanAnnA -> AppCtxt -> AppCtxt
    dec l (VACall f n _)        = VACall f (n-1) (locA l)
    dec _ ctxt@(VAExpansion {}) = ctxt

-- | Rebuild an application: takes a type-checked application head
-- expression together with arguments in the form of typechecked 'HsExprArg's
-- and returns a typechecked application of the head to the arguments.
--
-- This performs a representation-polymorphism check to ensure that the
-- remaining value arguments in an application have a fixed RuntimeRep.
--
-- See Note [Checking for representation-polymorphic built-ins].
rebuildHsApps :: HsExpr GhcTc
                      -- ^ the function being applied
              -> AppCtxt
              -> [HsExprArg 'TcpTc]
                      -- ^ the arguments to the function
              -> TcRhoType
                      -- ^ result type of the application
              -> TcM (HsExpr GhcTc)
rebuildHsApps fun ctxt args app_res_rho
  = do { tcRemainingValArgs args app_res_rho fun
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
        -> rebuild_hs_apps (HsApp noAnn lfun arg) ctxt' args
      ETypeArg { eva_hs_ty = hs_ty, eva_at = at, eva_ty = ty, eva_ctxt = ctxt' }
        -> rebuild_hs_apps (HsAppType ty lfun at hs_ty) ctxt' args
      EPrag ctxt' p
        -> rebuild_hs_apps (HsPragE noExtField p lfun) ctxt' args
      EWrap (EPar ctxt')
        -> rebuild_hs_apps (gHsPar lfun) ctxt' args
      EWrap (EExpand orig)
        -> rebuild_hs_apps (XExpr (ExpansionExpr (HsExpanded orig fun))) ctxt args
      EWrap (EHsWrap wrap)
        -> rebuild_hs_apps (mkHsWrap wrap fun) ctxt args
  where
    lfun = L (noAnnSrcSpan $ appCtxtLoc ctxt) fun

{- Note [Checking for representation-polymorphic built-ins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We cannot have representation-polymorphic or levity-polymorphic
function arguments. See Note [Representation polymorphism invariants]
in GHC.Core.  That is checked by the calls to `hasFixedRuntimeRep` in
`tcEValArg`.

But some /built-in/ functions have representation-polymorphic argument
types. Users can't define such Ids; they are all GHC built-ins or data
constructors.  Specifically they are:

1. A few wired-in Ids such as coerce and unsafeCoerce#,
2. Primops, such as raise#.
3. Newtype constructors with `UnliftedNewtypes` which have
   a representation-polymorphic argument.
4. Representation-polymorphic data constructors: unboxed tuples
   and unboxed sums.

For (1) consider
  badId :: forall r (a :: TYPE r). a -> a
  badId = unsafeCoerce# @r @r @a @a

The wired-in function
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

For primops (2) the situation is similar; they are eta-expanded in
CorePrep to be saturated, and that eta-expansion must not add a
representation-polymorphic lambda.

Test cases: T14561b, RepPolyWrappedVar, UnliftedNewtypesCoerceFail.

For (3), consider a representation-polymorphic newtype with
UnliftedNewtypes:

  type Id :: forall r. TYPE r -> TYPE r
  newtype Id a where { MkId :: a }

  bad :: forall r (a :: TYPE r). a -> Id a
  bad = MkId @r @a             -- Want to reject

  good :: forall (a :: Type). a -> Id a
  good = MkId @LiftedRep @a   -- Want to accept

Test cases: T18481, UnliftedNewtypesLevityBinder

So these cases need special treatment. We add a special case
in tcApp to check whether an application of an Id has any remaining
representation-polymorphic arguments, after instantiation and application
of previous arguments.  This is achieved by the tcRemainingValArgs
function, which computes the types of the remaining value arguments, and checks
that each of these have a fixed runtime representation.

Note that this function also ensures that data constructors always
appear saturated, by performing eta-expansion if necessary.
See Note [Typechecking data constructors].

Wrinkle [Arity]

  We don't want to check for arguments past the arity of the function.

  For example

      raise# :: forall {r :: RuntimeRep} (a :: Type) (b :: TYPE r). a -> b

  has arity 1, so an instantiation such as:

      foo :: forall w r (z :: TYPE r). w -> z -> z
      foo = raise# @w @(z -> z)

  is unproblematic.  This means we must take care not to perform a
  representation-polymorphism check on `z`.

  To achieve this, we consult the arity of the 'Id' which is the head
  of the application (or just use 1 for a newtype constructor),
  and keep track of how many value-level arguments we have seen,
  to ensure we stop checking once we reach the arity.
  This is slightly complicated by the need to include both visible
  and invisible arguments, as the arity counts both:
  see GHC.Tc.Gen.Head.countVisAndInvisValArgs.

  Test cases: T20330{a,b}

Wrinkle [Syntactic check]

  We only perform a syntactic check in tcRemainingValArgs. That is,
  we will reject partial applications such as:

    type RR :: RuntimeREp
    type family RR where { RR = IntRep }
    type T :: TYPE RR
    type family T where { T = Int# }

    (# , #) @LiftedRep @RR e1

  Why do we reject? Wee would need to elaborate this partial application
  of (# , #) as follows:

    let x1 = e1
    in
      ( \ @(ty2 :: TYPE RR) (x2 :: ty2 |> TYPE RR[0])
      -> ( ( (# , #) @LiftedRep @RR @Char @ty2 x1 ) |> co1 )
           x2
      ) |> co2

  That is, we need to cast the partial application

    (# , #) @LiftedRep @RR @Char @ty2 x1

  so that the next argument we provide to it has a fixed RuntimeRep,
  and then eta-expand it. This is quite tricky, and other parts
  of the compiler aren't set up to handle this mix of applications
  and casts (e.g. checkCanEtaExpand in GHC.Core.Lint).

  So we refrain from doing so, and instead limit ourselves to a simple syntactic
  check. See the wiki page https://gitlab.haskell.org/ghc/ghc/-/wikis/Remaining-ValArgs
  for a more in-depth discussion.
-}

-- | Typecheck the remaining value arguments in a partial application,
-- ensuring they have a fixed RuntimeRep in the sense of Note [Fixed RuntimeRep]
-- in GHC.Tc.Utils.Concrete.
--
-- Example:
--
-- > repPolyId :: forall r (a :: TYPE r). a -> a
-- > repPolyId = coerce
--
-- This is an invalid instantiation of 'coerce', as we can't eta expand it
-- to
--
-- > \@r \@(a :: TYPE r) (x :: a) -> coerce @r @a @a x
--
-- because the binder `x` does not have a fixed runtime representation.
tcRemainingValArgs :: HasDebugCallStack
                   => [HsExprArg 'TcpTc]
                   -> TcRhoType
                   -> HsExpr GhcTc
                   -> TcM ()
tcRemainingValArgs applied_args app_res_rho fun = case fun of

  HsVar _ (L _ fun_id)

    -- (1): unsafeCoerce#
    -- 'unsafeCoerce#' is peculiar: it is patched in manually as per
    -- Note [Wiring in unsafeCoerce#] in GHC.HsToCore.
    -- Unfortunately, even though its arity is set to 1 in GHC.HsToCore.mkUnsafeCoercePrimPair,
    -- at this stage, if we query idArity, we get 0. This is because
    -- we end up looking at the non-patched version of unsafeCoerce#
    -- defined in Unsafe.Coerce, and that one indeed has arity 0.
    --
    -- We thus manually specify the correct arity of 1 here.
    | idName fun_id == unsafeCoercePrimName
    -> tc_remaining_args 1 (RepPolyWiredIn fun_id)

    -- (2): primops and other wired-in representation-polymorphic functions,
    -- such as 'rightSection', 'oneShot', etc; see bindings with Compulsory unfoldings
    -- in GHC.Types.Id.Make
    | isWiredInName (idName fun_id) && hasNoBinding fun_id
    -> tc_remaining_args (idArity fun_id) (RepPolyWiredIn fun_id)
       -- NB: idArity consults the IdInfo of the Id. This can be a problem
       -- in the presence of hs-boot files, as we might not have finished
       -- typechecking; inspecting the IdInfo at this point can cause
       -- strange Core Lint errors (see #20447).
       -- We avoid this entirely by only checking wired-in names,
       -- as those are the only ones this check is applicable to anyway.

  XExpr (ConLikeTc (RealDataCon con) _ _)
    -- (3): Representation-polymorphic newtype constructors.
    | isNewDataCon con
    -- (4): Unboxed tuples and unboxed sums
    || isUnboxedTupleDataCon con
    || isUnboxedSumDataCon con
    -> tc_remaining_args (dc_val_arity con) (RepPolyDataCon con)

  _ -> return ()

  where

    dc_val_arity :: DataCon -> Arity
    dc_val_arity con = count (not . isEqPrimPred) (dataConTheta con)
                     + length (dataConStupidTheta con)
                     + dataConSourceArity con
      -- Count how many value-level arguments this data constructor expects:
      --    - dictionary arguments from the context (including the stupid context),
      --    - source value arguments.
      -- Tests: EtaExpandDataCon, EtaExpandStupid{1,2}.

    nb_applied_vis_val_args :: Int
    nb_applied_vis_val_args = count isHsValArg applied_args

    nb_applied_val_args :: Int
    nb_applied_val_args = countVisAndInvisValArgs applied_args

    tc_remaining_args :: Arity -> RepPolyFun -> TcM ()
    tc_remaining_args arity rep_poly_fun =
      tc_rem_args
        (nb_applied_vis_val_args + 1)
        (nb_applied_val_args + 1)
        rem_arg_tys

      where

      rem_arg_tys :: [(Scaled Type, FunTyFlag)]
      rem_arg_tys = getRuntimeArgTys app_res_rho
        -- We do not need to zonk app_res_rho first, because the number of arrows
        -- in the (possibly instantiated) inferred type of the function will
        -- be at least the arity of the function.

      -- The following function is essentially "mapM hasFixedRuntimeRep rem_arg_tys",
      -- but we need to keep track of indices for error messages, hence the manual recursion.
      tc_rem_args :: Int
                     -- visible value argument index, starting from 1
                     -- (only used to report the argument position in error messages)
                  -> Int
                     -- value argument index, starting from 1
                     -- used to count up to the arity to ensure that
                     -- we don't check too many argument types
                  -> [(Scaled Type, FunTyFlag)]
                     -- run-time argument types
                  -> TcM ()
      tc_rem_args _ i_val _
        | i_val > arity
        = return ()
      tc_rem_args _ _ []
        -- Should never happen: it would mean that the arity is higher
        -- than the number of arguments apparent from the type.
        = pprPanic "tcRemainingValArgs" debug_msg
      tc_rem_args i_visval !i_val ((Scaled _ arg_ty, af) : tys)
        = do { let (i_visval', arg_pos)
                     | isInvisibleFunArg af = ( i_visval    , ArgPosInvis )
                     | otherwise            = ( i_visval + 1, ArgPosVis i_visval )
                   frr_ctxt = FRRNoBindingResArg rep_poly_fun arg_pos
             ; hasFixedRuntimeRep_syntactic frr_ctxt arg_ty
                 -- Why is this a syntactic check? See Wrinkle [Syntactic check] in
                 -- Note [Checking for representation-polymorphic built-ins].
             ; tc_rem_args i_visval' (i_val + 1) tys }

      debug_msg :: SDoc
      debug_msg =
        vcat
          [ text "app_head =" <+> ppr fun
          , text "arity =" <+> ppr arity
          , text "applied_args =" <+> ppr applied_args
          , text "nb_applied_val_args =" <+> ppr nb_applied_val_args ]


isHsValArg :: HsExprArg id -> Bool
isHsValArg (EValArg {}) = True
isHsValArg _            = False

countLeadingValArgs :: [HsExprArg id] -> Int
countLeadingValArgs []                   = 0
countLeadingValArgs (EValArg {}  : args) = 1 + countLeadingValArgs args
countLeadingValArgs (EWrap {}    : args) = countLeadingValArgs args
countLeadingValArgs (EPrag {}    : args) = countLeadingValArgs args
countLeadingValArgs (ETypeArg {} : _)    = 0

isValArg :: HsExprArg id -> Bool
isValArg (EValArg {}) = True
isValArg _            = False

isVisibleArg :: HsExprArg id -> Bool
isVisibleArg (EValArg {})  = True
isVisibleArg (ETypeArg {}) = True
isVisibleArg _             = False

-- | Count visible and invisible value arguments in a list
-- of 'HsExprArg' arguments.
countVisAndInvisValArgs :: [HsExprArg id] -> Arity
countVisAndInvisValArgs []                  = 0
countVisAndInvisValArgs (EValArg {} : args) = 1 + countVisAndInvisValArgs args
countVisAndInvisValArgs (EWrap wrap : args) =
  case wrap of { EHsWrap hsWrap            -> countHsWrapperInvisArgs hsWrap + countVisAndInvisValArgs args
               ; EPar   {}                 -> countVisAndInvisValArgs args
               ; EExpand {}                -> countVisAndInvisValArgs args }
countVisAndInvisValArgs (EPrag {}   : args) = countVisAndInvisValArgs args
countVisAndInvisValArgs (ETypeArg {}: args) = countVisAndInvisValArgs args

-- | Counts the number of invisible term-level arguments applied by an 'HsWrapper'.
-- Precondition: this wrapper contains no abstractions.
countHsWrapperInvisArgs :: HsWrapper -> Arity
countHsWrapperInvisArgs = go
  where
    go WpHole = 0
    go (WpCompose wrap1 wrap2) = go wrap1 + go wrap2
    go fun@(WpFun {}) = nope fun
    go (WpCast {}) = 0
    go evLam@(WpEvLam {}) = nope evLam
    go (WpEvApp _) = 1
    go tyLam@(WpTyLam {}) = nope tyLam
    go (WpTyApp _) = 0
    go (WpLet _) = 0
    go (WpMultCoercion {}) = 0

    nope x = pprPanic "countHsWrapperInvisApps" (ppr x)

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
here in the typechecker, we desugar it to a use of HsExpanded.
That makes it possible to typecheck something like
     e1 `f` e2
where
   f :: forall a. t1 -> forall b. t2 -> t3

Note [Looking through HsExpanded]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When creating an application chain in splitHsApps, we must deal with
     HsExpanded f1 (f `HsApp` e1) `HsApp` e2 `HsApp` e3

as a single application chain `f e1 e2 e3`.  Otherwise stuff like overloaded
labels (#19154) won't work.

It's easy to achieve this: `splitHsApps` unwraps `HsExpanded`.
-}

{- *********************************************************************
*                                                                      *
                 tcInferAppHead
*                                                                      *
********************************************************************* -}

tcInferAppHead :: (HsExpr GhcRn, AppCtxt)
               -> [HsExprArg 'TcpRn]
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
-- Why do we need the arguments to infer the type of the head of the
-- application? Simply to inform add_head_ctxt about whether or not
-- to put push a new "In the expression..." context. (We don't push a
-- new one if there are no arguments, because we already have.)
--
-- Note that [] and (,,) are both HsVar:
--   see Note [Empty lists] and [ExplicitTuple] in GHC.Hs.Expr
--
-- NB: 'e' cannot be HsApp, HsTyApp, HsPrag, HsPar, because those
--     cases are dealt with by splitHsApps.
--
-- See Note [tcApp: typechecking applications] in GHC.Tc.Gen.App
tcInferAppHead (fun,ctxt) args
  = addHeadCtxt ctxt $
    do { mb_tc_fun <- tcInferAppHead_maybe fun args
       ; case mb_tc_fun of
            Just (fun', fun_sigma) -> return (fun', fun_sigma)
            Nothing -> tcInfer (tcExpr fun) }

tcInferAppHead_maybe :: HsExpr GhcRn
                     -> [HsExprArg 'TcpRn]
                     -> TcM (Maybe (HsExpr GhcTc, TcSigmaType))
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
-- Returns Nothing for a complicated head
tcInferAppHead_maybe fun args
  = case fun of
      HsVar _ (L _ nm)          -> Just <$> tcInferId nm
      HsRecSel _ f              -> Just <$> tcInferRecSelId f
      ExprWithTySig _ e hs_ty   -> Just <$> tcExprWithSig e hs_ty
      HsOverLit _ lit           -> Just <$> tcInferOverLit lit
      HsUntypedSplice (HsUntypedSpliceTop _ e) _
                                -> tcInferAppHead_maybe e args
      _                         -> return Nothing

addHeadCtxt :: AppCtxt -> TcM a -> TcM a
addHeadCtxt fun_ctxt thing_inside
  | not (isGoodSrcSpan fun_loc)   -- noSrcSpan => no arguments
  = thing_inside                  -- => context is already set
  | otherwise
  = setSrcSpan fun_loc $
    case fun_ctxt of
      VAExpansion orig _ -> addExprCtxt orig thing_inside
      VACall {}          -> thing_inside
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
obviousSig (HsPar _ _ p _)        = obviousSig (unLoc p)
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
       ; (expr', poly_ty) <- tcExprSig ctxt expr sig_info
       ; return (ExprWithTySig noExtField expr' hs_ty, poly_ty) }
  where
    loc = getLocA (dropWildCards hs_ty)
    ctxt = ExprSigCtxt (lhsSigWcTypeContextSpan hs_ty)

tcExprSig :: UserTypeCtxt -> LHsExpr GhcRn -> TcIdSigInfo -> TcM (LHsExpr GhcTc, TcSigmaType)
tcExprSig ctxt expr (CompleteSig { sig_bndr = poly_id, sig_loc = loc })
  = setSrcSpan loc $   -- Sets the location for the implication constraint
    do { let poly_ty = idType poly_id
       ; (wrap, expr') <- tcSkolemiseScoped ctxt poly_ty $ \rho_ty ->
                          tcCheckMonoExprNC expr rho_ty
       ; return (mkLHsWrap wrap expr', poly_ty) }

tcExprSig _ expr sig@(PartialSig { psig_name = name, sig_loc = loc })
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

       ; tau <- zonkTcType tau
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
           herald   = ExpectedFunTyArg thing (HsLit noAnn hs_lit)
       ; (wrap2, sarg_ty, res_ty) <- matchActualFunTySigma herald mb_thing
                                                           (1, []) from_ty

       ; co <- unifyType mb_thing (hsLitType hs_lit) (scaledThing sarg_ty)
       ; let lit_expr = L (l2l loc) $ mkHsWrapCo co $
                        HsLit noAnn hs_lit
             from_expr = mkHsWrap (wrap2 <.> wrap1) $
                         HsVar noExtField (L loc from_id)
             witness = HsApp noAnn (L (l2l loc) from_expr) lit_expr
             lit' = lit { ol_ext = OverLitTc { ol_rebindable = rebindable
                                             , ol_witness = witness
                                             , ol_type = res_ty } }
       ; return (HsOverLit noAnn lit', res_ty) }

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
             (tcTyThingTyCon_maybe -> Just tc) -> fail_tycon tc -- TyCon or TcTyCon
             ATyVar name _ -> fail_tyvar name

             _ -> failWithTc $ TcRnExpectedValueId thing }
  where
    fail_tycon tc = do
      gre <- getGlobalRdrEnv
      let nm = tyConName tc
          pprov = case lookupGRE_Name gre nm of
                      Just gre -> nest 2 (pprNameProvenance gre)
                      Nothing  -> empty
      fail_with_msg dataName nm pprov

    fail_tyvar nm =
      let pprov = nest 2 (text "bound at" <+> ppr (getSrcLoc nm))
      in fail_with_msg varName nm pprov

    fail_with_msg whatName nm pprov = do
      (import_errs, hints) <- get_suggestions whatName
      unit_state <- hsc_units <$> getTopEnv
      let
        -- TODO: unfortunate to have to convert to SDoc here.
        -- This should go away once we refactor ErrInfo.
        hint_msg = vcat $ map ppr hints
        import_err_msg = vcat $ map ppr import_errs
        info = ErrInfo { errInfoContext = pprov, errInfoSupplementary = import_err_msg $$ hint_msg }
      failWithTc $ TcRnMessageWithInfo unit_state (
              mkDetailedMessage info (TcRnIncorrectNameSpace nm False))

    get_suggestions ns = do
       let occ = mkOccNameFS ns (occNameFS (occName id_name))
       lcl_env <- getLocalRdrEnv
       unknownNameSuggestions lcl_env WL_Anything (mkRdrUnqual occ)

    return_id id = return (HsVar noExtField (noLocA id), idType id)

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
   `GHC.Tc.Utils.Zonk.commitFlexi`. So it won't just be an
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
         'matchActualFunTySigma'.

      B. If there are fewer arguments than there are bound term variables,
         hasFixedRuntimeRep_remainingValArgs will ensure that we are still
         instantiating at a representation-monomorphic type, e.g.

         ( /\r (a :: TYPE r). \ (x %p :: a). K @r @a x) @IntRep @Int#
           :: Int# -> T IntRep Int#

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
        ; checkTc (isTauTy id_ty) (TcRnSplicePolymorphicLocalVar id)
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
  = addLandmarkErrCtxtM (\env -> (env, ) <$> mk_msg) thing_inside
      -- NB: use a landmark error context, so that an empty context
      -- doesn't suppress some more useful context
  where
    mk_msg
      = do { mb_env_ty <- readExpType_maybe env_ty
                     -- by the time the message is rendered, the ExpType
                     -- will be filled in (except if we're debugging)
           ; fun_res' <- zonkTcType fun_res_ty
           ; env'     <- case mb_env_ty of
                           Just env_ty -> zonkTcType env_ty
                           Nothing     ->
                             do { dumping <- doptM Opt_D_dump_tc_trace
                                ; massert dumping
                                ; newFlexiTyVarTy liftedTypeKind }
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

addExprCtxt :: HsExpr GhcRn -> TcRn a -> TcRn a
addExprCtxt e thing_inside
  = case e of
      HsUnboundVar {} -> thing_inside
      _ -> addErrCtxt (exprCtxt e) thing_inside
   -- The HsUnboundVar special case addresses situations like
   --    f x = _
   -- when we don't want to say "In the expression: _",
   -- because it is mentioned in the error message itself

exprCtxt :: HsExpr GhcRn -> SDoc
exprCtxt expr = hang (text "In the expression:") 2 (ppr (stripParensHsExpr expr))
