
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
       ( HsExprArg(..), TcPass(..), QLFlag(..), EWrap(..)
       , AppCtxt(..), appCtxtLoc, insideExpansion
       , splitHsApps, rebuildHsApps
       , addArgWrap, isHsValArg
       , leadingValArgs, isVisibleArg

       , tcInferAppHead, tcInferAppHead_maybe
       , tcInferId, tcCheckId, tcInferConLike, obviousSig
       , tyConOf, tyConOfET
       , nonBidirectionalErr

       , pprArgInst
       , addHeadCtxt, addExprCtxt, addFunResCtxt ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcExpr, tcCheckPolyExprNC, tcPolyLExprSig )
import {-# SOURCE #-} GHC.Tc.Gen.Splice( getUntypedSpliceBody )
import {-# SOURCE #-} GHC.Tc.Gen.App( tcExprSigma )

import GHC.Prelude
import GHC.Hs
import GHC.Hs.Syn.Type

import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Bind( chooseInferredQuantifiers )
import GHC.Tc.Gen.Sig( tcUserTypeSig, tcInstSig )
import GHC.Tc.TyCl.PatSyn( patSynBuilderOcc )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family ( tcLookupDataFamInst )
import GHC.Tc.Errors.Types
import GHC.Tc.Solver          ( InferMode(..), simplifyInfer )
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Constraint( WantedConstraints )
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Zonk.TcType


import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Core.UsageEnv      ( singleUsageUE, UsageEnv )
import GHC.Core.PatSyn( PatSyn, patSynName )
import GHC.Core.ConLike( ConLike(..) )
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.Error

import GHC.Builtin.Types( multiplicityTy )
import GHC.Builtin.Names

import GHC.Driver.DynFlags
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Data.Maybe



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
      The result of tcInstFun, which instantiates the function type,
      perhaps taking a quick look at arguments.

  - HsExprArg TcpTc:
      The result of tcArg, which typechecks the value args
      In EValArg we now have a (LHsExpr GhcTc)

* rebuildPrefixApps is dual to splitHsApps, and zips an application
  back into a HsExpr

Invariants:

1. With QL switched off, all arguments are ValArg; no ValArgQL

2. With QL switched on, tcInstFun converts some ValArgs to ValArgQL,
   under the conditions when quick-look should happen (eg the argument
   type is guarded) -- see quickLookArg

Note [EValArgQL]
~~~~~~~~~~~~~~~~
Data constructor EValArgQL represents an argument that has been
partly-type-checked by Quick Look: the first part of `tcApp` has been
done, but not the second, `finishApp` part.

The constuctor captures all the bits and pieces needed to complete
typechecking.  (An alternative would to to store a function closure,
but that's less concrete.)  See Note [Quick Look at value arguments]
in GHC.Tc.Gen.App

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

data HsExprArg (p :: TcPass) where -- See Note [HsExprArg]

  -- Data constructor EValArg represents a value argument
  EValArg :: { ea_ctxt   :: AppCtxt
             , ea_arg_ty :: !(XEVAType p)
             , ea_arg    :: LHsExpr (GhcPass (XPass p)) }
          -> HsExprArg p

  -- Data constructor EValArgQL represents an argument that has been
  -- partly-type-checked by Quick Look; see Note [EValArgQL]
  EValArgQL :: { eaql_ctxt    :: AppCtxt
               , eaql_arg_ty  :: Scaled TcSigmaType  -- Argument type expected by function
               , eaql_larg    :: LHsExpr GhcRn       -- Original application, for
                                                     -- location and error msgs
               , eaql_tc_fun  :: (HsExpr GhcTc, AppCtxt) -- Typechecked head
               , eaql_fun_ue  :: UsageEnv -- Usage environment of the typechecked head (QLA5)
               , eaql_args    :: [HsExprArg 'TcpInst]    -- Args: instantiated, not typechecked
               , eaql_wanted  :: WantedConstraints
               , eaql_encl    :: Bool                  -- True <=> we have already qlUnified
                                                       --   eaql_arg_ty and eaql_res_rho
               , eaql_res_rho :: TcRhoType }           -- Result type of the application
            -> HsExprArg 'TcpInst  -- Only exists in TcpInst phase

  ETypeArg :: { ea_ctxt   :: AppCtxt
              , ea_hs_ty  :: LHsWcType GhcRn  -- The type arg
              , ea_ty_arg :: !(XETAType p) }  -- Kind-checked type arg
           -> HsExprArg p

  EPrag :: AppCtxt -> (HsPragE (GhcPass (XPass p))) -> HsExprArg p
  EWrap :: EWrap                                    -> HsExprArg p

type family XETAType (p :: TcPass) where  -- Type arguments
  XETAType 'TcpRn = NoExtField
  XETAType _      = Type

type family XEVAType (p :: TcPass) where   -- Value arguments
  XEVAType 'TcpInst = Scaled TcSigmaType
  XEVAType _        = NoExtField

data QLFlag = DoQL | NoQL

data EWrap = EPar    AppCtxt
           | EExpand (HsExpr GhcRn)
           | EHsWrap HsWrapper

data AppCtxt =
  VACall
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
appCtxtLoc (VACall _ _ l)    = l

insideExpansion :: AppCtxt -> Bool
insideExpansion (VACall _ _ loc)   = isGeneratedSrcSpan loc

instance Outputable QLFlag where
  ppr DoQL = text "DoQL"
  ppr NoQL = text "NoQL"

instance Outputable AppCtxt where
  ppr (VACall f n l)    = text "VACall" <+> int n <+> ppr f  <+> ppr l

type family XPass (p :: TcPass) where
  XPass 'TcpRn   = 'Renamed
  XPass 'TcpInst = 'Renamed
  XPass 'TcpTc   = 'Typechecked

mkEValArg :: AppCtxt -> LHsExpr GhcRn -> HsExprArg 'TcpRn
mkEValArg ctxt e = EValArg { ea_arg = e, ea_ctxt = ctxt
                           , ea_arg_ty = noExtField }

mkETypeArg :: AppCtxt -> LHsWcType GhcRn -> HsExprArg 'TcpRn
mkETypeArg ctxt hs_ty =
  ETypeArg { ea_ctxt = ctxt
           , ea_hs_ty = hs_ty
           , ea_ty_arg = noExtField }

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
    top_ctxt n (HsPar _ fun)        = top_lctxt n fun
    top_ctxt n (HsPragE _ _ fun)    = top_lctxt n fun
    top_ctxt n (HsAppType _ fun _)  = top_lctxt (n+1) fun
    top_ctxt n (HsApp _ fun _)      = top_lctxt (n+1) fun
    top_ctxt n (XExpr (PopErrCtxt fun)) = top_ctxt n fun
    top_ctxt n other_fun            = VACall other_fun n noSrcSpan

    top_lctxt :: Int -> LHsExpr GhcRn -> AppCtxt
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
           ; go fun ctxt' (EWrap (EExpand e) : args) }
      where
        ctxt' :: AppCtxt
        ctxt' = case splice of
            HsUntypedSpliceExpr _ (L l _) -> set l ctxt -- l :: SrcAnn AnnListItem
            HsQuasiQuote _ _ (L l _)      -> set l ctxt -- l :: SrcAnn NoEpAnns
            (XUntypedSplice (HsImplicitLiftSplice _ _ _ (L l _))) -> set l ctxt

    -- See Note [Desugar OpApp in the typechecker]
    go e@(OpApp _ arg1 (L l op) arg2) _ args
      = pure ( (op, VACall op 0 (locA l))
             ,   mkEValArg (VACall op 1 generatedSrcSpan) arg1
               : mkEValArg (VACall op 2 generatedSrcSpan) arg2
                    -- generatedSrcSpan because this the span of the call,
                    -- and its hard to say exactly what that is
               : EWrap (EExpand e)
               : args )
    go (XExpr (PopErrCtxt fun)) ctxt args = go fun ctxt args
      -- look through PopErrCtxt (cf. T17594f) we do not want to lose the opportunity of calling tcEValArgQL
      -- unlike HsPar, it is okay to forget about the PopErrCtxts as it does not persist over in GhcTc land

    go e ctxt args = pure ((e,ctxt), args)

    set :: EpAnn ann -> AppCtxt -> AppCtxt
    set l (VACall f n _)          = VACall f n (locA l)

    dec :: EpAnn ann -> AppCtxt -> AppCtxt
    dec l (VACall f n _)          = VACall f (n-1) (locA l)

-- | Rebuild an application: takes a type-checked application head
-- expression together with arguments in the form of typechecked 'HsExprArg's
-- and returns a typechecked application of the head to the arguments.
--
-- This performs a representation-polymorphism check to ensure that
-- representation-polymorphic unlifted newtypes have been eta-expanded.
--
-- See Note [Eta-expanding rep-poly unlifted newtypes].
rebuildHsApps :: (HsExpr GhcTc, AppCtxt)
                      -- ^ the function being applied
              -> [HsExprArg 'TcpTc]
                      -- ^ the arguments to the function
              -> HsExpr GhcTc
rebuildHsApps (fun, _) [] = fun
rebuildHsApps (fun, ctxt) (arg : args)
  = case arg of
      EValArg { ea_arg = arg, ea_ctxt = ctxt' }
        -> rebuildHsApps (HsApp noExtField lfun arg, ctxt') args
      ETypeArg { ea_hs_ty = hs_ty, ea_ty_arg = ty, ea_ctxt = ctxt' }
        -> rebuildHsApps (HsAppType ty lfun hs_ty, ctxt') args
      EPrag ctxt' p
        -> rebuildHsApps (HsPragE noExtField p lfun, ctxt') args
      EWrap (EPar ctxt')
        -> rebuildHsApps (gHsPar lfun, ctxt') args
      EWrap (EExpand o)
        -> rebuildHsApps (mkExpandedExprTc o fun, ctxt) args
      EWrap (EHsWrap wrap)
        -> rebuildHsApps (mkHsWrap wrap fun, ctxt) args
  where
    lfun = L (noAnnSrcSpan $ appCtxtLoc ctxt) fun

isHsValArg :: HsExprArg id -> Bool
isHsValArg (EValArg {}) = True
isHsValArg _            = False

leadingValArgs :: [HsExprArg 'TcpRn] -> [LHsExpr GhcRn]
leadingValArgs []                                = []
leadingValArgs (EValArg { ea_arg = arg } : args) = arg : leadingValArgs args
leadingValArgs (EWrap {}    : args)              = leadingValArgs args
leadingValArgs (EPrag {}    : args)              = leadingValArgs args
leadingValArgs (ETypeArg {} : _)                 = []

isValArg :: HsExprArg id -> Bool
isValArg (EValArg {}) = True
isValArg _            = False

isVisibleArg :: HsExprArg id -> Bool
isVisibleArg (EValArg {})  = True
isVisibleArg (ETypeArg {}) = True
isVisibleArg _             = False

instance OutputableBndrId (XPass p) => Outputable (HsExprArg p) where
  ppr (EPrag _ p)                     = text "EPrag" <+> ppr p
  ppr (ETypeArg { ea_hs_ty = hs_ty }) = char '@' <> ppr hs_ty
  ppr (EWrap wrap)                    = ppr wrap
  ppr (EValArg { ea_arg = arg, ea_ctxt = ctxt })
    = text "EValArg" <> braces (ppr ctxt) <+> ppr arg
  ppr (EValArgQL { eaql_tc_fun = fun, eaql_args = args, eaql_res_rho = ty})
    = hang (text "EValArgQL" <+> ppr fun)
         2 (vcat [ ppr args, text "ea_ql_ty:" <+> ppr ty ])

pprArgInst :: HsExprArg 'TcpInst -> SDoc
-- Ugh!  A special version for 'TcpInst, se we can print the arg_ty of EValArg
pprArgInst (EPrag _ p)                     = text "EPrag" <+> ppr p
pprArgInst (ETypeArg { ea_hs_ty = hs_ty }) = char '@' <> ppr hs_ty
pprArgInst (EWrap wrap)                    = ppr wrap
pprArgInst (EValArg { ea_arg = arg, ea_arg_ty = ty })
  = hang (text "EValArg" <+> ppr arg)
       2 (text "arg_ty" <+> ppr ty)
pprArgInst (EValArgQL { eaql_tc_fun = fun, eaql_args = args, eaql_res_rho = ty})
  = hang (text "EValArgQL" <+> ppr fun)
       2 (vcat [ vcat (map pprArgInst args), text "ea_ql_ty:" <+> ppr ty ])

instance Outputable EWrap where
  ppr (EPar _)       = text "EPar"
  ppr (EHsWrap w)    = text "EHsWrap" <+> ppr w
  ppr (EExpand orig) = text "EExpand" <+> ppr orig

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
      HsVar _ nm                  -> Just <$> tcInferId nm
      XExpr (HsRecSelRn f)        -> Just <$> tcInferRecSelId f
      XExpr (ExpandedThingRn _ e) -> Just <$> (setInGeneratedCode $ tcExprSigma False e) -- We do not want to instantiate e c.f. T19167
      XExpr (PopErrCtxt e)        -> tcInferAppHead_maybe e
      ExprWithTySig _ e hs_ty     -> Just <$> tcExprWithSig e hs_ty
      HsOverLit _ lit             -> Just <$> tcInferOverLit lit -- TODO: Do we need this?
      _                           -> return Nothing

addHeadCtxt :: AppCtxt -> TcM a -> TcM a
addHeadCtxt fun_ctxt thing_inside
  | not (isGoodSrcSpan fun_loc)       -- noSrcSpan => no arguments
  = thing_inside                      -- => context is already set
  | otherwise
  = setSrcSpan fun_loc thing_inside
  where fun_loc = appCtxtLoc fun_ctxt


{- *********************************************************************
*                                                                      *
                 Record selectors
*                                                                      *
********************************************************************* -}

tcInferRecSelId :: FieldOcc GhcRn
                -> TcM ( (HsExpr GhcTc, TcSigmaType))
tcInferRecSelId (FieldOcc lbl (L l sel_name))
     = do { sel_id <- tc_rec_sel_id
        ; let expr = XExpr (HsRecSelTc (FieldOcc lbl (L l sel_id)))
        ; return $ (expr, idType sel_id)
        }
     where
       occ :: OccName
       occ = nameOccName sel_name
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
           <- captureConstraints $
              simplifyInfer NotTopLevel tclvl infer_mode [sig_inst] [(name, tau)] wanted
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
    --
    -- See Note [Typechecking overloaded literals] in GHC.Tc.Gen.Expr
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
                         mkHsVar (L loc from_id)
             witness = HsApp noExtField (L (l2l loc) from_expr) lit_expr
             lit' = OverLit { ol_val = val
                            , ol_ext = OverLitTc { ol_rebindable = rebindable
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
  = do { (expr, actual_res_ty) <- tcInferId (noLocA $ noUserRdr name)
       ; traceTc "tcCheckId" (vcat [ppr name, ppr actual_res_ty, ppr res_ty])
       ; addFunResCtxt expr [] actual_res_ty res_ty $
         tcWrapResultO (OccurrenceOf name) rn_fun expr actual_res_ty res_ty }
  where
    rn_fun = mkHsVar (noLocA name)

------------------------
tcInferId :: LocatedN (WithUserRdr Name) -> TcM (HsExpr GhcTc, TcSigmaType)
-- Look up an occurrence of an Id
-- Do not instantiate its type
tcInferId lname@(L loc (WithUserRdr rdr id_name))

  | id_name `hasKey` assertIdKey
  = -- See Note [Overview of assertions]
    do { dflags <- getDynFlags
       ; if gopt Opt_IgnoreAsserts dflags
         then tc_infer_id lname
         else tc_infer_id (L loc $ WithUserRdr rdr assertErrorName) }

  | otherwise
  = tc_infer_id lname

tc_infer_id :: LocatedN (WithUserRdr Name) -> TcM (HsExpr GhcTc, TcSigmaType)
tc_infer_id (L loc (WithUserRdr rdr id_name))
 = do { thing <- tcLookup id_name
      ; (expr,ty) <- case thing of
             ATcId { tct_id = id }
               -> do { check_local_id id
                     ; return_id id }

             AGlobal (AnId id) -> return_id id
               -- A global cannot possibly be ill-staged
               -- nor does it need the 'lifting' treatment
               -- Hence no checkTh stuff here

             AGlobal (AConLike cl) -> tcInferConLike cl

             (tcTyThingTyCon_maybe -> Just tc) -> failIllegalTyCon WL_Term (WithUserRdr rdr (tyConName tc))
             ATyVar name _ -> failIllegalTyVar (WithUserRdr rdr name)

             _ -> failWithTc $ TcRnExpectedValueId thing

       ; traceTc "tcInferId" (ppr id_name <+> dcolon <+> ppr ty)
       ; return (expr, ty) }
  where
    return_id id = return (mkHsVar (L loc id), idType id)

{- Note [Overview of assertions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you write (assert pred x) then

  * If `-fignore-asserts` (which sets Opt_IgnoreAsserts) is on, the code is
    typechecked as written, but `assert`, defined in GHC.Internal.Base
       assert _pred r = r
    simply ignores `pred`

  * But without `-fignore-asserts`, GHC rewrites it to (assertError pred e)
    and that is defined in GHC.Internal.IO.Exception as
        assertError :: (?callStack :: CallStack) => Bool -> a -> a
    which does test the predicate and, if it is not True, throws an exception,
    capturing the CallStack.

    This rewrite is done in `tcInferId`.

So `-fignore-asserts` makes the assertion go away altogether, which may be good for
production code.

The reason that `assert` and `assertError` are defined in very different modules
is a historical accident.

Note: the Haddock for `assert` is on `GHC.Internal.Base.assert`, since that is
what appears in the user's source proram.

It's not entirely kosher to rewrite `assert` to `assertError`, because there's no
way to "undo" if you want to see the original source code in the typechecker
output.  We can fix this if it becomes a problem.

Note [Suppress hints with RequiredTypeArguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  = do { tcEmitBindingUsage $ singleUsageUE id }

check_naughty :: OccName -> TcId -> TcM ()
check_naughty lbl id
  | isNaughtyRecordSelector id = failWithTc (TcRnRecSelectorEscapedTyVar lbl)
  | otherwise                  = return ()

tcInferConLike :: ConLike -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferConLike (RealDataCon con) = tcInferDataCon con
tcInferConLike (PatSynCon ps)    = tcInferPatSyn  ps

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
                , mkForAllTys tvbs $ mkPhiTy full_theta $
                  mkScaledFunTys scaled_arg_tys res ) }
  where
    linear_to_poly :: Scaled Type -> TcM (Scaled Type)
    -- linear_to_poly implements point (3,4)
    -- of Note [Typechecking data constructors]
    linear_to_poly (Scaled OneTy ty) = do { mul_var <- newFlexiTyVarTy multiplicityTy
                                          ; return (Scaled mul_var ty) }
    linear_to_poly scaled_ty         = return scaled_ty

tcInferPatSyn :: PatSyn -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferPatSyn ps
  = case patSynBuilderOcc ps of
       Just (expr,ty) -> return (expr,ty)
       Nothing        -> failWithTc (nonBidirectionalErr (patSynName ps))

nonBidirectionalErr :: Name -> TcRnMessage
nonBidirectionalErr = TcRnPatSynNotBidirectional

{- Note [Typechecking data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

         See Note [Representation-polymorphic Ids with no binding] in GHC.Tc.Utils.Concrete

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

Note [Explicit Level Imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is the overview note which explains the whole implementation of ExplicitLevelImports

GHC Proposal: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0682-explicit-level-imports.rst
Paper: https://mpickering.github.io/papers/explicit-level-imports.pdf

The feature is turned on by the `ExplicitLevelImports` extension.
At the source level, the user marks imports with `quote` or `splice` to introduce
them at level 1 or -1.

The function GHC.Tc.Utils.Monad.getCurrentAndBindLevel. computes the levels
at which a Name is available:
  - for top-level Names, this information is stored in its GRE; it is either local
    (level 0) or imported, in which case the levels it is imported at are stored in the
    'ImpDeclSpec's for the GRE. The function 'greLevels' retrieves this information.
  - for locally-bound Names, this information is stored in the ThBindEnv.
GHC.Rename.Splice.checkCrossLevelLifting checks that levels in user-written programs
are correct.

Instances are checked by `checkWellLevelledDFun`, which computes the level of an
instance by calling `checkWellLevelledInstanceWhat`, which sees what is available at by looking at the module graph.

That's it for the main implementation of the feature; the rest is modifications
to the driver parts of the code to use this information. For example, in downsweep,
we only enable code generation for modules needed at the runtime stage.
See Note [-fno-code mode].

-}


{- *********************************************************************
*                                                                      *
         Error reporting for function result mis-matches
*                                                                      *
********************************************************************* -}

addFunResCtxt :: HsExpr GhcTc -> [HsExprArg p]
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
                 info =
                  FunResCtxt fun (count isValArg args) res_fun res_env
                    (length args_fun) (length args_env)
           ; return info }


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
      HsHole _ -> thing_inside
   -- The HsHole special case addresses situations like
   --    f x = _
   -- when we don't want to say "In the expression: _",
   -- because it is mentioned in the error message itself
      XExpr (PopErrCtxt _) -> thing_inside -- popErrCtxt shouldn't push ctxt. see typechecking let stmts
      _ -> addErrCtxt (ExprCtxt e) thing_inside -- no op in generated code
