
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
{-# LANGUAGE DisambiguateRecordFields #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

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
       , tyConOf, tyConOfET, lookupParents, fieldNotInType
       , notSelector, nonBidirectionalErr

       , addExprCtxt, addFunResCtxt ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcExpr, tcCheckMonoExprNC, tcCheckPolyExprNC )

import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Bind( chooseInferredQuantifiers )
import GHC.Tc.Gen.Sig( tcUserTypeSig, tcInstSig, lhsSigWcTypeContextSpan )
import GHC.Tc.TyCl.PatSyn( patSynBuilderOcc )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family ( tcLookupDataFamInst )
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Core.UsageEnv      ( unitUE )
import GHC.Rename.Utils       ( unknownSubordinateErr )
import GHC.Rename.Unbound     ( unknownNameSuggestions, WhatLooking(..) )
import GHC.Unit.Module        ( getModule )
import GHC.Tc.Errors.Types
import GHC.Tc.Solver          ( InferMode(..), simplifyInfer )
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Hs
import GHC.Hs.Syn.Type
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.PatSyn( PatSyn )
import GHC.Core.ConLike( ConLike(..) )
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Builtin.Types( multiplicityTy )
import GHC.Builtin.Names
import GHC.Builtin.Names.TH( liftStringName, liftName )
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import Control.Monad

import Data.Function

import GHC.Prelude


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
                         --    noSrcSpan if outermost

  | VACall
       (HsExpr GhcRn) Int  -- In the third argument of function f
       SrcSpan             -- The SrcSpan of the application (f e1 e2 e3)

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

mkETypeArg :: AppCtxt -> LHsWcType GhcRn -> HsExprArg 'TcpRn
mkETypeArg ctxt hs_ty = ETypeArg { eva_ctxt = ctxt, eva_hs_ty = hs_ty
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
    top_ctxt n (HsPar _ _ fun _)           = top_lctxt n fun
    top_ctxt n (HsPragE _ _ fun)           = top_lctxt n fun
    top_ctxt n (HsAppType _ fun _)         = top_lctxt (n+1) fun
    top_ctxt n (HsApp _ fun _)             = top_lctxt (n+1) fun
    top_ctxt n (XExpr (HsExpanded orig _)) = VACall orig      n noSrcSpan
    top_ctxt n other_fun                   = VACall other_fun n noSrcSpan

    top_lctxt n (L _ fun) = top_ctxt n fun

    go :: HsExpr GhcRn -> AppCtxt -> [HsExprArg 'TcpRn]
       -> ((HsExpr GhcRn, AppCtxt), [HsExprArg 'TcpRn])
    go (HsPar _ _ (L l fun) _)    ctxt args = go fun (set l ctxt) (EWrap (EPar ctxt)   : args)
    go (HsPragE _ p (L l fun))    ctxt args = go fun (set l ctxt) (EPrag      ctxt p   : args)
    go (HsAppType _ (L l fun) ty) ctxt args = go fun (dec l ctxt) (mkETypeArg ctxt ty  : args)
    go (HsApp _ (L l fun) arg)    ctxt args = go fun (dec l ctxt) (mkEValArg  ctxt arg : args)

    -- See Note [Looking through HsExpanded]
    go (XExpr (HsExpanded orig fun)) ctxt args
      = go fun (VAExpansion orig (appCtxtLoc ctxt)) (EWrap (EExpand orig) : args)

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

rebuildHsApps :: HsExpr GhcTc -> AppCtxt -> [HsExprArg 'TcpTc]-> HsExpr GhcTc
rebuildHsApps fun _ [] = fun
rebuildHsApps fun ctxt (arg : args)
  = case arg of
      EValArg { eva_arg = ValArg arg, eva_ctxt = ctxt' }
        -> rebuildHsApps (HsApp noAnn lfun arg) ctxt' args
      ETypeArg { eva_hs_ty = hs_ty, eva_ty  = ty, eva_ctxt = ctxt' }
        -> rebuildHsApps (HsAppType ty lfun hs_ty) ctxt' args
      EPrag ctxt' p
        -> rebuildHsApps (HsPragE noExtField p lfun) ctxt' args
      EWrap (EPar ctxt')
        -> rebuildHsApps (gHsPar lfun) ctxt' args
      EWrap (EExpand orig)
        -> rebuildHsApps (XExpr (ExpansionExpr (HsExpanded orig fun))) ctxt args
      EWrap (EHsWrap wrap)
        -> rebuildHsApps (mkHsWrap wrap fun) ctxt args
  where
    lfun = L (noAnnSrcSpan $ appCtxtLoc ctxt) fun

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
  = setSrcSpan (appCtxtLoc ctxt) $
    do { mb_tc_fun <- tcInferAppHead_maybe fun args
       ; case mb_tc_fun of
            Just (fun', fun_sigma) -> return (fun', fun_sigma)
            Nothing -> add_head_ctxt fun args $
                       tcInfer (tcExpr fun) }

tcInferAppHead_maybe :: HsExpr GhcRn
                     -> [HsExprArg 'TcpRn]
                     -> TcM (Maybe (HsExpr GhcTc, TcSigmaType))
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
-- Returns Nothing for a complicated head
tcInferAppHead_maybe fun args
  = case fun of
      HsVar _ (L _ nm)          -> Just <$> tcInferId nm
      HsRecSel _ f              -> Just <$> tcInferRecSelId f
      ExprWithTySig _ e hs_ty   -> add_head_ctxt fun args $
                                   Just <$> tcExprWithSig e hs_ty
      HsOverLit _ lit           -> Just <$> tcInferOverLit lit
      _                         -> return Nothing

add_head_ctxt :: HsExpr GhcRn -> [HsExprArg 'TcpRn] -> TcM a -> TcM a
-- Don't push an expression context if the arguments are empty,
-- because it has already been pushed by tcExpr
add_head_ctxt fun args thing_inside
  | null args = thing_inside
  | otherwise = addExprCtxt fun thing_inside


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

                    _ -> failWithTc $ TcRnUnknownMessage $ mkPlainError noHints $
                         ppr thing <+> text "used where a value identifier was expected" }

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


-- For an ambiguous record field, find all the candidate record
-- selectors (as GlobalRdrElts) and their parents.
lookupParents :: Bool -> RdrName -> RnM [(RecSelParent, GlobalRdrElt)]
lookupParents is_selector rdr
  = do { env <- getGlobalRdrEnv
        -- Filter by isRecFldGRE because otherwise a non-selector variable with
        -- an overlapping name can get through when NoFieldSelectors is enabled.
        -- See Note [NoFieldSelectors] in GHC.Rename.Env.
       ; let all_gres = lookupGRE_RdrName' rdr env
       ; let gres | is_selector = filter isFieldSelectorGRE all_gres
                  | otherwise   = filter isRecFldGRE all_gres
       ; mapM lookupParent gres }
  where
    lookupParent :: GlobalRdrElt -> RnM (RecSelParent, GlobalRdrElt)
    lookupParent gre = do { id <- tcLookupId (greMangledName gre)
                          ; case recordSelectorTyCon_maybe id of
                              Just rstc -> return (rstc, gre)
                              Nothing -> failWithTc (notSelector (greMangledName gre)) }


fieldNotInType :: RecSelParent -> RdrName -> TcRnMessage
fieldNotInType p rdr
  = TcRnUnknownMessage $ mkPlainError noHints $
    unknownSubordinateErr (text "field of type" <+> quotes (ppr p)) rdr

notSelector :: Name -> TcRnMessage
notSelector field
  = TcRnUnknownMessage $ mkPlainError noHints $
  hsep [quotes (ppr field), text "is not a record selector"]

naughtyRecordSel :: OccName -> TcRnMessage
naughtyRecordSel lbl
  = TcRnUnknownMessage $ mkPlainError noHints $
    text "Cannot use record selector" <+> quotes (ppr lbl) <+>
    text "as a function due to escaped type variables" $$
    text "Probable fix: use pattern-matching syntax instead"


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

tcExprSig :: UserTypeCtxt -> LHsExpr GhcRn -> TcIdSigInfo -> TcM (LHsExpr GhcTc, TcType)
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
       ; (qtvs, givens, ev_binds, _)
                 <- simplifyInfer tclvl infer_mode [sig_inst] [(name, tau)] wanted

       ; tau <- zonkTcType tau
       ; let inferred_theta = map evVarPred givens
             tau_tvs        = tyCoVarsOfType tau
       ; (binders, my_theta) <- chooseInferredQuantifiers inferred_theta
                                   tau_tvs qtvs (Just sig_inst)
       ; let inferred_sigma = mkInfSigmaTy qtvs inferred_theta tau
             my_sigma       = mkInvisForAllTys binders (mkPhiTy  my_theta tau)
       ; wrap <- if inferred_sigma `eqType` my_sigma -- NB: eqType ignores vis.
                 then return idHsWrapper  -- Fast path; also avoids complaint when we infer
                                          -- an ambiguous type and have AllowAmbiguousType
                                          -- e..g infer  x :: forall a. F a -> Int
                 else tcSubTypeSigma (ExprSigCtxt NoRRC) inferred_sigma my_sigma

       ; traceTc "tcExpSig" (ppr qtvs $$ ppr givens $$ ppr inferred_sigma $$ ppr my_sigma)
       ; let poly_wrap = wrap
                         <.> mkWpTyLams qtvs
                         <.> mkWpLams givens
                         <.> mkWpLet  ev_binds
       ; return (mkLHsWrap poly_wrap expr', my_sigma) }


{- Note [Partial expression signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Partial type signatures on expressions are easy to get wrong.  But
here is a guiding principile
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
    do { from_id <- tcLookupId from_name
       ; (wrap1, from_ty) <- topInstantiate orig (idType from_id)

       ; (wrap2, sarg_ty, res_ty) <- matchActualFunTySigma herald mb_doc
                                                           (1, []) from_ty
       ; hs_lit <- mkOverLit val
       ; co <- unifyType mb_doc (hsLitType hs_lit) (scaledThing sarg_ty)

       ; let lit_expr = L (l2l loc) $ mkHsWrapCo co $
                        HsLit noAnn hs_lit
             from_expr = mkHsWrap (wrap2 <.> wrap1) $
                         HsVar noExtField (L loc from_id)
             witness = HsApp noAnn (L (l2l loc) from_expr) lit_expr
             lit' = lit { ol_ext = OverLitTc { ol_rebindable = rebindable
                                             , ol_witness = witness
                                             , ol_type = res_ty } }
       ; return (HsOverLit noAnn lit', res_ty) }
  where
    orig   = LiteralOrigin lit
    mb_doc = Just (ppr from_name)
    herald = sep [ text "The function" <+> quotes (ppr from_name)
                 , text "is applied to"]


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
             AGlobal (ATyCon tc) -> fail_tycon tc
             ATcTyCon tc -> fail_tycon tc
             ATyVar name _ -> fail_tyvar name

             _ -> failWithTc $ TcRnUnknownMessage $ mkPlainError noHints $
                  ppr thing <+> text "used where a value identifier was expected" }
  where
    fail_tycon tc = do
       gre <- getGlobalRdrEnv
       suggestions <- get_suggestions dataName
       unit_state <- hsc_units <$> getTopEnv
       let pprov = case lookupGRE_Name gre (tyConName tc) of
                      Just gre -> nest 2 (pprNameProvenance gre)
                      Nothing  -> empty
           info = ErrInfo { errInfoContext = pprov, errInfoSupplementary = suggestions }
           msg = TcRnMessageWithInfo unit_state
               $ TcRnMessageDetailed info (TcRnIncorrectNameSpace (tyConName tc) False)
       failWithTc msg

    fail_tyvar name = do
       suggestions <- get_suggestions varName
       unit_state <- hsc_units <$> getTopEnv
       let pprov = nest 2 (text "bound at" <+> ppr (getSrcLoc name))
           info = ErrInfo { errInfoContext = pprov, errInfoSupplementary = suggestions }
           msg = TcRnMessageWithInfo unit_state
               $ TcRnMessageDetailed info (TcRnIncorrectNameSpace name False)
       failWithTc msg

    get_suggestions ns = do
       let occ = mkOccNameFS ns (occNameFS (occName id_name))
       dflags  <- getDynFlags
       rdr_env <- getGlobalRdrEnv
       lcl_env <- getLocalRdrEnv
       imp_info <- getImports
       curr_mod <- getModule
       hpt <- getHpt
       return $ unknownNameSuggestions WL_Anything dflags hpt curr_mod rdr_env
         lcl_env imp_info (mkRdrUnqual occ)

    return_id id = return (HsVar noExtField (noLocA id), idType id)

check_local_id :: Id -> TcM ()
check_local_id id
  = do { checkThLocalId id
       ; tcEmitBindingUsage $ unitUE (idName id) One }

check_naughty :: OccName -> TcId -> TcM ()
check_naughty lbl id
  | isNaughtyRecordSelector id = failWithTc (naughtyRecordSel lbl)
  | otherwise                  = return ()

tcInferDataCon :: DataCon -> TcM (HsExpr GhcTc, TcSigmaType)
-- See Note [Typechecking data constructors]
tcInferDataCon con
  = do { let tvs   = dataConUserTyVarBinders con
             theta = dataConOtherTheta con
             args  = dataConOrigArgTys con
             res   = dataConOrigResTy con
             stupid_theta = dataConStupidTheta con

       ; scaled_arg_tys <- mapM linear_to_poly args

       ; let full_theta  = stupid_theta ++ theta
             all_arg_tys = map unrestricted full_theta ++ scaled_arg_tys
                -- stupid-theta must come first
                -- See Note [Instantiating stupid theta]

       ; return ( XExpr (ConLikeTc (RealDataCon con) tvs all_arg_tys)
                , mkInvisForAllTys tvs $ mkPhiTy full_theta $
                  mkVisFunTys scaled_arg_tys res ) }
  where
    linear_to_poly :: Scaled Type -> TcM (Scaled Type)
    -- linear_to_poly implements point (3,4)
    -- of Note [Typechecking data constructors]
    linear_to_poly (Scaled One ty) = do { mul_var <- newFlexiTyVarTy multiplicityTy
                                        ; return (Scaled mul_var ty) }
    linear_to_poly scaled_ty       = return scaled_ty

tcInferPatSyn :: Name -> PatSyn -> TcM (HsExpr GhcTc, TcSigmaType)
tcInferPatSyn id_name ps
  = case patSynBuilderOcc ps of
       Just (expr,ty) -> return (expr,ty)
       Nothing        -> failWithTc (nonBidirectionalErr id_name)

nonBidirectionalErr :: Outputable name => name -> TcRnMessage
nonBidirectionalErr name = TcRnUnknownMessage $ mkPlainError noHints $
  text "non-bidirectional pattern synonym"
  <+> quotes (ppr name) <+> text "used in an expression"

{- Note [Typechecking data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As per Note [Polymorphisation of linear fields] in
GHC.Core.Multiplicity, linear fields of data constructors get a
polymorphic multiplicity when the data constructor is used as a term:

    Just :: forall {p} a. a %p -> Maybe a

So at an occurrence of a data constructor we do the following,
mostly in tcInferDataCon:

1. Get its type, say
    K :: forall (r :: RuntimeRep) (a :: TYPE r). a %1 -> T r a
   Note the %1: it is linear

2. We are going to return a ConLikeTc, thus:
     XExpr (ConLikeTc K [r,a] [Scaled p a])
      :: forall (r :: RuntimeRep) (a :: TYPE r). a %p -> T r a
   where 'p' is a fresh multiplicity unification variable.

   To get the returned ConLikeTc, we allocate a fresh multiplicity
   variable for each linear argument, and store the type, scaled by
   the fresh multiplicity variable in the ConLikeTc; along with
   the type of the ConLikeTc. This is done by linear_to_poly.

3. If the argument is not linear (perhaps explicitly declared as
   non-linear by the user), don't bother with this.

4. The (ConLikeTc K [r,a] [Scaled p a]) is later desugared by
   GHC.HsToCore.Expr.dsConLike to:
     (/\r a. \(x %p :: a). K @r @a x)
   which has the desired type given in the previous bullet.
   The 'p' is the multiplicity unification variable, which
   will by now have been unified to something, or defaulted in
   `GHC.Tc.Utils.Zonk.commitFlexi`. So it won't just be an
   (unbound) variable.

Wrinkles

* Why put [InvisTVBinder] in ConLikeTc, when we only need [TyVar] to
  desugar?  It's a bit of a toss-up, but having [InvisTvBinder] supports
  a future hsExprType :: HsExpr GhcTc -> Type

* Note that the [InvisTvBinder] is strictly redundant anyway; it's
  just the dataConUserTyVarBinders of the data constructor.  Similarly
  in the [Scaled TcType] field of ConLikeTc, the type comes directly
  from the data constructor.  The only bit that /isn't/ redundant is the
  fresh multiplicity variables!

  So an alternative would be to define ConLikeTc like this:
      | ConLikeTc [TcType]    -- Just the multiplicity variables
  But then the desugarer (and hsExprType, when we implement it) would
  need to repeat some of the work done here.  So for now at least
  ConLikeTc records this strictly-redundant info.

* See Note [Instantiating stupid theta] for an extra wrinkle


Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker transforms (assert e1 e2) to (assertError e1 e2).
This isn't really the Right Thing because there's no way to "undo"
if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.


Note [Instantiating stupid theta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a data type with a "stupid theta" (see
Note [The stupid context] in GHC.Core.DataCon):

  data Ord a => T a = MkT (Maybe a)

We want to generate an Ord constraint for every use of MkT; but
we also want to allow visible type application, such as
   MkT @Int

So we generate (ConLikeTc MkT [a] [Ord a, Maybe a]), with type
   forall a. Ord a => Maybe a -> T a

Now visible type application will work fine. But we desugar the
ConLikeTc to
   /\a \(d:Ord a) (x:Maybe a). MkT x
Notice that 'd' is dropped in this desugaring. We don't need it;
it was only there to generate a Wanted constraint. (That is why
it is stupid.)  To achieve this:

* We put the stupid-thata at the front of the list of argument
  types in ConLikeTc

* GHC.HsToCore.Expr.dsConLike generates /lambdas/ for all the
  arguments, but drops the stupid-theta arguments when building the
  /application/.

Nice.
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
        ; checkTc (isTauTy id_ty) (polySpliceErr id)
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
        ; addDetailedDiagnostic (TcRnImplicitLift id)

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

polySpliceErr :: Id -> TcRnMessage
polySpliceErr id
  = TcRnUnknownMessage $ mkPlainError noHints $
  text "Can't splice the polymorphic local variable" <+> quotes (ppr id)

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
                 -- No need to call tcSplitNestedSigmaTys here, since env_ty is
                 -- an ExpRhoTy, i.e., it's already instantiated.
                 (_, _, env_tau) = tcSplitSigmaTy env'
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

  f :: forall a. (Monoid a) => forall b. (Monoid b) => Maybe a -> Maybe b

If one uses `f` like so:

  do { f; putChar 'a' }

Then tcSplitSigmaTy will decompose the type of `f` into:

  Tyvars: [a]
  Context: (Monoid a)
  Argument types: []
  Return type: forall b. Monoid b => Maybe a -> Maybe b

That is, it will conclude that there are *no* argument types, and since `f`
was given no arguments, it won't print a helpful error message. On the other
hand, tcSplitNestedSigmaTys correctly decomposes `f`'s type down to:

  Tyvars: [a, b]
  Context: (Monoid a, Monoid b)
  Argument types: [Maybe a]
  Return type: Maybe b

So now GHC recognizes that `f` has one more argument type than it was actually
provided.
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
