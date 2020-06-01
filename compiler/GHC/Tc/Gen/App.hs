{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DataKinds, GADTs, TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

module GHC.Tc.Gen.App
       ( tcInferId, tcCheckId
       , tcApp, tcValArg, tcInferAppHead
       , tcExprPrag
       , tcInferSigmaTy

       , tcCheckRecSelId
       , disambiguateSelector, obviousSig, addAmbiguousNameErr
       , tyConOf, tyConOfET, lookupParents, fieldNotInType
       , notSelector, nonBidirectionalErr

       , addExprCtxt ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcExpr, tcCheckPolyExprNC, tcExprWithSig )

import GHC.Hs
import GHC.Tc.TyCl.PatSyn( patSynBuilderOcc )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Types.Basic
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst, tcLookupDataFamInst_maybe )
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Rename.Env         ( addUsedGRE )
import GHC.Rename.Utils       ( addNameClashErrRn, unknownSubordinateErr )
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Pat
import GHC.Tc.Gen.Sig( isCompleteHsSig )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Subst (substTyWithInScope)
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Types.Var.Set
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.Names
import GHC.Builtin.Names.TH( liftStringName, liftName )
import GHC.Driver.Session
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Types.Var.Env  ( emptyTidyEnv, mkInScopeSet )
import GHC.Data.Maybe
import GHC.Utils.Outputable as Outputable
import Control.Monad
import qualified GHC.LanguageExtensions as LangExt

import Data.Function

#include "HsVersions.h"

import GHC.Prelude

{- *********************************************************************
*                                                                      *
              Typechecking applications
*                                                                      *
********************************************************************* -}

{- Note [Typechecking applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We typecheck application chains (f e1 @ty e2) specially:

* So we can report errors like "in the third arument of a call of f"

* So we can do Visible Type Application (VTA), for which we must not
  eagerly instantiate the function part of the application.

* So that we can do Quick Look impredicativity.

The idea is:

* Use splitHsApps, which peels off
     HsApp, HsTypeApp, HsPrag, HsPar
  returning the function in the corner and the arguments

* Use tcInferAppHead to infer the type of the fuction,
    as an (uninstantiated) TcSigmaType
  There are special cases for
     HsVar, HsREcFld, and ExprWithTySig
  Otherwise, delegate back to tcExpr, which
    infers an (instantiated) TcRhoType

Some cases that /won't/ work:

1. Consider this (which uses visible type application):

    (let { f :: forall a. a -> a; f x = x } in f) @Int

   Since 'let' is not among the special cases for tcInferAppHead,
   we'll delegate back to tcExpr, which will instantiate f's type
   and the type application to @Int will fail.  Too bad!

-}

{- *********************************************************************
*                                                                      *
              HsExprArg: auxiliary data type
*                                                                      *
********************************************************************* -}

{- Note [HsExprArg]
~~~~~~~~~~~~~~~~~~~
The data typs HsExprArg :: TcPass -> Type
is a very local type, used only within this module.

* It's really a zipper for an application chain

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
-}

data TcPass = TcpRn     -- Arguments decomposed
            | TcpInst   -- Function instantiated
            | TcpTc     -- Typechecked

data HsExprArg (p :: TcPass)
  = EValArg  { eva_loc :: SrcSpan        -- Of the function
             , eva_arg :: EValArg p
             , eva_ty  :: !(XEType p) }

  | ETypeArg SrcSpan        -- Of the function
             (LHsWcType GhcRn)
             !(XEType p)

  | EPrag    SrcSpan
             (HsPragE (GhcPass (XPass p)))

  | EPar     SrcSpan         -- Of the nested expr

  | EWrap    !(XEWrap p)     -- Wrapper, after instantiation

data EValArg (p :: TcPass) where
  ValArg   :: LHsExpr (GhcPass (XPass p))
           -> EValArg p
  ValArgQL :: { va_expr :: LHsExpr GhcRn        -- Original expression
                                                -- For location and error msgs
              , va_fun  :: HsExpr GhcTc         -- Function, typechecked
              , va_args :: [HsExprArg 'TcpInst] -- Args, instantiated
              , va_ty   :: TcRhoType            -- Result type
              , va_rebuild :: Rebuilder }       -- How to reassemble
           -> EValArg 'TcpInst  -- Only exists in TcpInst phase

mkEValArg :: SrcSpan -> LHsExpr GhcRn -> HsExprArg 'TcpRn
mkEValArg l e = EValArg { eva_loc = l, eva_arg = ValArg e
                        , eva_ty = noExtField }

eValArgExpr :: EValArg 'TcpInst -> LHsExpr GhcRn
eValArgExpr (ValArg e)                 = e
eValArgExpr (ValArgQL { va_expr = e }) = e

type family XPass p where
  XPass 'TcpRn   = 'Renamed
  XPass 'TcpInst = 'Renamed
  XPass 'TcpTc   = 'Typechecked

type family XEType p where  -- Type arguments
  XEType 'TcpRn = NoExtField
  XEType _      = Type

type family XEWrap p where
  XEWrap 'TcpRn = NoExtCon
  XEWrap _      = HsWrapper

instance OutputableBndrId (XPass p) => Outputable (HsExprArg p) where
  ppr (EValArg { eva_arg = arg })
    = text "EValArg" <+> ppr arg
  ppr (EPrag _ p)          = text "EPrag" <+> ppr p
  ppr (ETypeArg _ hs_ty _) = char '@' <> ppr hs_ty
  ppr (EPar _)             = text "EPar"
  ppr (EWrap _)            = text "EWrap"
  -- ToDo: to print the wrapper properly we'll need to work harder

instance OutputableBndrId (XPass p) => Outputable (EValArg p) where
  ppr (ValArg e) = ppr e
  ppr (ValArgQL { va_fun = fun, va_args = args, va_ty = ty})
    = hang (text "ValArgQL" <+> ppr fun)
         2 (vcat [ ppr args, text "va_ty:" <+> ppr ty ])

pprHsExprArgTc :: HsExprArg 'TcpInst -> SDoc
pprHsExprArgTc (EValArg { eva_arg = tm, eva_ty = ty })
  = text "EValArg" <+> hang (ppr tm) 2 (dcolon <+> ppr ty)
pprHsExprArgTc arg = ppr arg

type family XExprTypeArg id where
  XExprTypeArg 'Parsed      = NoExtField
  XExprTypeArg 'Renamed     = NoExtField
  XExprTypeArg 'Typechecked = Type

type family XArgWrap id where
  XArgWrap 'Parsed      = NoExtField
  XArgWrap 'Renamed     = NoExtField
  XArgWrap 'Typechecked = HsWrapper

addArgWrap :: HsWrapper -> [HsExprArg 'TcpInst] -> [HsExprArg 'TcpInst]
addArgWrap wrap args
 | isIdHsWrapper wrap = args
 | otherwise          = EWrap wrap : args

type Rebuilder = HsExpr GhcTc -> [HsExprArg 'TcpTc]-> HsExpr GhcTc

zonkArg :: HsExprArg 'TcpInst -> TcM (HsExprArg 'TcpInst)
zonkArg eva@(EValArg { eva_ty = ty })
  = do { ty' <- zonkTcType ty
       ; return (eva { eva_ty = ty' }) }
zonkArg arg = return arg

splitHsApps :: HsExpr GhcRn -> (HsExpr GhcRn, [HsExprArg 'TcpRn], Rebuilder)
splitHsApps e
  = go e []
  where
    go (HsPar _     (L l fun))       args = go fun (EPar     l                  : args)
    go (HsPragE _ p (L l fun))       args = go fun (EPrag    l p                : args)
    go (HsAppType _ (L l fun) hs_ty) args = go fun (ETypeArg l hs_ty noExtField : args)
    go (HsApp _     (L l fun) arg)   args = go fun (mkEValArg l arg             : args)

    go (OpApp fix arg1 (L l op) arg2) args
      = ( op, mkEValArg l arg1 : mkEValArg l arg2 : args
        , rebuildInfixApps fix )

    go e args = (e, args, rebuildPrefixApps)

rebuildInfixApps :: Fixity -> Rebuilder
rebuildInfixApps fix fun args
  = go fun args
  where
    go fun (EValArg { eva_arg = ValArg arg1, eva_loc = l } :
            EValArg { eva_arg = ValArg arg2 } : args)
                                 = rebuildPrefixApps (OpApp fix arg1 (L l fun) arg2) args
    go fun (EWrap wrap : args) = go (mkHsWrap wrap fun) args
    go fun args                  = rebuildPrefixApps fun args

rebuildPrefixApps :: Rebuilder
rebuildPrefixApps fun args
  = go fun args
  where
    go fun [] = fun
    go fun (EWrap wrap : args)               = go (mkHsWrap wrap fun) args
    go fun (EValArg { eva_loc = l,
              eva_arg = ValArg arg } : args) = go (HsApp noExtField (L l fun) arg) args
    go fun (ETypeArg l hs_ty ty : args)      = go (HsAppType ty (L l fun) hs_ty) args
    go fun (EPar l : args)                   = go (HsPar noExtField (L l fun)) args
    go fun (EPrag l p : args)                = go (HsPragE noExtField p (L l fun)) args

isHsValArg :: HsExprArg id -> Bool
isHsValArg (EValArg {}) = True
isHsValArg _              = False

countLeadingValArgs :: [HsExprArg id] -> Int
countLeadingValArgs (EValArg {} : args) = 1 + countLeadingValArgs args
countLeadingValArgs (EPar {} : args)    = countLeadingValArgs args
countLeadingValArgs _                   = 0

isValArg :: HsExprArg id -> Bool
isValArg (EValArg {}) = True
isValArg _            = False

isArgPar :: HsExprArg id -> Bool
isArgPar (EPar {}) = True
isArgPar _           = False

setSrcSpanFromArgs :: [HsExprArg 'TcpRn] -> TcM a -> TcM a
setSrcSpanFromArgs [] thing_inside
  = thing_inside     -- Don't set the location twice
setSrcSpanFromArgs (arg:_) thing_inside
  = setSrcSpan (argFunLoc arg) thing_inside

argFunLoc :: HsExprArg 'TcpRn -> SrcSpan
argFunLoc (EValArg { eva_loc = l }) = l
argFunLoc (ETypeArg l _ _)          = l
argFunLoc (EPrag l _)               = l
argFunLoc (EPar l)                  = l


{- *********************************************************************
*                                                                      *
              The main event
*                                                                      *
********************************************************************* -}

tcInferSigmaTy :: LHsExpr GhcRn -> TcM TcSigmaType
-- Used only to implement :type; see GHC.Tc.Module.tcRnExpr
tcInferSigmaTy (L loc rn_expr)
  | (rn_fun, rn_args, _) <- splitHsApps rn_expr
  = setSrcSpan loc $
    do { impred <- xoptM LangExt.ImpredicativeTypes
       ; (tc_fun, fun_sigma) <- tcInferAppHead rn_fun rn_args
       ; (_delta, inst_args, app_res_sigma) <- tcInstFun impred False rn_fun fun_sigma rn_args
       ; _tc_args <- tcValArgs impred tc_fun inst_args
       ; return app_res_sigma }

tcApp :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
tcApp rn_expr exp_res_ty
  | (rn_fun, rn_args, rebuild) <- splitHsApps rn_expr
  = do { (tc_fun, fun_sigma) <- tcInferAppHead rn_fun rn_args

       -- Instantiate
       ; impred <- xoptM LangExt.ImpredicativeTypes
       ; (delta, inst_args, app_res_rho) <- tcInstFun impred True rn_fun fun_sigma rn_args

       -- Quick look at result
       ; when (impred && not (isEmptyVarSet delta)) $
         case checkingExpType_maybe exp_res_ty of
            Just exp_rho -> qlUnify delta app_res_rho exp_rho
            Nothing      -> return ()

       ; inst_args <- mapM zonkArg inst_args  -- Just for debug printing
       ; traceTc "tcApp" (vcat [ text "rn_fun"       <+> ppr rn_fun
                               , text "inst_args"    <+> brackets (pprWithCommas pprHsExprArgTc inst_args)
                               , text "impred on:  " <+> ppr impred
                               , text "fun_sigma:  " <+> ppr fun_sigma
                               , text "delta:      " <+> ppr delta
                               , text "app_res_rho:" <+> ppr app_res_rho
                               , text "exp_res_ty:"  <+> ppr exp_res_ty
                               , text "rn_expr:"     <+> ppr rn_expr ])

       -- Typecheck the value arguments separately
       ; tc_args <- tcValArgs impred tc_fun inst_args

       -- Special case for tagToEnum#
       ; if isTagToEnum rn_fun
         then tcTagToEnum rn_expr tc_fun tc_args app_res_rho exp_res_ty
         else

    do { -- Reconstruct
       ; let tc_expr = rebuild tc_fun tc_args

       -- Wrap the result
       -- NB: app_res_ty may be a polytype, via zonkQuickLook
       ; addFunResCtxt tc_fun tc_args app_res_rho exp_res_ty $
         tcWrapResult rn_expr tc_expr app_res_rho exp_res_ty } }

----------------
tcInferAppHead :: HsExpr GhcRn
               -> [HsExprArg 'TcpRn]
               -> TcM (HsExpr GhcTc, TcSigmaType)
-- Infer type of the head of an application
--   i.e. the 'f' in (f e1 ... en)
-- We get back a /SigmaType/ because we have special cases for
--   * A bare identifier (just look it up)
--     This case also covers a record selectro HsRecFld
--   * An expression with a type signature (e :: ty)
--
-- Note that [] and (,,) are both HsVar:
--   see Note [Empty lists] and [ExplicitTuple] in GHC.Hs.Expr
--
-- NB: 'e' cannot be HsApp, HsTyApp, HsPrag, HsPar, because those
--     cases are dealt with by splitHsApps.
--
-- See Note [Typechecking applications]
tcInferAppHead fun args
  = setSrcSpanFromArgs args $
    do { mb_tc_fun <- tcInferAppHead_maybe fun args
       ; case mb_tc_fun of
            Just (fun', fun_sigma) -> return (fun', fun_sigma)
            Nothing -> addErrCtxt (exprCtxt fun) $
                       tcInfer (tcExpr fun) }

tcInferAppHead_maybe :: HsExpr GhcRn
                     -> [HsExprArg 'TcpRn]
                     -> TcM (Maybe (HsExpr GhcTc, TcSigmaType))
-- Returns Nothing for a complicated head
tcInferAppHead_maybe fun args
  = case fun of
      HsVar _ (L _ nm)          -> Just <$> tcInferId nm
      HsRecFld _ f              -> Just <$> go_rec_fld f
      ExprWithTySig _ e hs_ty
        | isCompleteHsSig hs_ty -> addErrCtxt (exprCtxt fun) $
                                   Just <$> tcExprWithSig e hs_ty
      _                         -> return Nothing
  where
    -- Disgusting special case for ambiguous record selectors
    go_rec_fld (Ambiguous _ lbl)
      | arg1 : _ <- filterOut isArgPar args -- A value arg is first
      , EValArg { eva_arg = ValArg (L _ arg) } <- arg1
      , Just sig_ty <- obviousSig arg  -- A type sig on the arg disambiguates
      = do { sig_tc_ty <- tcHsSigWcType ExprSigCtxt sig_ty
           ; sel_name  <- disambiguateSelector lbl sig_tc_ty
           ; tcInferRecSelId (Unambiguous sel_name lbl) }

    go_rec_fld fld = tcInferRecSelId fld


----------------
tcValArgs :: Bool                    -- Quick-look on?
          -> HsExpr GhcTc            -- The function (for error messages)
          -> [HsExprArg 'TcpInst]    -- Actual argument
          -> TcM [HsExprArg 'TcpTc]  -- Resulting argument
tcValArgs quick_look fun args
  = go 1 args
  where
    go _ [] = return []
    go n (arg:args) = do { (n',arg') <- tc_arg n arg
                         ; args'     <- go n' args
                         ; return (arg' : args') }

    tc_arg :: Int -> HsExprArg 'TcpInst -> TcM (Int, HsExprArg 'TcpTc)
    tc_arg n (EPar l)              = return (n,   EPar l)
    tc_arg n (EPrag l p)           = return (n,   EPrag l (tcExprPrag p))
    tc_arg n (EWrap wrap)          = return (n,   EWrap wrap)
    tc_arg n (ETypeArg l hs_ty ty) = return (n+1, ETypeArg l hs_ty ty)

    tc_arg n eva@(EValArg { eva_arg = arg, eva_ty = arg_ty })
      = do { -- Crucial step: expose QL results before checking arg_ty
             arg_ty <- if quick_look then zonkTcType arg_ty
                                     else return arg_ty

             -- Now check the argument
           ; arg' <- addErrCtxt (funAppCtxt fun (eValArgExpr arg) n) $
                     do { traceTc "tcEValArg" $
                          vcat [ ppr n <+> text "of" <+> ppr fun
                               , text "arg type:" <+> ppr arg_ty
                               , text "arg:" <+> ppr arg ]
                        ; tcEValArg arg arg_ty }

           ; return (n+1, eva { eva_arg = ValArg arg', eva_ty = arg_ty }) }

tcEValArg :: EValArg 'TcpInst -> TcSigmaType -> TcM (LHsExpr GhcTc)
-- Typecheck one value argument of a function call
tcEValArg (ValArg arg) exp_arg_sigma
  = tcCheckPolyExprNC arg exp_arg_sigma

tcEValArg (ValArgQL { va_expr = L loc _, va_fun = fun, va_args = args
                    , va_ty = app_res_rho, va_rebuild = rebuild }) exp_arg_sigma
  = setSrcSpan loc $
    do { traceTc "tcEValArg {" (vcat [ ppr fun <+> ppr args ])
       ; tc_args <- tcValArgs True fun args
       ; co <- unifyType Nothing app_res_rho exp_arg_sigma
       ; traceTc "tcEValArg }" empty
       ; return (L loc $ mkHsWrapCo co $ rebuild fun tc_args) }

tcValArg :: HsExpr GhcRn          -- The function (for error messages)
         -> LHsExpr GhcRn         -- Actual argument
         -> TcSigmaType           -- expected arg type
         -> Int                   -- # of argument
         -> TcM (LHsExpr GhcTc)   -- Resulting argument
tcValArg fun arg arg_ty arg_no
   = addErrCtxt (funAppCtxt fun arg arg_no) $
     do { traceTc "tcValArg" $
          vcat [ ppr arg_no <+> text "of" <+> ppr fun
               , text "arg type:" <+> ppr arg_ty
               , text "arg:" <+> ppr arg ]
        ; tcCheckPolyExprNC arg arg_ty }


{- *********************************************************************
*                                                                      *
              Instantiating the call
*                                                                      *
********************************************************************* -}

tcInstFun :: Bool   -- True <=> ImpredicativeTypes is on; do quick-look
          -> Bool   -- False <=> Don't instantiate at the end: return a sigma-type
                    -- True  <=> Instantiate at the end: return a rho-type
          -> HsExpr GhcRn -> TcSigmaType -> [HsExprArg 'TcpRn]
          -> TcM ( Delta
                 , [HsExprArg 'TcpInst]
                 , TcSigmaType )
tcInstFun impred_on inst_final rn_fun fun_sigma rn_args
  = do { traceTc "tcInstFun" (ppr rn_fun $$ ppr rn_args)
       ; go emptyVarSet [] [] fun_sigma rn_args }
  where
    do_ql = impred_on || is_dollar rn_fun
            -- GHC's special case for ($)
    is_dollar (HsVar _ (L _ f)) =  f `hasKey` dollarIdKey
    is_dollar _                 = False

    fun_orig = exprCtOrigin rn_fun
    herald = sep [ text "The function" <+> quotes (ppr rn_fun)
                 , text "is applied to"]

    -- Count value args only when complaining about a function
    -- applied to too many value args
    -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify.
    n_val_args = count isHsValArg rn_args

    fun_is_out_of_scope  -- See Note [VTA for out-of-scope functions]
      = case rn_fun of
          HsUnboundVar {} -> True
          _               -> False

    need_instantiation :: [HsExprArg 'TcpRn] -> Bool
    need_instantiation []               = inst_final
    need_instantiation (EValArg {} : _) = True
    need_instantiation _                = False

    -----------
    -- go: If fun_ty=kappa, look it up in Theta
    go delta acc so_far fun_ty args
      | Just kappa <- tcGetTyVar_maybe fun_ty
      , kappa `elemVarSet` delta
      = do { cts <- readMetaTyVar kappa
           ; case cts of
                Indirect fun_ty' -> go  delta acc so_far fun_ty' args
                Flexi            -> go1 delta acc so_far fun_ty  args }
     | otherwise
     = go1 delta acc so_far fun_ty args

    go1 :: Delta
       -> [HsExprArg 'TcpInst]  -- Accumulator, reversed
       -> [TcSigmaType]         -- Value args to which applied so far
       -> TcSigmaType -> [HsExprArg 'TcpRn]
       -> TcM (Delta, [HsExprArg 'TcpInst], TcSigmaType)

    go1 delta acc so_far fun_ty args
      | need_instantiation args
      , (tvs, theta, body) <- tcSplitSigmaTy fun_ty
      , not (null tvs && null theta)
      = do { (inst_tvs, wrap, fun_rho) <- setSrcSpanFromArgs rn_args $
                                          instantiateSigma fun_orig tvs theta body
                 -- Setting the location is important for the class constraints
                 -- that may be emitted from instantiating fun_sigma
           ; go (delta `extendVarSetList` inst_tvs)
                (addArgWrap wrap acc) so_far fun_rho args }

    go1 delta acc _ fun_ty []
       = do { traceTc "tcInstFun:ret" (ppr fun_ty)
            ; return (delta, reverse acc, fun_ty) }

    go1 delta acc so_far fun_ty (EPar sp : args)
      = go1 delta (EPar sp : acc) so_far fun_ty args

    go1 delta acc so_far fun_ty (EPrag sp prag : args)
      = go1 delta (EPrag sp prag : acc) so_far fun_ty args

    go1 delta acc so_far fun_ty args@(ETypeArg loc hs_ty_arg _ : rest_args)
      | (tvbs,  body1) <- tcSplitSomeForAllTys (== Inferred) fun_ty
      , (theta, body2) <- tcSplitPhiTy body1
      , not (null tvbs && null theta)
      , let tvs = map binderVar tvbs
      = do { (inst_tvs, wrap, fun_rho) <- instantiateSigma fun_orig tvs theta body2
           ; go (delta `extendVarSetList` inst_tvs)
                (addArgWrap wrap acc) so_far fun_rho args }

      | fun_is_out_of_scope   -- See Note [VTA for out-of-scope functions]
      = go delta acc so_far fun_ty rest_args

      | otherwise
      = do { (ty_arg, inst_ty) <- tcVTA fun_ty hs_ty_arg
           ; let acc' = ETypeArg loc hs_ty_arg ty_arg : acc
           ; go delta acc' so_far inst_ty rest_args }

    go1 delta acc so_far fun_ty args@(EValArg {} : _)
      | Just kappa <- tcGetTyVar_maybe fun_ty
      , kappa `elemVarSet` delta
      = -- Function type was of form   f :: forall a b. t1 -> t2 -> b
        -- with 'b', one of the quantified type variables, in the corner
        -- but the call applies it to three or more value args.
        -- Suppose b is instantiated by kappa.  Then we want to make fresh
        -- instantiation variables nu1, nu2, and set kappa := nu1 -> nu2
        do { arg_nus <- replicateM (countLeadingValArgs args) newOpenFlexiTyVar
           ; res_nu  <- newOpenFlexiTyVar
           ; kind_co <- unifyKind Nothing liftedTypeKind (tyVarKind kappa)
           ; let delta'  = delta `extendVarSetList` (res_nu:arg_nus)
                 arg_tys = mkTyVarTys arg_nus
                 res_ty  = mkTyVarTy res_nu
                 fun_ty' = mkVisFunTys arg_tys res_ty
                 co_wrap = mkWpCastN (mkTcGReflLeftCo Nominal fun_ty' kind_co)
                 acc'    = addArgWrap co_wrap acc
           ; writeMetaTyVar kappa (mkCastTy fun_ty' kind_co)
           ; go delta' acc' so_far fun_ty' args }

    go1 delta acc so_far fun_ty
        (eva@(EValArg { eva_arg = ValArg arg })  : rest_args)
      = do { (wrap, arg_ty, res_ty) <- matchActualFunTy herald (Just (ppr rn_fun))
                                                        (n_val_args, so_far) fun_ty
          ; (delta', arg') <- if do_ql
                              then quickLookArg delta arg arg_ty
                              else return (delta, ValArg arg)
          ; let acc' = eva { eva_arg = arg', eva_ty = arg_ty }
                       : addArgWrap wrap acc
          ; go delta' acc' (arg_ty:so_far) res_ty rest_args }



{- *********************************************************************
*                                                                      *
              Visible type application
*                                                                      *
********************************************************************* -}

tcVTA :: TcType            -- Function type
      -> LHsWcType GhcRn   -- Argument type
      -> TcM (TcType, TcType)
-- Deal with a visible type application
-- The function type has already had its Inferred binders instantiated
tcVTA fun_ty hs_ty
  | Just (tvb, inner_ty) <- tcSplitForAllTy_maybe fun_ty
  , binderArgFlag tvb == Specified
    -- It really can't be Inferred, because we've just
    -- instantiated those. But, oddly, it might just be Required.
    -- See Note [Required quantifiers in the type of a term]
  = do { let tv   = binderVar tvb
             kind = tyVarKind tv
       ; ty_arg <- tcHsTypeApp hs_ty kind

       ; inner_ty <- zonkTcType inner_ty
             -- See Note [Visible type application zonk]
       ; let in_scope  = mkInScopeSet (tyCoVarsOfTypes [fun_ty, ty_arg])
             insted_ty = substTyWithInScope in_scope [tv] [ty_arg] inner_ty
                         -- NB: tv and ty_arg have the same kind, so this
                         --     substitution is kind-respecting
       ; traceTc "VTA" (vcat [ppr tv, debugPprType kind
                             , debugPprType ty_arg
                             , debugPprType (tcTypeKind ty_arg)
                             , debugPprType inner_ty
                             , debugPprType insted_ty ])
       ; return (ty_arg, insted_ty) }

  | otherwise
  = do { (_, fun_ty) <- zonkTidyTcType emptyTidyEnv fun_ty
       ; failWith $
         text "Cannot apply expression of type" <+> quotes (ppr fun_ty) $$
         text "to a visible type argument" <+> quotes (ppr hs_ty) }

{- Note [Required quantifiers in the type of a term]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#15859)

  data A k :: k -> Type      -- A      :: forall k -> k -> Type
  type KindOf (a :: k) = k   -- KindOf :: forall k. k -> Type
  a = (undefind :: KindOf A) @Int

With ImpredicativeTypes (thin ice, I know), we instantiate
KindOf at type (forall k -> k -> Type), so
  KindOf A = forall k -> k -> Type
whose first argument is Required

We want to reject this type application to Int, but in earlier
GHCs we had an ASSERT that Required could not occur here.

The ice is thin; c.f. Note [No Required TyCoBinder in terms]
in GHC.Core.TyCo.Rep.

Note [VTA for out-of-scope functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose 'wurble' is not in scope, and we have
   (wurble @Int @Bool True 'x')

Then the renamer will make (HsUnboundVar "wurble) for 'wurble',
and the typechecker will typecheck it with tcUnboundId, giving it
a type 'alpha', and emitting a deferred Hole constraint, to
be reported later.

But then comes the visible type application. If we do nothing, we'll
generate an immediate failure (in tc_app_err), saying that a function
of type 'alpha' can't be applied to Bool.  That's insane!  And indeed
users complain bitterly (#13834, #17150.)

The right error is the Hole, which has /already/ been emitted by
tcUnboundId.  It later reports 'wurble' as out of scope, and tries to
give its type.

Fortunately in tcValArgs we still have access to the function, so we can
check if it is a HsUnboundVar.  We use this info to simply skip over
any visible type arguments.  We've already inferred the type of the
function, so we'll /already/ have emitted a Hole constraint;
failing preserves that constraint.

We do /not/ want to fail altogether in this case (via failM) becuase
that may abandon an entire instance decl, which (in the presence of
-fdefer-type-errors) leads to leading to #17792.

Downside; the typechecked term has lost its visible type arguments; we
don't even kind-check them.  But let's jump that bridge if we come to
it.  Meanwhile, let's not crash!


Note [Visible type application zonk]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Substitutions should be kind-preserving, so we need kind(tv) = kind(ty_arg).

* tcHsTypeApp only guarantees that
    - ty_arg is zonked
    - kind(zonk(tv)) = kind(ty_arg)
  (checkExpectedKind zonks as it goes).

So we must zonk inner_ty as well, to guarantee consistency between zonk(tv)
and inner_ty.  Otherwise we can build an ill-kinded type.  An example was
#14158, where we had:
   id :: forall k. forall (cat :: k -> k -> *). forall (a :: k). cat a a
and we had the visible type application
  id @(->)

* We instantiated k := kappa, yielding
    forall (cat :: kappa -> kappa -> *). forall (a :: kappa). cat a a
* Then we called tcHsTypeApp (->) with expected kind (kappa -> kappa -> *).
* That instantiated (->) as ((->) q1 q1), and unified kappa := q1,
  Here q1 :: RuntimeRep
* Now we substitute
     cat  :->  (->) q1 q1 :: TYPE q1 -> TYPE q1 -> *
  but we must first zonk the inner_ty to get
      forall (a :: TYPE q1). cat a a
  so that the result of substitution is well-kinded
  Failing to do so led to #14158.

-}

{- *********************************************************************
*                                                                      *
              Guardedness
*                                                                      *
********************************************************************* -}

findNoKappa :: TcSigmaType -> [HsExprArg 'TcpRn] -> Bool
   -- True <=> there are no free quantified variables
   --          in the result of the call
findNoKappa fun_ty args
  = go emptyVarSet fun_ty args
  where
    need_instantiation []               = True
    need_instantiation (EValArg {} : _) = True
    need_instantiation _                = False

    go bvs fun_ty args
      | need_instantiation args
      , (tvs, theta, rho) <- tcSplitSigmaTy fun_ty
      , not (null tvs && null theta)
      = go (bvs `extendVarSetList` tvs) rho args

    go bvs fun_ty [] =  tyCoVarsOfType fun_ty `disjointVarSet` bvs

    go bvs fun_ty (EPar {}  : args) = go bvs fun_ty args
    go bvs fun_ty (EPrag {} : args) = go bvs fun_ty args

    go bvs fun_ty args@(ETypeArg {} : rest_args)
      | (tvbs,  body1) <- tcSplitSomeForAllTys (== Inferred) fun_ty
      , (theta, body2) <- tcSplitPhiTy body1
      , not (null tvbs && null theta)
      = go (bvs `extendVarSetList` binderVars tvbs) body2 args
      | Just (_tv, res_ty) <- tcSplitForAllTy_maybe fun_ty
      = go bvs res_ty rest_args

    go bvs fun_ty (EValArg {} : rest_args)
      | Just (_, res_ty) <- tcSplitFunTy_maybe fun_ty
      = go bvs res_ty rest_args

    go _ _ _ = False



{- *********************************************************************
*                                                                      *
              Quick Look
*                                                                      *
********************************************************************* -}

type Delta = TcTyVarSet   -- Set of instantiation variables

----------------
quickLookArg :: Delta -> LHsExpr GhcRn -> TcSigmaType
             -> TcM (Delta, EValArg 'TcpInst)
-- Special behaviour only for (f e1 .. en)
--
-- The returned Delta is a superset of the one passed in
-- with added instantiation variables from
--   (a) the call itself
--   (b) the arguments of the call
quickLookArg delta arg arg_ty
  | isEmptyVarSet delta  = return no_ql_result
  | otherwise            = go arg_ty
  where
    no_ql_result = (delta, ValArg arg)
    guarded      = isGuardedTy arg_ty
      -- NB: guardedness is computed based on the original,
      -- unzonked arg_ty, so we deliberately do not exploit
      -- guardedness that emerges a result of QL on earlier args

    go arg_ty | not (isRhoTy arg_ty)
              = return no_ql_result

              | Just kappa <- tcGetTyVar_maybe arg_ty
              , kappa `elemVarSet` delta
              = do { info <- readMetaTyVar kappa
                   ; case info of
                       Indirect arg_ty' -> go arg_ty'
                       Flexi            -> quickLookArg1 guarded delta arg arg_ty }
              | otherwise
              = quickLookArg1 guarded delta arg arg_ty

isGuardedTy :: TcType -> Bool
isGuardedTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | otherwise                               = False

quickLookArg1 :: Bool -> Delta -> LHsExpr GhcRn -> TcSigmaType
              -> TcM (Delta, EValArg 'TcpInst)
quickLookArg1 guarded delta larg@(L loc arg) arg_ty
  = setSrcSpan loc $
    do { let (rn_fun,rn_args,rebuild) = splitHsApps arg
       ; mb_fun_ty <- tcInferAppHead_maybe rn_fun rn_args
       ; traceTc "quickLookArg 1" $
         vcat [ text "arg:" <+> ppr arg
              , text "head:" <+> ppr rn_fun <+> dcolon <+> ppr mb_fun_ty
              , text "args:" <+> ppr rn_args ]

       ; case mb_fun_ty of {
           Nothing     -> -- fun is too complicated
                          return no_ql_result ;
           Just (fun', fun_sigma) ->

    do { let no_free_kappas = findNoKappa fun_sigma rn_args
       ; traceTc "quickLookArg 2" $
         vcat [ text "no_free_kappas:" <+> ppr no_free_kappas
              , text "guarded:" <+> ppr guarded ]
       ; if not (guarded || no_free_kappas)
         then return no_ql_result
         else
    do { impred_on <- xoptM LangExt.ImpredicativeTypes
         -- If the parent call is (e1 $ e2) then -XImpredicativeTypes might not be on
       ; (delta_app, inst_args, app_res_rho)
             <- tcInstFun impred_on True rn_fun fun_sigma rn_args
       ; traceTc "quickLookArg" $
         vcat [ text "arg:" <+> ppr arg
              , text "delta:" <+> ppr delta
              , text "delta_app:" <+> ppr delta_app
              , text "arg_ty:" <+> ppr arg_ty
              , text "app_res_rho:" <+> ppr app_res_rho ]

       -- Do quick-look unification
       -- NB: arg_ty may not be zonked, but that's ok
       ; let delta' = delta `unionVarSet` delta_app
       ; qlUnify delta' arg_ty app_res_rho

       ; let ql_arg = ValArgQL { va_expr = larg, va_fun = fun'
                               , va_args = inst_args
                               , va_ty = app_res_rho
                               , va_rebuild = rebuild }
       ; return (delta', ql_arg) } } } }
  where
    no_ql_result = (delta, ValArg larg)


---------------------
qlUnify :: Delta -> TcType -> TcType -> TcM ()
qlUnify delta ty1 ty2
  = do { traceTc "qlUnify" (ppr delta $$ ppr ty1 $$ ppr ty2)
       ; go (emptyVarSet,emptyVarSet) ty1 ty2 }
  where
    go :: (TyVarSet, TcTyVarSet)
       -> TcType -> TcType
       -> TcM ()
    -- The TyVarSets give the variables bound by enclosing foralls
    -- for the corresponding type. Don't unify with these.
    go bvs (TyVarTy tv) ty2
      | tv `elemVarSet` delta = go_kappa bvs tv ty2

    go (bvs1, bvs2) ty1 (TyVarTy tv)
      | tv `elemVarSet` delta = go_kappa (bvs2,bvs1) tv ty1

    go bvs (CastTy ty1 _) ty2 = go bvs ty1 ty2
    go bvs ty1 (CastTy ty2 _) = go bvs ty1 ty2

    go _ (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2 -- See GHC.Tc.Utils.Unify
      = return ()  -- Note [Expanding synonyms during unification]

    -- Now, and only now, expand synonyms
    go bvs rho1 rho2
      | Just rho1 <- tcView rho1 = go bvs rho1 rho2
      | Just rho2 <- tcView rho2 = go bvs rho1 rho2

    go bvs (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , not (isTypeFamilyTyCon tc1)
      , tys1 `equalLength` tys1
      = zipWithM_ (go bvs) tys1 tys2

    -- Decompose (arg1 -> res1) ~ (arg2 -> res2)
    -- and         (c1 => res1) ~   (c2 => res2)
    -- But for the latter we only learn instantiation info from t1~t2
    go bvs (FunTy { ft_af = af1, ft_arg = arg1, ft_res = res1 })
           (FunTy { ft_af = af2, ft_arg = arg2, ft_res = res2 })
      | af1 == af2
      = do { when (af1 == VisArg) (go bvs arg1 arg2)
           ; go bvs res1 res2 }

    -- ToDo: c.f. Tc.Utils.unify.uType,
    -- which does not split FunTy here
    -- Also NB tcRepSplitAppTy here, which does not split (c => t)
    go bvs (AppTy t1a t1b) ty2
      | Just (t2a, t2b) <- tcRepSplitAppTy_maybe ty2
      = do { go bvs t1a t2a; go bvs t1b t2b }

    go bvs ty1 (AppTy t2a t2b)
      | Just (t1a, t1b) <- tcRepSplitAppTy_maybe ty1
      = do { go bvs t1a t2a; go bvs t1b t2b }

    go (bvs1, bvs2) (ForAllTy bv1 ty1) (ForAllTy bv2 ty2)
      = go (bvs1',bvs2') ty1 ty2
      where
       bvs1' = bvs1 `extendVarSet` binderVar bv1
       bvs2' = bvs2 `extendVarSet` binderVar bv2

    go _ _ _ = return ()


    ----------------
    go_kappa bvs kappa ty2
      = ASSERT2( isMetaTyVar kappa, ppr kappa )
        do { info <- readMetaTyVar kappa
           ; case info of
               Indirect ty1 -> go bvs ty1 ty2
               Flexi        -> do { ty2 <- zonkTcType ty2
                                  ; go_flexi bvs kappa ty2 } }

    ----------------
    -- ToDo: what about other magic in Unify.metaTyVarUpdateOK?
    go_flexi (_,bvs2) kappa ty2  -- ty2 is zonked
      | ty2_tvs `intersectsVarSet` bvs2   -- We really only need shallow here
      = return ()   -- Can't instantiate a delta-var
                    -- to a forall-bound variable

      | kappa `elemVarSet` ty2_tvs
      = return ()   -- Occurs-check

      | otherwise
      = do { -- Unify the kinds; see Note [Kinds in QL unify]
           ; co <- unifyKind (Just (ppr ty2)) ty2_kind kappa_kind

           ; traceTc "qlUnify:update" $
             vcat [ hang (ppr kappa <+> dcolon <+> ppr kappa_kind)
                       2 (text ":=" <+> ppr ty2 <+> dcolon <+> ppr ty2_kind)
                 , text "co:" <+> ppr co ]
           ; writeMetaTyVar kappa (mkCastTy ty2 co) }
      where
        ty2_tvs    = tyCoVarsOfType ty2
        ty2_kind   = typeKind ty2
        kappa_kind = tyVarKind kappa

{- Note [Quick Look and type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Gah!  See impredicative/T18126-nasty.
-}


{- *********************************************************************
*                                                                      *
                 Record selectors
*                                                                      *
********************************************************************* -}

{-
Note [Disambiguating record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the -XDuplicateRecordFields extension is used, and the renamer
encounters a record selector or update that it cannot immediately
disambiguate (because it involves fields that belong to multiple
datatypes), it will defer resolution of the ambiguity to the
typechecker.  In this case, the `Ambiguous` constructor of
`AmbiguousFieldOcc` is used.

Consider the following definitions:

        data S = MkS { foo :: Int }
        data T = MkT { foo :: Int, bar :: Int }
        data U = MkU { bar :: Int, baz :: Int }

When the renamer sees `foo` as a selector or an update, it will not
know which parent datatype is in use.

For selectors, there are two possible ways to disambiguate:

1. Check if the pushed-in type is a function whose domain is a
   datatype, for example:

       f s = (foo :: S -> Int) s

       g :: T -> Int
       g = foo

    This is checked by `tcCheckRecSelId` when checking `HsRecFld foo`.

2. Check if the selector is applied to an argument that has a type
   signature, for example:

       h = foo (s :: S)

    This is checked by `tcApp`.


Updates are slightly more complex.  The `disambiguateRecordBinds`
function tries to determine the parent datatype in three ways:

1. Check for types that have all the fields being updated. For example:

        f x = x { foo = 3, bar = 2 }

   Here `f` must be updating `T` because neither `S` nor `U` have
   both fields. This may also discover that no possible type exists.
   For example the following will be rejected:

        f' x = x { foo = 3, baz = 3 }

2. Use the type being pushed in, if it is already a TyConApp. The
   following are valid updates to `T`:

        g :: T -> T
        g x = x { foo = 3 }

        g' x = x { foo = 3 } :: T

3. Use the type signature of the record expression, if it exists and
   is a TyConApp. Thus this is valid update to `T`:

        h x = (x :: T) { foo = 3 }


Note that we do not look up the types of variables being updated, and
no constraint-solving is performed, so for example the following will
be rejected as ambiguous:

     let bad (s :: S) = foo s

     let r :: T
         r = blah
     in r { foo = 3 }

     \r. (r { foo = 3 },  r :: T )

We could add further tests, of a more heuristic nature. For example,
rather than looking for an explicit signature, we could try to infer
the type of the argument to a selector or the record expression being
updated, in case we are lucky enough to get a TyConApp straight
away. However, it might be hard for programmers to predict whether a
particular update is sufficiently obvious for the signature to be
omitted. Moreover, this might change the behaviour of typechecker in
non-obvious ways.

See also Note [HsRecField and HsRecUpdField] in GHC.Hs.Pat.
-}

tcCheckRecSelId :: HsExpr GhcRn -> AmbiguousFieldOcc GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
tcCheckRecSelId rn_expr f@(Unambiguous {}) res_ty
  = do { (expr, actual_res_ty) <- tcInferRecSelId f
       ; tcWrapResult rn_expr expr actual_res_ty res_ty }
tcCheckRecSelId rn_expr (Ambiguous _ lbl) res_ty
  = case tcSplitFunTy_maybe =<< checkingExpType_maybe res_ty of
      Nothing       -> ambiguousSelector lbl
      Just (arg, _) -> do { sel_name <- disambiguateSelector lbl arg
                          ; tcCheckRecSelId rn_expr (Unambiguous sel_name lbl)
                                                    res_ty }

------------------------
tcInferRecSelId :: AmbiguousFieldOcc GhcRn -> TcM (HsExpr GhcTc, TcRhoType)
tcInferRecSelId (Unambiguous sel (L _ lbl))
  = do { (expr', ty) <- tc_infer_id lbl sel
       ; return (expr', ty) }
tcInferRecSelId (Ambiguous _ lbl)
  = ambiguousSelector lbl


------------------------
-- Given a RdrName that refers to multiple record fields, and the type
-- of its argument, try to determine the name of the selector that is
-- meant.
disambiguateSelector :: Located RdrName -> Type -> TcM Name
disambiguateSelector lr@(L _ rdr) parent_type
 = do { fam_inst_envs <- tcGetFamInstEnvs
      ; case tyConOf fam_inst_envs parent_type of
          Nothing -> ambiguousSelector lr
          Just p  ->
            do { xs <- lookupParents rdr
               ; let parent = RecSelData p
               ; case lookup parent xs of
                   Just gre -> do { addUsedGRE True gre
                                  ; return (gre_name gre) }
                   Nothing  -> failWithTc (fieldNotInType parent rdr) } }

-- This field name really is ambiguous, so add a suitable "ambiguous
-- occurrence" error, then give up.
ambiguousSelector :: Located RdrName -> TcM a
ambiguousSelector (L _ rdr)
  = do { addAmbiguousNameErr rdr
       ; failM }

-- | This name really is ambiguous, so add a suitable "ambiguous
-- occurrence" error, then continue
addAmbiguousNameErr :: RdrName -> TcM ()
addAmbiguousNameErr rdr
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_RdrName rdr env
       ; setErrCtxt [] $ addNameClashErrRn rdr gres}

-- A type signature on the argument of an ambiguous record selector or
-- the record expression in an update must be "obvious", i.e. the
-- outermost constructor ignoring parentheses.
obviousSig :: HsExpr GhcRn -> Maybe (LHsSigWcType GhcRn)
obviousSig (ExprWithTySig _ _ ty) = Just ty
obviousSig (HsPar _ p)          = obviousSig (unLoc p)
obviousSig _                    = Nothing

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
lookupParents :: RdrName -> RnM [(RecSelParent, GlobalRdrElt)]
lookupParents rdr
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_RdrName rdr env
       ; mapM lookupParent gres }
  where
    lookupParent :: GlobalRdrElt -> RnM (RecSelParent, GlobalRdrElt)
    lookupParent gre = do { id <- tcLookupId (gre_name gre)
                          ; if isRecordSelector id
                              then return (recordSelectorTyCon id, gre)
                              else failWithTc (notSelector (gre_name gre)) }


fieldNotInType :: RecSelParent -> RdrName -> SDoc
fieldNotInType p rdr
  = unknownSubordinateErr (text "field of type" <+> quotes (ppr p)) rdr

notSelector :: Name -> SDoc
notSelector field
  = hsep [quotes (ppr field), text "is not a record selector"]

naughtyRecordSel :: RdrName -> SDoc
naughtyRecordSel sel_id
  = text "Cannot use record selector" <+> quotes (ppr sel_id) <+>
    text "as a function due to escaped type variables" $$
    text "Probable fix: use pattern-matching syntax instead"


{- *********************************************************************
*                                                                      *
                 tcInferId, tcCheckId
*                                                                      *
********************************************************************* -}

tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTc)
tcCheckId name res_ty
  = do { (expr, actual_res_ty) <- tcInferId name
       ; traceTc "tcCheckId" (vcat [ppr name, ppr actual_res_ty, ppr res_ty])
       ; addFunResCtxt expr [] actual_res_ty res_ty $
         tcWrapResultO (OccurrenceOf name) (HsVar noExtField (noLoc name)) expr
                                           actual_res_ty res_ty }

------------------------
tcInferId :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Look up an occurrence of an Id
-- Do not instantiate its type
tcInferId id_name
  | id_name `hasKey` assertIdKey
  = do { dflags <- getDynFlags
       ; if gopt Opt_IgnoreAsserts dflags
         then tc_infer_id (nameRdrName id_name) id_name
         else tc_infer_assert id_name }

  | otherwise
  = do { (expr, ty) <- tc_infer_id (nameRdrName id_name) id_name
       ; traceTc "tcInferId" (ppr id_name <+> dcolon <+> ppr ty)
       ; return (expr, ty) }

tc_infer_assert :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Deal with an occurrence of 'assert'
-- See Note [Adding the implicit parameter to 'assert']
tc_infer_assert assert_name
  = do { assert_error_id <- tcLookupId assertErrorName
       ; (wrap, id_rho) <- topInstantiate (OccurrenceOf assert_name)
                                          (idType assert_error_id)
       ; return (mkHsWrap wrap (HsVar noExtField (noLoc assert_error_id)), id_rho)
       }

tc_infer_id :: RdrName -> Name -> TcM (HsExpr GhcTc, TcSigmaType)
tc_infer_id lbl id_name
 = do { thing <- tcLookup id_name
      ; case thing of
             ATcId { tct_id = id }
               -> do { check_naughty id        -- Note [Local record selectors]
                     ; checkThLocalId id
                     ; return_id id }

             AGlobal (AnId id)
               -> do { check_naughty id
                     ; return_id id }
                    -- A global cannot possibly be ill-staged
                    -- nor does it need the 'lifting' treatment
                    -- hence no checkTh stuff here

             AGlobal (AConLike cl) -> case cl of
                 RealDataCon con -> return_data_con con
                 PatSynCon ps
                   | Just (expr, ty) <- patSynBuilderOcc ps
                   -> return (expr, ty)
                   | otherwise
                   -> nonBidirectionalErr id_name

             _ -> failWithTc $
                  ppr thing <+> text "used where a value identifier was expected" }
  where
    return_id id = return (HsVar noExtField (noLoc id), idType id)

    return_data_con con
       -- For data constructors, must perform the stupid-theta check
      | null stupid_theta
      = return (HsConLikeOut noExtField (RealDataCon con), con_ty)

      | otherwise
       -- See Note [Instantiating stupid theta]
      = do { let (tvs, theta, rho) = tcSplitSigmaTy con_ty
           ; (subst, tvs') <- newMetaTyVars tvs
           ; let tys'   = mkTyVarTys tvs'
                 theta' = substTheta subst theta
                 rho'   = substTy subst rho
           ; wrap <- instCall (OccurrenceOf id_name) tys' theta'
           ; addDataConStupidTheta con tys'
           ; return ( mkHsWrap wrap (HsConLikeOut noExtField (RealDataCon con))
                    , rho') }

      where
        con_ty         = dataConUserType con
        stupid_theta   = dataConStupidTheta con

    check_naughty id
      | isNaughtyRecordSelector id = failWithTc (naughtyRecordSel lbl)
      | otherwise                  = return ()


nonBidirectionalErr :: Outputable name => name -> TcM a
nonBidirectionalErr name = failWithTc $
    text "non-bidirectional pattern synonym"
    <+> quotes (ppr name) <+> text "used in an expression"

{-
Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker transforms (assert e1 e2) to (assertError e1 e2).
This isn't really the Right Thing because there's no way to "undo"
if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.


Note [Instantiating stupid theta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, when we infer the type of an Id, we don't instantiate,
because we wish to allow for visible type application later on.
But if a datacon has a stupid theta, we're a bit stuck. We need
to emit the stupid theta constraints with instantiated types. It's
difficult to defer this to the lazy instantiation, because a stupid
theta has no spot to put it in a type. So we just instantiate eagerly
in this case. Thus, users cannot use visible type application with
a data constructor sporting a stupid theta. I won't feel so bad for
the users that complain.
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
-- Here we just just add constraints fro cross-stage lifting
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
                        ; return (HsVar noExtField (noLoc sid)) }
                  else
                     setConstraintVar lie_var   $
                          -- Put the 'lift' constraint into the right LIE
                     newMethodFromName (OccurrenceOf id_name)
                                       GHC.Builtin.Names.TH.liftName
                                       [getRuntimeRep id_ty, id_ty]

                   -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; let pending_splice = PendingTcSplice id_name
                                 (nlHsApp (mkLHsWrap (applyQuoteWrapper q) (noLoc lift))
                                          (nlHsVar id))
        ; writeMutVar ps_var (pending_splice : ps)

        ; return () }
  where
    id_name = idName id

checkCrossStageLifting _ _ _ = return ()

polySpliceErr :: Id -> SDoc
polySpliceErr id
  = text "Can't splice the polymorphic local variable" <+> quotes (ppr id)

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

Local record selectors
~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. TcTyClsBindings.mkAuxBinds.
-}


{- *********************************************************************
*                                                                      *
                 tagToEnum#
*                                                                      *
********************************************************************* -}

{- Note [tagToEnum#]
~~~~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude, because it relies on our
knowing *now* that the type is ok, which in turn relies on the
eager-unification part of the type checker pushing enough information
here.  In theory the Right Thing to do is to have a new form of
constraint but I definitely cannot face that!  And it works ok as-is.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

When data type families are involved it's a bit more complicated.
     data family F a
     data instance F [Int] = A | B | C
Then we want to generate something like
     tagToEnum# R:FListInt 3# |> co :: R:FListInt ~ F [Int]
Usually that coercion is hidden inside the wrappers for
constructors of F [Int] but here we have to do it explicitly.

It's all grotesquely complicated.
-}

isTagToEnum :: HsExpr GhcRn -> Bool
isTagToEnum (HsVar _ (L _ fun_id)) = fun_id `hasKey` tagToEnumKey
isTagToEnum _ = False

tcTagToEnum :: HsExpr GhcRn -> HsExpr GhcTc -> [HsExprArg 'TcpTc]
            -> TcSigmaType -> ExpRhoType
            -> TcM (HsExpr GhcTc)
-- tagToEnum# :: forall a. Int# -> a
-- See Note [tagToEnum#]   Urgh!
tcTagToEnum expr fun args app_res_ty res_ty
  | null val_args
  = failWithTc (text "tagToEnum# must appear applied to one argument")

  | otherwise
  = do { res_ty <- readExpType res_ty
       ; ty'    <- zonkTcType res_ty

       -- Check that the type is algebraic
       ; case tcSplitTyConApp_maybe ty' of {
           Nothing -> do { addErrTc (mk_error ty' doc1)
                         ; vanilla_result } ;
           Just (tc, tc_args) ->

    do { -- Look through any type family
       ; fam_envs <- tcGetFamInstEnvs
       ; case tcLookupDataFamInst_maybe fam_envs tc tc_args of {
           Nothing -> do { check_enumeration ty' tc
                         ; vanilla_result } ;
           Just (rep_tc, rep_args, coi) ->

    do { -- coi :: tc tc_args ~R rep_tc rep_args
         check_enumeration ty' rep_tc
       ; let rep_ty  = mkTyConApp rep_tc rep_args
             fun'    = mkHsWrap (WpTyApp rep_ty) fun
             expr'   = rebuildPrefixApps fun' val_args
             df_wrap = mkWpCastR (mkTcSymCo coi)
       ; return (mkHsWrap df_wrap expr') }}}}}

  where
    val_args = dropWhile (not . isHsValArg) args

    vanilla_result
      = do { let expr' = rebuildPrefixApps fun args
           ; tcWrapResult expr expr' app_res_ty res_ty }

    check_enumeration ty' tc
      | isEnumerationTyCon tc = return ()
      | otherwise             = addErrTc (mk_error ty' doc2)

    doc1 = vcat [ text "Specify the type by giving a type signature"
                , text "e.g. (tagToEnum# x) :: Bool" ]
    doc2 = text "Result type must be an enumeration type"

    mk_error :: TcType -> SDoc -> SDoc
    mk_error ty what
      = hang (text "Bad call to tagToEnum#"
               <+> text "at type" <+> ppr ty)
           2 what


{- *********************************************************************
*                                                                      *
         Error reporting for function result mis-matches
*                                                                      *
********************************************************************* -}

addFunResCtxt :: HsExpr GhcTc -> [HsExprArg 'TcpTc]
              -> TcType -> ExpRhoType
              -> TcM a -> TcM a
-- When we have a mis-match in the return type of a function
-- try to give a helpful message about too many/few arguments
addFunResCtxt fun args fun_res_ty env_ty
  = addLandmarkErrCtxtM (\env -> (env, ) <$> mk_msg)
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
                                ; MASSERT( dumping )
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
                 info  | n_fun == n_env = Outputable.empty
                       | n_fun > n_env
                       , not_fun res_env
                       = text "Probable cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too few arguments"

                       -- n_fun < n_env
                       | (n_fun + count isValArg args) >= n_env
                       , not_fun res_fun
                       = text "Possible cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too many arguments"

                       | otherwise
                       = Outputable.empty  -- Never suggest that a naked variable is                                         -- applied to too many args!
           ; return info }
      where
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
This is incorrect in the face of nested foralls, however! This caused Trac
#13311, for instance:

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
             Pragmas on expressions
*                                                                      *
********************************************************************* -}

tcExprPrag :: HsPragE GhcRn -> HsPragE GhcTc
tcExprPrag (HsPragSCC x1 src ann) = HsPragSCC x1 src ann
tcExprPrag (HsPragCore x1 src lbl) = HsPragCore x1 src lbl
tcExprPrag (HsPragTick x1 src info srcInfo) = HsPragTick x1 src info srcInfo


{- *********************************************************************
*                                                                      *
             Misc utility functions
*                                                                      *
********************************************************************* -}

addExprCtxt :: LHsExpr GhcRn -> TcRn a -> TcRn a
addExprCtxt (L _ e) thing_inside
   | isAtomicHsExpr e = thing_inside
   | otherwise        = addErrCtxt (exprCtxt e) thing_inside

exprCtxt :: HsExpr GhcRn -> SDoc
exprCtxt expr = hang (text "In the expression:") 2 (ppr (stripParensHsExpr expr))

