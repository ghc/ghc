{-|
Module      : GHC.Hs.Utils
Description : Generic helpers for the HsSyn type.
Copyright   : (c) The University of Glasgow, 1992-2006

Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by          Module
   ----------------          -------------
   GhcPs/RdrName             GHC.Parser.PostProcess
   GhcRn/Name                GHC.Rename.*
   GhcTc/Id                  GHC.Tc.Utils.Zonk

The @mk*@ functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the @nl*@ functions which
just attach noSrcSpan to everything.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Hs.Utils(
  -- * Terms
  mkHsPar, mkHsApp, mkHsAppWith, mkHsApps, mkHsAppsWith,
  mkHsAppType, mkHsAppTypes, mkHsCaseAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkMatch, mkPrefixFunRhs, mkHsLam, mkHsIf,
  mkHsWrap, mkLHsWrap, mkHsWrapCo, mkHsWrapCoR, mkLHsWrapCo,
  mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsComp, mkHsWrapPat, mkHsWrapPatCo,
  mkLHsPar, mkHsCmdWrap, mkLHsCmdWrap,
  mkHsCmdIf,

  nlHsTyApp, nlHsTyApps, nlHsVar, nl_HsVar, nlHsDataCon,
  nlHsLit, nlHsApp, nlHsApps, nlHsSyntaxApps,
  nlHsIntLit, nlHsVarApps,
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,

  -- * Constructing general big tuples
  -- $big_tuples
  mkChunkified, chunkify,

  -- * Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mkSimpleGeneratedFunBind, mkTopFunBind,
  mkPatSynBind,
  isInfixFunBind,

  -- * Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString, mkHsStringPrimLit,
  mkHsCharPrimLit,

  -- * Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConVarPatName, nlConPat,
  nlConPatName, nlInfixConPat, nlNullaryConPat, nlWildConPat, nlWildPat,
  nlWildPatName, nlTuplePat, mkParPat, nlParPat,
  mkBigLHsVarTup, mkBigLHsTup, mkBigLHsVarPatTup, mkBigLHsPatTup,

  -- * Types
  mkHsAppTy, mkHsAppKindTy,
  hsTypeToHsSigType, hsTypeToHsSigWcType, mkClassOpSigs, mkHsSigEnv,
  nlHsAppTy, nlHsAppKindTy, nlHsTyVar, nlHsFunTy, nlHsParTy, nlHsTyConApp,

  -- * Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt,
  mkPsBindStmt, mkRnBindStmt, mkTcBindStmt,
  mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt,
  emptyRecStmt, emptyRecStmtName, emptyRecStmtId, mkRecStmt,
  unitRecStmtTc,

  -- * Template Haskell
  mkUntypedSplice, mkTypedSplice,
  mkHsQuasiQuote,

  -- * Collecting binders
  isUnliftedHsBind, isBangedHsBind,

  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsIdBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,
  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,
  CollectPass(..),

  hsLTyClDeclBinders, hsTyClForeignBinders,
  hsPatSynSelectors, getPatSynBinds,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,

  -- * Collecting implicit binders
  lStmtsImplicits, hsValBindsImplicits, lPatImplicits
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Pat
import GHC.Hs.Type
import GHC.Hs.Lit
import GHC.Hs.Extension

import GHC.Tc.Types.Evidence
import GHC.Core.TyCo.Rep
import GHC.Core.Multiplicity ( pattern Many )
import GHC.Builtin.Types ( unitTy )
import GHC.Tc.Utils.TcType
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Set hiding ( unitFV )
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Var
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Fixity
import GHC.Types.SourceText
import GHC.Data.FastString
import GHC.Data.Bag
import GHC.Settings.Constants
import GHC.Parser.Annotation

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Either
import Data.Function
import Data.List
import Data.Proxy

{-
************************************************************************
*                                                                      *
        Some useful helpers for constructing syntax
*                                                                      *
************************************************************************

These functions attempt to construct a not-completely-useless 'SrcSpan'
from their components, compared with the @nl*@ functions below which
just attach 'noSrcSpan' to everything.
-}

-- | @e => (e)@
mkHsPar :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkHsPar e = L (getLoc e) (HsPar noExtField e)

mkSimpleMatch :: HsMatchContext (NoGhcTc (GhcPass p))
              -> [LPat (GhcPass p)] -> Located (body (GhcPass p))
              -> LMatch (GhcPass p) (Located (body (GhcPass p)))
mkSimpleMatch ctxt pats rhs
  = L loc $
    Match { m_ext = noExtField, m_ctxt = ctxt, m_pats = pats
          , m_grhss = unguardedGRHSs rhs }
  where
    loc = case pats of
                []      -> getLoc rhs
                (pat:_) -> combineSrcSpans (getLoc pat) (getLoc rhs)

unguardedGRHSs :: Located (body (GhcPass p))
               -> GRHSs (GhcPass p) (Located (body (GhcPass p)))
unguardedGRHSs rhs@(L loc _)
  = GRHSs noExtField (unguardedRHS loc rhs) (noLoc emptyLocalBinds)

unguardedRHS :: SrcSpan -> Located (body (GhcPass p))
             -> [LGRHS (GhcPass p) (Located (body (GhcPass p)))]
unguardedRHS loc rhs = [L loc (GRHS noExtField [] rhs)]

mkMatchGroup :: ( XMG (GhcPass p) (Located (body (GhcPass p))) ~ NoExtField )
                => Origin -> [Located (Match (GhcPass p) (Located (body (GhcPass p))))]
                -> MatchGroup (GhcPass p) (Located (body (GhcPass p)))
mkMatchGroup origin matches = MG { mg_ext = noExtField
                                 , mg_alts = mkLocatedList matches
                                 , mg_origin = origin }

mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms

mkHsApp :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkHsApp = mkHsAppWith addCLoc

mkHsAppWith
  :: (LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> HsExpr (GhcPass id) -> LHsExpr (GhcPass id))
  -> LHsExpr (GhcPass id)
  -> LHsExpr (GhcPass id)
  -> LHsExpr (GhcPass id)
mkHsAppWith mkLocated e1 e2 = mkLocated e1 e2 (HsApp noExtField e1 e2)

mkHsApps
  :: LHsExpr (GhcPass id) -> [LHsExpr (GhcPass id)] -> LHsExpr (GhcPass id)
mkHsApps = mkHsAppsWith addCLoc

mkHsAppsWith
 :: (LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> HsExpr (GhcPass id) -> LHsExpr (GhcPass id))
 -> LHsExpr (GhcPass id)
 -> [LHsExpr (GhcPass id)]
 -> LHsExpr (GhcPass id)
mkHsAppsWith mkLocated = foldl' (mkHsAppWith mkLocated)

mkHsAppType :: LHsExpr GhcRn -> LHsWcType GhcRn -> LHsExpr GhcRn
mkHsAppType e t = addCLoc e t_body (HsAppType noExtField e paren_wct)
  where
    t_body    = hswc_body t
    paren_wct = t { hswc_body = parenthesizeHsType appPrec t_body }

mkHsAppTypes :: LHsExpr GhcRn -> [LHsWcType GhcRn] -> LHsExpr GhcRn
mkHsAppTypes = foldl' mkHsAppType

mkHsLam :: IsPass p
        => (XMG (GhcPass p) (LHsExpr (GhcPass p)) ~ NoExtField)
        => [LPat (GhcPass p)]
        -> LHsExpr (GhcPass p)
        -> LHsExpr (GhcPass p)
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam noExtField matches))
  where
    matches = mkMatchGroup Generated
                           [mkSimpleMatch LambdaExpr pats' body]
    pats' = map (parenthesizePat appPrec) pats

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr GhcTc -> LHsExpr GhcTc
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars
                                       <.> mkWpLams dicts) expr

-- |A simple case alternative with a single pattern, no binds, no guards;
-- pre-typechecking
mkHsCaseAlt :: LPat (GhcPass p) -> (Located (body (GhcPass p)))
            -> LMatch (GhcPass p) (Located (body (GhcPass p)))
mkHsCaseAlt pat expr
  = mkSimpleMatch CaseAlt [pat] expr

nlHsTyApp :: Id -> [Type] -> LHsExpr GhcTc
nlHsTyApp fun_id tys
  = noLoc (mkHsWrap (mkWpTyApps tys) (HsVar noExtField (noLoc fun_id)))

nlHsTyApps :: Id -> [Type] -> [LHsExpr GhcTc] -> LHsExpr GhcTc
nlHsTyApps fun_id tys xs = foldl' nlHsApp (nlHsTyApp fun_id tys) xs

--------- Adding parens ---------
-- | Wrap in parens if @'hsExprNeedsParens' appPrec@ says it needs them
-- So @f x@ becomes @(f x)@, but @3@ stays as @3@.
mkLHsPar :: IsPass id => LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkLHsPar le@(L loc e)
  | hsExprNeedsParens appPrec e = L loc (HsPar noExtField le)
  | otherwise                   = le

mkParPat :: IsPass p => LPat (GhcPass p) -> LPat (GhcPass p)
mkParPat lp@(L loc p)
  | patNeedsParens appPrec p = L loc (ParPat noExtField lp)
  | otherwise                = lp

nlParPat :: LPat (GhcPass name) -> LPat (GhcPass name)
nlParPat p = noLoc (ParPat noExtField p)

-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See GHC.Rename.Env.lookupSyntax

mkHsIntegral   :: IntegralLit -> HsOverLit GhcPs
mkHsFractional :: FractionalLit -> HsOverLit GhcPs
mkHsIsString   :: SourceText -> FastString -> HsOverLit GhcPs
mkHsDo         :: HsStmtContext GhcRn -> [ExprLStmt GhcPs] -> HsExpr GhcPs
mkHsComp       :: HsStmtContext GhcRn -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
               -> HsExpr GhcPs

mkNPat      :: Located (HsOverLit GhcPs) -> Maybe (SyntaxExpr GhcPs)
            -> Pat GhcPs
mkNPlusKPat :: Located RdrName -> Located (HsOverLit GhcPs) -> Pat GhcPs

-- NB: The following functions all use noSyntaxExpr: the generated expressions
--     will not work with rebindable syntax if used after the renamer
mkLastStmt :: IsPass idR => Located (bodyR (GhcPass idR))
           -> StmtLR (GhcPass idL) (GhcPass idR) (Located (bodyR (GhcPass idR)))
mkBodyStmt :: Located (bodyR GhcPs)
           -> StmtLR (GhcPass idL) GhcPs (Located (bodyR GhcPs))
mkPsBindStmt :: LPat GhcPs -> Located (bodyR GhcPs)
             -> StmtLR GhcPs GhcPs (Located (bodyR GhcPs))
mkRnBindStmt :: LPat GhcRn -> Located (bodyR GhcRn)
             -> StmtLR GhcRn GhcRn (Located (bodyR GhcRn))
mkTcBindStmt :: LPat GhcTc -> Located (bodyR GhcTc)
             -> StmtLR GhcTc GhcTc (Located (bodyR GhcTc))

emptyRecStmt     :: StmtLR (GhcPass idL) GhcPs bodyR
emptyRecStmtName :: StmtLR GhcRn GhcRn bodyR
emptyRecStmtId   :: StmtLR GhcTc GhcTc bodyR
mkRecStmt        :: [LStmtLR (GhcPass idL) GhcPs bodyR]
                 -> StmtLR (GhcPass idL) GhcPs bodyR


mkHsIntegral     i  = OverLit noExtField (HsIntegral       i) noExpr
mkHsFractional   f  = OverLit noExtField (HsFractional     f) noExpr
mkHsIsString src s  = OverLit noExtField (HsIsString   src s) noExpr

mkHsDo ctxt stmts = HsDo noExtField ctxt (mkLocatedList stmts)
mkHsComp ctxt stmts expr = mkHsDo ctxt (stmts ++ [last_stmt])
  where
    last_stmt = L (getLoc expr) $ mkLastStmt expr

-- restricted to GhcPs because other phases might need a SyntaxExpr
mkHsIf :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsIf c a b = HsIf noExtField c a b

-- restricted to GhcPs because other phases might need a SyntaxExpr
mkHsCmdIf :: LHsExpr GhcPs -> LHsCmd GhcPs -> LHsCmd GhcPs -> HsCmd GhcPs
mkHsCmdIf c a b = HsCmdIf noExtField noSyntaxExpr c a b

mkNPat lit neg     = NPat noExtField lit neg noSyntaxExpr
mkNPlusKPat id lit
  = NPlusKPat noExtField id lit (unLoc lit) noSyntaxExpr noSyntaxExpr

mkTransformStmt    :: [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkTransformByStmt  :: [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> LHsExpr GhcPs -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkGroupUsingStmt   :: [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkGroupByUsingStmt :: [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)

emptyTransStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs)
emptyTransStmt = TransStmt { trS_ext = noExtField
                           , trS_form = panic "emptyTransStmt: form"
                           , trS_stmts = [], trS_bndrs = []
                           , trS_by = Nothing, trS_using = noLoc noExpr
                           , trS_ret = noSyntaxExpr, trS_bind = noSyntaxExpr
                           , trS_fmap = noExpr }
mkTransformStmt    ss u   = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  ss u b = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   ss u   = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt ss b u = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body = LastStmt noExtField body Nothing noSyntaxExpr
mkBodyStmt body
  = BodyStmt noExtField body noSyntaxExpr noSyntaxExpr
mkPsBindStmt pat body = BindStmt noExtField pat body
mkRnBindStmt pat body = BindStmt (XBindStmtRn { xbsrn_bindOp = noSyntaxExpr, xbsrn_failOp = Nothing }) pat body
mkTcBindStmt pat body = BindStmt (XBindStmtTc { xbstc_bindOp = noSyntaxExpr,
                                                xbstc_boundResultType = unitTy,
                                                   -- unitTy is a dummy value
                                                   -- can't panic here: it's forced during zonking
                                                xbstc_boundResultMult = Many,
                                                xbstc_failOp = Nothing }) pat body

emptyRecStmt' :: forall idL idR body. IsPass idR
              => XRecStmt (GhcPass idL) (GhcPass idR) body
              -> StmtLR (GhcPass idL) (GhcPass idR) body
emptyRecStmt' tyVal =
   RecStmt
     { recS_stmts = [], recS_later_ids = []
     , recS_rec_ids = []
     , recS_ret_fn = noSyntaxExpr
     , recS_mfix_fn = noSyntaxExpr
     , recS_bind_fn = noSyntaxExpr
     , recS_ext = tyVal }

unitRecStmtTc :: RecStmtTc
unitRecStmtTc = RecStmtTc { recS_bind_ty = unitTy
                          , recS_later_rets = []
                          , recS_rec_rets = []
                          , recS_ret_ty = unitTy }

emptyRecStmt     = emptyRecStmt' noExtField
emptyRecStmtName = emptyRecStmt' noExtField
emptyRecStmtId   = emptyRecStmt' unitRecStmtTc
                                        -- a panic might trigger during zonking
mkRecStmt stmts  = emptyRecStmt { recS_stmts = stmts }

-------------------------------
-- | A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr GhcPs -> IdP GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsOpApp e1 op e2 = OpApp noExtField e1 (noLoc (HsVar noExtField (noLoc op))) e2

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))

mkUntypedSplice :: SpliceDecoration -> LHsExpr GhcPs -> HsSplice GhcPs
mkUntypedSplice hasParen e = HsUntypedSplice noExtField hasParen unqualSplice e

mkTypedSplice :: SpliceDecoration -> LHsExpr GhcPs -> HsSplice GhcPs
mkTypedSplice hasParen e = HsTypedSplice noExtField hasParen unqualSplice e

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsSplice GhcPs
mkHsQuasiQuote quoter span quote
  = HsQuasiQuote noExtField unqualSplice quoter span quote

mkHsString :: String -> HsLit (GhcPass p)
mkHsString s = HsString NoSourceText (mkFastString s)

mkHsStringPrimLit :: FastString -> HsLit (GhcPass p)
mkHsStringPrimLit fs = HsStringPrim NoSourceText (bytesFS fs)

mkHsCharPrimLit :: Char -> HsLit (GhcPass p)
mkHsCharPrimLit c = HsChar NoSourceText c


{-
************************************************************************
*                                                                      *
        Constructing syntax with no location info
*                                                                      *
************************************************************************
-}

nlHsVar :: IdP (GhcPass id) -> LHsExpr (GhcPass id)
nlHsVar n = noLoc (HsVar noExtField (noLoc n))

nl_HsVar :: IdP (GhcPass id) -> HsExpr (GhcPass id)
nl_HsVar n = HsVar noExtField (noLoc n)

-- | NB: Only for 'LHsExpr' 'Id'.
nlHsDataCon :: DataCon -> LHsExpr GhcTc
nlHsDataCon con = noLoc (HsConLikeOut noExtField (RealDataCon con))

nlHsLit :: HsLit (GhcPass p) -> LHsExpr (GhcPass p)
nlHsLit n = noLoc (HsLit noExtField n)

nlHsIntLit :: Integer -> LHsExpr (GhcPass p)
nlHsIntLit n = noLoc (HsLit noExtField (HsInt noExtField (mkIntegralLit n)))

nlVarPat :: IdP (GhcPass id) -> LPat (GhcPass id)
nlVarPat n = noLoc (VarPat noExtField (noLoc n))

nlLitPat :: HsLit GhcPs -> LPat GhcPs
nlLitPat l = noLoc (LitPat noExtField l)

nlHsApp :: IsPass id => LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
nlHsApp f x = noLoc (HsApp noExtField f (mkLHsPar x))

nlHsSyntaxApps :: SyntaxExprTc -> [LHsExpr GhcTc]
               -> LHsExpr GhcTc
nlHsSyntaxApps (SyntaxExprTc { syn_expr      = fun
                             , syn_arg_wraps = arg_wraps
                             , syn_res_wrap  = res_wrap }) args
  = mkLHsWrap res_wrap (foldl' nlHsApp (noLoc fun) (zipWithEqual "nlHsSyntaxApps"
                                                     mkLHsWrap arg_wraps args))
nlHsSyntaxApps NoSyntaxExprTc args = pprPanic "nlHsSyntaxApps" (ppr args)
  -- this function should never be called in scenarios where there is no
  -- syntax expr

nlHsApps :: IsPass id => IdP (GhcPass id) -> [LHsExpr (GhcPass id)] -> LHsExpr (GhcPass id)
nlHsApps f xs = foldl' nlHsApp (nlHsVar f) xs

nlHsVarApps :: IdP (GhcPass id) -> [IdP (GhcPass id)] -> LHsExpr (GhcPass id)
nlHsVarApps f xs = noLoc (foldl' mk (HsVar noExtField (noLoc f))
                                               (map ((HsVar noExtField) . noLoc) xs))
                 where
                   mk f a = HsApp noExtField (noLoc f) (noLoc a)

nlConVarPat :: RdrName -> [RdrName] -> LPat GhcPs
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlConVarPatName :: Name -> [Name] -> LPat GhcRn
nlConVarPatName con vars = nlConPatName con (map nlVarPat vars)

nlInfixConPat :: RdrName -> LPat GhcPs -> LPat GhcPs -> LPat GhcPs
nlInfixConPat con l r = noLoc $ ConPat
  { pat_con = noLoc con
  , pat_args = InfixCon (parenthesizePat opPrec l)
                        (parenthesizePat opPrec r)
  , pat_con_ext = noExtField
  }

nlConPat :: RdrName -> [LPat GhcPs] -> LPat GhcPs
nlConPat con pats = noLoc $ ConPat
  { pat_con_ext = noExtField
  , pat_con = noLoc con
  , pat_args = PrefixCon [] (map (parenthesizePat appPrec) pats)
  }

nlConPatName :: Name -> [LPat GhcRn] -> LPat GhcRn
nlConPatName con pats = noLoc $ ConPat
  { pat_con_ext = noExtField
  , pat_con = noLoc con
  , pat_args = PrefixCon [] (map (parenthesizePat appPrec) pats)
  }

nlNullaryConPat :: RdrName -> LPat GhcPs
nlNullaryConPat con = noLoc $ ConPat
  { pat_con_ext = noExtField
  , pat_con = noLoc con
  , pat_args = PrefixCon [] []
  }

nlWildConPat :: DataCon -> LPat GhcPs
nlWildConPat con = noLoc $ ConPat
  { pat_con_ext = noExtField
  , pat_con = noLoc $ getRdrName con
  , pat_args = PrefixCon [] $
     replicate (dataConSourceArity con)
               nlWildPat
  }

-- | Wildcard pattern - after parsing
nlWildPat :: LPat GhcPs
nlWildPat  = noLoc (WildPat noExtField )

-- | Wildcard pattern - after renaming
nlWildPatName :: LPat GhcRn
nlWildPatName  = noLoc (WildPat noExtField )

nlHsDo :: HsStmtContext GhcRn -> [LStmt GhcPs (LHsExpr GhcPs)]
       -> LHsExpr GhcPs
nlHsDo ctxt stmts = noLoc (mkHsDo ctxt stmts)

nlHsOpApp :: LHsExpr GhcPs -> IdP GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
nlHsOpApp e1 op e2 = noLoc (mkHsOpApp e1 op e2)

nlHsLam  :: LMatch GhcPs (LHsExpr GhcPs) -> LHsExpr GhcPs
nlHsPar  :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
nlHsCase :: LHsExpr GhcPs -> [LMatch GhcPs (LHsExpr GhcPs)]
         -> LHsExpr GhcPs
nlList   :: [LHsExpr GhcPs] -> LHsExpr GhcPs

nlHsLam match          = noLoc (HsLam noExtField (mkMatchGroup Generated [match]))
nlHsPar e              = noLoc (HsPar noExtField e)

-- nlHsIf should generate if-expressions which are NOT subject to
-- RebindableSyntax, so the first field of HsIf is False. (#12080)
nlHsIf :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
nlHsIf cond true false = noLoc (HsIf noExtField cond true false)

nlHsCase expr matches
  = noLoc (HsCase noExtField expr (mkMatchGroup Generated matches))
nlList exprs          = noLoc (ExplicitList noExtField Nothing exprs)

nlHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsTyVar :: IdP (GhcPass p)                            -> LHsType (GhcPass p)
nlHsFunTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsParTy :: LHsType (GhcPass p)                        -> LHsType (GhcPass p)

nlHsAppTy f t = noLoc (HsAppTy noExtField f (parenthesizeHsType appPrec t))
nlHsTyVar x   = noLoc (HsTyVar noExtField NotPromoted (noLoc x))
nlHsFunTy a b = noLoc (HsFunTy noExtField (HsUnrestrictedArrow NormalSyntax) (parenthesizeHsType funPrec a) b)
nlHsParTy t   = noLoc (HsParTy noExtField t)

nlHsTyConApp :: LexicalFixity -> IdP (GhcPass p)
             -> [LHsTypeArg (GhcPass p)] -> LHsType (GhcPass p)
nlHsTyConApp fixity tycon tys
  | Infix <- fixity
  , HsValArg ty1 : HsValArg ty2 : rest <- tys
  = foldl' mk_app (noLoc $ HsOpTy noExtField ty1 (noLoc tycon) ty2) rest
  | otherwise
  = foldl' mk_app (nlHsTyVar tycon) tys
  where
    mk_app :: LHsType (GhcPass p) -> LHsTypeArg (GhcPass p) -> LHsType (GhcPass p)
    mk_app fun@(L _ (HsOpTy {})) arg = mk_app (noLoc $ HsParTy noExtField fun) arg
      -- parenthesize things like `(A + B) C`
    mk_app fun (HsValArg ty) = noLoc (HsAppTy noExtField fun (parenthesizeHsType appPrec ty))
    mk_app fun (HsTypeArg _ ki) = noLoc (HsAppKindTy noSrcSpan fun (parenthesizeHsType appPrec ki))
    mk_app fun (HsArgPar _) = noLoc (HsParTy noExtField fun)

nlHsAppKindTy ::
  LHsType (GhcPass p) -> LHsKind (GhcPass p) -> LHsType (GhcPass p)
nlHsAppKindTy f k
  = noLoc (HsAppKindTy noSrcSpan f (parenthesizeHsType appPrec k))

{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

mkLHsTupleExpr :: [LHsExpr (GhcPass a)] -> LHsExpr (GhcPass a)
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] = e
mkLHsTupleExpr es
  = noLoc $ ExplicitTuple noExtField (map (noLoc . (Present noExtField)) es) Boxed

mkLHsVarTuple :: [IdP (GhcPass a)] -> LHsExpr (GhcPass a)
mkLHsVarTuple ids  = mkLHsTupleExpr (map nlHsVar ids)

nlTuplePat :: [LPat GhcPs] -> Boxity -> LPat GhcPs
nlTuplePat pats box = noLoc (TuplePat noExtField pats box)

missingTupArg :: HsTupArg GhcPs
missingTupArg = Missing noExtField

mkLHsPatTup :: [LPat GhcRn] -> LPat GhcRn
mkLHsPatTup []     = noLoc $ TuplePat noExtField [] Boxed
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = L (getLoc (head lpats)) $ TuplePat noExtField lpats Boxed

-- | The Big equivalents for the source tuple expressions
mkBigLHsVarTup :: [IdP (GhcPass id)] -> LHsExpr (GhcPass id)
mkBigLHsVarTup ids = mkBigLHsTup (map nlHsVar ids)

mkBigLHsTup :: [LHsExpr (GhcPass id)] -> LHsExpr (GhcPass id)
mkBigLHsTup = mkChunkified mkLHsTupleExpr

-- | The Big equivalents for the source tuple patterns
mkBigLHsVarPatTup :: [IdP GhcRn] -> LPat GhcRn
mkBigLHsVarPatTup bs = mkBigLHsPatTup (map nlVarPat bs)

mkBigLHsPatTup :: [LPat GhcRn] -> LPat GhcRn
mkBigLHsPatTup = mkChunkified mkLHsPatTup

-- $big_tuples
-- #big_tuples#
--
-- GHCs built in tuples can only go up to 'mAX_TUPLE_SIZE' in arity, but
-- we might conceivably want to build such a massive tuple as part of the
-- output of a desugaring stage (notably that for list comprehensions).
--
-- We call tuples above this size \"big tuples\", and emulate them by
-- creating and pattern matching on >nested< tuples that are expressible
-- by GHC.
--
-- Nesting policy: it's better to have a 2-tuple of 10-tuples (3 objects)
-- than a 10-tuple of 2-tuples (11 objects), so we want the leaves of any
-- construction to be big.
--
-- If you just use the 'mkBigCoreTup', 'mkBigCoreVarTupTy', 'mkTupleSelector'
-- and 'mkTupleCase' functions to do all your work with tuples you should be
-- fine, and not have to worry about the arity limitation at all.

-- | Lifts a \"small\" constructor into a \"big\" constructor by recursive decomposition
mkChunkified :: ([a] -> a)      -- ^ \"Small\" constructor function, of maximum input arity 'mAX_TUPLE_SIZE'
             -> [a]             -- ^ Possible \"big\" list of things to construct from
             -> a               -- ^ Constructed thing made possible by recursive decomposition
mkChunkified small_tuple as = mk_big_tuple (chunkify as)
  where
        -- Each sub-list is short enough to fit in a tuple
    mk_big_tuple [as] = small_tuple as
    mk_big_tuple as_s = mk_big_tuple (chunkify (map small_tuple as_s))

chunkify :: [a] -> [[a]]
-- ^ Split a list into lists that are small enough to have a corresponding
-- tuple arity. The sub-lists of the result all have length <= 'mAX_TUPLE_SIZE'
-- But there may be more than 'mAX_TUPLE_SIZE' sub-lists
chunkify xs
  | n_xs <= mAX_TUPLE_SIZE = [xs]
  | otherwise              = split xs
  where
    n_xs     = length xs
    split [] = []
    split xs = take mAX_TUPLE_SIZE xs : split (drop mAX_TUPLE_SIZE xs)

{-
************************************************************************
*                                                                      *
        LHsSigType and LHsSigWcType
*                                                                      *
********************************************************************* -}

-- | Convert an 'LHsType' to an 'LHsSigType'.
hsTypeToHsSigType :: LHsType GhcPs -> LHsSigType GhcPs
hsTypeToHsSigType lty@(L loc ty) = L loc $ case ty of
  HsForAllTy { hst_tele = HsForAllInvis { hsf_invis_bndrs = bndrs }
             , hst_body = body }
    -> mkHsExplicitSigType bndrs body
  _ -> mkHsImplicitSigType lty

-- | Convert an 'LHsType' to an 'LHsSigWcType'.
hsTypeToHsSigWcType :: LHsType GhcPs -> LHsSigWcType GhcPs
hsTypeToHsSigWcType = mkHsWildCardBndrs . hsTypeToHsSigType

mkHsSigEnv :: forall a. (LSig GhcRn -> Maybe ([Located Name], a))
                     -> [LSig GhcRn]
                     -> NameEnv a
mkHsSigEnv get_info sigs
  = mkNameEnv          (mk_pairs ordinary_sigs)
   `extendNameEnvList` (mk_pairs gen_dm_sigs)
   -- The subtlety is this: in a class decl with a
   -- default-method signature as well as a method signature
   -- we want the latter to win (#12533)
   --    class C x where
   --       op :: forall a . x a -> x a
   --       default op :: forall b . x b -> x b
   --       op x = ...(e :: b -> b)...
   -- The scoped type variables of the 'default op', namely 'b',
   -- scope over the code for op.   The 'forall a' does not!
   -- This applies both in the renamer and typechecker, both
   -- of which use this function
  where
    (gen_dm_sigs, ordinary_sigs) = partition is_gen_dm_sig sigs
    is_gen_dm_sig (L _ (ClassOpSig _ True _ _)) = True
    is_gen_dm_sig _                             = False

    mk_pairs :: [LSig GhcRn] -> [(Name, a)]
    mk_pairs sigs = [ (n,a) | Just (ns,a) <- map get_info sigs
                            , L _ n <- ns ]

mkClassOpSigs :: [LSig GhcPs] -> [LSig GhcPs]
-- ^ Convert 'TypeSig' to 'ClassOpSig'.
-- The former is what is parsed, but the latter is
-- what we need in class/instance declarations
mkClassOpSigs sigs
  = map fiddle sigs
  where
    fiddle (L loc (TypeSig _ nms ty))
      = L loc (ClassOpSig noExtField False nms (dropWildCards ty))
    fiddle sig = sig

{- *********************************************************************
*                                                                      *
    --------- HsWrappers: type args, dict args, casts ---------
*                                                                      *
********************************************************************* -}

mkLHsWrap :: HsWrapper -> LHsExpr GhcTc -> LHsExpr GhcTc
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

-- | Avoid @'HsWrap' co1 ('HsWrap' co2 _)@ and @'HsWrap' co1 ('HsPar' _ _)@
-- See Note [Detecting forced eta expansion] in "GHC.HsToCore.Expr"
mkHsWrap :: HsWrapper -> HsExpr GhcTc -> HsExpr GhcTc
mkHsWrap co_fn e | isIdHsWrapper co_fn   = e
mkHsWrap co_fn (XExpr (WrapExpr (HsWrap co_fn' e))) = mkHsWrap (co_fn <.> co_fn') e
mkHsWrap co_fn (HsPar x (L l e))                = HsPar x (L l (mkHsWrap co_fn e))
mkHsWrap co_fn e                                = XExpr (WrapExpr $ HsWrap co_fn e)

mkHsWrapCo :: TcCoercionN   -- A Nominal coercion  a ~N b
           -> HsExpr GhcTc -> HsExpr GhcTc
mkHsWrapCo co e = mkHsWrap (mkWpCastN co) e

mkHsWrapCoR :: TcCoercionR   -- A Representational coercion  a ~R b
            -> HsExpr GhcTc -> HsExpr GhcTc
mkHsWrapCoR co e = mkHsWrap (mkWpCastR co) e

mkLHsWrapCo :: TcCoercionN -> LHsExpr GhcTc -> LHsExpr GhcTc
mkLHsWrapCo co (L loc e) = L loc (mkHsWrapCo co e)

mkHsCmdWrap :: HsWrapper -> HsCmd GhcTc -> HsCmd GhcTc
mkHsCmdWrap w cmd | isIdHsWrapper w = cmd
                  | otherwise       = XCmd (HsWrap w cmd)

mkLHsCmdWrap :: HsWrapper -> LHsCmd GhcTc -> LHsCmd GhcTc
mkLHsCmdWrap w (L loc c) = L loc (mkHsCmdWrap w c)

mkHsWrapPat :: HsWrapper -> Pat GhcTc -> Type -> Pat GhcTc
mkHsWrapPat co_fn p ty | isIdHsWrapper co_fn = p
                       | otherwise           = XPat $ CoPat co_fn p ty

mkHsWrapPatCo :: TcCoercionN -> Pat GhcTc -> Type -> Pat GhcTc
mkHsWrapPatCo co pat ty | isTcReflCo co = pat
                        | otherwise     = XPat $ CoPat (mkWpCastN co) pat ty

mkHsDictLet :: TcEvBinds -> LHsExpr GhcTc -> LHsExpr GhcTc
mkHsDictLet ev_binds expr = mkLHsWrap (mkWpLet ev_binds) expr

{-
l
************************************************************************
*                                                                      *
                Bindings; with a location at the top
*                                                                      *
************************************************************************
-}

mkFunBind :: Origin -> Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)]
          -> HsBind GhcPs
-- ^ Not infix, with place holders for coercion and free vars
mkFunBind origin fn ms
  = FunBind { fun_id = fn
            , fun_matches = mkMatchGroup origin ms
            , fun_ext = noExtField
            , fun_tick = [] }

mkTopFunBind :: Origin -> Located Name -> [LMatch GhcRn (LHsExpr GhcRn)]
             -> HsBind GhcRn
-- ^ In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn
                                    , fun_matches = mkMatchGroup origin ms
                                    , fun_ext  = emptyNameSet -- NB: closed
                                                              --     binding
                                    , fun_tick = [] }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
mkHsVarBind loc var rhs = mkSimpleGeneratedFunBind loc var [] rhs

mkVarBind :: IdP (GhcPass p) -> LHsExpr (GhcPass p) -> LHsBind (GhcPass p)
mkVarBind var rhs = L (getLoc rhs) $
                    VarBind { var_ext = noExtField,
                              var_id = var, var_rhs = rhs }

mkPatSynBind :: Located RdrName -> HsPatSynDetails GhcPs
             -> LPat GhcPs -> HsPatSynDir GhcPs -> HsBind GhcPs
mkPatSynBind name details lpat dir = PatSynBind noExtField psb
  where
    psb = PSB{ psb_ext = noExtField
             , psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir }

-- |If any of the matches in the 'FunBind' are infix, the 'FunBind' is
-- considered infix.
isInfixFunBind :: forall id1 id2. UnXRec id2 => HsBindLR id1 id2 -> Bool
isInfixFunBind (FunBind { fun_matches = MG _ matches _ })
  = any (isInfixMatch . unXRec @id2) (unXRec @id2 matches)
isInfixFunBind _ = False


------------
-- | Convenience function using 'mkFunBind'.
-- This is for generated bindings only, do not use for user-written code.
mkSimpleGeneratedFunBind :: SrcSpan -> RdrName -> [LPat GhcPs]
                -> LHsExpr GhcPs -> LHsBind GhcPs
mkSimpleGeneratedFunBind loc fun pats expr
  = L loc $ mkFunBind Generated (L loc fun)
              [mkMatch (mkPrefixFunRhs (L loc fun)) pats expr
                       (noLoc emptyLocalBinds)]

-- | Make a prefix, non-strict function 'HsMatchContext'
mkPrefixFunRhs :: LIdP p -> HsMatchContext p
mkPrefixFunRhs n = FunRhs { mc_fun = n
                          , mc_fixity = Prefix
                          , mc_strictness = NoSrcStrict }

------------
mkMatch :: forall p. IsPass p
        => HsMatchContext (NoGhcTc (GhcPass p))
        -> [LPat (GhcPass p)]
        -> LHsExpr (GhcPass p)
        -> Located (HsLocalBinds (GhcPass p))
        -> LMatch (GhcPass p) (LHsExpr (GhcPass p))
mkMatch ctxt pats expr lbinds
  = noLoc (Match { m_ext   = noExtField
                 , m_ctxt  = ctxt
                 , m_pats  = map paren pats
                 , m_grhss = GRHSs noExtField (unguardedRHS noSrcSpan expr) lbinds })
  where
    paren :: Located (Pat (GhcPass p)) -> Located (Pat (GhcPass p))
    paren lp@(L l p)
      | patNeedsParens appPrec p = L l (ParPat noExtField lp)
      | otherwise                = lp

{-
************************************************************************
*                                                                      *
        Collecting binders
*                                                                      *
************************************************************************

Get all the binders in some HsBindGroups, IN THE ORDER OF APPEARANCE. eg.

...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...

it should return [x, y, f, a, b] (remember, order important).

Note [Collect binders only after renaming]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These functions should only be used on HsSyn *after* the renamer,
to return a [Name] or [Id].  Before renaming the record punning
and wild-card mechanism makes it hard to know what is bound.
So these functions should not be applied to (HsSyn RdrName)

Note [Unlifted id check in isUnliftedHsBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function isUnliftedHsBind is used to complain if we make a top-level
binding for a variable of unlifted type.

Such a binding is illegal if the top-level binding would be unlifted;
but also if the local letrec generated by desugaring AbsBinds would be.
E.g.
      f :: Num a => (# a, a #)
      g :: Num a => a -> a
      f = ...g...
      g = ...g...

The top-level bindings for f,g are not unlifted (because of the Num a =>),
but the local, recursive, monomorphic bindings are:

      t = /\a \(d:Num a).
         letrec fm :: (# a, a #) = ...g...
                gm :: a -> a = ...f...
         in (fm, gm)

Here the binding for 'fm' is illegal.  So generally we check the abe_mono types.

BUT we have a special case when abs_sig is true;
  see Note [The abs_sig field of AbsBinds] in GHC.Hs.Binds
-}

----------------- Bindings --------------------------

-- | Should we treat this as an unlifted bind? This will be true for any
-- bind that binds an unlifted variable, but we must be careful around
-- AbsBinds. See Note [Unlifted id check in isUnliftedHsBind]. For usage
-- information, see Note [Strict binds checks] is GHC.HsToCore.Binds.
isUnliftedHsBind :: HsBind GhcTc -> Bool  -- works only over typechecked binds
isUnliftedHsBind bind
  | AbsBinds { abs_exports = exports, abs_sig = has_sig } <- bind
  = if has_sig
    then any (is_unlifted_id . abe_poly) exports
    else any (is_unlifted_id . abe_mono) exports
    -- If has_sig is True we will never generate a binding for abe_mono,
    -- so we don't need to worry about it being unlifted. The abe_poly
    -- binding might not be: e.g. forall a. Num a => (# a, a #)

  | otherwise
  = any is_unlifted_id (collectHsBindBinders bind)
  where
    is_unlifted_id id = isUnliftedType (idType id)

-- | Is a binding a strict variable or pattern bind (e.g. @!x = ...@)?
isBangedHsBind :: HsBind GhcTc -> Bool
isBangedHsBind (AbsBinds { abs_binds = binds })
  = anyBag (isBangedHsBind . unLoc) binds
isBangedHsBind (FunBind {fun_matches = matches})
  | [L _ match] <- unLoc $ mg_alts matches
  , FunRhs{mc_strictness = SrcStrict} <- m_ctxt match
  = True
isBangedHsBind (PatBind {pat_lhs = pat})
  = isBangedLPat pat
isBangedHsBind _
  = False

collectLocalBinders :: CollectPass (GhcPass idL)
                    => HsLocalBindsLR (GhcPass idL) (GhcPass idR)
                    -> [IdP (GhcPass idL)]
collectLocalBinders (HsValBinds _ binds) = collectHsIdBinders binds
                                         -- No pattern synonyms here
collectLocalBinders (HsIPBinds {})      = []
collectLocalBinders (EmptyLocalBinds _) = []

collectHsIdBinders :: CollectPass (GhcPass idL)
                   => HsValBindsLR (GhcPass idL) (GhcPass idR)
                   -> [IdP (GhcPass idL)]
-- ^ Collect 'Id' binders only, or 'Id's + pattern synonyms, respectively
collectHsIdBinders  = collect_hs_val_binders True

collectHsValBinders :: CollectPass (GhcPass idL)
                    => HsValBindsLR (GhcPass idL) (GhcPass idR)
                    -> [IdP (GhcPass idL)]
collectHsValBinders = collect_hs_val_binders False

collectHsBindBinders :: CollectPass p
                     => HsBindLR p idR
                     -> [IdP p]
-- ^ Collect both 'Id's and pattern-synonym binders
collectHsBindBinders b = collect_bind False b []

collectHsBindsBinders :: CollectPass p
                      => LHsBindsLR p idR
                      -> [IdP p]
collectHsBindsBinders binds = collect_binds False binds []

collectHsBindListBinders :: forall p idR. CollectPass p
                         => [LHsBindLR p idR]
                         -> [IdP p]
-- ^ Same as 'collectHsBindsBinders', but works over a list of bindings
collectHsBindListBinders = foldr (collect_bind False . unXRec @p) []

collect_hs_val_binders :: CollectPass (GhcPass idL)
                       => Bool
                       -> HsValBindsLR (GhcPass idL) (GhcPass idR)
                       -> [IdP (GhcPass idL)]
collect_hs_val_binders ps (ValBinds _ binds _) = collect_binds ps binds []
collect_hs_val_binders ps (XValBindsLR (NValBinds binds _))
  = collect_out_binds ps binds

collect_out_binds :: forall p. CollectPass p
                  => Bool
                  -> [(RecFlag, LHsBinds p)]
                  -> [IdP p]
collect_out_binds ps = foldr (collect_binds ps . snd) []

collect_binds :: forall p idR. CollectPass p
              => Bool
              -> LHsBindsLR p idR
              -> [IdP p]
              -> [IdP p]
-- ^ Collect 'Id's, or 'Id's + pattern synonyms, depending on boolean flag
collect_binds ps binds acc = foldr (collect_bind ps . unXRec @p) acc binds

collect_bind :: forall p idR. CollectPass p
             => Bool
             -> HsBindLR p idR
             -> [IdP p]
             -> [IdP p]
collect_bind _ (PatBind { pat_lhs = p })           acc = collect_lpat p acc
collect_bind _ (FunBind { fun_id = f })            acc = unXRec @p f : acc
collect_bind _ (VarBind { var_id = f })            acc = f : acc
collect_bind _ (AbsBinds { abs_exports = dbinds }) acc = map abe_poly dbinds ++ acc
        -- I don't think we want the binders from the abe_binds

        -- binding (hence see AbsBinds) is in zonking in GHC.Tc.Utils.Zonk
collect_bind omitPatSyn (PatSynBind _ (PSB { psb_id = ps })) acc
  | omitPatSyn                  = acc
  | otherwise                   = unXRec @p ps : acc
collect_bind _ (PatSynBind _ (XPatSynBind _)) acc = acc
collect_bind _ (XHsBindsLR _) acc = acc

collectMethodBinders :: forall idL idR. UnXRec idL => LHsBindsLR idL idR -> [LIdP idL]
-- ^ Used exclusively for the bindings of an instance decl which are all
-- 'FunBinds'
collectMethodBinders binds = foldr (get . unXRec @idL) [] binds
  where
    get (FunBind { fun_id = f }) fs = f : fs
    get _                        fs = fs
       -- Someone else complains about non-FunBinds

----------------- Statements --------------------------
collectLStmtsBinders :: (CollectPass (GhcPass idL))
                     => [LStmtLR (GhcPass idL) (GhcPass idR) body]
                     -> [IdP (GhcPass idL)]
collectLStmtsBinders = concatMap collectLStmtBinders

collectStmtsBinders :: (CollectPass (GhcPass idL))
                    => [StmtLR (GhcPass idL) (GhcPass idR) body]
                    -> [IdP (GhcPass idL)]
collectStmtsBinders = concatMap collectStmtBinders

collectLStmtBinders :: (CollectPass (GhcPass idL))
                    => LStmtLR (GhcPass idL) (GhcPass idR) body
                    -> [IdP (GhcPass idL)]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: (CollectPass (GhcPass idL))
                   => StmtLR (GhcPass idL) (GhcPass idR) body
                   -> [IdP (GhcPass idL)]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt _ pat _)      = collectPatBinders pat
collectStmtBinders (LetStmt _  binds)      = collectLocalBinders (unLoc binds)
collectStmtBinders (BodyStmt {})           = []
collectStmtBinders (LastStmt {})           = []
collectStmtBinders (ParStmt _ xs _ _)      = collectLStmtsBinders
                                    $ [s | ParStmtBlock _ ss _ _ <- xs, s <- ss]
collectStmtBinders (TransStmt { trS_stmts = stmts }) = collectLStmtsBinders stmts
collectStmtBinders (RecStmt { recS_stmts = ss })     = collectLStmtsBinders ss
collectStmtBinders (ApplicativeStmt _ args _) = concatMap collectArgBinders args
 where
  collectArgBinders (_, ApplicativeArgOne { app_arg_pattern = pat }) = collectPatBinders pat
  collectArgBinders (_, ApplicativeArgMany { bv_pattern = pat }) = collectPatBinders pat
  collectArgBinders (_, XApplicativeArg {}) = []


----------------- Patterns --------------------------
collectPatBinders :: CollectPass p => LPat p -> [IdP p]
collectPatBinders pat = collect_lpat pat []

collectPatsBinders :: CollectPass p => [LPat p] -> [IdP p]
collectPatsBinders pats = foldr collect_lpat [] pats

-------------
collect_lpat :: forall pass. (CollectPass pass)
             => LPat pass -> [IdP pass] -> [IdP pass]
collect_lpat p bndrs = collect_pat (unXRec @pass p) bndrs

collect_pat :: forall p. CollectPass p
            => Pat p
            -> [IdP p]
            -> [IdP p]
collect_pat pat bndrs = case pat of
  (VarPat _ var)          -> unXRec @p var : bndrs
  (WildPat _)             -> bndrs
  (LazyPat _ pat)         -> collect_lpat pat bndrs
  (BangPat _ pat)         -> collect_lpat pat bndrs
  (AsPat _ a pat)         -> unXRec @p a : collect_lpat pat bndrs
  (ViewPat _ _ pat)       -> collect_lpat pat bndrs
  (ParPat _ pat)          -> collect_lpat pat bndrs
  (ListPat _ pats)        -> foldr collect_lpat bndrs pats
  (TuplePat _ pats _)     -> foldr collect_lpat bndrs pats
  (SumPat _ pat _ _)      -> collect_lpat pat bndrs
  (ConPat {pat_args=ps})  -> foldr collect_lpat bndrs (hsConPatArgs ps)
  -- See Note [Dictionary binders in ConPatOut]
  (LitPat _ _)            -> bndrs
  (NPat {})               -> bndrs
  (NPlusKPat _ n _ _ _ _) -> unXRec @p n : bndrs
  (SigPat _ pat _)        -> collect_lpat pat bndrs
  (SplicePat _ (HsSpliced _ _ (HsSplicedPat pat)))
                          -> collect_pat pat bndrs
  (SplicePat _ _)         -> bndrs
  (XPat ext)              -> collectXXPat (Proxy @p) ext bndrs

-- | This class specifies how to collect variable identifiers from extension patterns in the given pass.
-- Consumers of the GHC API that define their own passes should feel free to implement instances in order
-- to make use of functions which depend on it.
--
-- In particular, Haddock already makes use of this, with an instance for its 'DocNameI' pass so that
-- it can reuse the code in GHC for collecting binders.
class UnXRec p => CollectPass p where
  collectXXPat :: Proxy p -> XXPat p -> [IdP p] -> [IdP p]

instance IsPass p => CollectPass (GhcPass p) where
  collectXXPat _ ext =
    case ghcPass @p of
      GhcTc -> let CoPat _ pat _ = ext in collect_pat pat
      GhcRn -> noExtCon ext
      GhcPs -> noExtCon ext

{-
Note [Dictionary binders in ConPatOut] See also same Note in GHC.HsToCore.Arrows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do *not* gather (a) dictionary and (b) dictionary bindings as binders
of a ConPatOut pattern.  For most calls it doesn't matter, because
it's pre-typechecker and there are no ConPatOuts.  But it does matter
more in the desugarer; for example, GHC.HsToCore.Utils.mkSelectorBinds uses
collectPatBinders.  In a lazy pattern, for example f ~(C x y) = ...,
we want to generate bindings for x,y but not for dictionaries bound by
C.  (The type checker ensures they would not be used.)

Desugaring of arrow case expressions needs these bindings (see GHC.HsToCore.Arrows
and arrowcase1), but SPJ (Jan 2007) says it's safer for it to use its
own pat-binder-collector:

Here's the problem.  Consider

data T a where
   C :: Num a => a -> Int -> T a

f ~(C (n+1) m) = (n,m)

Here, the pattern (C (n+1)) binds a hidden dictionary (d::Num a),
and *also* uses that dictionary to match the (n+1) pattern.  Yet, the
variables bound by the lazy pattern are n,m, *not* the dictionary d.
So in mkSelectorBinds in GHC.HsToCore.Utils, we want just m,n as the variables bound.
-}

hsGroupBinders :: HsGroup GhcRn -> [Name]
hsGroupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
                          hs_fords = foreign_decls })
  =  collectHsValBinders val_decls
  ++ hsTyClForeignBinders tycl_decls foreign_decls

hsTyClForeignBinders :: [TyClGroup GhcRn]
                     -> [LForeignDecl GhcRn]
                     -> [Name]
-- We need to look at instance declarations too,
-- because their associated types may bind data constructors
hsTyClForeignBinders tycl_decls foreign_decls
  =    map unLoc (hsForeignDeclsBinders foreign_decls)
    ++ getSelectorNames
         (foldMap (foldMap hsLTyClDeclBinders . group_tyclds) tycl_decls
         `mappend`
         foldMap (foldMap hsLInstDeclBinders . group_instds) tycl_decls)
  where
    getSelectorNames :: ([Located Name], [LFieldOcc GhcRn]) -> [Name]
    getSelectorNames (ns, fs) = map unLoc ns ++ map (extFieldOcc . unLoc) fs

-------------------
hsLTyClDeclBinders :: IsPass p
                   => Located (TyClDecl (GhcPass p))
                   -> ([Located (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
-- ^ Returns all the /binding/ names of the decl.  The first one is
-- guaranteed to be the name of the decl. The first component
-- represents all binding names except record fields; the second
-- represents field occurrences. For record fields mentioned in
-- multiple constructors, the SrcLoc will be from the first occurrence.
--
-- Each returned (Located name) has a SrcSpan for the /whole/ declaration.
-- See Note [SrcSpan for binders]

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl
                                            { fdLName = (L _ name) } }))
  = ([L loc name], [])
hsLTyClDeclBinders (L loc (SynDecl
                               { tcdLName = (L _ name) }))
  = ([L loc name], [])
hsLTyClDeclBinders (L loc (ClassDecl
                               { tcdLName = (L _ cls_name)
                               , tcdSigs  = sigs
                               , tcdATs   = ats }))
  = (L loc cls_name :
     [ L fam_loc fam_name | (L fam_loc (FamilyDecl
                                        { fdLName = L _ fam_name })) <- ats ]
     ++
     [ L mem_loc mem_name | (L mem_loc (ClassOpSig _ False ns _)) <- sigs
                          , (L _ mem_name) <- ns ]
    , [])
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = (L _ name)
                                       , tcdDataDefn = defn }))
  = (\ (xs, ys) -> (L loc name : xs, ys)) $ hsDataDefnBinders defn


-------------------
hsForeignDeclsBinders :: forall pass. (UnXRec pass, MapXRec pass) => [LForeignDecl pass] -> [LIdP pass]
-- ^ See Note [SrcSpan for binders]
hsForeignDeclsBinders foreign_decls
  = [ mapXRec @pass (const $ unXRec @pass n) fi
    | fi@(unXRec @pass -> ForeignImport { fd_name = n })
        <- foreign_decls]


-------------------
hsPatSynSelectors :: IsPass p => HsValBinds (GhcPass p) -> [IdP (GhcPass p)]
-- ^ Collects record pattern-synonym selectors only; the pattern synonym
-- names are collected by 'collectHsValBinders'.
hsPatSynSelectors (ValBinds _ _ _) = panic "hsPatSynSelectors"
hsPatSynSelectors (XValBindsLR (NValBinds binds _))
  = foldr addPatSynSelector [] . unionManyBags $ map snd binds

addPatSynSelector :: forall p. UnXRec p => LHsBind p -> [IdP p] -> [IdP p]
addPatSynSelector bind sels
  | PatSynBind _ (PSB { psb_args = RecCon as }) <- unXRec @p bind
  = map (unXRec @p . recordPatSynSelectorId) as ++ sels
  | otherwise = sels

getPatSynBinds :: forall id. UnXRec id
               => [(RecFlag, LHsBinds id)] -> [PatSynBind id id]
getPatSynBinds binds
  = [ psb | (_, lbinds) <- binds
          , (unXRec @id -> (PatSynBind _ psb)) <- bagToList lbinds ]

-------------------
hsLInstDeclBinders :: IsPass p
                   => LInstDecl (GhcPass p)
                   -> ([Located (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
hsLInstDeclBinders (L _ (ClsInstD
                             { cid_inst = ClsInstDecl
                                          { cid_datafam_insts = dfis }}))
  = foldMap (hsDataFamInstBinders . unLoc) dfis
hsLInstDeclBinders (L _ (DataFamInstD { dfid_inst = fi }))
  = hsDataFamInstBinders fi
hsLInstDeclBinders (L _ (TyFamInstD {})) = mempty

-------------------
-- | the 'SrcLoc' returned are for the whole declarations, not just the names
hsDataFamInstBinders :: IsPass p
                     => DataFamInstDecl (GhcPass p)
                     -> ([Located (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
hsDataFamInstBinders (DataFamInstDecl { dfid_eqn = FamEqn { feqn_rhs = defn }})
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- | the 'SrcLoc' returned are for the whole declarations, not just the names
hsDataDefnBinders :: IsPass p
                  => HsDataDefn (GhcPass p)
                  -> ([Located (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
hsDataDefnBinders (HsDataDefn { dd_cons = cons })
  = hsConDeclsBinders cons
  -- See Note [Binders in family instances]

-------------------
type Seen p = [LFieldOcc (GhcPass p)] -> [LFieldOcc (GhcPass p)]
                 -- Filters out ones that have already been seen

hsConDeclsBinders :: forall p. IsPass p
                  => [LConDecl (GhcPass p)]
                  -> ([Located (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
   -- See hsLTyClDeclBinders for what this does
   -- The function is boringly complicated because of the records
   -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons
  = go id cons
  where
    go :: Seen p -> [LConDecl (GhcPass p)]
       -> ([Located (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
    go _ [] = ([], [])
    go remSeen (r:rs)
      -- Don't re-mangle the location of field names, because we don't
      -- have a record of the full location of the field declaration anyway
      = let loc = getLoc r
        in case unLoc r of
           -- remove only the first occurrence of any seen field in order to
           -- avoid circumventing detection of duplicate fields (#9156)
           ConDeclGADT { con_names = names, con_g_args = args }
             -> (map (L loc . unLoc) names ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds_gadt remSeen args
                (ns, fs) = go remSeen' rs

           ConDeclH98 { con_name = name, con_args = args }
             -> ([L loc (unLoc name)] ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds_h98 remSeen args
                (ns, fs) = go remSeen' rs

    get_flds_h98 :: Seen p -> HsConDeclH98Details (GhcPass p)
                 -> (Seen p, [LFieldOcc (GhcPass p)])
    get_flds_h98 remSeen (RecCon flds) = get_flds remSeen flds
    get_flds_h98 remSeen _ = (remSeen, [])

    get_flds_gadt :: Seen p -> HsConDeclGADTDetails (GhcPass p)
                  -> (Seen p, [LFieldOcc (GhcPass p)])
    get_flds_gadt remSeen (RecConGADT flds) = get_flds remSeen flds
    get_flds_gadt remSeen _ = (remSeen, [])

    get_flds :: Seen p -> Located [LConDeclField (GhcPass p)]
             -> (Seen p, [LFieldOcc (GhcPass p)])
    get_flds remSeen flds = (remSeen', fld_names)
       where
          fld_names = remSeen (concatMap (cd_fld_names . unLoc) (unLoc flds))
          remSeen' = foldr (.) remSeen
                               [deleteBy ((==) `on` unLoc . rdrNameFieldOcc . unLoc) v
                               | v <- fld_names]

{-

Note [SrcSpan for binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When extracting the (Located RdrNme) for a binder, at least for the
main name (the TyCon of a type declaration etc), we want to give it
the @SrcSpan@ of the whole /declaration/, not just the name itself
(which is how it appears in the syntax tree).  This SrcSpan (for the
entire declaration) is used as the SrcSpan for the Name that is
finally produced, and hence for error messages.  (See #8607.)

Note [Binders in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type or data family instance declaration, the type
constructor is an *occurrence* not a binding site
    type instance T Int = Int -> Int   -- No binders
    data instance S Bool = S1 | S2     -- Binders are S1,S2


************************************************************************
*                                                                      *
        Collecting binders the user did not write
*                                                                      *
************************************************************************

The job of this family of functions is to run through binding sites and find the set of all Names
that were defined "implicitly", without being explicitly written by the user.

The main purpose is to find names introduced by record wildcards so that we can avoid
warning the user when they don't use those names (#4404)

Since the addition of -Wunused-record-wildcards, this function returns a pair
of [(SrcSpan, [Name])]. Each element of the list is one set of implicit
binders, the first component of the tuple is the document describes the possible
fix to the problem (by removing the ..).

This means there is some unfortunate coupling between this function and where it
is used but it's only used for one specific purpose in one place so it seemed
easier.
-}

lStmtsImplicits :: [LStmtLR GhcRn (GhcPass idR) (Located (body (GhcPass idR)))]
                -> [(SrcSpan, [Name])]
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: [LStmtLR GhcRn (GhcPass idR) (Located (body (GhcPass idR)))]
              -> [(SrcSpan, [Name])]
    hs_lstmts = concatMap (hs_stmt . unLoc)

    hs_stmt :: StmtLR GhcRn (GhcPass idR) (Located (body (GhcPass idR)))
            -> [(SrcSpan, [Name])]
    hs_stmt (BindStmt _ pat _) = lPatImplicits pat
    hs_stmt (ApplicativeStmt _ args _) = concatMap do_arg args
      where do_arg (_, ApplicativeArgOne { app_arg_pattern = pat }) = lPatImplicits pat
            do_arg (_, ApplicativeArgMany { app_stmts = stmts })    = hs_lstmts stmts
    hs_stmt (LetStmt _ binds)     = hs_local_binds (unLoc binds)
    hs_stmt (BodyStmt {})         = []
    hs_stmt (LastStmt {})         = []
    hs_stmt (ParStmt _ xs _ _)    = hs_lstmts [s | ParStmtBlock _ ss _ _ <- xs
                                                , s <- ss]
    hs_stmt (TransStmt { trS_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (RecStmt { recS_stmts = ss })     = hs_lstmts ss

    hs_local_binds (HsValBinds _ val_binds) = hsValBindsImplicits val_binds
    hs_local_binds (HsIPBinds {})           = []
    hs_local_binds (EmptyLocalBinds _)      = []

hsValBindsImplicits :: HsValBindsLR GhcRn (GhcPass idR) -> [(SrcSpan, [Name])]
hsValBindsImplicits (XValBindsLR (NValBinds binds _))
  = concatMap (lhsBindsImplicits . snd) binds
hsValBindsImplicits (ValBinds _ binds _)
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR GhcRn idR -> [(SrcSpan, [Name])]
lhsBindsImplicits = foldBag (++) (lhs_bind . unLoc) []
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = []

lPatImplicits :: LPat GhcRn -> [(SrcSpan, [Name])]
lPatImplicits = hs_lpat
  where
    hs_lpat lpat = hs_pat (unLoc lpat)

    hs_lpats = foldr (\pat rest -> hs_lpat pat ++ rest) []

    hs_pat (LazyPat _ pat)      = hs_lpat pat
    hs_pat (BangPat _ pat)      = hs_lpat pat
    hs_pat (AsPat _ _ pat)      = hs_lpat pat
    hs_pat (ViewPat _ _ pat)    = hs_lpat pat
    hs_pat (ParPat _ pat)       = hs_lpat pat
    hs_pat (ListPat _ pats)     = hs_lpats pats
    hs_pat (TuplePat _ pats _)  = hs_lpats pats

    hs_pat (SigPat _ pat _)     = hs_lpat pat

    hs_pat (ConPat {pat_con=con, pat_args=ps}) = details con ps

    hs_pat _ = []

    details :: Located Name -> HsConPatDetails GhcRn -> [(SrcSpan, [Name])]
    details _ (PrefixCon _ ps) = hs_lpats ps
    details n (RecCon fs)      =
      [(err_loc, collectPatsBinders implicit_pats) | Just{} <- [rec_dotdot fs] ]
        ++ hs_lpats explicit_pats

      where implicit_pats = map (hsRecFieldArg . unLoc) implicit
            explicit_pats = map (hsRecFieldArg . unLoc) explicit


            (explicit, implicit) = partitionEithers [if pat_explicit then Left fld else Right fld
                                                    | (i, fld) <- [0..] `zip` rec_flds fs
                                                    ,  let  pat_explicit =
                                                              maybe True ((i<) . unLoc)
                                                                         (rec_dotdot fs)]
            err_loc = maybe (getLoc n) getLoc (rec_dotdot fs)

    details _ (InfixCon p1 p2) = hs_lpat p1 ++ hs_lpat p2
