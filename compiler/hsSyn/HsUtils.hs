{-
(c) The University of Glasgow, 1992-2006


Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by          Module
   ----------------          -------------
   GhcPs/RdrName             parser/RdrHsSyn
   GhcRn/Name                rename/RnHsSyn
   GhcTc/Id                  typecheck/TcHsSyn
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module HsUtils(
  -- Terms
  mkHsPar, mkHsApp, mkHsAppType, mkHsAppTypes, mkHsCaseAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkMatch, mkPrefixFunRhs, mkHsLam, mkHsIf,
  mkHsWrap, mkLHsWrap, mkHsWrapCo, mkHsWrapCoR, mkLHsWrapCo,
  mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsComp, mkHsWrapPat, mkHsWrapPatCo,
  mkLHsPar, mkHsCmdWrap, mkLHsCmdWrap,

  nlHsTyApp, nlHsTyApps, nlHsVar, nlHsDataCon,
  nlHsLit, nlHsApp, nlHsApps, nlHsSyntaxApps,
  nlHsIntLit, nlHsVarApps,
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,
  typeToLHsType,

  -- * Constructing general big tuples
  -- $big_tuples
  mkChunkified, chunkify,

  -- Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mk_easy_FunBind, mkTopFunBind,
  mkPatSynBind,
  isInfixFunBind,

  -- Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString, mkHsStringPrimLit,

  -- Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConVarPatName, nlConPat,
  nlConPatName, nlInfixConPat, nlNullaryConPat, nlWildConPat, nlWildPat,
  nlWildPatName, nlTuplePat, mkParPat, nlParPat,
  mkBigLHsVarTup, mkBigLHsTup, mkBigLHsVarPatTup, mkBigLHsPatTup,

  -- Types
  mkHsAppTy, mkHsAppKindTy, userHsTyVarBndrs, userHsLTyVarBndrs,
  mkLHsSigType, mkLHsSigWcType, mkClassOpSigs, mkHsSigEnv,
  nlHsAppTy, nlHsAppKindTy, nlHsTyVar, nlHsFunTy, nlHsParTy, nlHsTyConApp,

  -- Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt, mkBindStmt, mkTcBindStmt,
  mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt,
  emptyRecStmt, emptyRecStmtName, emptyRecStmtId, mkRecStmt,
  unitRecStmtTc,

  -- Template Haskell
  mkHsSpliceTy, mkHsSpliceE, mkHsSpliceTE, mkUntypedSplice,
  mkHsQuasiQuote, unqualQuasiQuote,

  -- Collecting binders
  isUnliftedHsBind, isBangedHsBind,

  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsIdBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,
  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,

  hsLTyClDeclBinders, hsTyClForeignBinders,
  hsPatSynSelectors, getPatSynBinds,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,

  -- Collecting implicit binders
  lStmtsImplicits, hsValBindsImplicits, lPatImplicits
  ) where

#include "HsVersions.h"

import GhcPrelude

import HsDecls
import HsBinds
import HsExpr
import HsPat
import HsTypes
import HsLit
import PlaceHolder
import HsExtension

import TcEvidence
import RdrName
import Var
import TyCoRep
import Type   ( tyConArgFlags )
import TysWiredIn ( unitTy )
import TcType
import DataCon
import ConLike
import Id
import Name
import NameSet hiding ( unitFV )
import NameEnv
import BasicTypes
import SrcLoc
import FastString
import Util
import Bag
import Outputable
import Constants

import Data.Either
import Data.Function
import Data.List

{-
************************************************************************
*                                                                      *
        Some useful helpers for constructing syntax
*                                                                      *
************************************************************************

These functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the nl* functions below which
just attach noSrcSpan to everything.
-}

mkHsPar :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkHsPar e = cL (getLoc e) (HsPar noExt e)

mkSimpleMatch :: HsMatchContext (NameOrRdrName (IdP (GhcPass p)))
              -> [LPat (GhcPass p)] -> Located (body (GhcPass p))
              -> LMatch (GhcPass p) (Located (body (GhcPass p)))
mkSimpleMatch ctxt pats rhs
  = cL loc $
    Match { m_ext = noExt, m_ctxt = ctxt, m_pats = pats
          , m_grhss = unguardedGRHSs rhs }
  where
    loc = case pats of
                []      -> getLoc rhs
                (pat:_) -> combineSrcSpans (getLoc pat) (getLoc rhs)

unguardedGRHSs :: Located (body (GhcPass p))
               -> GRHSs (GhcPass p) (Located (body (GhcPass p)))
unguardedGRHSs rhs@(dL->L loc _)
  = GRHSs noExt (unguardedRHS loc rhs) (noLoc emptyLocalBinds)

unguardedRHS :: SrcSpan -> Located (body (GhcPass p))
             -> [LGRHS (GhcPass p) (Located (body (GhcPass p)))]
unguardedRHS loc rhs = [cL loc (GRHS noExt [] rhs)]

mkMatchGroup :: (XMG name (Located (body name)) ~ NoExt)
             => Origin -> [LMatch name (Located (body name))]
             -> MatchGroup name (Located (body name))
mkMatchGroup origin matches = MG { mg_ext = noExt
                                 , mg_alts = mkLocatedList matches
                                 , mg_origin = origin }

mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = cL (combineLocs (head ms) (last ms)) ms

mkHsApp :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkHsApp e1 e2 = addCLoc e1 e2 (HsApp noExt e1 e2)

mkHsAppType :: (NoGhcTc (GhcPass id) ~ GhcRn)
            => LHsExpr (GhcPass id) -> LHsWcType GhcRn -> LHsExpr (GhcPass id)
mkHsAppType e t = addCLoc e t_body (HsAppType noExt e paren_wct)
  where
    t_body    = hswc_body t
    paren_wct = t { hswc_body = parenthesizeHsType appPrec t_body }

mkHsAppTypes :: LHsExpr GhcRn -> [LHsWcType GhcRn] -> LHsExpr GhcRn
mkHsAppTypes = foldl' mkHsAppType

mkHsLam :: (XMG (GhcPass p) (LHsExpr (GhcPass p)) ~ NoExt) =>
  [LPat (GhcPass p)] -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
mkHsLam pats body = mkHsPar (cL (getLoc body) (HsLam noExt matches))
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

nlHsTyApp :: IdP (GhcPass id) -> [Type] -> LHsExpr (GhcPass id)
nlHsTyApp fun_id tys
  = noLoc (mkHsWrap (mkWpTyApps tys) (HsVar noExt (noLoc fun_id)))

nlHsTyApps :: IdP (GhcPass id) -> [Type] -> [LHsExpr (GhcPass id)]
           -> LHsExpr (GhcPass id)
nlHsTyApps fun_id tys xs = foldl' nlHsApp (nlHsTyApp fun_id tys) xs

--------- Adding parens ---------
mkLHsPar :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
-- Wrap in parens if (hsExprNeedsParens appPrec) says it needs them
-- So   'f x'  becomes '(f x)', but '3' stays as '3'
mkLHsPar le@(dL->L loc e)
  | hsExprNeedsParens appPrec e = cL loc (HsPar noExt le)
  | otherwise                   = le

mkParPat :: LPat (GhcPass name) -> LPat (GhcPass name)
mkParPat lp@(dL->L loc p)
  | patNeedsParens appPrec p = cL loc (ParPat noExt lp)
  | otherwise                = lp

nlParPat :: LPat (GhcPass name) -> LPat (GhcPass name)
nlParPat p = noLoc (ParPat noExt p)

-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: IntegralLit -> HsOverLit GhcPs
mkHsFractional :: FractionalLit -> HsOverLit GhcPs
mkHsIsString   :: SourceText -> FastString -> HsOverLit GhcPs
mkHsDo         :: HsStmtContext Name -> [ExprLStmt GhcPs] -> HsExpr GhcPs
mkHsComp       :: HsStmtContext Name -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
               -> HsExpr GhcPs

mkNPat      :: Located (HsOverLit GhcPs) -> Maybe (SyntaxExpr GhcPs)
            -> Pat GhcPs
mkNPlusKPat :: Located RdrName -> Located (HsOverLit GhcPs) -> Pat GhcPs

mkLastStmt :: Located (bodyR (GhcPass idR))
           -> StmtLR (GhcPass idL) (GhcPass idR) (Located (bodyR (GhcPass idR)))
mkBodyStmt :: Located (bodyR GhcPs)
           -> StmtLR (GhcPass idL) GhcPs (Located (bodyR GhcPs))
mkBindStmt :: (XBindStmt (GhcPass idL) (GhcPass idR)
                         (Located (bodyR (GhcPass idR))) ~ NoExt)
           => LPat (GhcPass idL) -> Located (bodyR (GhcPass idR))
           -> StmtLR (GhcPass idL) (GhcPass idR) (Located (bodyR (GhcPass idR)))
mkTcBindStmt :: LPat GhcTc -> Located (bodyR GhcTc)
             -> StmtLR GhcTc GhcTc (Located (bodyR GhcTc))

emptyRecStmt     :: StmtLR (GhcPass idL) GhcPs bodyR
emptyRecStmtName :: StmtLR GhcRn GhcRn bodyR
emptyRecStmtId   :: StmtLR GhcTc GhcTc bodyR
mkRecStmt        :: [LStmtLR (GhcPass idL) GhcPs bodyR]
                 -> StmtLR (GhcPass idL) GhcPs bodyR


mkHsIntegral     i  = OverLit noExt (HsIntegral       i) noExpr
mkHsFractional   f  = OverLit noExt (HsFractional     f) noExpr
mkHsIsString src s  = OverLit noExt (HsIsString   src s) noExpr

mkHsDo ctxt stmts = HsDo noExt ctxt (mkLocatedList stmts)
mkHsComp ctxt stmts expr = mkHsDo ctxt (stmts ++ [last_stmt])
  where
    last_stmt = cL (getLoc expr) $ mkLastStmt expr

mkHsIf :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
       -> HsExpr (GhcPass p)
mkHsIf c a b = HsIf noExt (Just noSyntaxExpr) c a b

mkNPat lit neg     = NPat noExt lit neg noSyntaxExpr
mkNPlusKPat id lit
  = NPlusKPat noExt id lit (unLoc lit) noSyntaxExpr noSyntaxExpr

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
emptyTransStmt = TransStmt { trS_ext = noExt
                           , trS_form = panic "emptyTransStmt: form"
                           , trS_stmts = [], trS_bndrs = []
                           , trS_by = Nothing, trS_using = noLoc noExpr
                           , trS_ret = noSyntaxExpr, trS_bind = noSyntaxExpr
                           , trS_fmap = noExpr }
mkTransformStmt    ss u   = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  ss u b = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   ss u   = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt ss b u = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body = LastStmt noExt body False noSyntaxExpr
mkBodyStmt body
  = BodyStmt noExt body noSyntaxExpr noSyntaxExpr
mkBindStmt pat body
  = BindStmt noExt pat body noSyntaxExpr noSyntaxExpr
mkTcBindStmt pat body = BindStmt unitTy pat body noSyntaxExpr noSyntaxExpr
  -- don't use placeHolderTypeTc above, because that panics during zonking

emptyRecStmt' :: forall idL idR body.
                 XRecStmt (GhcPass idL) (GhcPass idR) body
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

emptyRecStmt     = emptyRecStmt' noExt
emptyRecStmtName = emptyRecStmt' noExt
emptyRecStmtId   = emptyRecStmt' unitRecStmtTc
                                        -- a panic might trigger during zonking
mkRecStmt stmts  = emptyRecStmt { recS_stmts = stmts }

-------------------------------
--- A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr GhcPs -> IdP GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsOpApp e1 op e2 = OpApp noExt e1 (noLoc (HsVar noExt (noLoc op))) e2

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))

mkUntypedSplice :: SpliceDecoration -> LHsExpr GhcPs -> HsSplice GhcPs
mkUntypedSplice hasParen e = HsUntypedSplice noExt hasParen unqualSplice e

mkHsSpliceE :: SpliceDecoration -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsSpliceE hasParen e = HsSpliceE noExt (mkUntypedSplice hasParen e)

mkHsSpliceTE :: SpliceDecoration -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsSpliceTE hasParen e
  = HsSpliceE noExt (HsTypedSplice noExt hasParen unqualSplice e)

mkHsSpliceTy :: SpliceDecoration -> LHsExpr GhcPs -> HsType GhcPs
mkHsSpliceTy hasParen e = HsSpliceTy noExt
                      (HsUntypedSplice noExt hasParen unqualSplice e)

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsSplice GhcPs
mkHsQuasiQuote quoter span quote
  = HsQuasiQuote noExt unqualSplice quoter span quote

unqualQuasiQuote :: RdrName
unqualQuasiQuote = mkRdrUnqual (mkVarOccFS (fsLit "quasiquote"))
                -- A name (uniquified later) to
                -- identify the quasi-quote

mkHsString :: String -> HsLit (GhcPass p)
mkHsString s = HsString NoSourceText (mkFastString s)

mkHsStringPrimLit :: FastString -> HsLit (GhcPass p)
mkHsStringPrimLit fs
  = HsStringPrim NoSourceText (fastStringToByteString fs)

-------------
userHsLTyVarBndrs :: SrcSpan -> [Located (IdP (GhcPass p))]
                  -> [LHsTyVarBndr (GhcPass p)]
-- Caller sets location
userHsLTyVarBndrs loc bndrs = [ cL loc (UserTyVar noExt v) | v <- bndrs ]

userHsTyVarBndrs :: SrcSpan -> [IdP (GhcPass p)] -> [LHsTyVarBndr (GhcPass p)]
-- Caller sets location
userHsTyVarBndrs loc bndrs = [ cL loc (UserTyVar noExt (cL loc v))
                             | v <- bndrs ]


{-
************************************************************************
*                                                                      *
        Constructing syntax with no location info
*                                                                      *
************************************************************************
-}

nlHsVar :: IdP (GhcPass id) -> LHsExpr (GhcPass id)
nlHsVar n = noLoc (HsVar noExt (noLoc n))

-- NB: Only for LHsExpr **Id**
nlHsDataCon :: DataCon -> LHsExpr GhcTc
nlHsDataCon con = noLoc (HsConLikeOut noExt (RealDataCon con))

nlHsLit :: HsLit (GhcPass p) -> LHsExpr (GhcPass p)
nlHsLit n = noLoc (HsLit noExt n)

nlHsIntLit :: Integer -> LHsExpr (GhcPass p)
nlHsIntLit n = noLoc (HsLit noExt (HsInt noExt (mkIntegralLit n)))

nlVarPat :: IdP (GhcPass id) -> LPat (GhcPass id)
nlVarPat n = noLoc (VarPat noExt (noLoc n))

nlLitPat :: HsLit GhcPs -> LPat GhcPs
nlLitPat l = noLoc (LitPat noExt l)

nlHsApp :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
nlHsApp f x = noLoc (HsApp noExt f (mkLHsPar x))

nlHsSyntaxApps :: SyntaxExpr (GhcPass id) -> [LHsExpr (GhcPass id)]
               -> LHsExpr (GhcPass id)
nlHsSyntaxApps (SyntaxExpr { syn_expr      = fun
                           , syn_arg_wraps = arg_wraps
                           , syn_res_wrap  = res_wrap }) args
  | [] <- arg_wraps   -- in the noSyntaxExpr case
  = ASSERT( isIdHsWrapper res_wrap )
    foldl' nlHsApp (noLoc fun) args

  | otherwise
  = mkLHsWrap res_wrap (foldl' nlHsApp (noLoc fun) (zipWithEqual "nlHsSyntaxApps"
                                                     mkLHsWrap arg_wraps args))

nlHsApps :: IdP (GhcPass id) -> [LHsExpr (GhcPass id)] -> LHsExpr (GhcPass id)
nlHsApps f xs = foldl' nlHsApp (nlHsVar f) xs

nlHsVarApps :: IdP (GhcPass id) -> [IdP (GhcPass id)] -> LHsExpr (GhcPass id)
nlHsVarApps f xs = noLoc (foldl' mk (HsVar noExt (noLoc f))
                                               (map ((HsVar noExt) . noLoc) xs))
                 where
                   mk f a = HsApp noExt (noLoc f) (noLoc a)

nlConVarPat :: RdrName -> [RdrName] -> LPat GhcPs
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlConVarPatName :: Name -> [Name] -> LPat GhcRn
nlConVarPatName con vars = nlConPatName con (map nlVarPat vars)

nlInfixConPat :: RdrName -> LPat GhcPs -> LPat GhcPs -> LPat GhcPs
nlInfixConPat con l r = noLoc (ConPatIn (noLoc con)
                              (InfixCon (parenthesizePat opPrec l)
                                        (parenthesizePat opPrec r)))

nlConPat :: RdrName -> [LPat GhcPs] -> LPat GhcPs
nlConPat con pats =
  noLoc (ConPatIn (noLoc con) (PrefixCon (map (parenthesizePat appPrec) pats)))

nlConPatName :: Name -> [LPat GhcRn] -> LPat GhcRn
nlConPatName con pats =
  noLoc (ConPatIn (noLoc con) (PrefixCon (map (parenthesizePat appPrec) pats)))

nlNullaryConPat :: IdP (GhcPass p) -> LPat (GhcPass p)
nlNullaryConPat con = noLoc (ConPatIn (noLoc con) (PrefixCon []))

nlWildConPat :: DataCon -> LPat GhcPs
nlWildConPat con = noLoc (ConPatIn (noLoc (getRdrName con))
                         (PrefixCon (nOfThem (dataConSourceArity con)
                                             nlWildPat)))

nlWildPat :: LPat GhcPs
nlWildPat  = noLoc (WildPat noExt )  -- Pre-typechecking

nlWildPatName :: LPat GhcRn
nlWildPatName  = noLoc (WildPat noExt )  -- Pre-typechecking

nlHsDo :: HsStmtContext Name -> [LStmt GhcPs (LHsExpr GhcPs)]
       -> LHsExpr GhcPs
nlHsDo ctxt stmts = noLoc (mkHsDo ctxt stmts)

nlHsOpApp :: LHsExpr GhcPs -> IdP GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
nlHsOpApp e1 op e2 = noLoc (mkHsOpApp e1 op e2)

nlHsLam  :: LMatch GhcPs (LHsExpr GhcPs) -> LHsExpr GhcPs
nlHsPar  :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
nlHsIf   :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
         -> LHsExpr (GhcPass id)
nlHsCase :: LHsExpr GhcPs -> [LMatch GhcPs (LHsExpr GhcPs)]
         -> LHsExpr GhcPs
nlList   :: [LHsExpr GhcPs] -> LHsExpr GhcPs

nlHsLam match          = noLoc (HsLam noExt (mkMatchGroup Generated [match]))
nlHsPar e              = noLoc (HsPar noExt e)

-- Note [Rebindable nlHsIf]
-- nlHsIf should generate if-expressions which are NOT subject to
-- RebindableSyntax, so the first field of HsIf is Nothing. (#12080)
nlHsIf cond true false = noLoc (HsIf noExt Nothing cond true false)

nlHsCase expr matches
  = noLoc (HsCase noExt expr (mkMatchGroup Generated matches))
nlList exprs          = noLoc (ExplicitList noExt Nothing exprs)

nlHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsTyVar :: IdP (GhcPass p)                            -> LHsType (GhcPass p)
nlHsFunTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsParTy :: LHsType (GhcPass p)                        -> LHsType (GhcPass p)

nlHsAppTy f t = noLoc (HsAppTy noExt f (parenthesizeHsType appPrec t))
nlHsTyVar x   = noLoc (HsTyVar noExt NotPromoted (noLoc x))
nlHsFunTy a b = noLoc (HsFunTy noExt (parenthesizeHsType funPrec a) b)
nlHsParTy t   = noLoc (HsParTy noExt t)

nlHsTyConApp :: IdP (GhcPass p) -> [LHsType (GhcPass p)] -> LHsType (GhcPass p)
nlHsTyConApp tycon tys  = foldl' nlHsAppTy (nlHsTyVar tycon) tys

nlHsAppKindTy ::
  LHsType (GhcPass p) -> LHsKind (GhcPass p) -> LHsType (GhcPass p)
nlHsAppKindTy f k = noLoc (HsAppKindTy noSrcSpan f (parenthesizeHsType appPrec k))

{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

mkLHsTupleExpr :: [LHsExpr (GhcPass a)] -> LHsExpr (GhcPass a)
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] = e
mkLHsTupleExpr es
  = noLoc $ ExplicitTuple noExt (map (noLoc . (Present noExt)) es) Boxed

mkLHsVarTuple :: [IdP (GhcPass a)] -> LHsExpr (GhcPass a)
mkLHsVarTuple ids  = mkLHsTupleExpr (map nlHsVar ids)

nlTuplePat :: [LPat GhcPs] -> Boxity -> LPat GhcPs
nlTuplePat pats box = noLoc (TuplePat noExt pats box)

missingTupArg :: HsTupArg GhcPs
missingTupArg = Missing noExt

mkLHsPatTup :: [LPat GhcRn] -> LPat GhcRn
mkLHsPatTup []     = noLoc $ TuplePat noExt [] Boxed
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = cL (getLoc (head lpats)) $ TuplePat noExt lpats Boxed

-- The Big equivalents for the source tuple expressions
mkBigLHsVarTup :: [IdP (GhcPass id)] -> LHsExpr (GhcPass id)
mkBigLHsVarTup ids = mkBigLHsTup (map nlHsVar ids)

mkBigLHsTup :: [LHsExpr (GhcPass id)] -> LHsExpr (GhcPass id)
mkBigLHsTup = mkChunkified mkLHsTupleExpr

-- The Big equivalents for the source tuple patterns
mkBigLHsVarPatTup :: [IdP GhcRn] -> LPat GhcRn
mkBigLHsVarPatTup bs = mkBigLHsPatTup (map nlVarPat bs)

mkBigLHsPatTup :: [LPat GhcRn] -> LPat GhcRn
mkBigLHsPatTup = mkChunkified mkLHsPatTup

-- $big_tuples
-- #big_tuples#
--
-- GHCs built in tuples can only go up to 'mAX_TUPLE_SIZE' in arity, but
-- we might concievably want to build such a massive tuple as part of the
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

-- | Lifts a \"small\" constructor into a \"big\" constructor by recursive decompositon
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

mkLHsSigType :: LHsType GhcPs -> LHsSigType GhcPs
mkLHsSigType ty = mkHsImplicitBndrs ty

mkLHsSigWcType :: LHsType GhcPs -> LHsSigWcType GhcPs
mkLHsSigWcType ty = mkHsWildCardBndrs (mkHsImplicitBndrs ty)

mkHsSigEnv :: forall a. (LSig GhcRn -> Maybe ([Located Name], a))
                     -> [LSig GhcRn]
                     -> NameEnv a
mkHsSigEnv get_info sigs
  = mkNameEnv          (mk_pairs ordinary_sigs)
   `extendNameEnvList` (mk_pairs gen_dm_sigs)
   -- The subtlety is this: in a class decl with a
   -- default-method signature as well as a method signature
   -- we want the latter to win (Trac #12533)
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
    is_gen_dm_sig (dL->L _ (ClassOpSig _ True _ _)) = True
    is_gen_dm_sig _                                 = False

    mk_pairs :: [LSig GhcRn] -> [(Name, a)]
    mk_pairs sigs = [ (n,a) | Just (ns,a) <- map get_info sigs
                            , (dL->L _ n) <- ns ]

mkClassOpSigs :: [LSig GhcPs] -> [LSig GhcPs]
-- Convert TypeSig to ClassOpSig
-- The former is what is parsed, but the latter is
-- what we need in class/instance declarations
mkClassOpSigs sigs
  = map fiddle sigs
  where
    fiddle (dL->L loc (TypeSig _ nms ty))
      = cL loc (ClassOpSig noExt False nms (dropWildCards ty))
    fiddle sig = sig

typeToLHsType :: Type -> LHsType GhcPs
-- ^ Converting a Type to an HsType RdrName
-- This is needed to implement GeneralizedNewtypeDeriving.
--
-- Note that we use 'getRdrName' extensively, which
-- generates Exact RdrNames rather than strings.
typeToLHsType ty
  = go ty
  where
    go :: Type -> LHsType GhcPs
    go ty@(FunTy arg _)
      | isPredTy arg
      , (theta, tau) <- tcSplitPhiTy ty
      = noLoc (HsQualTy { hst_ctxt = noLoc (map go theta)
                        , hst_xqual = noExt
                        , hst_body = go tau })
    go (FunTy arg res) = nlHsFunTy (go arg) (go res)
    go ty@(ForAllTy {})
      | (tvs, tau) <- tcSplitForAllTys ty
      = noLoc (HsForAllTy { hst_bndrs = map go_tv tvs
                          , hst_xforall = noExt
                          , hst_body = go tau })
    go (TyVarTy tv)         = nlHsTyVar (getRdrName tv)
    go (AppTy t1 t2)        = nlHsAppTy (go t1) (go t2)
    go (LitTy (NumTyLit n))
      = noLoc $ HsTyLit NoExt (HsNumTy NoSourceText n)
    go (LitTy (StrTyLit s))
      = noLoc $ HsTyLit NoExt (HsStrTy NoSourceText s)
    go ty@(TyConApp tc args)
      | tyConAppNeedsKindSig True tc (length args)
        -- We must produce an explicit kind signature here to make certain
        -- programs kind-check. See Note [Kind signatures in typeToLHsType].
      = nlHsParTy $ noLoc $ HsKindSig NoExt lhs_ty (go (typeKind ty))
      | otherwise = lhs_ty
       where
        arg_flags :: [ArgFlag]
        arg_flags = tyConArgFlags tc args

        lhs_ty :: LHsType GhcPs
        lhs_ty = foldl' (\f (arg, flag) ->
                          let arg' = go arg in
                          case flag of
                            Inferred  -> f
                            Specified -> f `nlHsAppKindTy` arg'
                            Required  -> f `nlHsAppTy`     arg')
                        (nlHsTyVar (getRdrName tc))
                        (zip args arg_flags)
    go (CastTy ty _)        = go ty
    go (CoercionTy co)      = pprPanic "toLHsSigWcType" (ppr co)

         -- Source-language types have _invisible_ kind arguments,
         -- so we must remove them here (Trac #8563)

    go_tv :: TyVar -> LHsTyVarBndr GhcPs
    go_tv tv = noLoc $ KindedTyVar noExt (noLoc (getRdrName tv))
                                   (go (tyVarKind tv))

{-
Note [Kind signatures in typeToLHsType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are types that typeToLHsType can produce which require explicit kind
signatures in order to kind-check. Here is an example from Trac #14579:

  -- type P :: forall {k} {t :: k}. Proxy t
  type P = 'Proxy

  -- type Wat :: forall a. Proxy a -> *
  newtype Wat (x :: Proxy (a :: Type)) = MkWat (Maybe a)
    deriving Eq

  -- type Wat2 :: forall {a}. Proxy a -> *
  type Wat2 = Wat

  -- type Glurp :: * -> *
  newtype Glurp a = MkGlurp (Wat2 (P :: Proxy a))
    deriving Eq

The derived Eq instance for Glurp (without any kind signatures) would be:

  instance Eq a => Eq (Glurp a) where
    (==) = coerce @(Wat2 P  -> Wat2 P  -> Bool)
                  @(Glurp a -> Glurp a -> Bool)
                  (==) :: Glurp a -> Glurp a -> Bool

(Where the visible type applications use types produced by typeToLHsType.)

The type P (in Wat2 P) has an underspecified kind, so we must ensure that
typeToLHsType ascribes it with its kind: Wat2 (P :: Proxy a). To accomplish
this, whenever we see an application of a tycon to some arguments, we use
the tyConAppNeedsKindSig function to determine if it requires an explicit kind
signature to resolve some ambiguity. (See Note
Note [When does a tycon application need an explicit kind signature?] for a
more detailed explanation of how this works.)

Note that we pass True to tyConAppNeedsKindSig since we are generated code with
visible kind applications, so even specified arguments count towards injective
positions in the kind of the tycon.
-}

{- *********************************************************************
*                                                                      *
    --------- HsWrappers: type args, dict args, casts ---------
*                                                                      *
********************************************************************* -}

mkLHsWrap :: HsWrapper -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkLHsWrap co_fn (dL->L loc e) = cL loc (mkHsWrap co_fn e)

-- Avoid (HsWrap co (HsWrap co' _)).
-- See Note [Detecting forced eta expansion] in DsExpr
mkHsWrap :: HsWrapper -> HsExpr (GhcPass id) -> HsExpr (GhcPass id)
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
mkHsWrap co_fn (HsWrap _ co_fn' e)     = mkHsWrap (co_fn <.> co_fn') e
mkHsWrap co_fn e                       = HsWrap noExt co_fn e

mkHsWrapCo :: TcCoercionN   -- A Nominal coercion  a ~N b
           -> HsExpr (GhcPass id) -> HsExpr (GhcPass id)
mkHsWrapCo co e = mkHsWrap (mkWpCastN co) e

mkHsWrapCoR :: TcCoercionR   -- A Representational coercion  a ~R b
            -> HsExpr (GhcPass id) -> HsExpr (GhcPass id)
mkHsWrapCoR co e = mkHsWrap (mkWpCastR co) e

mkLHsWrapCo :: TcCoercionN -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkLHsWrapCo co (dL->L loc e) = cL loc (mkHsWrapCo co e)

mkHsCmdWrap :: HsWrapper -> HsCmd (GhcPass p) -> HsCmd (GhcPass p)
mkHsCmdWrap w cmd | isIdHsWrapper w = cmd
                  | otherwise       = HsCmdWrap noExt w cmd

mkLHsCmdWrap :: HsWrapper -> LHsCmd (GhcPass p) -> LHsCmd (GhcPass p)
mkLHsCmdWrap w (dL->L loc c) = cL loc (mkHsCmdWrap w c)

mkHsWrapPat :: HsWrapper -> Pat (GhcPass id) -> Type -> Pat (GhcPass id)
mkHsWrapPat co_fn p ty | isIdHsWrapper co_fn = p
                       | otherwise           = CoPat noExt co_fn p ty

mkHsWrapPatCo :: TcCoercionN -> Pat (GhcPass id) -> Type -> Pat (GhcPass id)
mkHsWrapPatCo co pat ty | isTcReflCo co = pat
                        | otherwise    = CoPat noExt (mkWpCastN co) pat ty

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

mkFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)]
          -> HsBind GhcPs
-- Not infix, with place holders for coercion and free vars
mkFunBind fn ms = FunBind { fun_id = fn
                          , fun_matches = mkMatchGroup Generated ms
                          , fun_co_fn = idHsWrapper
                          , fun_ext = noExt
                          , fun_tick = [] }

mkTopFunBind :: Origin -> Located Name -> [LMatch GhcRn (LHsExpr GhcRn)]
             -> HsBind GhcRn
-- In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn
                                    , fun_matches = mkMatchGroup origin ms
                                    , fun_co_fn = idHsWrapper
                                    , fun_ext  = emptyNameSet -- NB: closed
                                                              --     binding
                                    , fun_tick = [] }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
mkHsVarBind loc var rhs = mk_easy_FunBind loc var [] rhs

mkVarBind :: IdP (GhcPass p) -> LHsExpr (GhcPass p) -> LHsBind (GhcPass p)
mkVarBind var rhs = cL (getLoc rhs) $
                    VarBind { var_ext = noExt,
                              var_id = var, var_rhs = rhs, var_inline = False }

mkPatSynBind :: Located RdrName -> HsPatSynDetails (Located RdrName)
             -> LPat GhcPs -> HsPatSynDir GhcPs -> HsBind GhcPs
mkPatSynBind name details lpat dir = PatSynBind noExt psb
  where
    psb = PSB{ psb_ext = noExt
             , psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir }

-- |If any of the matches in the 'FunBind' are infix, the 'FunBind' is
-- considered infix.
isInfixFunBind :: HsBindLR id1 id2 -> Bool
isInfixFunBind (FunBind _ _ (MG _ matches _) _ _)
  = any (isInfixMatch . unLoc) (unLoc matches)
isInfixFunBind _ = False


------------
mk_easy_FunBind :: SrcSpan -> RdrName -> [LPat GhcPs]
                -> LHsExpr GhcPs -> LHsBind GhcPs
mk_easy_FunBind loc fun pats expr
  = cL loc $ mkFunBind (cL loc fun)
              [mkMatch (mkPrefixFunRhs (cL loc fun)) pats expr
                       (noLoc emptyLocalBinds)]

-- | Make a prefix, non-strict function 'HsMatchContext'
mkPrefixFunRhs :: Located id -> HsMatchContext id
mkPrefixFunRhs n = FunRhs { mc_fun = n
                          , mc_fixity = Prefix
                          , mc_strictness = NoSrcStrict }

------------
mkMatch :: HsMatchContext (NameOrRdrName (IdP (GhcPass p)))
        -> [LPat (GhcPass p)] -> LHsExpr (GhcPass p)
        -> Located (HsLocalBinds (GhcPass p))
        -> LMatch (GhcPass p) (LHsExpr (GhcPass p))
mkMatch ctxt pats expr lbinds
  = noLoc (Match { m_ext   = noExt
                 , m_ctxt  = ctxt
                 , m_pats  = map paren pats
                 , m_grhss = GRHSs noExt (unguardedRHS noSrcSpan expr) lbinds })
  where
    paren lp@(dL->L l p)
      | patNeedsParens appPrec p = cL l (ParPat noExt lp)
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
  see HsBinds Note [The abs_sig field of AbsBinds]
-}

----------------- Bindings --------------------------

-- | Should we treat this as an unlifted bind? This will be true for any
-- bind that binds an unlifted variable, but we must be careful around
-- AbsBinds. See Note [Unlifted id check in isUnliftedHsBind]. For usage
-- information, see Note [Strict binds check] is DsBinds.
isUnliftedHsBind :: HsBind GhcTc -> Bool  -- works only over typechecked binds
isUnliftedHsBind bind
  | AbsBinds { abs_exports = exports, abs_sig = has_sig } <- bind
  = if has_sig
    then any (is_unlifted_id . abe_poly) exports
    else any (is_unlifted_id . abe_mono) exports
    -- If has_sig is True we wil never generate a binding for abe_mono,
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
  | [dL->L _ match] <- unLoc $ mg_alts matches
  , FunRhs{mc_strictness = SrcStrict} <- m_ctxt match
  = True
isBangedHsBind (PatBind {pat_lhs = pat})
  = isBangedLPat pat
isBangedHsBind _
  = False

collectLocalBinders :: HsLocalBindsLR (GhcPass idL) (GhcPass idR)
                    -> [IdP (GhcPass idL)]
collectLocalBinders (HsValBinds _ binds) = collectHsIdBinders binds
                                         -- No pattern synonyms here
collectLocalBinders (HsIPBinds {})      = []
collectLocalBinders (EmptyLocalBinds _) = []
collectLocalBinders (XHsLocalBindsLR _) = []

collectHsIdBinders, collectHsValBinders
  :: HsValBindsLR (GhcPass idL) (GhcPass idR) -> [IdP (GhcPass idL)]
-- Collect Id binders only, or Ids + pattern synonyms, respectively
collectHsIdBinders  = collect_hs_val_binders True
collectHsValBinders = collect_hs_val_binders False

collectHsBindBinders :: (SrcSpanLess (LPat p) ~ Pat p, HasSrcSpan (LPat p))=>
                        HsBindLR p idR -> [IdP p]
-- Collect both Ids and pattern-synonym binders
collectHsBindBinders b = collect_bind False b []

collectHsBindsBinders :: LHsBindsLR (GhcPass p) idR -> [IdP (GhcPass p)]
collectHsBindsBinders binds = collect_binds False binds []

collectHsBindListBinders :: [LHsBindLR (GhcPass p) idR] -> [IdP (GhcPass p)]
-- Same as collectHsBindsBinders, but works over a list of bindings
collectHsBindListBinders = foldr (collect_bind False . unLoc) []

collect_hs_val_binders :: Bool -> HsValBindsLR (GhcPass idL) (GhcPass idR)
                       -> [IdP (GhcPass idL)]
collect_hs_val_binders ps (ValBinds _ binds _) = collect_binds ps binds []
collect_hs_val_binders ps (XValBindsLR (NValBinds binds _))
  = collect_out_binds ps binds

collect_out_binds :: Bool -> [(RecFlag, LHsBinds (GhcPass p))] ->
                     [IdP (GhcPass p)]
collect_out_binds ps = foldr (collect_binds ps . snd) []

collect_binds :: Bool -> LHsBindsLR (GhcPass p) idR ->
                 [IdP (GhcPass p)] -> [IdP (GhcPass p)]
-- Collect Ids, or Ids + pattern synonyms, depending on boolean flag
collect_binds ps binds acc = foldrBag (collect_bind ps . unLoc) acc binds

collect_bind :: (SrcSpanLess (LPat p) ~ Pat p , HasSrcSpan (LPat p)) =>
                Bool -> HsBindLR p idR -> [IdP p] -> [IdP p]
collect_bind _ (PatBind { pat_lhs = p })           acc = collect_lpat p acc
collect_bind _ (FunBind { fun_id = (dL->L _ f) })  acc = f : acc
collect_bind _ (VarBind { var_id = f })            acc = f : acc
collect_bind _ (AbsBinds { abs_exports = dbinds }) acc = map abe_poly dbinds ++ acc
        -- I don't think we want the binders from the abe_binds

        -- binding (hence see AbsBinds) is in zonking in TcHsSyn
collect_bind omitPatSyn (PatSynBind _ (PSB { psb_id = (dL->L _ ps) })) acc
  | omitPatSyn                  = acc
  | otherwise                   = ps : acc
collect_bind _ (PatSynBind _ (XPatSynBind _)) acc = acc
collect_bind _ (XHsBindsLR _) acc = acc

collectMethodBinders :: LHsBindsLR idL idR -> [Located (IdP idL)]
-- Used exclusively for the bindings of an instance decl which are all FunBinds
collectMethodBinders binds = foldrBag (get . unLoc) [] binds
  where
    get (FunBind { fun_id = f }) fs = f : fs
    get _                        fs = fs
       -- Someone else complains about non-FunBinds

----------------- Statements --------------------------
collectLStmtsBinders :: [LStmtLR (GhcPass idL) (GhcPass idR) body]
                     -> [IdP (GhcPass idL)]
collectLStmtsBinders = concatMap collectLStmtBinders

collectStmtsBinders :: [StmtLR (GhcPass idL) (GhcPass idR) body]
                    -> [IdP (GhcPass idL)]
collectStmtsBinders = concatMap collectStmtBinders

collectLStmtBinders :: LStmtLR (GhcPass idL) (GhcPass idR) body
                    -> [IdP (GhcPass idL)]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: StmtLR (GhcPass idL) (GhcPass idR) body
                   -> [IdP (GhcPass idL)]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt _ pat _ _ _)  = collectPatBinders pat
collectStmtBinders (LetStmt _  binds)      = collectLocalBinders (unLoc binds)
collectStmtBinders (BodyStmt {})           = []
collectStmtBinders (LastStmt {})           = []
collectStmtBinders (ParStmt _ xs _ _)      = collectLStmtsBinders
                                    $ [s | ParStmtBlock _ ss _ _ <- xs, s <- ss]
collectStmtBinders (TransStmt { trS_stmts = stmts }) = collectLStmtsBinders stmts
collectStmtBinders (RecStmt { recS_stmts = ss })     = collectLStmtsBinders ss
collectStmtBinders (ApplicativeStmt _ args _) = concatMap collectArgBinders args
 where
  collectArgBinders (_, ApplicativeArgOne _ pat _ _) = collectPatBinders pat
  collectArgBinders (_, ApplicativeArgMany _ _ _ pat) = collectPatBinders pat
  collectArgBinders _ = []
collectStmtBinders XStmtLR{} = panic "collectStmtBinders"


----------------- Patterns --------------------------
collectPatBinders :: LPat (GhcPass p) -> [IdP (GhcPass p)]
collectPatBinders pat = collect_lpat pat []

collectPatsBinders :: [LPat (GhcPass p)] -> [IdP (GhcPass p)]
collectPatsBinders pats = foldr collect_lpat [] pats

-------------
collect_lpat :: (SrcSpanLess (LPat p) ~ Pat p , HasSrcSpan (LPat p)) =>
                 LPat p -> [IdP p] -> [IdP p]
collect_lpat p bndrs
  = go (unLoc p)
  where
    go (VarPat _ var)             = unLoc var : bndrs
    go (WildPat _)                = bndrs
    go (LazyPat _ pat)            = collect_lpat pat bndrs
    go (BangPat _ pat)            = collect_lpat pat bndrs
    go (AsPat _ a pat)            = unLoc a : collect_lpat pat bndrs
    go (ViewPat _ _ pat)          = collect_lpat pat bndrs
    go (ParPat _ pat)             = collect_lpat pat bndrs

    go (ListPat _ pats)           = foldr collect_lpat bndrs pats
    go (TuplePat _ pats _)        = foldr collect_lpat bndrs pats
    go (SumPat _ pat _ _)         = collect_lpat pat bndrs

    go (ConPatIn _ ps)            = foldr collect_lpat bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps})  = foldr collect_lpat bndrs (hsConPatArgs ps)
        -- See Note [Dictionary binders in ConPatOut]
    go (LitPat _ _)               = bndrs
    go (NPat {})                  = bndrs
    go (NPlusKPat _ n _ _ _ _)    = unLoc n : bndrs

    go (SigPat _ pat _)           = collect_lpat pat bndrs

    go (SplicePat _ (HsSpliced _ _ (HsSplicedPat pat)))
                                  = go pat
    go (SplicePat _ _)            = bndrs
    go (CoPat _ _ pat _)          = go pat
    go (XPat {})                  = bndrs

{-
Note [Dictionary binders in ConPatOut] See also same Note in DsArrows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do *not* gather (a) dictionary and (b) dictionary bindings as binders
of a ConPatOut pattern.  For most calls it doesn't matter, because
it's pre-typechecker and there are no ConPatOuts.  But it does matter
more in the desugarer; for example, DsUtils.mkSelectorBinds uses
collectPatBinders.  In a lazy pattern, for example f ~(C x y) = ...,
we want to generate bindings for x,y but not for dictionaries bound by
C.  (The type checker ensures they would not be used.)

Desugaring of arrow case expressions needs these bindings (see DsArrows
and arrowcase1), but SPJ (Jan 2007) says it's safer for it to use its
own pat-binder-collector:

Here's the problem.  Consider

data T a where
   C :: Num a => a -> Int -> T a

f ~(C (n+1) m) = (n,m)

Here, the pattern (C (n+1)) binds a hidden dictionary (d::Num a),
and *also* uses that dictionary to match the (n+1) pattern.  Yet, the
variables bound by the lazy pattern are n,m, *not* the dictionary d.
So in mkSelectorBinds in DsUtils, we want just m,n as the variables bound.
-}

hsGroupBinders :: HsGroup GhcRn -> [Name]
hsGroupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
                          hs_fords = foreign_decls })
  =  collectHsValBinders val_decls
  ++ hsTyClForeignBinders tycl_decls foreign_decls
hsGroupBinders (XHsGroup {}) = panic "hsGroupBinders"

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
hsLTyClDeclBinders :: Located (TyClDecl pass)
                   -> ([Located (IdP pass)], [LFieldOcc pass])
-- ^ Returns all the /binding/ names of the decl.  The first one is
-- guaranteed to be the name of the decl. The first component
-- represents all binding names except record fields; the second
-- represents field occurrences. For record fields mentioned in
-- multiple constructors, the SrcLoc will be from the first occurrence.
--
-- Each returned (Located name) has a SrcSpan for the /whole/ declaration.
-- See Note [SrcSpan for binders]

hsLTyClDeclBinders (dL->L loc (FamDecl { tcdFam = FamilyDecl
                                            { fdLName = (dL->L _ name) } }))
  = ([cL loc name], [])
hsLTyClDeclBinders (dL->L _ (FamDecl { tcdFam = XFamilyDecl _ }))
  = panic "hsLTyClDeclBinders"
hsLTyClDeclBinders (dL->L loc (SynDecl
                               { tcdLName = (dL->L _ name) }))
  = ([cL loc name], [])
hsLTyClDeclBinders (dL->L loc (ClassDecl
                               { tcdLName = (dL->L _ cls_name)
                               , tcdSigs  = sigs
                               , tcdATs   = ats }))
  = (cL loc cls_name :
     [ cL fam_loc fam_name | (dL->L fam_loc (FamilyDecl
                                        { fdLName = L _ fam_name })) <- ats ]
     ++
     [ cL mem_loc mem_name | (dL->L mem_loc (ClassOpSig _ False ns _)) <- sigs
                           , (dL->L _ mem_name) <- ns ]
    , [])
hsLTyClDeclBinders (dL->L loc (DataDecl    { tcdLName = (dL->L _ name)
                                           , tcdDataDefn = defn }))
  = (\ (xs, ys) -> (cL loc name : xs, ys)) $ hsDataDefnBinders defn
hsLTyClDeclBinders (dL->L _ (XTyClDecl _)) = panic "hsLTyClDeclBinders"
hsLTyClDeclBinders _ = panic "hsLTyClDeclBinders: Impossible Match"
                             -- due to #15884


-------------------
hsForeignDeclsBinders :: [LForeignDecl pass] -> [Located (IdP pass)]
-- See Note [SrcSpan for binders]
hsForeignDeclsBinders foreign_decls
  = [ cL decl_loc n
    | (dL->L decl_loc (ForeignImport { fd_name = (dL->L _ n) }))
        <- foreign_decls]


-------------------
hsPatSynSelectors :: HsValBinds (GhcPass p) -> [IdP (GhcPass p)]
-- Collects record pattern-synonym selectors only; the pattern synonym
-- names are collected by collectHsValBinders.
hsPatSynSelectors (ValBinds _ _ _) = panic "hsPatSynSelectors"
hsPatSynSelectors (XValBindsLR (NValBinds binds _))
  = foldrBag addPatSynSelector [] . unionManyBags $ map snd binds

addPatSynSelector:: LHsBind p -> [IdP p] -> [IdP p]
addPatSynSelector bind sels
  | PatSynBind _ (PSB { psb_args = RecCon as }) <- unLoc bind
  = map (unLoc . recordPatSynSelectorId) as ++ sels
  | otherwise = sels

getPatSynBinds :: [(RecFlag, LHsBinds id)] -> [PatSynBind id id]
getPatSynBinds binds
  = [ psb | (_, lbinds) <- binds
          , (dL->L _ (PatSynBind _ psb)) <- bagToList lbinds ]

-------------------
hsLInstDeclBinders :: LInstDecl (GhcPass p)
                   -> ([Located (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
hsLInstDeclBinders (dL->L _ (ClsInstD
                             { cid_inst = ClsInstDecl
                                          { cid_datafam_insts = dfis }}))
  = foldMap (hsDataFamInstBinders . unLoc) dfis
hsLInstDeclBinders (dL->L _ (DataFamInstD { dfid_inst = fi }))
  = hsDataFamInstBinders fi
hsLInstDeclBinders (dL->L _ (TyFamInstD {})) = mempty
hsLInstDeclBinders (dL->L _ (ClsInstD _ (XClsInstDecl {})))
  = panic "hsLInstDeclBinders"
hsLInstDeclBinders (dL->L _ (XInstDecl _))
  = panic "hsLInstDeclBinders"
hsLInstDeclBinders _ = panic "hsLInstDeclBinders: Impossible Match"
                             -- due to #15884

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataFamInstBinders :: DataFamInstDecl pass
                     -> ([Located (IdP pass)], [LFieldOcc pass])
hsDataFamInstBinders (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                       FamEqn { feqn_rhs = defn }}})
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders
hsDataFamInstBinders (DataFamInstDecl
                                    { dfid_eqn = HsIB { hsib_body = XFamEqn _}})
  = panic "hsDataFamInstBinders"
hsDataFamInstBinders (DataFamInstDecl (XHsImplicitBndrs _))
  = panic "hsDataFamInstBinders"

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataDefnBinders :: HsDataDefn pass -> ([Located (IdP pass)], [LFieldOcc pass])
hsDataDefnBinders (HsDataDefn { dd_cons = cons })
  = hsConDeclsBinders cons
  -- See Note [Binders in family instances]
hsDataDefnBinders (XHsDataDefn _) = panic "hsDataDefnBinders"

-------------------
type Seen pass = [LFieldOcc pass] -> [LFieldOcc pass]
                 -- Filters out ones that have already been seen

hsConDeclsBinders :: [LConDecl pass] -> ([Located (IdP pass)], [LFieldOcc pass])
   -- See hsLTyClDeclBinders for what this does
   -- The function is boringly complicated because of the records
   -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons
  = go id cons
  where
    go :: Seen pass -> [LConDecl pass]
       -> ([Located (IdP pass)], [LFieldOcc pass])
    go _ [] = ([], [])
    go remSeen (r:rs)
      -- Don't re-mangle the location of field names, because we don't
      -- have a record of the full location of the field declaration anyway
      = let loc = getLoc r
        in case unLoc r of
           -- remove only the first occurrence of any seen field in order to
           -- avoid circumventing detection of duplicate fields (#9156)
           ConDeclGADT { con_names = names, con_args = args }
             -> (map (cL loc . unLoc) names ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds remSeen args
                (ns, fs) = go remSeen' rs

           ConDeclH98 { con_name = name, con_args = args }
             -> ([cL loc (unLoc name)] ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds remSeen args
                (ns, fs) = go remSeen' rs

           XConDecl _ -> panic "hsConDeclsBinders"

    get_flds :: Seen pass -> HsConDeclDetails pass
             -> (Seen pass, [LFieldOcc pass])
    get_flds remSeen (RecCon flds)
       = (remSeen', fld_names)
       where
          fld_names = remSeen (concatMap (cd_fld_names . unLoc) (unLoc flds))
          remSeen' = foldr (.) remSeen
                               [deleteBy ((==) `on` unLoc . rdrNameFieldOcc . unLoc) v
                               | v <- fld_names]
    get_flds remSeen _
       = (remSeen, [])

{-

Note [SrcSpan for binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When extracting the (Located RdrNme) for a binder, at least for the
main name (the TyCon of a type declaration etc), we want to give it
the @SrcSpan@ of the whole /declaration/, not just the name itself
(which is how it appears in the syntax tree).  This SrcSpan (for the
entire declaration) is used as the SrcSpan for the Name that is
finally produced, and hence for error messages.  (See Trac #8607.)

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
-}

lStmtsImplicits :: [LStmtLR GhcRn (GhcPass idR) (Located (body (GhcPass idR)))]
                -> NameSet
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: [LStmtLR GhcRn (GhcPass idR) (Located (body (GhcPass idR)))]
              -> NameSet
    hs_lstmts = foldr (\stmt rest -> unionNameSet (hs_stmt (unLoc stmt)) rest) emptyNameSet

    hs_stmt :: StmtLR GhcRn (GhcPass idR) (Located (body (GhcPass idR)))
            -> NameSet
    hs_stmt (BindStmt _ pat _ _ _) = lPatImplicits pat
    hs_stmt (ApplicativeStmt _ args _) = unionNameSets (map do_arg args)
      where do_arg (_, ApplicativeArgOne _ pat _ _) = lPatImplicits pat
            do_arg (_, ApplicativeArgMany _ stmts _ _) = hs_lstmts stmts
            do_arg (_, XApplicativeArg _) = panic "lStmtsImplicits"
    hs_stmt (LetStmt _ binds)     = hs_local_binds (unLoc binds)
    hs_stmt (BodyStmt {})         = emptyNameSet
    hs_stmt (LastStmt {})         = emptyNameSet
    hs_stmt (ParStmt _ xs _ _)    = hs_lstmts [s | ParStmtBlock _ ss _ _ <- xs
                                                , s <- ss]
    hs_stmt (TransStmt { trS_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (RecStmt { recS_stmts = ss })     = hs_lstmts ss
    hs_stmt (XStmtLR {})          = panic "lStmtsImplicits"

    hs_local_binds (HsValBinds _ val_binds) = hsValBindsImplicits val_binds
    hs_local_binds (HsIPBinds {})           = emptyNameSet
    hs_local_binds (EmptyLocalBinds _)      = emptyNameSet
    hs_local_binds (XHsLocalBindsLR _)      = emptyNameSet

hsValBindsImplicits :: HsValBindsLR GhcRn (GhcPass idR) -> NameSet
hsValBindsImplicits (XValBindsLR (NValBinds binds _))
  = foldr (unionNameSet . lhsBindsImplicits . snd) emptyNameSet binds
hsValBindsImplicits (ValBinds _ binds _)
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR GhcRn idR -> NameSet
lhsBindsImplicits = foldBag unionNameSet (lhs_bind . unLoc) emptyNameSet
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = emptyNameSet

lPatImplicits :: LPat GhcRn -> NameSet
lPatImplicits = hs_lpat
  where
    hs_lpat lpat = hs_pat (unLoc lpat)

    hs_lpats = foldr (\pat rest -> hs_lpat pat `unionNameSet` rest) emptyNameSet

    hs_pat (LazyPat _ pat)      = hs_lpat pat
    hs_pat (BangPat _ pat)      = hs_lpat pat
    hs_pat (AsPat _ _ pat)      = hs_lpat pat
    hs_pat (ViewPat _ _ pat)    = hs_lpat pat
    hs_pat (ParPat _ pat)       = hs_lpat pat
    hs_pat (ListPat _ pats)     = hs_lpats pats
    hs_pat (TuplePat _ pats _)  = hs_lpats pats

    hs_pat (SigPat _ pat _)     = hs_lpat pat
    hs_pat (CoPat _ _ pat _)    = hs_pat pat

    hs_pat (ConPatIn _ ps)           = details ps
    hs_pat (ConPatOut {pat_args=ps}) = details ps

    hs_pat _ = emptyNameSet

    details (PrefixCon ps)   = hs_lpats ps
    details (RecCon fs)      = hs_lpats explicit `unionNameSet` mkNameSet (collectPatsBinders implicit)
      where (explicit, implicit) = partitionEithers [if pat_explicit then Left pat else Right pat
                                                    | (i, fld) <- [0..] `zip` rec_flds fs
                                                    , let pat = hsRecFieldArg
                                                                     (unLoc fld)
                                                          pat_explicit = maybe True (i<) (rec_dotdot fs)]
    details (InfixCon p1 p2) = hs_lpat p1 `unionNameSet` hs_lpat p2
