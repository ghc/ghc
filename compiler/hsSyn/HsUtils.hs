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

module HsUtils(
  -- Terms
  mkHsPar, mkHsApp, mkHsAppType, mkHsAppTypes, mkHsAppTypeOut, mkHsCaseAlt,
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
  nlWildPatName, nlWildPatId, nlTuplePat, mkParPat, nlParPat,
  mkBigLHsVarTup, mkBigLHsTup, mkBigLHsVarPatTup, mkBigLHsPatTup,

  -- Types
  mkHsAppTy, mkHsAppTys, userHsTyVarBndrs, userHsLTyVarBndrs,
  mkLHsSigType, mkLHsSigWcType, mkClassOpSigs, mkHsSigEnv,
  nlHsAppTy, nlHsTyVar, nlHsFunTy, nlHsParTy, nlHsTyConApp,

  -- Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt, mkBindStmt, mkTcBindStmt,
  mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt,
  emptyRecStmt, emptyRecStmtName, emptyRecStmtId, mkRecStmt,

  -- Template Haskell
  mkHsSpliceTy, mkHsSpliceE, mkHsSpliceTE, mkUntypedSplice,
  mkHsQuasiQuote, unqualQuasiQuote,

  -- Flags
  noRebindableInfo,

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
import Type   ( filterOutInvisibleTypes )
import TysWiredIn ( unitTy )
import TcType
import DataCon
import ConLike
import Id
import Name
import NameSet
import NameEnv
import BasicTypes
import SrcLoc
import FastString
import Util
import Bag
import Outputable
import Constants
import TyCon

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

mkHsPar :: LHsExpr id -> LHsExpr id
mkHsPar e = L (getLoc e) (HsPar e)

mkSimpleMatch :: HsMatchContext (NameOrRdrName (IdP id))
              -> [LPat id] -> Located (body id)
              -> LMatch id (Located (body id))
mkSimpleMatch ctxt pats rhs
  = L loc $
    Match { m_ctxt = ctxt, m_pats = pats
          , m_grhss = unguardedGRHSs rhs }
  where
    loc = case pats of
                []      -> getLoc rhs
                (pat:_) -> combineSrcSpans (getLoc pat) (getLoc rhs)

unguardedGRHSs :: Located (body id) -> GRHSs id (Located (body id))
unguardedGRHSs rhs@(L loc _)
  = GRHSs (unguardedRHS loc rhs) (noLoc emptyLocalBinds)

unguardedRHS :: SrcSpan -> Located (body id) -> [LGRHS id (Located (body id))]
unguardedRHS loc rhs = [L loc (GRHS [] rhs)]

mkMatchGroup :: (PostTc name Type ~ PlaceHolder)
             => Origin -> [LMatch name (Located (body name))]
             -> MatchGroup name (Located (body name))
mkMatchGroup origin matches = MG { mg_alts = mkLocatedList matches
                                 , mg_arg_tys = []
                                 , mg_res_ty = placeHolderType
                                 , mg_origin = origin }

mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms

mkHsApp :: LHsExpr name -> LHsExpr name -> LHsExpr name
mkHsApp e1 e2 = addCLoc e1 e2 (HsApp e1 e2)

mkHsAppType :: LHsExpr name -> LHsWcType name -> LHsExpr name
mkHsAppType e t = addCLoc e (hswc_body t) (HsAppType e t)

mkHsAppTypes :: LHsExpr name -> [LHsWcType name] -> LHsExpr name
mkHsAppTypes = foldl mkHsAppType

mkHsAppTypeOut :: LHsExpr GhcTc -> LHsWcType GhcRn -> LHsExpr GhcTc
mkHsAppTypeOut e t = addCLoc e (hswc_body t) (HsAppTypeOut e t)

mkHsLam :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam matches))
  where
    matches = mkMatchGroup Generated
                           [mkSimpleMatch LambdaExpr pats' body]
    pats' = map parenthesizeCompoundPat pats

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr GhcTc -> LHsExpr GhcTc
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars
                                       <.> mkWpLams dicts) expr

-- |A simple case alternative with a single pattern, no binds, no guards;
-- pre-typechecking
mkHsCaseAlt :: LPat id -> (Located (body id)) -> LMatch id (Located (body id))
mkHsCaseAlt pat expr
  = mkSimpleMatch CaseAlt [pat] expr

nlHsTyApp :: IdP name -> [Type] -> LHsExpr name
nlHsTyApp fun_id tys = noLoc (mkHsWrap (mkWpTyApps tys) (HsVar (noLoc fun_id)))

nlHsTyApps :: IdP name -> [Type] -> [LHsExpr name] -> LHsExpr name
nlHsTyApps fun_id tys xs = foldl nlHsApp (nlHsTyApp fun_id tys) xs

--------- Adding parens ---------
mkLHsPar :: LHsExpr name -> LHsExpr name
-- Wrap in parens if hsExprNeedsParens says it needs them
-- So   'f x'  becomes '(f x)', but '3' stays as '3'
mkLHsPar le@(L loc e) | hsExprNeedsParens e = L loc (HsPar le)
                      | otherwise           = le

mkParPat :: LPat name -> LPat name
mkParPat lp@(L loc p) | hsPatNeedsParens p = L loc (ParPat lp)
                      | otherwise          = lp

nlParPat :: LPat name -> LPat name
nlParPat p = noLoc (ParPat p)

-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: IntegralLit -> PostTc GhcPs Type
               -> HsOverLit GhcPs
mkHsFractional :: FractionalLit -> PostTc GhcPs Type -> HsOverLit GhcPs
mkHsIsString :: SourceText -> FastString -> PostTc GhcPs Type
             -> HsOverLit GhcPs
mkHsDo         :: HsStmtContext Name -> [ExprLStmt GhcPs] -> HsExpr GhcPs
mkHsComp       :: HsStmtContext Name -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
               -> HsExpr GhcPs

mkNPat      :: Located (HsOverLit GhcPs) -> Maybe (SyntaxExpr GhcPs)
            -> Pat GhcPs
mkNPlusKPat :: Located RdrName -> Located (HsOverLit GhcPs) -> Pat GhcPs

mkLastStmt :: SourceTextX idR
           => Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))
mkBodyStmt :: Located (bodyR GhcPs)
           -> StmtLR idL GhcPs (Located (bodyR GhcPs))
mkBindStmt :: (SourceTextX idR, PostTc idR Type ~ PlaceHolder)
           => LPat idL -> Located (bodyR idR)
           -> StmtLR idL idR (Located (bodyR idR))
mkTcBindStmt :: LPat GhcTc -> Located (bodyR GhcTc)
             -> StmtLR GhcTc GhcTc (Located (bodyR GhcTc))

emptyRecStmt     :: StmtLR idL  GhcPs bodyR
emptyRecStmtName :: StmtLR GhcRn GhcRn bodyR
emptyRecStmtId   :: StmtLR GhcTc GhcTc bodyR
mkRecStmt    :: [LStmtLR idL GhcPs bodyR] -> StmtLR idL GhcPs bodyR


mkHsIntegral     i  = OverLit (HsIntegral       i) noRebindableInfo noExpr
mkHsFractional   f  = OverLit (HsFractional     f) noRebindableInfo noExpr
mkHsIsString src s  = OverLit (HsIsString   src s) noRebindableInfo noExpr

noRebindableInfo :: PlaceHolder
noRebindableInfo = PlaceHolder -- Just another placeholder;

mkHsDo ctxt stmts = HsDo ctxt (mkLocatedList stmts) placeHolderType
mkHsComp ctxt stmts expr = mkHsDo ctxt (stmts ++ [last_stmt])
  where
    last_stmt = L (getLoc expr) $ mkLastStmt expr

mkHsIf :: SourceTextX p => LHsExpr p -> LHsExpr p -> LHsExpr p -> HsExpr p
mkHsIf c a b = HsIf (Just noSyntaxExpr) c a b

mkNPat lit neg     = NPat lit neg noSyntaxExpr placeHolderType
mkNPlusKPat id lit = NPlusKPat id lit (unLoc lit) noSyntaxExpr noSyntaxExpr placeHolderType

mkTransformStmt    :: (SourceTextX idR, PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL] -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkTransformByStmt  :: (SourceTextX idR, PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupUsingStmt   :: (SourceTextX idR, PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL]                -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupByUsingStmt :: (SourceTextX idR, PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)

emptyTransStmt :: (SourceTextX idR, PostTc idR Type ~ PlaceHolder)
               => StmtLR idL idR (LHsExpr idR)
emptyTransStmt = TransStmt { trS_form = panic "emptyTransStmt: form"
                           , trS_stmts = [], trS_bndrs = []
                           , trS_by = Nothing, trS_using = noLoc noExpr
                           , trS_ret = noSyntaxExpr, trS_bind = noSyntaxExpr
                           , trS_bind_arg_ty = PlaceHolder
                           , trS_fmap = noExpr }
mkTransformStmt    ss u   = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  ss u b = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   ss u   = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt ss b u = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body     = LastStmt body False noSyntaxExpr
mkBodyStmt body     = BodyStmt body noSyntaxExpr noSyntaxExpr placeHolderType
mkBindStmt pat body = BindStmt pat body noSyntaxExpr noSyntaxExpr PlaceHolder
mkTcBindStmt pat body = BindStmt pat body noSyntaxExpr noSyntaxExpr unitTy
  -- don't use placeHolderTypeTc above, because that panics during zonking

emptyRecStmt' :: forall idL idR body. SourceTextX idR =>
                       PostTc idR Type -> StmtLR idL idR body
emptyRecStmt' tyVal =
   RecStmt
     { recS_stmts = [], recS_later_ids = []
     , recS_rec_ids = []
     , recS_ret_fn = noSyntaxExpr
     , recS_mfix_fn = noSyntaxExpr
     , recS_bind_fn = noSyntaxExpr, recS_bind_ty = tyVal
     , recS_later_rets = []
     , recS_rec_rets = [], recS_ret_ty = tyVal }

emptyRecStmt     = emptyRecStmt' placeHolderType
emptyRecStmtName = emptyRecStmt' placeHolderType
emptyRecStmtId   = emptyRecStmt' unitTy -- a panic might trigger during zonking
mkRecStmt stmts  = emptyRecStmt { recS_stmts = stmts }

-------------------------------
--- A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr id -> IdP id -> LHsExpr id -> HsExpr id
mkHsOpApp e1 op e2 = OpApp e1 (noLoc (HsVar (noLoc op)))
                           (error "mkOpApp:fixity") e2

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))

mkUntypedSplice :: SpliceDecoration -> LHsExpr GhcPs -> HsSplice GhcPs
mkUntypedSplice hasParen e = HsUntypedSplice hasParen unqualSplice e

mkHsSpliceE :: SpliceDecoration -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsSpliceE hasParen e = HsSpliceE (mkUntypedSplice hasParen e)

mkHsSpliceTE :: SpliceDecoration -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsSpliceTE hasParen e = HsSpliceE (HsTypedSplice hasParen unqualSplice e)

mkHsSpliceTy :: SpliceDecoration -> LHsExpr GhcPs -> HsType GhcPs
mkHsSpliceTy hasParen e
  = HsSpliceTy (HsUntypedSplice hasParen unqualSplice e) placeHolderKind

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsSplice GhcPs
mkHsQuasiQuote quoter span quote = HsQuasiQuote unqualSplice quoter span quote

unqualQuasiQuote :: RdrName
unqualQuasiQuote = mkRdrUnqual (mkVarOccFS (fsLit "quasiquote"))
                -- A name (uniquified later) to
                -- identify the quasi-quote

mkHsString :: SourceTextX p => String -> HsLit p
mkHsString s = HsString noSourceText (mkFastString s)

mkHsStringPrimLit :: SourceTextX p => FastString -> HsLit p
mkHsStringPrimLit fs
  = HsStringPrim noSourceText (fastStringToByteString fs)

-------------
userHsLTyVarBndrs :: SrcSpan -> [Located (IdP name)] -> [LHsTyVarBndr name]
-- Caller sets location
userHsLTyVarBndrs loc bndrs = [ L loc (UserTyVar v) | v <- bndrs ]

userHsTyVarBndrs :: SrcSpan -> [IdP name] -> [LHsTyVarBndr name]
-- Caller sets location
userHsTyVarBndrs loc bndrs = [ L loc (UserTyVar (L loc v)) | v <- bndrs ]


{-
************************************************************************
*                                                                      *
        Constructing syntax with no location info
*                                                                      *
************************************************************************
-}

nlHsVar :: IdP id -> LHsExpr id
nlHsVar n = noLoc (HsVar (noLoc n))

-- NB: Only for LHsExpr **Id**
nlHsDataCon :: DataCon -> LHsExpr GhcTc
nlHsDataCon con = noLoc (HsConLikeOut (RealDataCon con))

nlHsLit :: HsLit p -> LHsExpr p
nlHsLit n = noLoc (HsLit n)

nlHsIntLit :: HasDefaultX p => Integer -> LHsExpr p
nlHsIntLit n = noLoc (HsLit (HsInt def (mkIntegralLit n)))

nlVarPat :: IdP id -> LPat id
nlVarPat n = noLoc (VarPat (noLoc n))

nlLitPat :: HsLit p -> LPat p
nlLitPat l = noLoc (LitPat l)

nlHsApp :: LHsExpr id -> LHsExpr id -> LHsExpr id
nlHsApp f x = noLoc (HsApp f (mkLHsPar x))

nlHsSyntaxApps :: SyntaxExpr id -> [LHsExpr id] -> LHsExpr id
nlHsSyntaxApps (SyntaxExpr { syn_expr      = fun
                           , syn_arg_wraps = arg_wraps
                           , syn_res_wrap  = res_wrap }) args
  | [] <- arg_wraps   -- in the noSyntaxExpr case
  = ASSERT( isIdHsWrapper res_wrap )
    foldl nlHsApp (noLoc fun) args

  | otherwise
  = mkLHsWrap res_wrap (foldl nlHsApp (noLoc fun) (zipWithEqual "nlHsSyntaxApps"
                                                     mkLHsWrap arg_wraps args))

nlHsApps :: IdP id -> [LHsExpr id] -> LHsExpr id
nlHsApps f xs = foldl nlHsApp (nlHsVar f) xs

nlHsVarApps :: IdP id -> [IdP id] -> LHsExpr id
nlHsVarApps f xs = noLoc (foldl mk (HsVar (noLoc f)) (map (HsVar . noLoc) xs))
                 where
                   mk f a = HsApp (noLoc f) (noLoc a)

nlConVarPat :: RdrName -> [RdrName] -> LPat GhcPs
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlConVarPatName :: Name -> [Name] -> LPat GhcRn
nlConVarPatName con vars = nlConPatName con (map nlVarPat vars)

nlInfixConPat :: IdP id -> LPat id -> LPat id -> LPat id
nlInfixConPat con l r = noLoc (ConPatIn (noLoc con) (InfixCon l r))

nlConPat :: RdrName -> [LPat GhcPs] -> LPat GhcPs
nlConPat con pats =
  noLoc (ConPatIn (noLoc con) (PrefixCon (map parenthesizeCompoundPat pats)))

nlConPatName :: Name -> [LPat GhcRn] -> LPat GhcRn
nlConPatName con pats =
  noLoc (ConPatIn (noLoc con) (PrefixCon (map parenthesizeCompoundPat pats)))

nlNullaryConPat :: IdP id -> LPat id
nlNullaryConPat con = noLoc (ConPatIn (noLoc con) (PrefixCon []))

nlWildConPat :: DataCon -> LPat GhcPs
nlWildConPat con = noLoc (ConPatIn (noLoc (getRdrName con))
                         (PrefixCon (nOfThem (dataConSourceArity con)
                                             nlWildPat)))

nlWildPat :: LPat GhcPs
nlWildPat  = noLoc (WildPat placeHolderType )  -- Pre-typechecking

nlWildPatName :: LPat GhcRn
nlWildPatName  = noLoc (WildPat placeHolderType )  -- Pre-typechecking

nlWildPatId :: LPat GhcTc
nlWildPatId  = noLoc (WildPat placeHolderTypeTc )  -- Post-typechecking

nlHsDo :: HsStmtContext Name -> [LStmt GhcPs (LHsExpr GhcPs)]
       -> LHsExpr GhcPs
nlHsDo ctxt stmts = noLoc (mkHsDo ctxt stmts)

nlHsOpApp :: LHsExpr id -> IdP id -> LHsExpr id -> LHsExpr id
nlHsOpApp e1 op e2 = noLoc (mkHsOpApp e1 op e2)

nlHsLam  :: LMatch GhcPs (LHsExpr GhcPs) -> LHsExpr GhcPs
nlHsPar  :: LHsExpr id -> LHsExpr id
nlHsIf   :: LHsExpr id -> LHsExpr id -> LHsExpr id -> LHsExpr id
nlHsCase :: LHsExpr GhcPs -> [LMatch GhcPs (LHsExpr GhcPs)]
         -> LHsExpr GhcPs
nlList   :: [LHsExpr GhcPs] -> LHsExpr GhcPs

nlHsLam match          = noLoc (HsLam (mkMatchGroup Generated [match]))
nlHsPar e              = noLoc (HsPar e)

-- Note [Rebindable nlHsIf]
-- nlHsIf should generate if-expressions which are NOT subject to
-- RebindableSyntax, so the first field of HsIf is Nothing. (#12080)
nlHsIf cond true false = noLoc (HsIf Nothing cond true false)

nlHsCase expr matches  = noLoc (HsCase expr (mkMatchGroup Generated matches))
nlList exprs           = noLoc (ExplicitList placeHolderType Nothing exprs)

nlHsAppTy :: LHsType name -> LHsType name -> LHsType name
nlHsTyVar :: IdP name                     -> LHsType name
nlHsFunTy :: LHsType name -> LHsType name -> LHsType name
nlHsParTy :: LHsType name                 -> LHsType name

nlHsAppTy f t = noLoc (HsAppTy f (parenthesizeCompoundHsType t))
nlHsTyVar x   = noLoc (HsTyVar NotPromoted (noLoc x))
nlHsFunTy a b = noLoc (HsFunTy a b)
nlHsParTy t   = noLoc (HsParTy t)

nlHsTyConApp :: IdP name -> [LHsType name] -> LHsType name
nlHsTyConApp tycon tys  = foldl nlHsAppTy (nlHsTyVar tycon) tys

{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

mkLHsTupleExpr :: [LHsExpr a] -> LHsExpr a
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] = e
mkLHsTupleExpr es  = noLoc $ ExplicitTuple (map (noLoc . Present) es) Boxed

mkLHsVarTuple :: [IdP a] -> LHsExpr a
mkLHsVarTuple ids  = mkLHsTupleExpr (map nlHsVar ids)

nlTuplePat :: [LPat id] -> Boxity -> LPat id
nlTuplePat pats box = noLoc (TuplePat pats box [])

missingTupArg :: HsTupArg GhcPs
missingTupArg = Missing placeHolderType

mkLHsPatTup :: [LPat id] -> LPat id
mkLHsPatTup []     = noLoc $ TuplePat [] Boxed []
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = L (getLoc (head lpats)) $ TuplePat lpats Boxed []

-- The Big equivalents for the source tuple expressions
mkBigLHsVarTup :: [IdP id] -> LHsExpr id
mkBigLHsVarTup ids = mkBigLHsTup (map nlHsVar ids)

mkBigLHsTup :: [LHsExpr id] -> LHsExpr id
mkBigLHsTup = mkChunkified mkLHsTupleExpr

-- The Big equivalents for the source tuple patterns
mkBigLHsVarPatTup :: [IdP id] -> LPat id
mkBigLHsVarPatTup bs = mkBigLHsPatTup (map nlVarPat bs)

mkBigLHsPatTup :: [LPat id] -> LPat id
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
    is_gen_dm_sig (L _ (ClassOpSig True _ _)) = True
    is_gen_dm_sig _                           = False

    mk_pairs :: [LSig GhcRn] -> [(Name, a)]
    mk_pairs sigs = [ (n,a) | Just (ns,a) <- map get_info sigs
                            , L _ n <- ns ]

mkClassOpSigs :: [LSig GhcPs] -> [LSig GhcPs]
-- Convert TypeSig to ClassOpSig
-- The former is what is parsed, but the latter is
-- what we need in class/instance declarations
mkClassOpSigs sigs
  = map fiddle sigs
  where
    fiddle (L loc (TypeSig nms ty)) = L loc (ClassOpSig False nms (dropWildCards ty))
    fiddle sig                      = sig

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
                        , hst_body = go tau })
    go (FunTy arg res) = nlHsFunTy (go arg) (go res)
    go ty@(ForAllTy {})
      | (tvs, tau) <- tcSplitForAllTys ty
      = noLoc (HsForAllTy { hst_bndrs = map go_tv tvs
                          , hst_body = go tau })
    go (TyVarTy tv)         = nlHsTyVar (getRdrName tv)
    go (AppTy t1 t2)        = nlHsAppTy (go t1) (go t2)
    go (LitTy (NumTyLit n)) = noLoc $ HsTyLit (HsNumTy noSourceText n)
    go (LitTy (StrTyLit s)) = noLoc $ HsTyLit (HsStrTy noSourceText s)
    go ty@(TyConApp tc args)
      | any isInvisibleTyConBinder (tyConBinders tc)
        -- We must produce an explicit kind signature here to make certain
        -- programs kind-check. See Note [Kind signatures in typeToLHsType].
      = noLoc $ HsKindSig lhs_ty (go (typeKind ty))
      | otherwise = lhs_ty
       where
        lhs_ty = nlHsTyConApp (getRdrName tc) (map go args')
        args'  = filterOutInvisibleTypes tc args
    go (CastTy ty _)        = go ty
    go (CoercionTy co)      = pprPanic "toLHsSigWcType" (ppr co)

         -- Source-language types have _invisible_ kind arguments,
         -- so we must remove them here (Trac #8563)

    go_tv :: TyVar -> LHsTyVarBndr GhcPs
    go_tv tv = noLoc $ KindedTyVar (noLoc (getRdrName tv))
                                   (go (tyVarKind tv))

{-
Note [Kind signatures in typeToLHsType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are types that typeToLHsType can produce which require explicit kind
signatures in order to kind-check. Here is an example from Trac #14579:

  newtype Wat (x :: Proxy (a :: Type)) = MkWat (Maybe a) deriving Eq
  newtype Glurp a = MkGlurp (Wat ('Proxy :: Proxy a)) deriving Eq

The derived Eq instance for Glurp (without any kind signatures) would be:

  instance Eq a => Eq (Glurp a) where
    (==) = coerce @(Wat 'Proxy -> Wat 'Proxy -> Bool)
                  @(Glurp a    -> Glurp a    -> Bool)
                  (==)

(Where the visible type applications use types produced by typeToLHsType.)

The type 'Proxy has an underspecified kind, so we must ensure that
typeToLHsType ascribes it with its kind: ('Proxy :: Proxy a).

We must be careful not to produce too many kind signatures, or else
typeToLHsType can produce noisy types like
('Proxy :: Proxy (a :: (Type :: Type))). In pursuit of this goal, we adopt the
following criterion for choosing when to annotate types with kinds:

* If there is a tycon application with any invisible arguments, annotate
  the tycon application with its kind.

Why is this the right criterion? The problem we encountered earlier was the
result of an invisible argument (the `a` in ('Proxy :: Proxy a)) being
underspecified, so producing a kind signature for 'Proxy will catch this.
If there are no invisible arguments, then there is nothing to do, so we can
avoid polluting the result type with redundant noise.

What about a more complicated tycon, such as this?

  T :: forall {j} (a :: j). a -> Type

Unlike in the previous 'Proxy example, annotating an application of `T` to an
argument (e.g., annotating T ty to obtain (T ty :: Type)) will not fix
its invisible argument `j`. But because we apply this strategy recursively,
`j` will be fixed because the kind of `ty` will be fixed! That is to say,
something to the effect of (T (ty :: j) :: Type) will be produced.

This strategy certainly isn't foolproof, as tycons that contain type families
in their kind might break down. But we'd likely need visible kind application
to make those work.
-}

{- *********************************************************************
*                                                                      *
    --------- HsWrappers: type args, dict args, casts ---------
*                                                                      *
********************************************************************* -}

mkLHsWrap :: HsWrapper -> LHsExpr id -> LHsExpr id
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

-- Avoid (HsWrap co (HsWrap co' _)).
-- See Note [Detecting forced eta expansion] in DsExpr
mkHsWrap :: HsWrapper -> HsExpr id -> HsExpr id
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
mkHsWrap co_fn (HsWrap co_fn' e)       = mkHsWrap (co_fn <.> co_fn') e
mkHsWrap co_fn e                       = HsWrap co_fn e

mkHsWrapCo :: TcCoercionN   -- A Nominal coercion  a ~N b
           -> HsExpr id -> HsExpr id
mkHsWrapCo co e = mkHsWrap (mkWpCastN co) e

mkHsWrapCoR :: TcCoercionR   -- A Representational coercion  a ~R b
            -> HsExpr id -> HsExpr id
mkHsWrapCoR co e = mkHsWrap (mkWpCastR co) e

mkLHsWrapCo :: TcCoercionN -> LHsExpr id -> LHsExpr id
mkLHsWrapCo co (L loc e) = L loc (mkHsWrapCo co e)

mkHsCmdWrap :: HsWrapper -> HsCmd id -> HsCmd id
mkHsCmdWrap w cmd | isIdHsWrapper w = cmd
                  | otherwise       = HsCmdWrap w cmd

mkLHsCmdWrap :: HsWrapper -> LHsCmd id -> LHsCmd id
mkLHsCmdWrap w (L loc c) = L loc (mkHsCmdWrap w c)

mkHsWrapPat :: HsWrapper -> Pat id -> Type -> Pat id
mkHsWrapPat co_fn p ty | isIdHsWrapper co_fn = p
                       | otherwise           = CoPat co_fn p ty

mkHsWrapPatCo :: TcCoercionN -> Pat id -> Type -> Pat id
mkHsWrapPatCo co pat ty | isTcReflCo co = pat
                        | otherwise     = CoPat (mkWpCastN co) pat ty

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
                          , bind_fvs = placeHolderNames
                          , fun_tick = [] }

mkTopFunBind :: Origin -> Located Name -> [LMatch GhcRn (LHsExpr GhcRn)]
             -> HsBind GhcRn
-- In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn
                                    , fun_matches = mkMatchGroup origin ms
                                    , fun_co_fn = idHsWrapper
                                    , bind_fvs = emptyNameSet -- NB: closed
                                                              --     binding
                                    , fun_tick = [] }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
mkHsVarBind loc var rhs = mk_easy_FunBind loc var [] rhs

mkVarBind :: IdP p -> LHsExpr p -> LHsBind p
mkVarBind var rhs = L (getLoc rhs) $
                    VarBind { var_id = var, var_rhs = rhs, var_inline = False }

mkPatSynBind :: Located RdrName -> HsPatSynDetails (Located RdrName)
             -> LPat GhcPs -> HsPatSynDir GhcPs -> HsBind GhcPs
mkPatSynBind name details lpat dir = PatSynBind psb
  where
    psb = PSB{ psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir
             , psb_fvs = placeHolderNames }

-- |If any of the matches in the 'FunBind' are infix, the 'FunBind' is
-- considered infix.
isInfixFunBind :: HsBindLR id1 id2 -> Bool
isInfixFunBind (FunBind _ (MG matches _ _ _) _ _ _)
  = any (isInfixMatch . unLoc) (unLoc matches)
isInfixFunBind _ = False


------------
mk_easy_FunBind :: SrcSpan -> RdrName -> [LPat GhcPs]
                -> LHsExpr GhcPs -> LHsBind GhcPs
mk_easy_FunBind loc fun pats expr
  = L loc $ mkFunBind (L loc fun)
              [mkMatch (mkPrefixFunRhs (L loc fun)) pats expr
                       (noLoc emptyLocalBinds)]

-- | Make a prefix, non-strict function 'HsMatchContext'
mkPrefixFunRhs :: Located id -> HsMatchContext id
mkPrefixFunRhs n = FunRhs { mc_fun = n
                          , mc_fixity = Prefix
                          , mc_strictness = NoSrcStrict }

------------
mkMatch :: HsMatchContext (NameOrRdrName (IdP p)) -> [LPat p] -> LHsExpr p
        -> Located (HsLocalBinds p) -> LMatch p (LHsExpr p)
mkMatch ctxt pats expr lbinds
  = noLoc (Match { m_ctxt  = ctxt
                 , m_pats  = map paren pats
                 , m_grhss = GRHSs (unguardedRHS noSrcSpan expr) lbinds })
  where
    paren lp@(L l p) | hsPatNeedsParens p = L l (ParPat lp)
                     | otherwise          = lp

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
  | [L _ match] <- unLoc $ mg_alts matches
  , FunRhs{mc_strictness = SrcStrict} <- m_ctxt match
  = True
isBangedHsBind (PatBind {pat_lhs = pat})
  = isBangedLPat pat
isBangedHsBind _
  = False

collectLocalBinders :: HsLocalBindsLR idL idR -> [IdP idL]
collectLocalBinders (HsValBinds binds) = collectHsIdBinders binds
                                         -- No pattern synonyms here
collectLocalBinders (HsIPBinds _)      = []
collectLocalBinders EmptyLocalBinds    = []

collectHsIdBinders, collectHsValBinders :: HsValBindsLR idL idR -> [IdP idL]
-- Collect Id binders only, or Ids + pattern synonyms, respectively
collectHsIdBinders  = collect_hs_val_binders True
collectHsValBinders = collect_hs_val_binders False

collectHsBindBinders :: HsBindLR idL idR -> [IdP idL]
-- Collect both Ids and pattern-synonym binders
collectHsBindBinders b = collect_bind False b []

collectHsBindsBinders :: LHsBindsLR idL idR -> [IdP idL]
collectHsBindsBinders binds = collect_binds False binds []

collectHsBindListBinders :: [LHsBindLR idL idR] -> [IdP idL]
-- Same as collectHsBindsBinders, but works over a list of bindings
collectHsBindListBinders = foldr (collect_bind False . unLoc) []

collect_hs_val_binders :: Bool -> HsValBindsLR idL idR -> [IdP idL]
collect_hs_val_binders ps (ValBindsIn  binds _) = collect_binds     ps binds []
collect_hs_val_binders ps (ValBindsOut binds _) = collect_out_binds ps binds

collect_out_binds :: Bool -> [(RecFlag, LHsBinds p)] -> [IdP p]
collect_out_binds ps = foldr (collect_binds ps . snd) []

collect_binds :: Bool -> LHsBindsLR idL idR -> [IdP idL] -> [IdP idL]
-- Collect Ids, or Ids + pattern synonyms, depending on boolean flag
collect_binds ps binds acc = foldrBag (collect_bind ps . unLoc) acc binds

collect_bind :: Bool -> HsBindLR idL idR -> [IdP idL] -> [IdP idL]
collect_bind _ (PatBind { pat_lhs = p })           acc = collect_lpat p acc
collect_bind _ (FunBind { fun_id = L _ f })        acc = f : acc
collect_bind _ (VarBind { var_id = f })            acc = f : acc
collect_bind _ (AbsBinds { abs_exports = dbinds }) acc = map abe_poly dbinds ++ acc
        -- I don't think we want the binders from the abe_binds
        -- The only time we collect binders from a typechecked
        -- binding (hence see AbsBinds) is in zonking in TcHsSyn
collect_bind omitPatSyn (PatSynBind (PSB { psb_id = L _ ps })) acc
  | omitPatSyn                  = acc
  | otherwise                   = ps : acc

collectMethodBinders :: LHsBindsLR GhcPs idR -> [Located RdrName]
-- Used exclusively for the bindings of an instance decl which are all FunBinds
collectMethodBinders binds = foldrBag (get . unLoc) [] binds
  where
    get (FunBind { fun_id = f }) fs = f : fs
    get _                        fs = fs
       -- Someone else complains about non-FunBinds

----------------- Statements --------------------------
collectLStmtsBinders :: [LStmtLR idL idR body] -> [IdP idL]
collectLStmtsBinders = concatMap collectLStmtBinders

collectStmtsBinders :: [StmtLR idL idR body] -> [IdP idL]
collectStmtsBinders = concatMap collectStmtBinders

collectLStmtBinders :: LStmtLR idL idR body -> [IdP idL]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: StmtLR idL idR body -> [IdP idL]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt pat _ _ _ _)= collectPatBinders pat
collectStmtBinders (LetStmt (L _ binds)) = collectLocalBinders binds
collectStmtBinders (BodyStmt {})         = []
collectStmtBinders (LastStmt {})         = []
collectStmtBinders (ParStmt xs _ _ _) = collectLStmtsBinders
                                      $ [s | ParStmtBlock ss _ _ <- xs, s <- ss]
collectStmtBinders (TransStmt { trS_stmts = stmts }) = collectLStmtsBinders stmts
collectStmtBinders (RecStmt { recS_stmts = ss })     = collectLStmtsBinders ss
collectStmtBinders ApplicativeStmt{} = []


----------------- Patterns --------------------------
collectPatBinders :: LPat a -> [IdP a]
collectPatBinders pat = collect_lpat pat []

collectPatsBinders :: [LPat a] -> [IdP a]
collectPatsBinders pats = foldr collect_lpat [] pats

-------------
collect_lpat :: LPat pass -> [IdP pass] -> [IdP pass]
collect_lpat (L _ pat) bndrs
  = go pat
  where
    go (VarPat (L _ var))         = var : bndrs
    go (WildPat _)                = bndrs
    go (LazyPat pat)              = collect_lpat pat bndrs
    go (BangPat pat)              = collect_lpat pat bndrs
    go (AsPat (L _ a) pat)        = a : collect_lpat pat bndrs
    go (ViewPat _ pat _)          = collect_lpat pat bndrs
    go (ParPat  pat)              = collect_lpat pat bndrs

    go (ListPat pats _ _)         = foldr collect_lpat bndrs pats
    go (PArrPat pats _)           = foldr collect_lpat bndrs pats
    go (TuplePat pats _ _)        = foldr collect_lpat bndrs pats
    go (SumPat pat _ _ _)         = collect_lpat pat bndrs

    go (ConPatIn _ ps)            = foldr collect_lpat bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps})  = foldr collect_lpat bndrs (hsConPatArgs ps)
        -- See Note [Dictionary binders in ConPatOut]
    go (LitPat _)                 = bndrs
    go (NPat {})                  = bndrs
    go (NPlusKPat (L _ n) _ _ _ _ _)= n : bndrs

    go (SigPatIn pat _)           = collect_lpat pat bndrs
    go (SigPatOut pat _)          = collect_lpat pat bndrs

    go (SplicePat (HsSpliced _ (HsSplicedPat pat)))
                                  = go pat
    go (SplicePat _)              = bndrs
    go (CoPat _ pat _)            = go pat

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
    getSelectorNames (ns, fs) = map unLoc ns ++ map (selectorFieldOcc.unLoc) fs

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

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl { fdLName = L _ name } }))
  = ([L loc name], [])
hsLTyClDeclBinders (L loc (SynDecl     { tcdLName = L _ name })) = ([L loc name], [])
hsLTyClDeclBinders (L loc (ClassDecl   { tcdLName = L _ cls_name
                                       , tcdSigs = sigs, tcdATs = ats }))
  = (L loc cls_name :
     [ L fam_loc fam_name | L fam_loc (FamilyDecl { fdLName = L _ fam_name }) <- ats ] ++
     [ L mem_loc mem_name | L mem_loc (ClassOpSig False ns _) <- sigs, L _ mem_name <- ns ]
    , [])
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = L _ name, tcdDataDefn = defn }))
  = (\ (xs, ys) -> (L loc name : xs, ys)) $ hsDataDefnBinders defn

-------------------
hsForeignDeclsBinders :: [LForeignDecl pass] -> [Located (IdP pass)]
-- See Note [SrcSpan for binders]
hsForeignDeclsBinders foreign_decls
  = [ L decl_loc n
    | L decl_loc (ForeignImport { fd_name = L _ n }) <- foreign_decls]


-------------------
hsPatSynSelectors :: HsValBinds p -> [IdP p]
-- Collects record pattern-synonym selectors only; the pattern synonym
-- names are collected by collectHsValBinders.
hsPatSynSelectors (ValBindsIn _ _) = panic "hsPatSynSelectors"
hsPatSynSelectors (ValBindsOut binds _)
  = foldrBag addPatSynSelector [] . unionManyBags $ map snd binds

addPatSynSelector:: LHsBind p -> [IdP p] -> [IdP p]
addPatSynSelector bind sels
  | L _ (PatSynBind (PSB { psb_args = RecCon as })) <- bind
  = map (unLoc . recordPatSynSelectorId) as ++ sels
  | otherwise = sels

getPatSynBinds :: [(RecFlag, LHsBinds id)] -> [PatSynBind id id]
getPatSynBinds binds
  = [ psb | (_, lbinds) <- binds
          , L _ (PatSynBind psb) <- bagToList lbinds ]

-------------------
hsLInstDeclBinders :: LInstDecl pass
                   -> ([Located (IdP pass)], [LFieldOcc pass])
hsLInstDeclBinders (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = dfis } }))
  = foldMap (hsDataFamInstBinders . unLoc) dfis
hsLInstDeclBinders (L _ (DataFamInstD { dfid_inst = fi }))
  = hsDataFamInstBinders fi
hsLInstDeclBinders (L _ (TyFamInstD {})) = mempty

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataFamInstBinders :: DataFamInstDecl pass
                     -> ([Located (IdP pass)], [LFieldOcc pass])
hsDataFamInstBinders (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                       FamEqn { feqn_rhs = defn }}})
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataDefnBinders :: HsDataDefn pass -> ([Located (IdP pass)], [LFieldOcc pass])
hsDataDefnBinders (HsDataDefn { dd_cons = cons })
  = hsConDeclsBinders cons
  -- See Note [Binders in family instances]

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
      = case r of
           -- remove only the first occurrence of any seen field in order to
           -- avoid circumventing detection of duplicate fields (#9156)
           L loc (ConDeclGADT { con_names = names, con_args = args })
             -> (map (L loc . unLoc) names ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds remSeen args
                (ns, fs) = go remSeen' rs

           L loc (ConDeclH98 { con_name = name, con_args = args })
             -> ([L loc (unLoc name)] ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds remSeen args
                (ns, fs) = go remSeen' rs

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

lStmtsImplicits :: [LStmtLR GhcRn idR (Located (body idR))] -> NameSet
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: [LStmtLR GhcRn idR (Located (body idR))] -> NameSet
    hs_lstmts = foldr (\stmt rest -> unionNameSet (hs_stmt (unLoc stmt)) rest) emptyNameSet

    hs_stmt :: StmtLR GhcRn idR (Located (body idR)) -> NameSet
    hs_stmt (BindStmt pat _ _ _ _) = lPatImplicits pat
    hs_stmt (ApplicativeStmt args _ _) = unionNameSets (map do_arg args)
      where do_arg (_, ApplicativeArgOne pat _ _) = lPatImplicits pat
            do_arg (_, ApplicativeArgMany stmts _ _) = hs_lstmts stmts
    hs_stmt (LetStmt binds)      = hs_local_binds (unLoc binds)
    hs_stmt (BodyStmt {})        = emptyNameSet
    hs_stmt (LastStmt {})        = emptyNameSet
    hs_stmt (ParStmt xs _ _ _)   = hs_lstmts [s | ParStmtBlock ss _ _ <- xs, s <- ss]
    hs_stmt (TransStmt { trS_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (RecStmt { recS_stmts = ss })     = hs_lstmts ss

    hs_local_binds (HsValBinds val_binds) = hsValBindsImplicits val_binds
    hs_local_binds (HsIPBinds _)         = emptyNameSet
    hs_local_binds EmptyLocalBinds       = emptyNameSet

hsValBindsImplicits :: HsValBindsLR GhcRn idR -> NameSet
hsValBindsImplicits (ValBindsOut binds _)
  = foldr (unionNameSet . lhsBindsImplicits . snd) emptyNameSet binds
hsValBindsImplicits (ValBindsIn binds _)
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR GhcRn idR -> NameSet
lhsBindsImplicits = foldBag unionNameSet (lhs_bind . unLoc) emptyNameSet
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = emptyNameSet

lPatImplicits :: LPat GhcRn -> NameSet
lPatImplicits = hs_lpat
  where
    hs_lpat (L _ pat) = hs_pat pat

    hs_lpats = foldr (\pat rest -> hs_lpat pat `unionNameSet` rest) emptyNameSet

    hs_pat (LazyPat pat)       = hs_lpat pat
    hs_pat (BangPat pat)       = hs_lpat pat
    hs_pat (AsPat _ pat)       = hs_lpat pat
    hs_pat (ViewPat _ pat _)   = hs_lpat pat
    hs_pat (ParPat  pat)       = hs_lpat pat
    hs_pat (ListPat pats _ _)  = hs_lpats pats
    hs_pat (PArrPat pats _)    = hs_lpats pats
    hs_pat (TuplePat pats _ _) = hs_lpats pats

    hs_pat (SigPatIn pat _)  = hs_lpat pat
    hs_pat (SigPatOut pat _) = hs_lpat pat
    hs_pat (CoPat _ pat _)   = hs_pat pat

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
