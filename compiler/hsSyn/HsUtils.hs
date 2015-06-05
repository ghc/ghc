{-
(c) The University of Glasgow, 1992-2006


Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by     Module
   ----------------     -------------
   RdrName              parser/RdrHsSyn
   Name                 rename/RnHsSyn
   Id                   typecheck/TcHsSyn
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HsUtils(
  -- Terms
  mkHsPar, mkHsApp, mkHsConApp, mkSimpleHsAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkMatchGroupName, mkMatch, mkHsLam, mkHsIf,
  mkHsWrap, mkLHsWrap, mkHsWrapCo, mkHsWrapCoR, mkLHsWrapCo,
  coToHsWrapper, mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsComp, mkHsWrapPat, mkHsWrapPatCo,
  mkLHsPar, mkHsCmdCast,

  nlHsTyApp, nlHsTyApps, nlHsVar, nlHsLit, nlHsApp, nlHsApps, nlHsIntLit, nlHsVarApps,
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,
  toHsType, toHsKind,

  -- Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mk_easy_FunBind, mkTopFunBind, mkPatSynBind,

  -- Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString,

  -- Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConPat,
  nlConPatName, nlInfixConPat, nlNullaryConPat, nlWildConPat, nlWildPat,
  nlWildPatName, nlWildPatId, nlTuplePat, mkParPat,

  -- Types
  mkHsAppTy, userHsTyVarBndrs,
  nlHsAppTy, nlHsTyVar, nlHsFunTy, nlHsTyConApp,

  -- Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt, mkBindStmt, mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt,
  emptyRecStmt, emptyRecStmtName, emptyRecStmtId, mkRecStmt,

  -- Template Haskell
  mkHsSpliceTy, mkHsSpliceE, mkHsSpliceTE, mkHsSplice,
  mkHsQuasiQuote, unqualQuasiQuote,

  -- Flags
  noRebindableInfo,

  -- Collecting binders
  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsIdBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,
  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,

  hsLTyClDeclBinders, hsTyClForeignBinders, hsPatSynBinders,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,

  -- Collecting implicit binders
  lStmtsImplicits, hsValBindsImplicits, lPatImplicits
  ) where

#include "HsVersions.h"

import HsDecls
import HsBinds
import HsExpr
import HsPat
import HsTypes
import HsLit
import PlaceHolder

import TcEvidence
import RdrName
import Var
import TypeRep
import TcType
import Kind
import DataCon
import Name
import NameSet
import BasicTypes
import SrcLoc
import FastString
import Util
import Bag
import Outputable

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

mkSimpleMatch :: [LPat id] -> Located (body id) -> LMatch id (Located (body id))
mkSimpleMatch pats rhs
  = L loc $
    Match Nothing pats Nothing (unguardedGRHSs rhs)
  where
    loc = case pats of
                []      -> getLoc rhs
                (pat:_) -> combineSrcSpans (getLoc pat) (getLoc rhs)

unguardedGRHSs :: Located (body id) -> GRHSs id (Located (body id))
unguardedGRHSs rhs@(L loc _) = GRHSs (unguardedRHS loc rhs) emptyLocalBinds

unguardedRHS :: SrcSpan -> Located (body id) -> [LGRHS id (Located (body id))]
unguardedRHS loc rhs = [L loc (GRHS [] rhs)]

mkMatchGroup :: Origin -> [LMatch RdrName (Located (body RdrName))]
             -> MatchGroup RdrName (Located (body RdrName))
mkMatchGroup origin matches = MG { mg_alts = matches, mg_arg_tys = []
                                 , mg_res_ty = placeHolderType
                                 , mg_origin = origin }

mkMatchGroupName :: Origin -> [LMatch Name (Located (body Name))]
             -> MatchGroup Name (Located (body Name))
mkMatchGroupName origin matches = MG { mg_alts = matches, mg_arg_tys = []
                                     , mg_res_ty = placeHolderType
                                     , mg_origin = origin }

mkHsAppTy :: LHsType name -> LHsType name -> LHsType name
mkHsAppTy t1 t2 = addCLoc t1 t2 (HsAppTy t1 t2)

mkHsApp :: LHsExpr name -> LHsExpr name -> LHsExpr name
mkHsApp e1 e2 = addCLoc e1 e2 (HsApp e1 e2)

mkHsLam :: [LPat RdrName] -> LHsExpr RdrName -> LHsExpr RdrName
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam matches))
        where
          matches = mkMatchGroup Generated [mkSimpleMatch pats body]

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr Id -> LHsExpr Id
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars
                                       <.> mkWpLams dicts) expr

mkHsConApp :: DataCon -> [Type] -> [HsExpr Id] -> LHsExpr Id
-- Used for constructing dictionary terms etc, so no locations
mkHsConApp data_con tys args
  = foldl mk_app (nlHsTyApp (dataConWrapId data_con) tys) args
  where
    mk_app f a = noLoc (HsApp f (noLoc a))

mkSimpleHsAlt :: LPat id -> (Located (body id)) -> LMatch id (Located (body id))
-- A simple lambda with a single pattern, no binds, no guards; pre-typechecking
mkSimpleHsAlt pat expr
  = mkSimpleMatch [pat] expr

nlHsTyApp :: name -> [Type] -> LHsExpr name
nlHsTyApp fun_id tys = noLoc (HsWrap (mkWpTyApps tys) (HsVar fun_id))

nlHsTyApps :: name -> [Type] -> [LHsExpr name] -> LHsExpr name
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


-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: String -> Integer -> PostTc RdrName Type -> HsOverLit RdrName
mkHsFractional :: FractionalLit -> PostTc RdrName Type -> HsOverLit RdrName
mkHsIsString :: String -> FastString -> PostTc RdrName Type -> HsOverLit RdrName
mkHsDo         :: HsStmtContext Name -> [ExprLStmt RdrName] -> HsExpr RdrName
mkHsComp       :: HsStmtContext Name -> [ExprLStmt RdrName] -> LHsExpr RdrName
               -> HsExpr RdrName

mkNPat      :: Located (HsOverLit id) -> Maybe (SyntaxExpr id) -> Pat id
mkNPlusKPat :: Located id -> Located (HsOverLit id) -> Pat id

mkLastStmt :: Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))
mkBodyStmt :: Located (bodyR RdrName)
           -> StmtLR idL RdrName (Located (bodyR RdrName))
mkBindStmt :: LPat idL -> Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))

emptyRecStmt     :: StmtLR idL  RdrName bodyR
emptyRecStmtName :: StmtLR Name Name    bodyR
emptyRecStmtId   :: StmtLR Id   Id      bodyR
mkRecStmt    :: [LStmtLR idL RdrName bodyR] -> StmtLR idL RdrName bodyR


mkHsIntegral src i  = OverLit (HsIntegral   src i) noRebindableInfo noSyntaxExpr
mkHsFractional   f  = OverLit (HsFractional     f) noRebindableInfo noSyntaxExpr
mkHsIsString src s  = OverLit (HsIsString   src s) noRebindableInfo noSyntaxExpr

noRebindableInfo :: PlaceHolder
noRebindableInfo = PlaceHolder -- Just another placeholder;

mkHsDo ctxt stmts = HsDo ctxt stmts placeHolderType
mkHsComp ctxt stmts expr = mkHsDo ctxt (stmts ++ [last_stmt])
  where
    last_stmt = L (getLoc expr) $ mkLastStmt expr

mkHsIf :: LHsExpr id -> LHsExpr id -> LHsExpr id -> HsExpr id
mkHsIf c a b = HsIf (Just noSyntaxExpr) c a b

mkNPat lit neg     = NPat lit neg noSyntaxExpr
mkNPlusKPat id lit = NPlusKPat id lit noSyntaxExpr noSyntaxExpr

mkTransformStmt    :: [ExprLStmt idL] -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkTransformByStmt  :: [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupUsingStmt   :: [ExprLStmt idL]                -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupByUsingStmt :: [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)

emptyTransStmt :: StmtLR idL idR (LHsExpr idR)
emptyTransStmt = TransStmt { trS_form = panic "emptyTransStmt: form"
                           , trS_stmts = [], trS_bndrs = []
                           , trS_by = Nothing, trS_using = noLoc noSyntaxExpr
                           , trS_ret = noSyntaxExpr, trS_bind = noSyntaxExpr
                           , trS_fmap = noSyntaxExpr }
mkTransformStmt    ss u   = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  ss u b = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   ss u   = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt ss b u = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body     = LastStmt body noSyntaxExpr
mkBodyStmt body     = BodyStmt body noSyntaxExpr noSyntaxExpr placeHolderType
mkBindStmt pat body = BindStmt pat body noSyntaxExpr noSyntaxExpr


emptyRecStmt' :: forall idL idR body.
                       PostTc idR Type -> StmtLR idL idR body
emptyRecStmt' tyVal =
   RecStmt
     { recS_stmts = [], recS_later_ids = []
     , recS_rec_ids = []
     , recS_ret_fn = noSyntaxExpr
     , recS_mfix_fn = noSyntaxExpr
     , recS_bind_fn = noSyntaxExpr, recS_later_rets = []
     , recS_rec_rets = [], recS_ret_ty = tyVal }

emptyRecStmt     = emptyRecStmt' placeHolderType
emptyRecStmtName = emptyRecStmt' placeHolderType
emptyRecStmtId   = emptyRecStmt' placeHolderTypeTc
mkRecStmt stmts  = emptyRecStmt { recS_stmts = stmts }

-------------------------------
--- A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr id -> id -> LHsExpr id -> HsExpr id
mkHsOpApp e1 op e2 = OpApp e1 (noLoc (HsVar op)) (error "mkOpApp:fixity") e2

mkHsSplice :: LHsExpr RdrName -> HsSplice RdrName
mkHsSplice e = HsSplice unqualSplice e

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))

mkHsSpliceE :: LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceE e = HsSpliceE False (mkHsSplice e)

mkHsSpliceTE :: LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceTE e = HsSpliceE True (mkHsSplice e)

mkHsSpliceTy :: LHsExpr RdrName -> HsType RdrName
mkHsSpliceTy e = HsSpliceTy (mkHsSplice e) placeHolderKind

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsQuasiQuote RdrName
mkHsQuasiQuote quoter span quote = HsQuasiQuote quoter span quote

unqualQuasiQuote :: RdrName
unqualQuasiQuote = mkRdrUnqual (mkVarOccFS (fsLit "quasiquote"))
                -- A name (uniquified later) to
                -- identify the quasi-quote

mkHsString :: String -> HsLit
mkHsString s = HsString s (mkFastString s)

-------------
userHsTyVarBndrs :: SrcSpan -> [name] -> [Located (HsTyVarBndr name)]
-- Caller sets location
userHsTyVarBndrs loc bndrs = [ L loc (UserTyVar v) | v <- bndrs ]

{-
************************************************************************
*                                                                      *
        Constructing syntax with no location info
*                                                                      *
************************************************************************
-}

nlHsVar :: id -> LHsExpr id
nlHsVar n = noLoc (HsVar n)

nlHsLit :: HsLit -> LHsExpr id
nlHsLit n = noLoc (HsLit n)

nlVarPat :: id -> LPat id
nlVarPat n = noLoc (VarPat n)

nlLitPat :: HsLit -> LPat id
nlLitPat l = noLoc (LitPat l)

nlHsApp :: LHsExpr id -> LHsExpr id -> LHsExpr id
nlHsApp f x = noLoc (HsApp f x)

nlHsIntLit :: Integer -> LHsExpr id
nlHsIntLit n = noLoc (HsLit (HsInt (show n) n))

nlHsApps :: id -> [LHsExpr id] -> LHsExpr id
nlHsApps f xs = foldl nlHsApp (nlHsVar f) xs

nlHsVarApps :: id -> [id] -> LHsExpr id
nlHsVarApps f xs = noLoc (foldl mk (HsVar f) (map HsVar xs))
                 where
                   mk f a = HsApp (noLoc f) (noLoc a)

nlConVarPat :: RdrName -> [RdrName] -> LPat RdrName
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlInfixConPat :: id -> LPat id -> LPat id -> LPat id
nlInfixConPat con l r = noLoc (ConPatIn (noLoc con) (InfixCon l r))

nlConPat :: RdrName -> [LPat RdrName] -> LPat RdrName
nlConPat con pats = noLoc (ConPatIn (noLoc con) (PrefixCon pats))

nlConPatName :: Name -> [LPat Name] -> LPat Name
nlConPatName con pats = noLoc (ConPatIn (noLoc con) (PrefixCon pats))

nlNullaryConPat :: id -> LPat id
nlNullaryConPat con = noLoc (ConPatIn (noLoc con) (PrefixCon []))

nlWildConPat :: DataCon -> LPat RdrName
nlWildConPat con = noLoc (ConPatIn (noLoc (getRdrName con))
                         (PrefixCon (nOfThem (dataConSourceArity con)
                                             nlWildPat)))

nlWildPat :: LPat RdrName
nlWildPat  = noLoc (WildPat placeHolderType )  -- Pre-typechecking

nlWildPatName :: LPat Name
nlWildPatName  = noLoc (WildPat placeHolderType )  -- Pre-typechecking

nlWildPatId :: LPat Id
nlWildPatId  = noLoc (WildPat placeHolderTypeTc )  -- Post-typechecking

nlHsDo :: HsStmtContext Name -> [LStmt RdrName (LHsExpr RdrName)]
       -> LHsExpr RdrName
nlHsDo ctxt stmts = noLoc (mkHsDo ctxt stmts)

nlHsOpApp :: LHsExpr id -> id -> LHsExpr id -> LHsExpr id
nlHsOpApp e1 op e2 = noLoc (mkHsOpApp e1 op e2)

nlHsLam  :: LMatch RdrName (LHsExpr RdrName) -> LHsExpr RdrName
nlHsPar  :: LHsExpr id -> LHsExpr id
nlHsIf   :: LHsExpr id -> LHsExpr id -> LHsExpr id -> LHsExpr id
nlHsCase :: LHsExpr RdrName -> [LMatch RdrName (LHsExpr RdrName)]
         -> LHsExpr RdrName
nlList   :: [LHsExpr RdrName] -> LHsExpr RdrName

nlHsLam match          = noLoc (HsLam (mkMatchGroup Generated [match]))
nlHsPar e              = noLoc (HsPar e)
nlHsIf cond true false = noLoc (mkHsIf cond true false)
nlHsCase expr matches  = noLoc (HsCase expr (mkMatchGroup Generated matches))
nlList exprs           = noLoc (ExplicitList placeHolderType Nothing exprs)

nlHsAppTy :: LHsType name -> LHsType name -> LHsType name
nlHsTyVar :: name                         -> LHsType name
nlHsFunTy :: LHsType name -> LHsType name -> LHsType name

nlHsAppTy f t           = noLoc (HsAppTy f t)
nlHsTyVar x             = noLoc (HsTyVar x)
nlHsFunTy a b           = noLoc (HsFunTy a b)

nlHsTyConApp :: name -> [LHsType name] -> LHsType name
nlHsTyConApp tycon tys  = foldl nlHsAppTy (nlHsTyVar tycon) tys

{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

mkLHsTupleExpr :: [LHsExpr a] -> LHsExpr a
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] = e
mkLHsTupleExpr es  = noLoc $ ExplicitTuple (map (noLoc . Present) es) Boxed

mkLHsVarTuple :: [a] -> LHsExpr a
mkLHsVarTuple ids  = mkLHsTupleExpr (map nlHsVar ids)

nlTuplePat :: [LPat id] -> Boxity -> LPat id
nlTuplePat pats box = noLoc (TuplePat pats box [])

missingTupArg :: HsTupArg RdrName
missingTupArg = Missing placeHolderType

{-
************************************************************************
*                                                                      *
        Converting a Type to an HsType RdrName
*                                                                      *
************************************************************************

This is needed to implement GeneralizedNewtypeDeriving.
-}

toHsType :: Type -> LHsType RdrName
toHsType ty
  | [] <- tvs_only
  , [] <- theta
  = to_hs_type tau
  | otherwise
  = noLoc $
    mkExplicitHsForAllTy (map mk_hs_tvb tvs_only)
                         (noLoc $ map toHsType theta)
                         (to_hs_type tau)

  where
    (tvs, theta, tau) = tcSplitSigmaTy ty
    tvs_only = filter isTypeVar tvs

    to_hs_type (TyVarTy tv) = nlHsTyVar (getRdrName tv)
    to_hs_type (AppTy t1 t2) = nlHsAppTy (toHsType t1) (toHsType t2)
    to_hs_type (TyConApp tc args) = nlHsTyConApp (getRdrName tc) (map toHsType args')
       where
         args' = filterOut isKind args
         -- Source-language types have _implicit_ kind arguments,
         -- so we must remove them here (Trac #8563)
    to_hs_type (FunTy arg res) = ASSERT( not (isConstraintKind (typeKind arg)) )
                                 nlHsFunTy (toHsType arg) (toHsType res)
    to_hs_type t@(ForAllTy {}) = pprPanic "toHsType" (ppr t)
    to_hs_type (LitTy (NumTyLit n)) = noLoc $ HsTyLit (HsNumTy "" n)
    to_hs_type (LitTy (StrTyLit s)) = noLoc $ HsTyLit (HsStrTy "" s)

    mk_hs_tvb tv = noLoc $ KindedTyVar (noLoc (getRdrName tv))
                                       (toHsKind (tyVarKind tv))

toHsKind :: Kind -> LHsKind RdrName
toHsKind = toHsType

--------- HsWrappers: type args, dict args, casts ---------
mkLHsWrap :: HsWrapper -> LHsExpr id -> LHsExpr id
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

mkHsWrap :: HsWrapper -> HsExpr id -> HsExpr id
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
                 | otherwise           = HsWrap co_fn e

mkHsWrapCo :: TcCoercion   -- A Nominal coercion  a ~N b
           -> HsExpr id -> HsExpr id
mkHsWrapCo co e = mkHsWrap (coToHsWrapper co) e

mkHsWrapCoR :: TcCoercion   -- A Representational coercion  a ~R b
            -> HsExpr id -> HsExpr id
mkHsWrapCoR co e = mkHsWrap (coToHsWrapperR co) e

mkLHsWrapCo :: TcCoercion -> LHsExpr id -> LHsExpr id
mkLHsWrapCo co (L loc e) = L loc (mkHsWrapCo co e)

mkHsCmdCast :: TcCoercion -> HsCmd id -> HsCmd id
mkHsCmdCast co cmd | isTcReflCo co = cmd
                   | otherwise     = HsCmdCast co cmd

coToHsWrapper :: TcCoercion -> HsWrapper   -- A Nominal coercion
coToHsWrapper co | isTcReflCo co = idHsWrapper
                 | otherwise     = mkWpCast (mkTcSubCo co)

coToHsWrapperR :: TcCoercion -> HsWrapper   -- A Representational coercion
coToHsWrapperR co | isTcReflCo co = idHsWrapper
                  | otherwise     = mkWpCast co

mkHsWrapPat :: HsWrapper -> Pat id -> Type -> Pat id
mkHsWrapPat co_fn p ty | isIdHsWrapper co_fn = p
                       | otherwise           = CoPat co_fn p ty

-- input coercion is Nominal
mkHsWrapPatCo :: TcCoercion -> Pat id -> Type -> Pat id
mkHsWrapPatCo co pat ty | isTcReflCo co = pat
                        | otherwise     = CoPat (mkWpCast (mkTcSubCo co)) pat ty

mkHsDictLet :: TcEvBinds -> LHsExpr Id -> LHsExpr Id
mkHsDictLet ev_binds expr = mkLHsWrap (mkWpLet ev_binds) expr

{-
l
************************************************************************
*                                                                      *
                Bindings; with a location at the top
*                                                                      *
************************************************************************
-}

mkFunBind :: Located RdrName -> [LMatch RdrName (LHsExpr RdrName)]
          -> HsBind RdrName
-- Not infix, with place holders for coercion and free vars
mkFunBind fn ms = FunBind { fun_id = fn, fun_infix = False
                          , fun_matches = mkMatchGroup Generated ms
                          , fun_co_fn = idHsWrapper
                          , bind_fvs = placeHolderNames
                          , fun_tick = [] }

mkTopFunBind :: Origin -> Located Name -> [LMatch Name (LHsExpr Name)]
             -> HsBind Name
-- In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn, fun_infix = False
                                    , fun_matches = mkMatchGroupName origin ms
                                    , fun_co_fn = idHsWrapper
                                    , bind_fvs = emptyNameSet -- NB: closed
                                                              --     binding
                                    , fun_tick = [] }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr RdrName -> LHsBind RdrName
mkHsVarBind loc var rhs = mk_easy_FunBind loc var [] rhs

mkVarBind :: id -> LHsExpr id -> LHsBind id
mkVarBind var rhs = L (getLoc rhs) $
                    VarBind { var_id = var, var_rhs = rhs, var_inline = False }

mkPatSynBind :: Located RdrName -> HsPatSynDetails (Located RdrName)
             -> LPat RdrName -> HsPatSynDir RdrName -> HsBind RdrName
mkPatSynBind name details lpat dir = PatSynBind psb
  where
    psb = PSB{ psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir
             , psb_fvs = placeHolderNames }

------------
mk_easy_FunBind :: SrcSpan -> RdrName -> [LPat RdrName]
                -> LHsExpr RdrName -> LHsBind RdrName
mk_easy_FunBind loc fun pats expr
  = L loc $ mkFunBind (L loc fun) [mkMatch pats expr emptyLocalBinds]

------------
mkMatch :: [LPat id] -> LHsExpr id -> HsLocalBinds id -> LMatch id (LHsExpr id)
mkMatch pats expr binds
  = noLoc (Match Nothing (map paren pats) Nothing
                 (GRHSs (unguardedRHS noSrcSpan expr) binds))
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
-}

----------------- Bindings --------------------------
collectLocalBinders :: HsLocalBindsLR idL idR -> [idL]
collectLocalBinders (HsValBinds binds) = collectHsIdBinders binds
                                         -- No pattern synonyms here
collectLocalBinders (HsIPBinds _)      = []
collectLocalBinders EmptyLocalBinds    = []

collectHsIdBinders, collectHsValBinders :: HsValBindsLR idL idR -> [idL]
-- Collect Id binders only, or Ids + pattern synonmys, respectively
collectHsIdBinders  = collect_hs_val_binders True
collectHsValBinders = collect_hs_val_binders False

collectHsBindBinders :: HsBindLR idL idR -> [idL]
-- Collect both Ids and pattern-synonym binders
collectHsBindBinders b = collect_bind False b []

collectHsBindsBinders :: LHsBindsLR idL idR -> [idL]
collectHsBindsBinders binds = collect_binds False binds []

collectHsBindListBinders :: [LHsBindLR idL idR] -> [idL]
-- Same as collectHsBindsBinders, but works over a list of bindings
collectHsBindListBinders = foldr (collect_bind False . unLoc) []

collect_hs_val_binders :: Bool -> HsValBindsLR idL idR -> [idL]
collect_hs_val_binders ps (ValBindsIn  binds _) = collect_binds     ps binds []
collect_hs_val_binders ps (ValBindsOut binds _) = collect_out_binds ps binds

collect_out_binds :: Bool -> [(RecFlag, LHsBinds id)] -> [id]
collect_out_binds ps = foldr (collect_binds ps . snd) []

collect_binds :: Bool -> LHsBindsLR idL idR -> [idL] -> [idL]
-- Collect Ids, or Ids + patter synonyms, depending on boolean flag
collect_binds ps binds acc = foldrBag (collect_bind ps . unLoc) acc binds

collect_bind :: Bool -> HsBindLR idL idR -> [idL] -> [idL]
collect_bind _ (PatBind { pat_lhs = p })           acc = collect_lpat p acc
collect_bind _ (FunBind { fun_id = L _ f })        acc = f : acc
collect_bind _ (VarBind { var_id = f })            acc = f : acc
collect_bind _ (AbsBinds { abs_exports = dbinds }) acc = map abe_poly dbinds ++ acc
        -- I don't think we want the binders from the abe_binds
        -- The only time we collect binders from a typechecked
        -- binding (hence see AbsBinds) is in zonking in TcHsSyn
collect_bind omitPatSyn (PatSynBind (PSB { psb_id = L _ ps })) acc =
    if omitPatSyn then acc else ps : acc

collectMethodBinders :: LHsBindsLR RdrName idR -> [Located RdrName]
-- Used exclusively for the bindings of an instance decl which are all FunBinds
collectMethodBinders binds = foldrBag (get . unLoc) [] binds
  where
    get (FunBind { fun_id = f }) fs = f : fs
    get _                        fs = fs
       -- Someone else complains about non-FunBinds

----------------- Statements --------------------------
collectLStmtsBinders :: [LStmtLR idL idR body] -> [idL]
collectLStmtsBinders = concatMap collectLStmtBinders

collectStmtsBinders :: [StmtLR idL idR body] -> [idL]
collectStmtsBinders = concatMap collectStmtBinders

collectLStmtBinders :: LStmtLR idL idR body -> [idL]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: StmtLR idL idR body -> [idL]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt pat _ _ _) = collectPatBinders pat
collectStmtBinders (LetStmt binds)      = collectLocalBinders binds
collectStmtBinders (BodyStmt {})        = []
collectStmtBinders (LastStmt {})        = []
collectStmtBinders (ParStmt xs _ _)     = collectLStmtsBinders
                                        $ [s | ParStmtBlock ss _ _ <- xs, s <- ss]
collectStmtBinders (TransStmt { trS_stmts = stmts }) = collectLStmtsBinders stmts
collectStmtBinders (RecStmt { recS_stmts = ss })     = collectLStmtsBinders ss


----------------- Patterns --------------------------
collectPatBinders :: LPat a -> [a]
collectPatBinders pat = collect_lpat pat []

collectPatsBinders :: [LPat a] -> [a]
collectPatsBinders pats = foldr collect_lpat [] pats

-------------
collect_lpat :: LPat name -> [name] -> [name]
collect_lpat (L _ pat) bndrs
  = go pat
  where
    go (VarPat var)               = var : bndrs
    go (WildPat _)                = bndrs
    go (LazyPat pat)              = collect_lpat pat bndrs
    go (BangPat pat)              = collect_lpat pat bndrs
    go (AsPat (L _ a) pat)        = a : collect_lpat pat bndrs
    go (ViewPat _ pat _)          = collect_lpat pat bndrs
    go (ParPat  pat)              = collect_lpat pat bndrs

    go (ListPat pats _ _)         = foldr collect_lpat bndrs pats
    go (PArrPat pats _)           = foldr collect_lpat bndrs pats
    go (TuplePat pats _ _)        = foldr collect_lpat bndrs pats

    go (ConPatIn _ ps)            = foldr collect_lpat bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps})  = foldr collect_lpat bndrs (hsConPatArgs ps)
        -- See Note [Dictionary binders in ConPatOut]
    go (LitPat _)                 = bndrs
    go (NPat _ _ _)               = bndrs
    go (NPlusKPat (L _ n) _ _ _)  = n : bndrs

    go (SigPatIn pat _)           = collect_lpat pat bndrs
    go (SigPatOut pat _)          = collect_lpat pat bndrs
    go (SplicePat _)              = bndrs
    go (QuasiQuotePat _)          = bndrs
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

hsGroupBinders :: HsGroup Name -> [Name]
hsGroupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
                          hs_instds = inst_decls, hs_fords = foreign_decls })
  =  collectHsValBinders val_decls
  ++ hsTyClForeignBinders tycl_decls inst_decls foreign_decls

hsTyClForeignBinders :: [TyClGroup Name] -> [LInstDecl Name]
                     -> [LForeignDecl Name] -> [Name]
-- We need to look at instance declarations too,
-- because their associated types may bind data constructors
hsTyClForeignBinders tycl_decls inst_decls foreign_decls
  = map unLoc $
    hsForeignDeclsBinders foreign_decls ++
    concatMap (concatMap hsLTyClDeclBinders . group_tyclds) tycl_decls ++
    concatMap hsLInstDeclBinders inst_decls

-------------------
hsLTyClDeclBinders :: Eq name => Located (TyClDecl name) -> [Located name]
-- ^ Returns all the /binding/ names of the decl.
-- The first one is guaranteed to be the name of the decl. For record fields
-- mentioned in multiple constructors, the SrcLoc will be from the first
-- occurrence.  We use the equality to filter out duplicate field names.
--
-- Each returned (Located name) has a SrcSpan for the /whole/ declaration.
-- See Note [SrcSpan for binders]

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl { fdLName = L _ name } }))
  = [L loc name]
hsLTyClDeclBinders (L loc (SynDecl     { tcdLName = L _ name })) = [L loc name]
hsLTyClDeclBinders (L loc (ClassDecl   { tcdLName = L _ cls_name
                                       , tcdSigs = sigs, tcdATs = ats }))
  = L loc cls_name :
    [ L fam_loc fam_name | L fam_loc (FamilyDecl { fdLName = L _ fam_name }) <- ats ] ++
    [ L mem_loc mem_name | L mem_loc (TypeSig ns _ _) <- sigs, L _ mem_name <- ns ]
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = L _ name, tcdDataDefn = defn }))
  = L loc name : hsDataDefnBinders defn

-------------------
hsForeignDeclsBinders :: [LForeignDecl name] -> [Located name]
-- See Note [SrcSpan for binders]
hsForeignDeclsBinders foreign_decls
  = [ L decl_loc n
    | L decl_loc (ForeignImport (L _ n) _ _ _) <- foreign_decls]

-------------------
hsPatSynBinders :: LHsBindsLR idL idR -> [Located idL]
-- Collect pattern-synonym binders only, not Ids
-- See Note [SrcSpan for binders]
hsPatSynBinders binds = foldrBag addPatSynBndr [] binds

addPatSynBndr :: LHsBindLR idL idR -> [Located idL] -> [Located idL]
-- See Note [SrcSpan for binders]
addPatSynBndr bind pss
  | L bind_loc (PatSynBind (PSB { psb_id = L _ n })) <- bind
  = L bind_loc n : pss
  | otherwise
  = pss

-------------------
hsLInstDeclBinders :: Eq name => LInstDecl name -> [Located name]
hsLInstDeclBinders (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = dfis } }))
  = concatMap (hsDataFamInstBinders . unLoc) dfis
hsLInstDeclBinders (L _ (DataFamInstD { dfid_inst = fi }))
  = hsDataFamInstBinders fi
hsLInstDeclBinders (L _ (TyFamInstD {})) = []

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataFamInstBinders :: Eq name => DataFamInstDecl name -> [Located name]
hsDataFamInstBinders (DataFamInstDecl { dfid_defn = defn })
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataDefnBinders :: Eq name => HsDataDefn name -> [Located name]
hsDataDefnBinders (HsDataDefn { dd_cons = cons })
  = hsConDeclsBinders cons
  -- See Note [Binders in family instances]

-------------------
hsConDeclsBinders :: forall name. (Eq name) => [LConDecl name] -> [Located name]
  -- See hsLTyClDeclBinders for what this does
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons = go id cons
  where go :: ([Located name] -> [Located name]) -> [LConDecl name] -> [Located name]
        go _ [] = []
        go remSeen (r:rs) =
          -- don't re-mangle the location of field names, because we don't
          -- have a record of the full location of the field declaration anyway
          case r of
             -- remove only the first occurrence of any seen field in order to
             -- avoid circumventing detection of duplicate fields (#9156)
             L loc (ConDecl { con_names = names, con_details = RecCon flds }) ->
               (map (L loc . unLoc) names) ++ r' ++ go remSeen' rs
                  where r' = remSeen (concatMap (cd_fld_names . unLoc)
                                                (unLoc flds))
                        remSeen' = foldr (.) remSeen [deleteBy ((==) `on` unLoc) v | v <- r']
             L loc (ConDecl { con_names = names }) ->
                (map (L loc . unLoc) names) ++ go remSeen rs

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

lStmtsImplicits :: [LStmtLR Name idR (Located (body idR))] -> NameSet
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: [LStmtLR Name idR (Located (body idR))] -> NameSet
    hs_lstmts = foldr (\stmt rest -> unionNameSet (hs_stmt (unLoc stmt)) rest) emptyNameSet

    hs_stmt (BindStmt pat _ _ _) = lPatImplicits pat
    hs_stmt (LetStmt binds)      = hs_local_binds binds
    hs_stmt (BodyStmt {})        = emptyNameSet
    hs_stmt (LastStmt {})        = emptyNameSet
    hs_stmt (ParStmt xs _ _)     = hs_lstmts [s | ParStmtBlock ss _ _ <- xs, s <- ss]
    hs_stmt (TransStmt { trS_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (RecStmt { recS_stmts = ss })     = hs_lstmts ss

    hs_local_binds (HsValBinds val_binds) = hsValBindsImplicits val_binds
    hs_local_binds (HsIPBinds _)         = emptyNameSet
    hs_local_binds EmptyLocalBinds       = emptyNameSet

hsValBindsImplicits :: HsValBindsLR Name idR -> NameSet
hsValBindsImplicits (ValBindsOut binds _)
  = foldr (unionNameSet . lhsBindsImplicits . snd) emptyNameSet binds
hsValBindsImplicits (ValBindsIn binds _)
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR Name idR -> NameSet
lhsBindsImplicits = foldBag unionNameSet (lhs_bind . unLoc) emptyNameSet
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = emptyNameSet

lPatImplicits :: LPat Name -> NameSet
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
