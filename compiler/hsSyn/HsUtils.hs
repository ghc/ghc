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
{-# LANGUAGE TypeFamilies #-}

module HsUtils(
  -- Terms
  mkHsPar, mkHsApp, mkHsAppType, mkHsAppTypeOut, mkHsCaseAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkMatch, mkHsLam, mkHsIf,
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
  isUnliftedHsBind,

  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsIdBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,
  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,

  hsLTyClDeclBinders, hsTyClForeignBinders,
  hsPatSynSelectors, getPatSynBinds,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,
  hsDataDefnBinders,

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
import HsEmbellished

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

mkSimpleMatch :: HsMatchContext (NameOrRdrName id)
              -> [LPat id] -> Located (body id)
              -> LMatch id (Located (body id))
mkSimpleMatch ctxt pats rhs
  = L loc $
    Match ctxt pats Nothing (unguardedGRHSs rhs)
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

mkHsAppTypeOut :: LHsExpr Id -> LHsWcType Name -> LHsExpr Id
mkHsAppTypeOut e t = addCLoc e (hswc_body t) (HsAppTypeOut e t)

mkHsLam :: [LPat RdrName] -> LHsExpr RdrName -> LHsExpr RdrName
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam matches))
  where
    matches = mkMatchGroup Generated
                           [mkSimpleMatch LambdaExpr pats body]

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr Id -> LHsExpr Id
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars
                                       <.> mkWpLams dicts) expr

-- |A simple case alternative with a single pattern, no binds, no guards;
-- pre-typechecking
mkHsCaseAlt :: LPat id -> (Located (body id)) -> LMatch id (Located (body id))
mkHsCaseAlt pat expr
  = mkSimpleMatch CaseAlt [pat] expr

nlHsTyApp :: name -> [Type] -> LHsExpr name
nlHsTyApp fun_id tys = noLoc (HsWrap (mkWpTyApps tys) (HsVar (noEmb fun_id)))

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

nlParPat :: LPat name -> LPat name
nlParPat p = noLoc (ParPat p)

-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: SourceText -> Integer -> PostTc RdrName Type
               -> HsOverLit RdrName
mkHsFractional :: FractionalLit -> PostTc RdrName Type -> HsOverLit RdrName
mkHsIsString :: SourceText -> FastString -> PostTc RdrName Type
             -> HsOverLit RdrName
mkHsDo         :: HsStmtContext Name -> [ExprLStmt RdrName] -> HsExpr RdrName
mkHsComp       :: HsStmtContext Name -> [ExprLStmt RdrName] -> LHsExpr RdrName
               -> HsExpr RdrName

mkNPat      :: Located (HsOverLit RdrName) -> Maybe (SyntaxExpr RdrName) -> Pat RdrName
mkNPlusKPat :: Located RdrName -> Located (HsOverLit RdrName) -> Pat RdrName

mkLastStmt :: Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))
mkBodyStmt :: Located (bodyR RdrName)
           -> StmtLR idL RdrName (Located (bodyR RdrName))
mkBindStmt :: (PostTc idR Type ~ PlaceHolder)
           => LPat idL -> Located (bodyR idR)
           -> StmtLR idL idR (Located (bodyR idR))
mkTcBindStmt :: LPat Id -> Located (bodyR Id) -> StmtLR Id Id (Located (bodyR Id))

emptyRecStmt     :: StmtLR idL  RdrName bodyR
emptyRecStmtName :: StmtLR Name Name    bodyR
emptyRecStmtId   :: StmtLR Id   Id      bodyR
mkRecStmt    :: [LStmtLR idL RdrName bodyR] -> StmtLR idL RdrName bodyR


mkHsIntegral src i  = OverLit (HsIntegral   src i) noRebindableInfo noExpr
mkHsFractional   f  = OverLit (HsFractional     f) noRebindableInfo noExpr
mkHsIsString src s  = OverLit (HsIsString   src s) noRebindableInfo noExpr

noRebindableInfo :: PlaceHolder
noRebindableInfo = PlaceHolder -- Just another placeholder;

mkHsDo ctxt stmts = HsDo ctxt (mkLocatedList stmts) placeHolderType
mkHsComp ctxt stmts expr = mkHsDo ctxt (stmts ++ [last_stmt])
  where
    last_stmt = L (getLoc expr) $ mkLastStmt expr

mkHsIf :: LHsExpr id -> LHsExpr id -> LHsExpr id -> HsExpr id
mkHsIf c a b = HsIf (Just noSyntaxExpr) c a b

mkNPat lit neg     = NPat lit neg noSyntaxExpr placeHolderType
mkNPlusKPat id lit = NPlusKPat id lit (unLoc lit) noSyntaxExpr noSyntaxExpr placeHolderType

mkTransformStmt    :: (PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL] -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkTransformByStmt  :: (PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupUsingStmt   :: (PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL]                -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupByUsingStmt :: (PostTc idR Type ~ PlaceHolder)
                   => [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)

emptyTransStmt :: (PostTc idR Type ~ PlaceHolder) => StmtLR idL idR (LHsExpr idR)
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

emptyRecStmt' :: forall idL idR body.
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
mkHsOpApp :: LHsExpr id -> id -> LHsExpr id -> HsExpr id
mkHsOpApp e1 op e2 = OpApp e1 (noLoc (HsVar (noEmb op)))
                           (error "mkOpApp:fixity") e2

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))

mkUntypedSplice :: HasParens -> LHsExpr RdrName -> HsSplice RdrName
mkUntypedSplice hasParen e = HsUntypedSplice hasParen unqualSplice e

mkHsSpliceE :: HasParens -> LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceE hasParen e = HsSpliceE (mkUntypedSplice hasParen e)

mkHsSpliceTE :: HasParens -> LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceTE hasParen e = HsSpliceE (HsTypedSplice hasParen unqualSplice e)

mkHsSpliceTy :: HasParens -> LHsExpr RdrName -> HsType RdrName
mkHsSpliceTy hasParen e
  = HsSpliceTy (HsUntypedSplice hasParen unqualSplice e) placeHolderKind

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsSplice RdrName
mkHsQuasiQuote quoter span quote = HsQuasiQuote unqualSplice quoter span quote

unqualQuasiQuote :: RdrName
unqualQuasiQuote = mkRdrUnqual (mkVarOccFS (fsLit "quasiquote"))
                -- A name (uniquified later) to
                -- identify the quasi-quote

mkHsString :: String -> HsLit
mkHsString s = HsString NoSourceText (mkFastString s)

mkHsStringPrimLit :: FastString -> HsLit
mkHsStringPrimLit fs
  = HsStringPrim NoSourceText (fastStringToByteString fs)

-------------
userHsLTyVarBndrs :: SrcSpan -> [Located name] -> [LHsTyVarBndr name]
-- Caller sets location
userHsLTyVarBndrs loc bndrs = [ L loc (UserTyVar v) | v <- bndrs ]

userHsTyVarBndrs :: SrcSpan -> [name] -> [LHsTyVarBndr name]
-- Caller sets location
userHsTyVarBndrs loc bndrs = [ L loc (UserTyVar (L loc v)) | v <- bndrs ]


{-
************************************************************************
*                                                                      *
        Constructing syntax with no location info
*                                                                      *
************************************************************************
-}

nlHsVar :: id -> LHsExpr id
nlHsVar n = noLoc (HsVar (noEmb n))

-- NB: Only for LHsExpr **Id**
nlHsDataCon :: DataCon -> LHsExpr Id
nlHsDataCon con = noLoc (HsConLikeOut (RealDataCon con))

nlHsLit :: HsLit -> LHsExpr id
nlHsLit n = noLoc (HsLit n)

nlVarPat :: id -> LPat id
nlVarPat n = noLoc (VarPat (noLoc n))

nlLitPat :: HsLit -> LPat id
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

nlHsIntLit :: Integer -> LHsExpr id
nlHsIntLit n = noLoc (HsLit (HsInt NoSourceText n))

nlHsApps :: id -> [LHsExpr id] -> LHsExpr id
nlHsApps f xs = foldl nlHsApp (nlHsVar f) xs

nlHsVarApps :: id -> [id] -> LHsExpr id
nlHsVarApps f xs = noLoc (foldl mk (HsVar (noEmb f)) (map (HsVar . noEmb) xs))
                 where
                   mk f a = HsApp (noLoc f) (noLoc a)

nlConVarPat :: RdrName -> [RdrName] -> LPat RdrName
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlConVarPatName :: Name -> [Name] -> LPat Name
nlConVarPatName con vars = nlConPatName con (map nlVarPat vars)

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

-- Note [Rebindable nlHsIf]
-- nlHsIf should generate if-expressions which are NOT subject to
-- RebindableSyntax, so the first field of HsIf is Nothing. (#12080)
nlHsIf cond true false = noLoc (HsIf Nothing cond true false)

nlHsCase expr matches  = noLoc (HsCase expr (mkMatchGroup Generated matches))
nlList exprs           = noLoc (ExplicitList placeHolderType Nothing exprs)

nlHsAppTy :: LHsType name -> LHsType name -> LHsType name
nlHsTyVar :: name                         -> LHsType name
nlHsFunTy :: LHsType name -> LHsType name -> LHsType name
nlHsParTy :: LHsType name                 -> LHsType name

nlHsAppTy f t           = noLoc (HsAppTy f t)
nlHsTyVar x             = noLoc (HsTyVar NotPromoted (noEmb x))
nlHsFunTy a b           = noLoc (HsFunTy a b)
nlHsParTy t             = noLoc (HsParTy t)

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

mkLHsPatTup :: [LPat id] -> LPat id
mkLHsPatTup []     = noLoc $ TuplePat [] Boxed []
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = L (getLoc (head lpats)) $ TuplePat lpats Boxed []

-- The Big equivalents for the source tuple expressions
mkBigLHsVarTup :: [id] -> LHsExpr id
mkBigLHsVarTup ids = mkBigLHsTup (map nlHsVar ids)

mkBigLHsTup :: [LHsExpr id] -> LHsExpr id
mkBigLHsTup = mkChunkified mkLHsTupleExpr

-- The Big equivalents for the source tuple patterns
mkBigLHsVarPatTup :: [id] -> LPat id
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

mkLHsSigType :: LHsType RdrName -> LHsSigType RdrName
mkLHsSigType ty = mkHsImplicitBndrs ty

mkLHsSigWcType :: LHsType RdrName -> LHsSigWcType RdrName
mkLHsSigWcType ty = mkHsWildCardBndrs (mkHsImplicitBndrs ty)

mkHsSigEnv :: forall a. (LSig Name -> Maybe ([Located Name], a))
                     -> [LSig Name]
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

    mk_pairs :: [LSig Name] -> [(Name, a)]
    mk_pairs sigs = [ (n,a) | Just (ns,a) <- map get_info sigs
                            , L _ n <- ns ]

mkClassOpSigs :: [LSig RdrName] -> [LSig RdrName]
-- Convert TypeSig to ClassOpSig
-- The former is what is parsed, but the latter is
-- what we need in class/instance declarations
mkClassOpSigs sigs
  = map fiddle sigs
  where
    fiddle (L loc (TypeSig nms ty)) = L loc (ClassOpSig False nms (dropWildCards ty))
    fiddle sig                      = sig

typeToLHsType :: Type -> LHsType RdrName
-- ^ Converting a Type to an HsType RdrName
-- This is needed to implement GeneralizedNewtypeDeriving.
--
-- Note that we use 'getRdrName' extensively, which
-- generates Exact RdrNames rather than strings.
typeToLHsType ty
  = go ty
  where
    go :: Type -> LHsType RdrName
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
    go (LitTy (NumTyLit n)) = noLoc $ HsTyLit (HsNumTy NoSourceText n)
    go (LitTy (StrTyLit s)) = noLoc $ HsTyLit (HsStrTy NoSourceText s)
    go (TyConApp tc args)   = nlHsTyConApp (getRdrName tc) (map go args')
       where
         args' = filterOutInvisibleTypes tc args
    go (CastTy ty _)        = go ty
    go (CoercionTy co)      = pprPanic "toLHsSigWcType" (ppr co)

         -- Source-language types have _invisible_ kind arguments,
         -- so we must remove them here (Trac #8563)

    go_tv :: TyVar -> LHsTyVarBndr RdrName
    go_tv tv = noLoc $ KindedTyVar (noLoc (getRdrName tv))
                                   (go (tyVarKind tv))


{- *********************************************************************
*                                                                      *
    --------- HsWrappers: type args, dict args, casts ---------
*                                                                      *
********************************************************************* -}

mkLHsWrap :: HsWrapper -> LHsExpr id -> LHsExpr id
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

mkHsWrap :: HsWrapper -> HsExpr id -> HsExpr id
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
                 | otherwise           = HsWrap co_fn e

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
mkFunBind fn ms = FunBind { fun_id = fn
                          , fun_matches = mkMatchGroup Generated ms
                          , fun_co_fn = idHsWrapper
                          , bind_fvs = placeHolderNames
                          , fun_tick = [] }

mkTopFunBind :: Origin -> Located Name -> [LMatch Name (LHsExpr Name)]
             -> HsBind Name
-- In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn
                                    , fun_matches = mkMatchGroup origin ms
                                    , fun_co_fn = idHsWrapper
                                    , bind_fvs = emptyNameSet -- NB: closed
                                                              --     binding
                                    , fun_tick = [] }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr RdrName -> LHsBind RdrName
mkHsVarBind loc var rhs = mk_easy_FunBind loc var [] rhs

mkVarBind :: id -> LHsExpr id -> LHsBind id
mkVarBind var rhs = L (getLoc rhs) $
                    VarBind { var_id = var, var_rhs = rhs, var_inline = False }

mkPatSynBind :: LEmbellished RdrName -> HsPatSynDetails (Located RdrName)
             -> LPat RdrName -> HsPatSynDir RdrName -> HsBind RdrName
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
mk_easy_FunBind :: SrcSpan -> RdrName -> [LPat RdrName]
                -> LHsExpr RdrName -> LHsBind RdrName
mk_easy_FunBind loc fun pats expr
  = L loc $ mkFunBind (L loc fun)
              [mkMatch (FunRhs (L loc fun) Prefix) pats expr
                       (noLoc emptyLocalBinds)]

------------
mkMatch :: HsMatchContext (NameOrRdrName id) -> [LPat id] -> LHsExpr id
        -> Located (HsLocalBinds id) -> LMatch id (LHsExpr id)
mkMatch ctxt pats expr lbinds
  = noLoc (Match ctxt (map paren pats) Nothing
                 (GRHSs (unguardedRHS noSrcSpan expr) lbinds))
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

Note [Unlifted id check in isHsUnliftedBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose there is a binding with the type (Num a => (# a, a #)). Is this a
strict binding that should be disallowed at the top level? At first glance,
no, because it's a function. But consider how this is desugared via
AbsBinds:

  -- x :: Num a => (# a, a #)
  x = (# 3, 4 #)

becomes

  x = \ $dictNum ->
      let x_mono = (# fromInteger $dictNum 3, fromInteger $dictNum 4 #) in
      x_mono

Note that the inner let is strict. And thus if we have a bunch of mutually
recursive bindings of this form, we could end up in trouble. This was shown
up in #9140.

But if there is a type signature on x, everything changes because of the
desugaring used by AbsBindsSig:

  x :: Num a => (# a, a #)
  x = (# 3, 4 #)

becomes

  x = \ $dictNum -> (# fromInteger $dictNum 3, fromInteger $dictNum 4 #)

No strictness anymore! The bottom line here is that, for inferred types, we
care about the strictness of the type after the =>. For checked types
(AbsBindsSig), we care about the overall strictness.

This matters. If we don't separate out the AbsBindsSig case, then GHC runs into
a problem when compiling

  undefined :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => a

Looking only after the =>, we cannot tell if this is strict or not. (GHC panics
if you try.) Looking at the whole type, on the other hand, tells you that this
is a lifted function type, with no trouble at all.

-}

----------------- Bindings --------------------------

-- | Should we treat this as an unlifted bind? This will be true for any
-- bind that binds an unlifted variable, but we must be careful around
-- AbsBinds. See Note [Unlifted id check in isUnliftedHsBind]. For usage
-- information, see Note [Strict binds check] is DsBinds.
isUnliftedHsBind :: HsBind Id -> Bool  -- works only over typechecked binds
isUnliftedHsBind (AbsBindsSig { abs_sig_export = id })
  = isUnliftedType (idType id)
isUnliftedHsBind bind
  = any is_unlifted_id (collectHsBindBinders bind)
  where
    is_unlifted_id id
      = case tcSplitSigmaTy (idType id) of
          (_, _, tau) -> isUnliftedType tau
          -- For the is_unlifted check, we need to look inside polymorphism
          -- and overloading.  E.g.  x = (# 1, True #)
          -- would get type forall a. Num a => (# a, Bool #)
          -- and we want to reject that.  See Trac #9140

collectLocalBinders :: HsLocalBindsLR idL idR -> [idL]
collectLocalBinders (HsValBinds binds) = collectHsIdBinders binds
                                         -- No pattern synonyms here
collectLocalBinders (HsIPBinds _)      = []
collectLocalBinders EmptyLocalBinds    = []

collectHsIdBinders, collectHsValBinders :: HsValBindsLR idL idR -> [idL]
-- Collect Id binders only, or Ids + pattern synonyms, respectively
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
-- Collect Ids, or Ids + pattern synonyms, depending on boolean flag
collect_binds ps binds acc = foldrBag (collect_bind ps . unLoc) acc binds

collect_bind :: Bool -> HsBindLR idL idR -> [idL] -> [idL]
collect_bind _ (PatBind { pat_lhs = p })           acc = collect_lpat p acc
collect_bind _ (FunBind { fun_id = L _ f })        acc = f : acc
collect_bind _ (VarBind { var_id = f })            acc = f : acc
collect_bind _ (AbsBinds { abs_exports = dbinds }) acc = map abe_poly dbinds ++ acc
        -- I don't think we want the binders from the abe_binds
        -- The only time we collect binders from a typechecked
        -- binding (hence see AbsBinds) is in zonking in TcHsSyn
collect_bind _ (AbsBindsSig { abs_sig_export = poly }) acc = poly : acc
collect_bind omitPatSyn (PatSynBind (PSB { psb_id = L _ ps })) acc
  | omitPatSyn                  = acc
  | otherwise                   = unEmb ps : acc

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
collectPatBinders :: LPat a -> [a]
collectPatBinders pat = collect_lpat pat []

collectPatsBinders :: [LPat a] -> [a]
collectPatsBinders pats = foldr collect_lpat [] pats

-------------
collect_lpat :: LPat name -> [name] -> [name]
collect_lpat (L _ pat) bndrs
  = go pat
  where
    go (VarPat (L _ var))         = var : bndrs
    go (WildPat _)                = bndrs
    go (LazyPat pat)              = collect_lpat pat bndrs
    go (BangPat pat)              = collect_lpat pat bndrs
    go (AsPat (L _ a) pat)        = unEmb a : collect_lpat pat bndrs
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

hsGroupBinders :: HsGroup Name -> [Name]
hsGroupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
                          hs_fords = foreign_decls })
  =  collectHsValBinders val_decls
  ++ hsTyClForeignBinders tycl_decls foreign_decls

hsTyClForeignBinders :: [TyClGroup Name]
                     -> [LForeignDecl Name]
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
    getSelectorNames :: ([LEmbellished Name], [LFieldOcc Name]) -> [Name]
    getSelectorNames (ns, fs)
      = map unLocEmb ns ++ map (selectorFieldOcc.unLoc) fs

-------------------
hsLTyClDeclBinders :: Located (TyClDecl name)
                   -> ([LEmbellished name], [LFieldOcc name])
-- ^ Returns all the /binding/ names of the decl.  The first one is

-- guaranteed to be the name of the decl. The first component
-- represents all binding names except record fields; the second
-- represents field occurrences. For record fields mentioned in
-- multiple constructors, the SrcLoc will be from the first occurrence.
--
-- Each returned (Located name) has a SrcSpan for the /whole/ declaration.
-- See Note [SrcSpan for binders]

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl { fdLName = L _ name } }))
  = ([L loc $ EName name], [])
hsLTyClDeclBinders (L loc (SynDecl     { tcdLName = L _ name }))
  = ([L loc (EName name)], [])
hsLTyClDeclBinders (L loc (ClassDecl   { tcdLName = L _ cls_name
                                       , tcdSigs = sigs, tcdATs = ats }))
  = (L loc (EName cls_name) :
     [ L fam_loc (EName fam_name) |
                 L fam_loc (FamilyDecl { fdLName = L _ fam_name }) <- ats ] ++
     [ L mem_loc (EName mem_name) | L mem_loc (ClassOpSig False ns _) <- sigs
                          , L _ mem_name <- (map unLEmb ns) ]
    , [])
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = L _ name, tcdDataDefn = defn }))
  = (\ (xs, ys) -> (L loc (EName name) : xs, ys)) $ hsDataDefnBinders defn

-------------------
hsForeignDeclsBinders :: [LForeignDecl name] -> [Located name]
-- See Note [SrcSpan for binders]
hsForeignDeclsBinders foreign_decls
  = [ L decl_loc n
    | L decl_loc (ForeignImport { fd_name = L _ n }) <- foreign_decls]


-------------------
hsPatSynSelectors :: HsValBinds id -> [id]
-- Collects record pattern-synonym selectors only; the pattern synonym
-- names are collected by collectHsValBinders.
hsPatSynSelectors (ValBindsIn _ _) = panic "hsPatSynSelectors"
hsPatSynSelectors (ValBindsOut binds _)
  = foldrBag addPatSynSelector [] . unionManyBags $ map snd binds

addPatSynSelector:: LHsBind id -> [id] -> [id]
addPatSynSelector bind sels
  | L _ (PatSynBind (PSB { psb_args = RecordPatSyn as })) <- bind
  = map (unLoc . recordPatSynSelectorId) as ++ sels
  | otherwise = sels

getPatSynBinds :: [(RecFlag, LHsBinds id)] -> [PatSynBind id id]
getPatSynBinds binds
  = [ psb | (_, lbinds) <- binds
          , L _ (PatSynBind psb) <- bagToList lbinds ]

-------------------
hsLInstDeclBinders :: LInstDecl name -> ([LEmbellished name], [LFieldOcc name])
hsLInstDeclBinders (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = dfis } }))
  = foldMap (hsDataFamInstBinders . unLoc) dfis
hsLInstDeclBinders (L _ (DataFamInstD { dfid_inst = fi }))
  = hsDataFamInstBinders fi
hsLInstDeclBinders (L _ (TyFamInstD {})) = mempty

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataFamInstBinders :: DataFamInstDecl name
                     -> ([LEmbellished name], [LFieldOcc name])
hsDataFamInstBinders (DataFamInstDecl { dfid_defn = defn })
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataDefnBinders :: HsDataDefn name -> ([LEmbellished name], [LFieldOcc name])
hsDataDefnBinders (HsDataDefn { dd_cons = cons })
  = hsConDeclsBinders cons
  -- See Note [Binders in family instances]

-------------------
hsConDeclsBinders :: [LConDecl name] -> ([LEmbellished name], [LFieldOcc name])
  -- See hsLTyClDeclBinders for what this does
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons = go id cons
  where go :: ([LFieldOcc name] -> [LFieldOcc name])
           -> [LConDecl name] -> ([LEmbellished name], [LFieldOcc name])
        go _ [] = ([], [])
        go remSeen (r:rs) =
          -- don't re-mangle the location of field names, because we don't
          -- have a record of the full location of the field declaration anyway
          case r of
             -- remove only the first occurrence of any seen field in order to
             -- avoid circumventing detection of duplicate fields (#9156)
             L loc (ConDeclGADT { con_names = names
                                , con_type = HsIB { hsib_body = res_ty}}) ->
               case tau of
                 L _ (HsFunTy
                      (L _ (HsAppsTy
                            [L _ (HsAppPrefix (L _ (HsRecTy flds)))])) _res_ty)
                         -> record_gadt flds
                 L _ (HsFunTy (L _ (HsRecTy flds)) _res_ty)
                         -> record_gadt flds

                 _other  -> (map (L loc . unLoc) names ++ ns, fs)
                            where (ns, fs) = go remSeen rs
               where
                 (_tvs, _cxt, tau) = splitLHsSigmaTy res_ty
                 record_gadt flds = (map (L loc . unLoc) names ++ ns
                                    , r' ++ fs)
                   where r' = remSeen (concatMap (cd_fld_names . unLoc) flds)
                         remSeen' = foldr (.) remSeen
                                        [deleteBy ((==) `on`
                                              unLoc . rdrNameFieldOcc . unLoc) v
                                        | v <- r']
                         (ns, fs) = go remSeen' rs

             L loc (ConDeclH98 { con_name = name
                               , con_details = RecCon flds }) ->
               ([L loc (unLoc name)] ++ ns, r' ++ fs)
                  where r' = remSeen (concatMap (cd_fld_names . unLoc)
                                                (unLoc flds))
                        remSeen'
                          = foldr (.) remSeen
                               [deleteBy ((==) `on`
                                   unLoc . rdrNameFieldOcc . unLoc) v | v <- r']
                        (ns, fs) = go remSeen' rs
             L loc (ConDeclH98 { con_name = name }) ->
                ([L loc (unLoc name)] ++ ns, fs)
                  where (ns, fs) = go remSeen rs

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

    hs_stmt :: StmtLR Name idR (Located (body idR)) -> NameSet
    hs_stmt (BindStmt pat _ _ _ _) = lPatImplicits pat
    hs_stmt (ApplicativeStmt args _ _) = unionNameSets (map do_arg args)
      where do_arg (_, ApplicativeArgOne pat _) = lPatImplicits pat
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
