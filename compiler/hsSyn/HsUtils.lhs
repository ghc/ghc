%
% (c) The University of Glasgow, 1992-2006
%

Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the intantiated versions are located elsewhere:

   Parameterised by	Module
   ----------------     -------------
   RdrName		parser/RdrHsSyn
   Name			rename/RnHsSyn
   Id			typecheck/TcHsSyn	

\begin{code}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module HsUtils(
  -- Terms
  mkHsPar, mkHsApp, mkHsConApp, mkSimpleHsAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS, 
  mkMatchGroup, mkMatch, mkHsLam, mkHsIf,
  mkHsWrap, mkLHsWrap, mkHsWrapCo, mkLHsWrapCo,
  coToHsWrapper, mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsComp, mkHsWrapPat, mkHsWrapPatCo,
  mkLHsPar, mkHsCmdCast,

  nlHsTyApp, nlHsVar, nlHsLit, nlHsApp, nlHsApps, nlHsIntLit, nlHsVarApps, 
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,
  toHsType, toHsKind,

  -- Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mk_easy_FunBind, mkTopFunBind, mkPatSynBind,

  -- Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString, 

  -- Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConPat, nlInfixConPat,
  nlNullaryConPat, nlWildConPat, nlWildPat, nlTuplePat, mkParPat,

  -- Types
  mkHsAppTy, userHsTyVarBndrs,
  nlHsAppTy, nlHsTyVar, nlHsFunTy, nlHsTyConApp, 

  -- Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt, mkBindStmt, mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt, 
  emptyRecStmt, mkRecStmt, 

  -- Template Haskell
  mkHsSpliceTy, mkHsSpliceE, mkHsSpliceTE, mkHsSplice,
  mkHsQuasiQuote, unqualQuasiQuote,

  -- Flags
  noRebindableInfo, 

  -- Collecting binders
  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,
  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,

  hsLTyClDeclBinders, hsTyClDeclsBinders, 
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
\end{code}


%************************************************************************
%*									*
	Some useful helpers for constructing syntax
%*									*
%************************************************************************

These functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the nl* functions below which
just attach noSrcSpan to everything.

\begin{code}
mkHsPar :: LHsExpr id -> LHsExpr id
mkHsPar e = L (getLoc e) (HsPar e)

mkSimpleMatch :: [LPat id] -> Located (body id) -> LMatch id (Located (body id))
mkSimpleMatch pats rhs 
  = L loc $
    Match pats Nothing (unguardedGRHSs rhs)
  where
    loc = case pats of
		[]      -> getLoc rhs
		(pat:_) -> combineSrcSpans (getLoc pat) (getLoc rhs)

unguardedGRHSs :: Located (body id) -> GRHSs id (Located (body id))
unguardedGRHSs rhs = GRHSs (unguardedRHS rhs) emptyLocalBinds

unguardedRHS :: Located (body id) -> [LGRHS id (Located (body id))]
unguardedRHS rhs@(L loc _) = [L loc (GRHS [] rhs)]

mkMatchGroup :: Origin -> [LMatch id (Located (body id))] -> MatchGroup id (Located (body id))
mkMatchGroup origin matches = MG { mg_alts = matches, mg_arg_tys = [], mg_res_ty = placeHolderType, mg_origin = origin }

mkHsAppTy :: LHsType name -> LHsType name -> LHsType name
mkHsAppTy t1 t2 = addCLoc t1 t2 (HsAppTy t1 t2)

mkHsApp :: LHsExpr name -> LHsExpr name -> LHsExpr name
mkHsApp e1 e2 = addCLoc e1 e2 (HsApp e1 e2)

mkHsLam :: [LPat id] -> LHsExpr id -> LHsExpr id
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam matches))
	where
          matches = mkMatchGroup Generated [mkSimpleMatch pats body]

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr Id -> LHsExpr Id
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars <.> mkWpLams dicts) expr

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

mkHsIntegral   :: Integer -> PostTcType -> HsOverLit id
mkHsFractional :: FractionalLit -> PostTcType -> HsOverLit id
mkHsIsString   :: FastString -> PostTcType -> HsOverLit id
mkHsDo         :: HsStmtContext Name -> [ExprLStmt id] -> HsExpr id
mkHsComp       :: HsStmtContext Name -> [ExprLStmt id] -> LHsExpr id -> HsExpr id

mkNPat      :: HsOverLit id -> Maybe (SyntaxExpr id) -> Pat id
mkNPlusKPat :: Located id -> HsOverLit id -> Pat id

mkLastStmt :: Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))
mkBodyStmt :: Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))
mkBindStmt :: LPat idL -> Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))

emptyRecStmt :: StmtLR idL idR bodyR
mkRecStmt    :: [LStmtLR idL idR bodyR] -> StmtLR idL idR bodyR


mkHsIntegral   i       = OverLit (HsIntegral   i)  noRebindableInfo noSyntaxExpr
mkHsFractional f       = OverLit (HsFractional f)  noRebindableInfo noSyntaxExpr
mkHsIsString   s       = OverLit (HsIsString   s)  noRebindableInfo noSyntaxExpr

noRebindableInfo :: Bool
noRebindableInfo = error "noRebindableInfo" 	-- Just another placeholder; 

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

emptyRecStmt = RecStmt { recS_stmts = [], recS_later_ids = [], recS_rec_ids = []
                       , recS_ret_fn = noSyntaxExpr, recS_mfix_fn = noSyntaxExpr
                       , recS_bind_fn = noSyntaxExpr, recS_later_rets = []
                       , recS_rec_rets = [], recS_ret_ty = placeHolderType }

mkRecStmt stmts = emptyRecStmt { recS_stmts = stmts }

-------------------------------
--- A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr id -> id -> LHsExpr id -> HsExpr id
mkHsOpApp e1 op e2 = OpApp e1 (noLoc (HsVar op)) (error "mkOpApp:fixity") e2

mkHsSplice :: LHsExpr RdrName -> HsSplice RdrName
mkHsSplice e = HsSplice unqualSplice e

mkHsSpliceE :: LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceE e = HsSpliceE False (mkHsSplice e)

mkHsSpliceTE :: LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceTE e = HsSpliceE True (mkHsSplice e)

mkHsSpliceTy :: LHsExpr RdrName -> HsType RdrName
mkHsSpliceTy e = HsSpliceTy (mkHsSplice e) placeHolderKind

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))
		-- A name (uniquified later) to
		-- identify the splice

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsQuasiQuote RdrName
mkHsQuasiQuote quoter span quote = HsQuasiQuote quoter span quote

unqualQuasiQuote :: RdrName
unqualQuasiQuote = mkRdrUnqual (mkVarOccFS (fsLit "quasiquote"))
		-- A name (uniquified later) to
		-- identify the quasi-quote

mkHsString :: String -> HsLit
mkHsString s = HsString (mkFastString s)

-------------
userHsTyVarBndrs :: SrcSpan -> [name] -> [Located (HsTyVarBndr name)]
-- Caller sets location
userHsTyVarBndrs loc bndrs = [ L loc (UserTyVar v) | v <- bndrs ]
\end{code}


%************************************************************************
%*									*
	Constructing syntax with no location info
%*									*
%************************************************************************

\begin{code}
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
nlHsIntLit n = noLoc (HsLit (HsInt n))

nlHsApps :: id -> [LHsExpr id] -> LHsExpr id
nlHsApps f xs = foldl nlHsApp (nlHsVar f) xs
	     
nlHsVarApps :: id -> [id] -> LHsExpr id
nlHsVarApps f xs = noLoc (foldl mk (HsVar f) (map HsVar xs))
		 where
		   mk f a = HsApp (noLoc f) (noLoc a)

nlConVarPat :: id -> [id] -> LPat id
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlInfixConPat :: id -> LPat id -> LPat id -> LPat id
nlInfixConPat con l r = noLoc (ConPatIn (noLoc con) (InfixCon l r))

nlConPat :: id -> [LPat id] -> LPat id
nlConPat con pats = noLoc (ConPatIn (noLoc con) (PrefixCon pats))

nlNullaryConPat :: id -> LPat id
nlNullaryConPat con = noLoc (ConPatIn (noLoc con) (PrefixCon []))

nlWildConPat :: DataCon -> LPat RdrName
nlWildConPat con = noLoc (ConPatIn (noLoc (getRdrName con))
				   (PrefixCon (nOfThem (dataConSourceArity con) nlWildPat)))

nlWildPat :: LPat id
nlWildPat  = noLoc (WildPat placeHolderType)	-- Pre-typechecking

nlHsDo :: HsStmtContext Name -> [LStmt id (LHsExpr id)] -> LHsExpr id
nlHsDo ctxt stmts = noLoc (mkHsDo ctxt stmts)

nlHsOpApp :: LHsExpr id -> id -> LHsExpr id -> LHsExpr id
nlHsOpApp e1 op e2 = noLoc (mkHsOpApp e1 op e2)

nlHsLam  :: LMatch id (LHsExpr id) -> LHsExpr id
nlHsPar  :: LHsExpr id -> LHsExpr id
nlHsIf   :: LHsExpr id -> LHsExpr id -> LHsExpr id -> LHsExpr id
nlHsCase :: LHsExpr id -> [LMatch id (LHsExpr id)] -> LHsExpr id
nlList   :: [LHsExpr id] -> LHsExpr id

nlHsLam	match          = noLoc (HsLam (mkMatchGroup Generated [match]))
nlHsPar e              = noLoc (HsPar e)
nlHsIf cond true false = noLoc (mkHsIf cond true false)
nlHsCase expr matches  = noLoc (HsCase expr (mkMatchGroup Generated matches))
nlList exprs           = noLoc (ExplicitList placeHolderType Nothing exprs)

nlHsAppTy :: LHsType name -> LHsType name -> LHsType name
nlHsTyVar :: name                         -> LHsType name
nlHsFunTy :: LHsType name -> LHsType name -> LHsType name

nlHsAppTy f t		= noLoc (HsAppTy f t)
nlHsTyVar x		= noLoc (HsTyVar x)
nlHsFunTy a b		= noLoc (HsFunTy a b)

nlHsTyConApp :: name -> [LHsType name] -> LHsType name
nlHsTyConApp tycon tys  = foldl nlHsAppTy (nlHsTyVar tycon) tys
\end{code}

Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.

\begin{code}
mkLHsTupleExpr :: [LHsExpr a] -> LHsExpr a
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] = e
mkLHsTupleExpr es  = noLoc $ ExplicitTuple (map Present es) Boxed

mkLHsVarTuple :: [a] -> LHsExpr a
mkLHsVarTuple ids  = mkLHsTupleExpr (map nlHsVar ids)

nlTuplePat :: [LPat id] -> Boxity -> LPat id
nlTuplePat pats box = noLoc (TuplePat pats box [])

missingTupArg :: HsTupArg a
missingTupArg = Missing placeHolderType
\end{code}


%************************************************************************
%*									*
        Converting a Type to an HsType RdrName
%*									*
%************************************************************************

This is needed to implement GeneralizedNewtypeDeriving.

\begin{code}
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
    to_hs_type (LitTy (NumTyLit n)) = noLoc $ HsTyLit (HsNumTy n)
    to_hs_type (LitTy (StrTyLit s)) = noLoc $ HsTyLit (HsStrTy s)

    mk_hs_tvb tv = noLoc $ KindedTyVar (getRdrName tv) (toHsKind (tyVarKind tv))

toHsKind :: Kind -> LHsKind RdrName
toHsKind = toHsType

\end{code}

\begin{code}
--------- HsWrappers: type args, dict args, casts ---------
mkLHsWrap :: HsWrapper -> LHsExpr id -> LHsExpr id
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

mkHsWrap :: HsWrapper -> HsExpr id -> HsExpr id
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
		 | otherwise	       = HsWrap co_fn e

mkHsWrapCo :: TcCoercion -> HsExpr id -> HsExpr id
mkHsWrapCo co e = mkHsWrap (coToHsWrapper co) e

mkLHsWrapCo :: TcCoercion -> LHsExpr id -> LHsExpr id
mkLHsWrapCo co (L loc e) = L loc (mkHsWrapCo co e)

mkHsCmdCast :: TcCoercion -> HsCmd id -> HsCmd id
mkHsCmdCast co cmd | isTcReflCo co = cmd
                   | otherwise     = HsCmdCast co cmd

coToHsWrapper :: TcCoercion -> HsWrapper
coToHsWrapper co | isTcReflCo co = idHsWrapper
                 | otherwise     = mkWpCast (mkTcSubCo co)

mkHsWrapPat :: HsWrapper -> Pat id -> Type -> Pat id
mkHsWrapPat co_fn p ty | isIdHsWrapper co_fn = p
		       | otherwise	     = CoPat co_fn p ty

mkHsWrapPatCo :: TcCoercion -> Pat id -> Type -> Pat id
mkHsWrapPatCo co pat ty | isTcReflCo co = pat
                        | otherwise     = CoPat (mkWpCast co) pat ty

mkHsDictLet :: TcEvBinds -> LHsExpr Id -> LHsExpr Id
mkHsDictLet ev_binds expr = mkLHsWrap (mkWpLet ev_binds) expr
\end{code}
l
%************************************************************************
%*									*
		Bindings; with a location at the top
%*									*
%************************************************************************

\begin{code}
mkFunBind :: Located RdrName -> [LMatch RdrName (LHsExpr RdrName)] -> HsBind RdrName
-- Not infix, with place holders for coercion and free vars
mkFunBind fn ms = FunBind { fun_id = fn, fun_infix = False
                          , fun_matches = mkMatchGroup Generated ms
                          , fun_co_fn = idHsWrapper
                          , bind_fvs = placeHolderNames
                          , fun_tick = Nothing }

mkTopFunBind :: Origin -> Located Name -> [LMatch Name (LHsExpr Name)] -> HsBind Name
-- In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn, fun_infix = False
                                    , fun_matches = mkMatchGroup origin ms
                                    , fun_co_fn = idHsWrapper
                                    , bind_fvs = emptyNameSet	-- NB: closed binding
                                    , fun_tick = Nothing }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr RdrName -> LHsBind RdrName
mkHsVarBind loc var rhs = mk_easy_FunBind loc var [] rhs

mkVarBind :: id -> LHsExpr id -> LHsBind id
mkVarBind var rhs = L (getLoc rhs) $
		    VarBind { var_id = var, var_rhs = rhs, var_inline = False }

mkPatSynBind :: Located RdrName -> HsPatSynDetails (Located RdrName) -> LPat RdrName -> HsPatSynDir RdrName -> HsBind RdrName
mkPatSynBind name details lpat dir = PatSynBind{ patsyn_id = name
                                               , patsyn_args = details
                                               , patsyn_def = lpat
                                               , patsyn_dir = dir
                                               , bind_fvs = placeHolderNames }

------------
mk_easy_FunBind :: SrcSpan -> RdrName -> [LPat RdrName]
		-> LHsExpr RdrName -> LHsBind RdrName
mk_easy_FunBind loc fun pats expr
  = L loc $ mkFunBind (L loc fun) [mkMatch pats expr emptyLocalBinds]

------------
mkMatch :: [LPat id] -> LHsExpr id -> HsLocalBinds id -> LMatch id (LHsExpr id)
mkMatch pats expr binds
  = noLoc (Match (map paren pats) Nothing 
		 (GRHSs (unguardedRHS expr) binds))
  where
    paren lp@(L l p) | hsPatNeedsParens p = L l (ParPat lp) 
		     | otherwise          = lp
\end{code}


%************************************************************************
%*									*
	Collecting binders
%*									*
%************************************************************************

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

\begin{code}
----------------- Bindings --------------------------
collectLocalBinders :: HsLocalBindsLR idL idR -> [idL]
collectLocalBinders (HsValBinds val_binds) = collectHsValBinders val_binds
collectLocalBinders (HsIPBinds _)   = []
collectLocalBinders EmptyLocalBinds = []

collectHsValBinders :: HsValBindsLR idL idR -> [idL]
collectHsValBinders (ValBindsIn  binds _) = collectHsBindsBinders binds
collectHsValBinders (ValBindsOut binds _) = foldr collect_one [] binds
  where
   collect_one (_,binds) acc = collect_binds binds acc

collectHsBindBinders :: HsBindLR idL idR -> [idL]
collectHsBindBinders b = collect_bind b []

collect_bind :: HsBindLR idL idR -> [idL] -> [idL]
collect_bind (PatBind { pat_lhs = p })    acc = collect_lpat p acc
collect_bind (FunBind { fun_id = L _ f }) acc = f : acc
collect_bind (VarBind { var_id = f })     acc = f : acc
collect_bind (AbsBinds { abs_exports = dbinds, abs_binds = _binds }) acc
  = map abe_poly dbinds ++ acc 
	-- ++ foldr collect_bind acc binds
	-- I don't think we want the binders from the nested binds
	-- The only time we collect binders from a typechecked 
	-- binding (hence see AbsBinds) is in zonking in TcHsSyn
collect_bind (PatSynBind { patsyn_id = L _ ps }) acc = ps : acc

collectHsBindsBinders :: LHsBindsLR idL idR -> [idL]
collectHsBindsBinders binds = collect_binds binds []

collectHsBindListBinders :: [LHsBindLR idL idR] -> [idL]
collectHsBindListBinders = foldr (collect_bind . unLoc) []

collect_binds :: LHsBindsLR idL idR -> [idL] -> [idL]
collect_binds binds acc = foldrBag (collect_bind . unLoc) acc binds

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
    go (VarPat var) 	   	  = var : bndrs
    go (WildPat _)	      	  = bndrs
    go (LazyPat pat)     	  = collect_lpat pat bndrs
    go (BangPat pat)     	  = collect_lpat pat bndrs
    go (AsPat (L _ a) pat)     	  = a : collect_lpat pat bndrs
    go (ViewPat _ pat _)          = collect_lpat pat bndrs
    go (ParPat  pat)     	  = collect_lpat pat bndrs
				  
    go (ListPat pats _ _)         = foldr collect_lpat bndrs pats
    go (PArrPat pats _)    	  = foldr collect_lpat bndrs pats
    go (TuplePat pats _ _)  	  = foldr collect_lpat bndrs pats
				  
    go (ConPatIn _ ps)            = foldr collect_lpat bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps})  = foldr collect_lpat bndrs (hsConPatArgs ps)
	-- See Note [Dictionary binders in ConPatOut]
    go (LitPat _)	      	  = bndrs
    go (NPat _ _ _)		  = bndrs
    go (NPlusKPat (L _ n) _ _ _)  = n : bndrs
 				  
    go (SigPatIn pat _)	 	  = collect_lpat pat bndrs
    go (SigPatOut pat _)	  = collect_lpat pat bndrs
    go (SplicePat _)              = bndrs
    go (QuasiQuotePat _)          = bndrs
    go (CoPat _ pat _)            = go pat
\end{code}

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

\begin{code}
hsGroupBinders :: HsGroup Name -> [Name]
hsGroupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
                          hs_instds = inst_decls, hs_fords = foreign_decls })
-- Collect the binders of a Group
  =  collectHsValBinders val_decls
  ++ hsTyClDeclsBinders tycl_decls inst_decls
  ++ hsForeignDeclsBinders foreign_decls

hsForeignDeclsBinders :: [LForeignDecl Name] -> [Name]
hsForeignDeclsBinders foreign_decls
  = [n | L _ (ForeignImport (L _ n) _ _ _) <- foreign_decls]

hsTyClDeclsBinders :: [TyClGroup Name] -> [Located (InstDecl Name)] -> [Name]
-- We need to look at instance declarations too, 
-- because their associated types may bind data constructors
hsTyClDeclsBinders tycl_decls inst_decls
  = map unLoc (concatMap (concatMap hsLTyClDeclBinders . group_tyclds) tycl_decls ++
               concatMap (hsInstDeclBinders . unLoc) inst_decls)

-------------------
hsLTyClDeclBinders :: Eq name => Located (TyClDecl name) -> [Located name]
-- ^ Returns all the /binding/ names of the decl.
-- The first one is guaranteed to be the name of the decl. For record fields
-- mentioned in multiple constructors, the SrcLoc will be from the first
-- occurrence.  We use the equality to filter out duplicate field names.
--
-- Each returned (Located name) is wrapped in a @SrcSpan@ of the whole
-- /declaration/, not just the name itself (which is how it appears in
-- the syntax tree).  This SrcSpan (for the entire declaration) is used
-- as the SrcSpan for the Name that is finally produced, and hence for
-- error messages.  (See Trac #8607.)

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl { fdLName = L _ name } }))
  = [L loc name]
hsLTyClDeclBinders (L loc (ForeignType { tcdLName = L _ name })) = [L loc name]
hsLTyClDeclBinders (L loc (SynDecl     { tcdLName = L _ name })) = [L loc name]
hsLTyClDeclBinders (L loc (ClassDecl   { tcdLName = L _ cls_name
                                       , tcdSigs = sigs, tcdATs = ats }))
  = L loc cls_name :
    [ L fam_loc fam_name | L fam_loc (FamilyDecl { fdLName = L _ fam_name }) <- ats ] ++
    [ L mem_loc mem_name | L mem_loc (TypeSig ns _) <- sigs, L _ mem_name <- ns ]
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = L _ name, tcdDataDefn = defn }))
  = L loc name : hsDataDefnBinders defn

-------------------
hsInstDeclBinders :: Eq name => InstDecl name -> [Located name]
hsInstDeclBinders (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = dfis } })
  = concatMap (hsDataFamInstBinders . unLoc) dfis
hsInstDeclBinders (DataFamInstD { dfid_inst = fi }) = hsDataFamInstBinders fi
hsInstDeclBinders (TyFamInstD {}) = []

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataFamInstBinders :: Eq name => DataFamInstDecl name -> [Located name]
hsDataFamInstBinders (DataFamInstDecl { dfid_defn = defn })
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- the SrcLoc returned are for the whole declarations, not just the names
hsDataDefnBinders :: Eq name => HsDataDefn name -> [Located name]
hsDataDefnBinders (HsDataDefn { dd_cons = cons }) = hsConDeclsBinders cons
  -- See Note [Binders in family instances]

-------------------
hsConDeclsBinders :: (Eq name) => [LConDecl name] -> [Located name]
  -- See hsLTyClDeclBinders for what this does
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons
  = snd (foldl do_one ([], []) cons)
  where
    do_one (flds_seen, acc) (L loc (ConDecl { con_name = L _ name
                                            , con_details = RecCon flds }))
	= (map unLoc new_flds ++ flds_seen, L loc name : new_flds ++ acc)
	where
          -- don't re-mangle the location of field names, because we don't
          -- have a record of the full location of the field declaration anyway
	  new_flds = filterOut (\f -> unLoc f `elem` flds_seen) 
			       (map cd_fld_name flds)

    do_one (flds_seen, acc) (L loc (ConDecl { con_name = L _ name }))
	= (flds_seen, L loc name : acc)
\end{code}

Note [Binders in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type or data family instance declaration, the type 
constructor is an *occurrence* not a binding site
    type instance T Int = Int -> Int   -- No binders
    data instance S Bool = S1 | S2     -- Binders are S1,S2


%************************************************************************
%*									*
	Collecting binders the user did not write
%*									*
%************************************************************************

The job of this family of functions is to run through binding sites and find the set of all Names
that were defined "implicitly", without being explicitly written by the user.

The main purpose is to find names introduced by record wildcards so that we can avoid
warning the user when they don't use those names (#4404)

\begin{code}
lStmtsImplicits :: [LStmtLR Name idR (Located (body idR))] -> NameSet
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: [LStmtLR Name idR (Located (body idR))] -> NameSet
    hs_lstmts = foldr (\stmt rest -> unionNameSets (hs_stmt (unLoc stmt)) rest) emptyNameSet
    
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
  = foldr (unionNameSets . lhsBindsImplicits . snd) emptyNameSet binds
hsValBindsImplicits (ValBindsIn binds _) 
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR Name idR -> NameSet
lhsBindsImplicits = foldBag unionNameSets (lhs_bind . unLoc) emptyNameSet
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = emptyNameSet

lPatImplicits :: LPat Name -> NameSet
lPatImplicits = hs_lpat
  where
    hs_lpat (L _ pat) = hs_pat pat
    
    hs_lpats = foldr (\pat rest -> hs_lpat pat `unionNameSets` rest) emptyNameSet
    
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
    details (RecCon fs)      = hs_lpats explicit `unionNameSets` mkNameSet (collectPatsBinders implicit)
      where (explicit, implicit) = partitionEithers [if pat_explicit then Left pat else Right pat
                                                    | (i, fld) <- [0..] `zip` rec_flds fs
                                                    , let pat = hsRecFieldArg fld
                                                          pat_explicit = maybe True (i<) (rec_dotdot fs)]
    details (InfixCon p1 p2) = hs_lpat p1 `unionNameSets` hs_lpat p2
\end{code}
