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
module HsUtils where

import HsBinds
import HsExpr
import HsPat
import HsTypes	
import HsLit

import RdrName
import Var
import Coercion
import Type
import DataCon
import Name
import BasicTypes
import SrcLoc
import FastString
import Outputable
import Util
import Bag
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

mkSimpleMatch :: [LPat id] -> LHsExpr id -> LMatch id
mkSimpleMatch pats rhs 
  = L loc $
    Match pats Nothing (unguardedGRHSs rhs)
  where
    loc = case pats of
		[]      -> getLoc rhs
		(pat:_) -> combineSrcSpans (getLoc pat) (getLoc rhs)

unguardedGRHSs :: LHsExpr id -> GRHSs id
unguardedGRHSs rhs = GRHSs (unguardedRHS rhs) emptyLocalBinds

unguardedRHS :: LHsExpr id -> [LGRHS id]
unguardedRHS rhs@(L loc _) = [L loc (GRHS [] rhs)]

mkHsAppTy :: LHsType name -> LHsType name -> LHsType name
mkHsAppTy t1 t2 = addCLoc t1 t2 (HsAppTy t1 t2)

mkHsApp :: LHsExpr name -> LHsExpr name -> LHsExpr name
mkHsApp e1 e2 = addCLoc e1 e2 (HsApp e1 e2)

nlHsTyApp :: name -> [Type] -> LHsExpr name
nlHsTyApp fun_id tys = noLoc (HsWrap (mkWpTyApps tys) (HsVar fun_id))

mkLHsWrap :: HsWrapper -> LHsExpr id -> LHsExpr id
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

mkHsWrap :: HsWrapper -> HsExpr id -> HsExpr id
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
		 | otherwise	       = HsWrap co_fn e

mkHsWrapCoI :: CoercionI -> HsExpr id -> HsExpr id
mkHsWrapCoI IdCo     e = e
mkHsWrapCoI (ACo co) e = mkHsWrap (WpCast co) e

coiToHsWrapper :: CoercionI -> HsWrapper
coiToHsWrapper IdCo     = idHsWrapper
coiToHsWrapper (ACo co) = WpCast co

mkHsLam :: [LPat id] -> LHsExpr id -> LHsExpr id
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam matches))
	where
	  matches = mkMatchGroup [mkSimpleMatch pats body]

mkMatchGroup :: [LMatch id] -> MatchGroup id
mkMatchGroup matches = MatchGroup matches placeHolderType

mkHsDictLet :: LHsBinds Id -> LHsExpr Id -> LHsExpr Id
-- Used for the dictionary bindings gotten from TcSimplify
-- We make them recursive to be on the safe side
mkHsDictLet binds expr 
  | isEmptyLHsBinds binds = expr
  | otherwise             = L (getLoc expr) (HsLet (HsValBinds val_binds) expr)
			  where
			    val_binds = ValBindsOut [(Recursive, binds)] []

mkHsConApp :: DataCon -> [Type] -> [HsExpr Id] -> LHsExpr Id
-- Used for constructing dictionary terms etc, so no locations 
mkHsConApp data_con tys args 
  = foldl mk_app (nlHsTyApp (dataConWrapId data_con) tys) args
  where
    mk_app f a = noLoc (HsApp f (noLoc a))

mkSimpleHsAlt :: LPat id -> LHsExpr id -> LMatch id
-- A simple lambda with a single pattern, no binds, no guards; pre-typechecking
mkSimpleHsAlt pat expr 
  = mkSimpleMatch [pat] expr

-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: Integer -> PostTcType -> HsOverLit id
mkHsFractional :: Rational -> PostTcType -> HsOverLit id
mkHsIsString   :: FastString -> PostTcType -> HsOverLit id
mkHsDo         :: HsStmtContext Name -> [LStmt id] -> LHsExpr id -> HsExpr id

mkNPat      :: HsOverLit id -> Maybe (SyntaxExpr id) -> Pat id
mkNPlusKPat :: Located id -> HsOverLit id -> Pat id

mkTransformStmt   :: [LStmt idL] -> LHsExpr idR                -> StmtLR idL idR
mkTransformByStmt :: [LStmt idL] -> LHsExpr idR -> LHsExpr idR -> StmtLR idL idR

mkGroupUsingStmt   :: [LStmt idL]                -> LHsExpr idR -> StmtLR idL idR
mkGroupByStmt      :: [LStmt idL] -> LHsExpr idR                -> StmtLR idL idR
mkGroupByUsingStmt :: [LStmt idL] -> LHsExpr idR -> LHsExpr idR -> StmtLR idL idR

mkExprStmt :: LHsExpr idR -> StmtLR idL idR
mkBindStmt :: LPat idL -> LHsExpr idR -> StmtLR idL idR

emptyRecStmt :: StmtLR idL idR
mkRecStmt    :: [LStmtLR idL idR] -> StmtLR idL idR


mkHsIntegral   i       = OverLit (HsIntegral   i)  noRebindableInfo noSyntaxExpr
mkHsFractional f       = OverLit (HsFractional f)  noRebindableInfo noSyntaxExpr
mkHsIsString   s       = OverLit (HsIsString   s)  noRebindableInfo noSyntaxExpr

noRebindableInfo :: Bool
noRebindableInfo = error "noRebindableInfo" 	-- Just another placeholder; 

mkHsDo ctxt stmts body = HsDo ctxt stmts body placeHolderType

mkNPat lit neg     = NPat lit neg noSyntaxExpr
mkNPlusKPat id lit = NPlusKPat id lit noSyntaxExpr noSyntaxExpr

mkTransformStmt   stmts usingExpr        = TransformStmt (stmts, []) usingExpr Nothing
mkTransformByStmt stmts usingExpr byExpr = TransformStmt (stmts, []) usingExpr (Just byExpr)

mkGroupUsingStmt   stmts usingExpr        = GroupStmt (stmts, []) (GroupByNothing usingExpr)
mkGroupByStmt      stmts byExpr           = GroupStmt (stmts, []) (GroupBySomething (Right noSyntaxExpr) byExpr)
mkGroupByUsingStmt stmts byExpr usingExpr = GroupStmt (stmts, []) (GroupBySomething (Left usingExpr) byExpr)

mkExprStmt expr	    = ExprStmt expr noSyntaxExpr placeHolderType
mkBindStmt pat expr = BindStmt pat expr noSyntaxExpr noSyntaxExpr

emptyRecStmt = RecStmt { recS_stmts = [], recS_later_ids = [], recS_rec_ids = []
                       , recS_ret_fn = noSyntaxExpr, recS_mfix_fn = noSyntaxExpr
		       , recS_bind_fn = noSyntaxExpr
                       , recS_rec_rets = [], recS_dicts = emptyLHsBinds }

mkRecStmt stmts = emptyRecStmt { recS_stmts = stmts }

-------------------------------
--- A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr id -> id -> LHsExpr id -> HsExpr id
mkHsOpApp e1 op e2 = OpApp e1 (noLoc (HsVar op)) (error "mkOpApp:fixity") e2

mkHsSplice :: LHsExpr RdrName -> HsSplice RdrName
mkHsSplice e = HsSplice unqualSplice e

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))
		-- A name (uniquified later) to
		-- identify the splice

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsQuasiQuote RdrName
mkHsQuasiQuote quoter span quote = HsQuasiQuote unqualQuasiQuote quoter span quote

unqualQuasiQuote :: RdrName
unqualQuasiQuote = mkRdrUnqual (mkVarOccFS (fsLit "quasiquote"))
		-- A name (uniquified later) to
		-- identify the quasi-quote

mkHsString :: String -> HsLit
mkHsString s = HsString (mkFastString s)

-------------
userHsTyVarBndrs :: [Located name] -> [Located (HsTyVarBndr name)]
userHsTyVarBndrs bndrs = [ L loc (UserTyVar v) | L loc v <- bndrs ]
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

nlHsDo :: HsStmtContext Name -> [LStmt id] -> LHsExpr id -> LHsExpr id
nlHsDo ctxt stmts body = noLoc (mkHsDo ctxt stmts body)

nlHsOpApp :: LHsExpr id -> id -> LHsExpr id -> LHsExpr id
nlHsOpApp e1 op e2 = noLoc (mkHsOpApp e1 op e2)

nlHsLam  :: LMatch id -> LHsExpr id
nlHsPar  :: LHsExpr id -> LHsExpr id
nlHsIf   :: LHsExpr id -> LHsExpr id -> LHsExpr id -> LHsExpr id
nlHsCase :: LHsExpr id -> [LMatch id] -> LHsExpr id
nlList   :: [LHsExpr id] -> LHsExpr id

nlHsLam	match		= noLoc (HsLam (mkMatchGroup [match]))
nlHsPar e		= noLoc (HsPar e)
nlHsIf cond true false	= noLoc (HsIf cond true false)
nlHsCase expr matches	= noLoc (HsCase expr (mkMatchGroup matches))
nlList exprs		= noLoc (ExplicitList placeHolderType exprs)

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
nlTuplePat pats box = noLoc (TuplePat pats box placeHolderType)

missingTupArg :: HsTupArg a
missingTupArg = Missing placeHolderType
\end{code}

%************************************************************************
%*									*
		Bindings; with a location at the top
%*									*
%************************************************************************

\begin{code}
mkFunBind :: Located id -> [LMatch id] -> HsBind id
-- Not infix, with place holders for coercion and free vars
mkFunBind fn ms = FunBind { fun_id = fn, fun_infix = False, fun_matches = mkMatchGroup ms,
			    fun_co_fn = idHsWrapper, bind_fvs = placeHolderNames,
			    fun_tick = Nothing }


mkVarBind :: SrcSpan -> id -> LHsExpr id -> LHsBind id
mkVarBind loc var rhs = mk_easy_FunBind loc var [] rhs

------------
mk_easy_FunBind :: SrcSpan -> id -> [LPat id]
		-> LHsExpr id -> LHsBind id

mk_easy_FunBind loc fun pats expr
  = L loc $ mkFunBind (L loc fun) [mkMatch pats expr emptyLocalBinds]

------------
mk_FunBind :: SrcSpan -> id
	   -> [([LPat id], LHsExpr id)]
	   -> LHsBind id

mk_FunBind _   _   [] = panic "TcGenDeriv:mk_FunBind"
mk_FunBind loc fun pats_and_exprs
  = L loc $ mkFunBind (L loc fun) matches
  where
    matches = [mkMatch p e emptyLocalBinds | (p,e) <-pats_and_exprs]

------------
mkMatch :: [LPat id] -> LHsExpr id -> HsLocalBinds id -> LMatch id
mkMatch pats expr binds
  = noLoc (Match (map paren pats) Nothing 
		 (GRHSs (unguardedRHS expr) binds))
  where
    paren lp@(L l p) | hsPatNeedsParens p = L l (ParPat lp) 
		     | otherwise          = lp
\end{code}


%************************************************************************
%*									*
	Collecting binders from HsBindGroups and HsBinds
%*									*
%************************************************************************

Get all the binders in some HsBindGroups, IN THE ORDER OF APPEARANCE. eg.

...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...

it should return [x, y, f, a, b] (remember, order important).

\begin{code}
collectLocalBinders :: HsLocalBindsLR idL idR -> [Located idL]
collectLocalBinders (HsValBinds val_binds) = collectHsValBinders val_binds
collectLocalBinders (HsIPBinds _)   = []
collectLocalBinders EmptyLocalBinds = []

collectHsValBinders :: HsValBindsLR idL idR -> [Located idL]
collectHsValBinders (ValBindsIn  binds _) = collectHsBindLocatedBinders binds
collectHsValBinders (ValBindsOut binds _) = foldr collect_one [] binds
  where
   collect_one (_,binds) acc = foldrBag (collectAcc . unLoc) acc binds

collectAcc :: HsBindLR idL idR -> [Located idL] -> [Located idL]
collectAcc (PatBind { pat_lhs = p }) acc = collectLocatedPatBinders p ++ acc
collectAcc (FunBind { fun_id = f })  acc    = f : acc
collectAcc (VarBind { var_id = f })  acc    = noLoc f : acc
collectAcc (AbsBinds { abs_exports = dbinds, abs_binds = _binds }) acc
  = [noLoc dp | (_,dp,_,_) <- dbinds] ++ acc
	-- ++ foldr collectAcc acc binds
	-- I don't think we want the binders from the nested binds
	-- The only time we collect binders from a typechecked 
	-- binding (hence see AbsBinds) is in zonking in TcHsSyn

collectHsBindBinders :: LHsBindsLR idL idR -> [idL]
collectHsBindBinders binds = map unLoc (collectHsBindLocatedBinders binds)

collectHsBindLocatedBinders :: LHsBindsLR idL idR -> [Located idL]
collectHsBindLocatedBinders binds = foldrBag (collectAcc . unLoc) [] binds
\end{code}


%************************************************************************
%*									*
	Getting binders from statements
%*									*
%************************************************************************

\begin{code}
collectLStmtsBinders :: [LStmtLR idL idR] -> [Located idL]
collectLStmtsBinders = concatMap collectLStmtBinders

collectStmtsBinders :: [StmtLR idL idR] -> [Located idL]
collectStmtsBinders = concatMap collectStmtBinders

collectLStmtBinders :: LStmtLR idL idR -> [Located idL]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: StmtLR idL idR -> [Located idL]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders (BindStmt pat _ _ _) = collectLocatedPatBinders pat
collectStmtBinders (LetStmt binds)      = collectLocalBinders binds
collectStmtBinders (ExprStmt _ _ _)     = []
collectStmtBinders (ParStmt xs)         = collectLStmtsBinders
                                        $ concatMap fst xs
collectStmtBinders (TransformStmt (stmts, _) _ _) = collectLStmtsBinders stmts
collectStmtBinders (GroupStmt (stmts, _) _)       = collectLStmtsBinders stmts
collectStmtBinders (RecStmt { recS_stmts = ss })  = collectLStmtsBinders ss
\end{code}


%************************************************************************
%*									*
%* 	Gathering stuff out of patterns
%*									*
%************************************************************************

This function @collectPatBinders@ works with the ``collectBinders''
functions for @HsBinds@, etc.  The order in which the binders are
collected is important; see @HsBinds.lhs@.

It collects the bounds *value* variables in renamed patterns; type variables
are *not* collected.

\begin{code}
collectPatBinders :: LPat a -> [a]
collectPatBinders pat = map unLoc (collectLocatedPatBinders pat)

collectLocatedPatBinders :: LPat a -> [Located a]
collectLocatedPatBinders pat = collectl pat []

collectPatsBinders :: [LPat a] -> [a]
collectPatsBinders pats = map unLoc (collectLocatedPatsBinders pats)

collectLocatedPatsBinders :: [LPat a] -> [Located a]
collectLocatedPatsBinders pats = foldr collectl [] pats

---------------------
collectl :: LPat name -> [Located name] -> [Located name]
collectl (L l pat) bndrs
  = go pat
  where
    go (VarPat var) 	   	  = L l var : bndrs
    go (VarPatOut var bs) 	  = L l var : collectHsBindLocatedBinders bs 
				    ++ bndrs
    go (WildPat _)	      	  = bndrs
    go (LazyPat pat)     	  = collectl pat bndrs
    go (BangPat pat)     	  = collectl pat bndrs
    go (AsPat a pat)     	  = a : collectl pat bndrs
    go (ViewPat _ pat _)          = collectl pat bndrs
    go (ParPat  pat)     	  = collectl pat bndrs
				  
    go (ListPat pats _)    	  = foldr collectl bndrs pats
    go (PArrPat pats _)    	  = foldr collectl bndrs pats
    go (TuplePat pats _ _)  	  = foldr collectl bndrs pats
				  
    go (ConPatIn _ ps)            = foldr collectl bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps})  = foldr collectl bndrs (hsConPatArgs ps)
	-- See Note [Dictionary binders in ConPatOut]
    go (LitPat _)	      	  = bndrs
    go (NPat _ _ _)		  = bndrs
    go (NPlusKPat n _ _ _)        = n : bndrs
 				  
    go (SigPatIn pat _)	 	  = collectl pat bndrs
    go (SigPatOut pat _)	  = collectl pat bndrs
    go (QuasiQuotePat _)          = bndrs
    go (TypePat _)                = bndrs
    go (CoPat _ pat _)            = collectl (noLoc pat) bndrs
\end{code}

Note [Dictionary binders in ConPatOut]
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
collectSigTysFromPats :: [InPat name] -> [LHsType name]
collectSigTysFromPats pats = foldr collect_lpat [] pats

collectSigTysFromPat :: InPat name -> [LHsType name]
collectSigTysFromPat pat = collect_lpat pat []

collect_lpat :: InPat name -> [LHsType name] -> [LHsType name]
collect_lpat pat acc = collect_pat (unLoc pat) acc

collect_pat :: Pat name -> [LHsType name] -> [LHsType name]
collect_pat (SigPatIn pat ty)  	acc = collect_lpat pat (ty:acc)
collect_pat (TypePat ty)       	acc = ty:acc

collect_pat (LazyPat pat)      	acc = collect_lpat pat acc
collect_pat (BangPat pat)      	acc = collect_lpat pat acc
collect_pat (AsPat _ pat)      	acc = collect_lpat pat acc
collect_pat (ParPat  pat)      	acc = collect_lpat pat acc
collect_pat (ListPat pats _)   	acc = foldr collect_lpat acc pats
collect_pat (PArrPat pats _)   	acc = foldr collect_lpat acc pats
collect_pat (TuplePat pats _ _) acc = foldr collect_lpat acc pats
collect_pat (ConPatIn _ ps)     acc = foldr collect_lpat acc (hsConPatArgs ps)
collect_pat _                   acc = acc       -- Literals, vars, wildcard
\end{code}
