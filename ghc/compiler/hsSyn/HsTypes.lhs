%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsTypes]{Abstract syntax: user-defined types}

\begin{code}
module HsTypes (
	  HsType(..), HsUsageAnn(..), HsTyVarBndr(..),
	, HsContext, HsPred(..)
	, HsTupCon(..), hsTupParens, mkHsTupCon,

	, mkHsForAllTy, mkHsUsForAllTy, mkHsDictTy, mkHsIParamTy
	, getTyVarName, replaceTyVarName

	-- Printing
	, pprParendHsType, pprHsForAll, pprHsContext, pprHsTyVarBndr

	-- Equality over Hs things
	, EqHsEnv, emptyEqHsEnv, extendEqHsEnv,
	, eqWithHsTyVars, eq_hsVar, eq_hsVars, eq_hsType, eq_hsContext, eqListBy

	-- Converting from Type to HsType
	, toHsType, toHsTyVar, toHsTyVars, toHsContext, toHsFDs
    ) where

#include "HsVersions.h"

import Class		( FunDep )
import Type		( Type, Kind, PredType(..), UsageAnn(..), ClassContext,
			  getTyVar_maybe, splitFunTy_maybe, splitAppTy_maybe,
			  splitTyConApp_maybe, splitPredTy_maybe,
			  splitUsgTy, splitSigmaTy, unUsgTy, boxedTypeKind
			)
import TypeRep		( Type(..), TyNote(..) )	-- toHsType sees the representation
import TyCon		( isTupleTyCon, tupleTyConBoxity, tyConArity, tyConClass_maybe )
import PrelInfo         ( mkTupConRdrName )
import RdrName		( RdrName )
import Name		( toRdrName )
import OccName		( NameSpace )
import Var		( TyVar, tyVarKind )
import PprType		( {- instance Outputable Kind -}, pprParendKind )
import BasicTypes	( Arity, Boxity(..), tupleParens )
import Unique		( hasKey, listTyConKey, Uniquable(..) )
import Maybes		( maybeToBool )
import FiniteMap
import Outputable
\end{code}

This is the syntax for types as seen in type signatures.

\begin{code}
type HsContext name = [HsPred name]

data HsPred name = HsPClass name [HsType name]
		 | HsPIParam name (HsType name)

data HsType name
  = HsForAllTy	(Maybe [HsTyVarBndr name])	-- Nothing for implicitly quantified signatures
		(HsContext name)
		(HsType name)

  | HsTyVar		name		-- Type variable

  | HsAppTy		(HsType name)
			(HsType name)

  | HsFunTy		(HsType name) -- function type
			(HsType name)

  | HsListTy		(HsType name)	-- Element type

  | HsTupleTy		(HsTupCon name)
			[HsType name]	-- Element types (length gives arity)

  -- these next two are only used in interfaces
  | HsPredTy		(HsPred name)

  | HsUsgTy           (HsUsageAnn name)
                        (HsType name)

  | HsUsgForAllTy     name
                        (HsType name)

data HsUsageAnn name
  = HsUsOnce
  | HsUsMany
  | HsUsVar name
  

-----------------------
data HsTupCon name = HsTupCon name Boxity

instance Eq name => Eq (HsTupCon name) where
  (HsTupCon _ b1) == (HsTupCon _ b2) = b1==b2
   
mkHsTupCon :: NameSpace -> Boxity -> [a] -> HsTupCon RdrName
mkHsTupCon space boxity args = HsTupCon (mkTupConRdrName space boxity (length args)) boxity

hsTupParens :: HsTupCon name -> SDoc -> SDoc
hsTupParens (HsTupCon _ b) p = tupleParens b p

-----------------------
-- Combine adjacent for-alls. 
-- The following awkward situation can happen otherwise:
--	f :: forall a. ((Num a) => Int)
-- might generate HsForAll (Just [a]) [] (HsForAll Nothing [Num a] t)
-- Then a isn't discovered as ambiguous, and we abstract the AbsBinds wrt []
-- but the export list abstracts f wrt [a].  Disaster.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkHsForAllTy (Just []) [] ty = ty	-- Explicit for-all with no tyvars
mkHsForAllTy mtvs1     [] (HsForAllTy mtvs2 ctxt ty) = mkHsForAllTy (mtvs1 `plus` mtvs2) ctxt ty
						     where
						       mtvs1       `plus` Nothing     = mtvs1
						       Nothing     `plus` mtvs2       = mtvs2 
						       (Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)
mkHsForAllTy tvs ctxt ty = HsForAllTy tvs ctxt ty

mkHsUsForAllTy uvs ty = foldr (\ uv ty -> HsUsgForAllTy uv ty)
                              ty uvs

mkHsDictTy cls tys = HsPredTy (HsPClass cls tys)
mkHsIParamTy v ty  = HsPredTy (HsPIParam v ty)

data HsTyVarBndr name
  = UserTyVar name
  | IfaceTyVar name Kind
	-- *** NOTA BENE *** A "monotype" in a pragma can have
	-- for-alls in it, (mostly to do with dictionaries).  These
	-- must be explicitly Kinded.

getTyVarName (UserTyVar n)    = n
getTyVarName (IfaceTyVar n _) = n

replaceTyVarName :: HsTyVarBndr name1 -> name2 -> HsTyVarBndr name2
replaceTyVarName (UserTyVar n)    n' = UserTyVar n'
replaceTyVarName (IfaceTyVar n k) n' = IfaceTyVar n' k
\end{code}


%************************************************************************
%*									*
\subsection{Pretty printing}
%*									*
%************************************************************************

NB: these types get printed into interface files, so 
    don't change the printing format lightly

\begin{code}
instance (Outputable name) => Outputable (HsType name) where
    ppr ty = pprHsType ty

instance (Outputable name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar name)       = ppr name
    ppr (IfaceTyVar name kind) = pprHsTyVarBndr name kind

instance Outputable name => Outputable (HsPred name) where
    ppr (HsPClass clas tys) = ppr clas <+> hsep (map pprParendHsType tys)
    ppr (HsPIParam n ty)    = hsep [{- char '?' <> -} ppr n, text "::", ppr ty]

pprHsTyVarBndr :: Outputable name => name -> Kind -> SDoc
pprHsTyVarBndr name kind | kind == boxedTypeKind = ppr name
			 | otherwise 	         = hsep [ppr name, dcolon, pprParendKind kind]

pprHsForAll []  []  = empty
pprHsForAll tvs cxt = ptext SLIT("__forall") <+> interppSP tvs <+> ppr_context cxt <+> ptext SLIT("=>")

pprHsContext :: (Outputable name) => HsContext name -> SDoc
pprHsContext []	 = empty
pprHsContext cxt = ppr_context cxt <+> ptext SLIT("=>")

ppr_context []  = empty
ppr_context cxt = parens (interpp'SP cxt)
\end{code}

\begin{code}
pREC_TOP = (0 :: Int)
pREC_FUN = (1 :: Int)
pREC_CON = (2 :: Int)

maybeParen :: Bool -> SDoc -> SDoc
maybeParen True  p = parens p
maybeParen False p = p
	
-- printing works more-or-less as for Types

pprHsType, pprParendHsType :: (Outputable name) => HsType name -> SDoc

pprHsType ty       = ppr_mono_ty pREC_TOP ty
pprParendHsType ty = ppr_mono_ty pREC_CON ty

ppr_mono_ty ctxt_prec (HsForAllTy maybe_tvs ctxt ty)
  = maybeParen (ctxt_prec >= pREC_FUN) $
    sep [pp_header, pprHsType ty]
  where
    pp_header = case maybe_tvs of
		  Just tvs -> pprHsForAll tvs ctxt
		  Nothing  -> pprHsContext ctxt

ppr_mono_ty ctxt_prec (HsTyVar name)
  = ppr name

ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)
  = let p1 = ppr_mono_ty pREC_FUN ty1
	p2 = ppr_mono_ty pREC_TOP ty2
    in
    maybeParen (ctxt_prec >= pREC_FUN)
	       (sep [p1, (<>) (ptext SLIT("-> ")) p2])

ppr_mono_ty ctxt_prec (HsTupleTy con tys) = hsTupParens con (interpp'SP tys)
ppr_mono_ty ctxt_prec (HsListTy ty)	  = brackets (ppr_mono_ty pREC_TOP ty)

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen (ctxt_prec >= pREC_CON)
	       (hsep [ppr_mono_ty pREC_FUN fun_ty, ppr_mono_ty pREC_CON arg_ty])

ppr_mono_ty ctxt_prec (HsPredTy pred) 
  = maybeParen (ctxt_prec >= pREC_FUN) $
    braces (ppr pred)

ppr_mono_ty ctxt_prec ty@(HsUsgForAllTy _ _)
  = 
    sep [ ptext SLIT("__fuall") <+> brackets pp_uvars <+> ptext SLIT("=>"),
          ppr_mono_ty pREC_TOP sigma
        ]
  where
    (uvars,sigma) = split [] ty
    pp_uvars      = interppSP uvars

    split uvs (HsUsgForAllTy uv ty') = split (uv:uvs) ty'
    split uvs ty'                      = (reverse uvs,ty')

ppr_mono_ty ctxt_prec (HsUsgTy u ty)
  = maybeParen (ctxt_prec >= pREC_CON) $
    ptext SLIT("__u") <+> pp_ua <+> ppr_mono_ty pREC_CON ty
  where
    pp_ua = case u of
              HsUsOnce   -> ptext SLIT("-")
              HsUsMany   -> ptext SLIT("!")
              HsUsVar uv -> ppr uv
\end{code}


%************************************************************************
%*									*
\subsection{Converting from Type to HsType}
%*									*
%************************************************************************

@toHsType@ converts from a Type to a HsType, making the latter look as
user-friendly as possible.  Notably, it uses synonyms where possible, and
expresses overloaded functions using the '=>' context part of a HsForAllTy.

\begin{code}
toHsTyVar :: TyVar -> HsTyVarBndr RdrName
toHsTyVar tv = IfaceTyVar (toRdrName tv) (tyVarKind tv)

toHsTyVars tvs = map toHsTyVar tvs

toHsType :: Type -> HsType RdrName
toHsType ty = toHsType' (unUsgTy ty)
	-- For now we just discard the usage
--  = case splitUsgTy ty of
--	(usg, tau) -> HsUsgTy (toHsUsg usg) (toHsType' tau)
	
toHsType' :: Type -> HsType RdrName
-- Called after the usage is stripped off
-- This function knows the representation of types
toHsType' (TyVarTy tv)    = HsTyVar (toRdrName tv)
toHsType' (FunTy arg res) = HsFunTy (toHsType arg) (toHsType res)
toHsType' (AppTy fun arg) = HsAppTy (toHsType fun) (toHsType arg) 

toHsType' (NoteTy (SynNote ty) _) = toHsType ty		-- Use synonyms if possible!!
toHsType' (NoteTy _ ty)		  = toHsType ty

toHsType' ty@(TyConApp tc tys)	-- Must be saturated because toHsType's arg is of kind *
  | not saturated	     = generic_case
  | isTupleTyCon tc	     = HsTupleTy (HsTupCon (toRdrName tc) (tupleTyConBoxity tc)) tys'
  | tc `hasKey` listTyConKey = HsListTy (head tys')
  | maybeToBool maybe_class  = HsPredTy (HsPClass (toRdrName clas) tys')
  | otherwise		     = generic_case
  where
     generic_case = foldl HsAppTy (HsTyVar (toRdrName tc)) tys'
     maybe_class  = tyConClass_maybe tc
     Just clas    = maybe_class
     tys'         = map toHsType tys
     saturated    = length tys == tyConArity tc

toHsType' ty@(ForAllTy _ _) = case splitSigmaTy ty of
			        (tvs, preds, tau) -> HsForAllTy (Just (map toHsTyVar tvs))
			 				        (map toHsPred preds)
						                (toHsType tau)


toHsPred (Class cls tys) = HsPClass (toRdrName cls) (map toHsType tys)
toHsPred (IParam n ty)	 = HsPIParam (toRdrName n)  (toHsType ty)

toHsContext :: ClassContext -> HsContext RdrName
toHsContext cxt = [HsPClass (toRdrName cls) (map toHsType tys) | (cls,tys) <- cxt]

toHsUsg UsOnce    = HsUsOnce
toHsUsg UsMany    = HsUsMany
toHsUsg (UsVar v) = HsUsVar (toRdrName v)

toHsFDs :: [FunDep TyVar] -> [FunDep RdrName]
toHsFDs fds = [(map toRdrName ns, map toRdrName ms) | (ns,ms) <- fds]
\end{code}


%************************************************************************
%*									*
\subsection{Comparison}
%*									*
%************************************************************************

\begin{code}
instance Ord a => Eq (HsType a) where
	-- The Ord is needed because we keep a
	-- finite map of variables to variables
   (==) a b = eq_hsType emptyEqHsEnv a b

instance Ord a => Eq (HsPred a) where
   (==) a b = eq_hsPred emptyEqHsEnv a b

eqWithHsTyVars :: Ord name =>
	          [HsTyVarBndr name] -> [HsTyVarBndr name]
	       -> (EqHsEnv name -> Bool) -> Bool
eqWithHsTyVars = eq_hsTyVars emptyEqHsEnv
\end{code}

\begin{code}
type EqHsEnv n = FiniteMap n n
-- Tracks the mapping from L-variables to R-variables

eq_hsVar :: Ord n => EqHsEnv n -> n -> n -> Bool
eq_hsVar env n1 n2 = case lookupFM env n1 of
		      Just n1 -> n1 == n2
		      Nothing -> n1 == n2

extendEqHsEnv env n1 n2 
  | n1 == n2  = env
  | otherwise = addToFM env n1 n2

emptyEqHsEnv :: EqHsEnv n
emptyEqHsEnv = emptyFM
\end{code}

We do define a specialised equality for these \tr{*Type} types; used
in checking interfaces.

\begin{code}
-------------------
eq_hsTyVars env [] 	    []	       k = k env
eq_hsTyVars env (tv1:tvs1) (tv2:tvs2)  k = eq_hsTyVar env tv1 tv2 $ \ env ->
					   eq_hsTyVars env tvs1 tvs2 k
eq_hsTyVars env _ _ _ = False

eq_hsTyVar env (UserTyVar v1)     (UserTyVar v2)     k = k (extendEqHsEnv env v1 v2)
eq_hsTyVar env (IfaceTyVar v1 k1) (IfaceTyVar v2 k2) k = k1 == k2 && k (extendEqHsEnv env v1 v2)
eq_hsTyVar env _ _ _ = False

eq_hsVars env []       []       k = k env
eq_hsVars env (v1:bs1) (v2:bs2) k = eq_hsVars (extendEqHsEnv env v1 v2) bs1 bs2 k
eq_hsVars env _ _ _ = False
\end{code}

\begin{code}
-------------------
eq_hsTypes env = eqListBy (eq_hsType env)

-------------------
eq_hsType env (HsForAllTy tvs1 c1 t1) (HsForAllTy tvs2 c2 t2)
  = eq_tvs tvs1 tvs2 		$ \env ->
    eq_hsContext env c1 c2   	&&
    eq_hsType env t1 t2
  where
    eq_tvs Nothing     (Just _) k    = False
    eq_tvs Nothing     Nothing  k    = k env
    eq_tvs (Just _)    Nothing  k    = False
    eq_tvs (Just tvs1) (Just tvs2) k = eq_hsTyVars env tvs1 tvs2 k

eq_hsType env (HsTyVar n1) (HsTyVar n2)
  = eq_hsVar env n1 n2

eq_hsType env (HsTupleTy c1 tys1) (HsTupleTy c2 tys2)
  = (c1 == c2) && eq_hsTypes env tys1 tys2

eq_hsType env (HsListTy ty1) (HsListTy ty2)
  = eq_hsType env ty1 ty2

eq_hsType env (HsAppTy fun_ty1 arg_ty1) (HsAppTy fun_ty2 arg_ty2)
  = eq_hsType env fun_ty1 fun_ty2 && eq_hsType env arg_ty1 arg_ty2

eq_hsType env (HsFunTy a1 b1) (HsFunTy a2 b2)
  = eq_hsType env a1 a2 && eq_hsType env b1 b2

eq_hsType env (HsPredTy p1) (HsPredTy p2)
  = eq_hsPred env p1 p2

eq_hsType env (HsUsgTy u1 ty1) (HsUsgTy u2 ty2)
  = eqUsg u1 u2 && eq_hsType env ty1 ty2

eq_hsType env ty1 ty2 = False


-------------------
eq_hsContext env a b = eqListBy (eq_hsPred env) a b

-------------------
eq_hsPred env (HsPClass c1 tys1) (HsPClass c2 tys2)
  = c1 == c2 &&  eq_hsTypes env tys1 tys2
eq_hsPred env (HsPIParam n1 ty1) (HsPIParam n2 ty2)
  = n1 == n2 && eq_hsType env ty1 ty2
eq_hsPred env _ _ = False

-------------------
eqUsg  HsUsOnce     HsUsOnce    = True
eqUsg  HsUsMany     HsUsMany    = True
eqUsg (HsUsVar u1) (HsUsVar u2) = u1 == u2
eqUsg _	_ = False

-------------------
eqListBy :: (a->a->Bool) -> [a] -> [a] -> Bool
eqListBy eq []     []     = True
eqListBy eq (x:xs) (y:ys) = eq x y && eqListBy eq xs ys
eqListBy eq xs     ys     = False
\end{code}
