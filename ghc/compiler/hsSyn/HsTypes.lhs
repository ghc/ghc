%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsTypes]{Abstract syntax: user-defined types}

\begin{code}
module HsTypes (
	  HsType(..), HsTyVarBndr(..), HsTyOp(..),
	, HsContext, HsPred(..)
	, HsTupCon(..), hsTupParens, mkHsTupCon,

	, mkHsForAllTy, mkHsDictTy, mkHsIParamTy
	, hsTyVarName, hsTyVarNames, replaceTyVarName
	, splitHsInstDeclTy
	
	-- Type place holder
	, PostTcType, placeHolderType,

	-- Name place holder
	, SyntaxName, placeHolderName,

	-- Printing
	, pprParendHsType, pprHsForAll, pprHsContext, ppr_hs_context, pprHsTyVarBndr

	-- Equality over Hs things
	, EqHsEnv, emptyEqHsEnv, extendEqHsEnv,
	, eqWithHsTyVars, eq_hsVar, eq_hsVars, eq_hsTyVars, eq_hsType, eq_hsContext, eqListBy

	-- Converting from Type to HsType
	, toHsType, toHsTyVar, toHsTyVars, toHsContext, toHsFDs
    ) where

#include "HsVersions.h"

import Class		( FunDep )
import TcType		( Type, Kind, ThetaType, SourceType(..), 
			  tcSplitSigmaTy, liftedTypeKind, eqKind, tcEqType
			)
import TypeRep		( Type(..), TyNote(..) )	-- toHsType sees the representation
import TyCon		( isTupleTyCon, tupleTyConBoxity, tyConArity, isNewTyCon, getSynTyConDefn )
import RdrName		( mkUnqual )
import Name		( Name, getName, mkInternalName )
import OccName		( NameSpace, mkVarOcc, tvName )
import Var		( TyVar, tyVarKind )
import Subst		( substTyWith )
import PprType		( {- instance Outputable Kind -}, pprParendKind, pprKind )
import BasicTypes	( Boxity(..), Arity, IPName, tupleParens )
import PrelNames	( listTyConKey, parrTyConKey,
			  hasKey, unboundKey )
import SrcLoc		( noSrcLoc )
import Util		( eqListBy, lengthIs )
import FiniteMap
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Annotating the syntax}
%*									*
%************************************************************************

\begin{code}
type PostTcType = Type		-- Used for slots in the abstract syntax
				-- where we want to keep slot for a type
				-- to be added by the type checker...but
				-- before typechecking it's just bogus

placeHolderType :: PostTcType	-- Used before typechecking
placeHolderType  = panic "Evaluated the place holder for a PostTcType"


type SyntaxName = Name		-- These names are filled in by the renamer
				-- Before then they are a placeHolderName (so that
				--	we can still print the HsSyn)
				-- They correspond to "rebindable syntax";
				-- See RnEnv.lookupSyntaxName

placeHolderName :: SyntaxName
placeHolderName = mkInternalName unboundKey 
			(mkVarOcc FSLIT("syntaxPlaceHolder")) 
			noSrcLoc
\end{code}


%************************************************************************
%*									*
\subsection{Data types}
%*									*
%************************************************************************

This is the syntax for types as seen in type signatures.

\begin{code}
type HsContext name = [HsPred name]

data HsPred name = HsClassP name [HsType name]
		 | HsIParam (IPName name) (HsType name)

data HsType name
  = HsForAllTy	(Maybe [HsTyVarBndr name])	-- Nothing for implicitly quantified signatures
		(HsContext name)
		(HsType name)

  | HsTyVar		name		-- Type variable or type constructor

  | HsAppTy		(HsType name)
			(HsType name)

  | HsFunTy		(HsType name)   -- function type
			(HsType name)

  | HsListTy		(HsType name)	-- Element type

  | HsPArrTy		(HsType name)	-- Elem. type of parallel array: [:t:]

  | HsTupleTy		HsTupCon
			[HsType name]	-- Element types (length gives arity)

  | HsOpTy		(HsType name) (HsTyOp name) (HsType name)

  | HsParTy		(HsType name)   
	-- Parenthesis preserved for the precedence re-arrangement in RnTypes
	-- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!
	-- 
	-- However, NB that toHsType doesn't add HsParTys (in an effort to keep
	-- interface files smaller), so when printing a HsType we may need to
	-- add parens.  

  | HsNumTy             Integer		-- Generics only

  -- these next two are only used in interfaces
  | HsPredTy		(HsPred name)

  | HsKindSig		(HsType name)	-- (ty :: kind)
			Kind		-- A type with a kind signature


data HsTyOp name = HsArrow | HsTyOp name
	-- Function arrows from *source* get read in as HsOpTy t1 HsArrow t2
	-- But when we generate or parse interface files, we use HsFunTy.
	-- This keeps interfaces a bit smaller, because there are a lot of arrows

-----------------------
data HsTupCon = HsTupCon Boxity Arity

instance Eq HsTupCon where
  (HsTupCon b1 a1) == (HsTupCon b2 a2) = b1==b2 && a1==a2
   
mkHsTupCon :: NameSpace -> Boxity -> [a] -> HsTupCon
mkHsTupCon space boxity args = HsTupCon boxity (length args)

hsTupParens :: HsTupCon -> SDoc -> SDoc
hsTupParens (HsTupCon b _) p = tupleParens b p

-----------------------
-- Combine adjacent for-alls. 
-- The following awkward situation can happen otherwise:
--	f :: forall a. ((Num a) => Int)
-- might generate HsForAll (Just [a]) [] (HsForAll Nothing [Num a] t)
-- Then a isn't discovered as ambiguous, and we abstract the AbsBinds wrt []
-- but the export list abstracts f wrt [a].  Disaster.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkHsForAllTy mtvs []   ty = mk_forall_ty mtvs ty
mkHsForAllTy mtvs ctxt ty = HsForAllTy mtvs ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty (Just []) ty 			  = ty	-- Explicit for-all with no tyvars
mk_forall_ty mtvs1     (HsParTy ty)		  = mk_forall_ty mtvs1 ty
mk_forall_ty mtvs1     (HsForAllTy mtvs2 ctxt ty) = mkHsForAllTy (mtvs1 `plus` mtvs2) ctxt ty
mk_forall_ty mtvs1     ty			  = HsForAllTy mtvs1 [] ty

mtvs1       `plus` Nothing     = mtvs1
Nothing     `plus` mtvs2       = mtvs2 
(Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)

mkHsDictTy cls tys = HsPredTy (HsClassP cls tys)
mkHsIParamTy v ty  = HsPredTy (HsIParam v ty)

data HsTyVarBndr name
  = UserTyVar name
  | IfaceTyVar name Kind
	-- *** NOTA BENE *** A "monotype" in a pragma can have
	-- for-alls in it, (mostly to do with dictionaries).  These
	-- must be explicitly Kinded.

hsTyVarName (UserTyVar n)    = n
hsTyVarName (IfaceTyVar n _) = n

hsTyVarNames tvs = map hsTyVarName tvs

replaceTyVarName :: HsTyVarBndr name1 -> name2 -> HsTyVarBndr name2
replaceTyVarName (UserTyVar n)    n' = UserTyVar n'
replaceTyVarName (IfaceTyVar n k) n' = IfaceTyVar n' k
\end{code}


\begin{code}
splitHsInstDeclTy 
    :: Outputable name
    => HsType name 
    -> ([HsTyVarBndr name], HsContext name, name, [HsType name])
	-- Split up an instance decl type, returning the pieces

-- In interface files, the instance declaration head is created
-- by HsTypes.toHsType, which does not guarantee to produce a
-- HsForAllTy.  For example, if we had the weird decl
--	instance Foo T => Foo [T]
-- then we'd get the instance type
--	Foo T -> Foo [T]
-- So when colleting the instance context, to be on the safe side
-- we gather predicate arguments
-- 
-- For source code, the parser ensures the type will have the right shape.
-- (e.g. see ParseUtil.checkInstType)

splitHsInstDeclTy inst_ty
  = case inst_ty of
	HsForAllTy (Just tvs) cxt1 tau 
	      -> (tvs, cxt1++cxt2, cls, tys)
	      where
		 (cxt2, cls, tys) = split_tau tau

	other -> ([],  cxt2,  cls, tys)
	      where
		 (cxt2, cls, tys) = split_tau inst_ty

  where
    split_tau (HsFunTy (HsPredTy p) ty)	= (p:ps, cls, tys)
					where
					  (ps, cls, tys) = split_tau ty
    split_tau (HsPredTy (HsClassP cls tys)) = ([], cls,tys)
    split_tau other = pprPanic "splitHsInstDeclTy" (ppr inst_ty)
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

instance (Outputable name) => Outputable (HsTyOp name) where
    ppr HsArrow    = ftext FSLIT("->")
    ppr (HsTyOp n) = ppr n

instance (Outputable name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar name)       = ppr name
    ppr (IfaceTyVar name kind) = pprHsTyVarBndr name kind

instance Outputable name => Outputable (HsPred name) where
    ppr (HsClassP clas tys) = ppr clas <+> hsep (map pprParendHsType tys)
    ppr (HsIParam n ty)    = hsep [ppr n, dcolon, ppr ty]

pprHsTyVarBndr :: Outputable name => name -> Kind -> SDoc
pprHsTyVarBndr name kind | kind `eqKind` liftedTypeKind = ppr name
			 | otherwise 	  	        = hsep [ppr name, dcolon, pprParendKind kind]

pprHsForAll []  []  = empty
pprHsForAll tvs cxt 
	-- This printer is used for both interface files and
	-- printing user types in error messages; and alas the
	-- two use slightly different syntax.  Ah well.
  = getPprStyle $ \ sty ->
    if userStyle sty then
	ptext SLIT("forall") <+> interppSP tvs <> dot <+> 
              -- **! ToDo: want to hide uvars from user, but not enough info
              -- in a HsTyVarBndr name (see PprType).  KSW 2000-10.
	pprHsContext cxt
    else	-- Used in interfaces
	ptext SLIT("__forall") <+> interppSP tvs <+> 
	ppr_hs_context cxt <+> ptext SLIT("=>")

pprHsContext :: (Outputable name) => HsContext name -> SDoc
pprHsContext []	 = empty
pprHsContext cxt = ppr_hs_context cxt <+> ptext SLIT("=>")

ppr_hs_context []  = empty
ppr_hs_context cxt = parens (interpp'SP cxt)
\end{code}

\begin{code}
pREC_TOP = (0 :: Int)  -- type   in ParseIface.y
pREC_FUN = (1 :: Int)  -- btype  in ParseIface.y
			-- Used for LH arg of (->)
pREC_OP  = (2 :: Int)	-- Used for arg of any infix operator
			-- (we don't keep their fixities around)
pREC_CON = (3 :: Int)	-- Used for arg of type applicn: 
			-- always parenthesise unless atomic

maybeParen :: Int 	-- Precedence of context
	   -> Int	-- Precedence of top-level operator
	   -> SDoc -> SDoc	-- Wrap in parens if (ctxt >= op)
maybeParen ctxt_prec op_prec p | ctxt_prec >= op_prec = parens p
			       | otherwise	      = p
	
-- printing works more-or-less as for Types

pprHsType, pprParendHsType :: (Outputable name) => HsType name -> SDoc

pprHsType ty       = ppr_mono_ty pREC_TOP (de_paren ty)
pprParendHsType ty = ppr_mono_ty pREC_CON ty

-- Remove outermost HsParTy parens before printing a type
de_paren (HsParTy ty) = de_paren ty
de_paren ty	      = ty

ppr_mono_ty ctxt_prec (HsForAllTy maybe_tvs ctxt ty)
  = maybeParen ctxt_prec pREC_FUN $
    sep [pp_header, pprHsType ty]
  where
    pp_header = case maybe_tvs of
		  Just tvs -> pprHsForAll tvs ctxt
		  Nothing  -> pprHsContext ctxt

ppr_mono_ty ctxt_prec (HsTyVar name)      = ppr name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   = ppr_fun_ty ctxt_prec ty1 ty2
ppr_mono_ty ctxt_prec (HsTupleTy con tys) = hsTupParens con (interpp'SP tys)
ppr_mono_ty ctxt_prec (HsKindSig ty kind) = parens (ppr_mono_ty pREC_TOP ty <+> dcolon <+> pprKind kind)
ppr_mono_ty ctxt_prec (HsListTy ty)	  = brackets (ppr_mono_ty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPArrTy ty)	  = pabrackets (ppr_mono_ty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPredTy pred)     = braces (ppr pred)
ppr_mono_ty ctxt_prec (HsNumTy n)         = integer n  -- generics only

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_ty pREC_FUN fun_ty, ppr_mono_ty pREC_CON arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 HsArrow ty2) 
  = ppr_fun_ty ctxt_prec ty1 ty2

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2)  
  = maybeParen ctxt_prec pREC_OP $
    ppr_mono_ty pREC_OP ty1 <+> ppr op <+> ppr_mono_ty pREC_OP ty2

ppr_mono_ty ctxt_prec (HsParTy ty)
  = parens (ppr_mono_ty pREC_TOP ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --	toHsType doesn't put in any HsParTys, so we may still need them

--------------------------
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_ty pREC_FUN ty1
	p2 = ppr_mono_ty pREC_TOP ty2
    in
    maybeParen ctxt_prec pREC_FUN $
    sep [p1, ptext SLIT("->") <+> p2]

--------------------------
pabrackets p = ptext SLIT("[:") <> p <> ptext SLIT(":]")
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
toHsTyVar :: TyVar -> HsTyVarBndr Name
toHsTyVar tv = IfaceTyVar (getName tv) (tyVarKind tv)

toHsTyVars tvs = map toHsTyVar tvs

toHsType :: Type -> HsType Name
-- This function knows the representation of types
toHsType (TyVarTy tv)    = HsTyVar (getName tv)
toHsType (FunTy arg res) = HsFunTy (toHsType arg) (toHsType res)
toHsType (AppTy fun arg) = HsAppTy (toHsType fun) (toHsType arg) 

toHsType (NoteTy (SynNote ty@(TyConApp tycon tyargs)) real_ty)
  | isNewTyCon tycon = toHsType ty
  | syn_matches      = toHsType ty             -- Use synonyms if possible!!
  | otherwise        = 
#ifdef DEBUG
                       pprTrace "WARNING: synonym info lost in .hi file for " (ppr syn_ty) $
#endif
                       toHsType real_ty              -- but drop it if not.
  where
    syn_matches      = ty_from_syn `tcEqType` real_ty
    (tyvars,syn_ty)  = getSynTyConDefn tycon
    ty_from_syn      = substTyWith tyvars tyargs syn_ty

    -- We only use the type synonym in the file if this doesn't cause
    -- us to lose important information.  This matters for usage
    -- annotations.  It's an issue if some of the args to the synonym
    -- have arrows in them, or if the synonym's RHS has an arrow; for
    -- example, with nofib/real/ebnf2ps/ in Parsers.using.

    -- **! It would be nice if when this test fails we could still
    -- write the synonym in as a Note, so we don't lose the info for
    -- error messages, but it's too much work for right now.
    -- KSW 2000-07.

toHsType (NoteTy _ ty)	       = toHsType ty

toHsType (SourceTy (NType tc tys)) = foldl HsAppTy (HsTyVar (getName tc)) (map toHsType tys)
toHsType (SourceTy pred)	   = HsPredTy (toHsPred pred)

toHsType ty@(TyConApp tc tys)	-- Must be saturated because toHsType's arg is of kind *
  | not saturated	       = generic_case
  | isTupleTyCon tc	       = HsTupleTy (HsTupCon (tupleTyConBoxity tc) (tyConArity tc)) tys'
  | tc `hasKey` listTyConKey   = HsListTy (head tys')
  | tc `hasKey` parrTyConKey   = HsPArrTy (head tys')
  | otherwise		       = generic_case
  where
     generic_case = foldl HsAppTy (HsTyVar (getName tc)) tys'
     tys'         = map toHsType tys
     saturated    = tys `lengthIs` tyConArity tc

toHsType ty@(ForAllTy _ _) = case tcSplitSigmaTy ty of
			        (tvs, preds, tau) -> HsForAllTy (Just (map toHsTyVar tvs))
			 				        (map toHsPred preds)
						                (toHsType tau)

toHsPred (ClassP cls tys) = HsClassP (getName cls) (map toHsType tys)
toHsPred (IParam n ty)    = HsIParam n		   (toHsType ty)

toHsContext :: ThetaType -> HsContext Name
toHsContext theta = map toHsPred theta

toHsFDs :: [FunDep TyVar] -> [FunDep Name]
toHsFDs fds = [(map getName ns, map getName ms) | (ns,ms) <- fds]
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
eq_hsTyVar env (IfaceTyVar v1 k1) (IfaceTyVar v2 k2) k = k1 `eqKind` k2 && k (extendEqHsEnv env v1 v2)
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

eq_hsType env (HsKindSig ty1 k1) (HsKindSig ty2 k2)
  = eq_hsType env ty1 ty2 && k1 `eqKind` k2

eq_hsType env (HsPArrTy ty1) (HsPArrTy ty2)
  = eq_hsType env ty1 ty2

eq_hsType env (HsAppTy fun_ty1 arg_ty1) (HsAppTy fun_ty2 arg_ty2)
  = eq_hsType env fun_ty1 fun_ty2 && eq_hsType env arg_ty1 arg_ty2

eq_hsType env (HsFunTy a1 b1) (HsFunTy a2 b2)
  = eq_hsType env a1 a2 && eq_hsType env b1 b2

eq_hsType env (HsPredTy p1) (HsPredTy p2)
  = eq_hsPred env p1 p2

eq_hsType env (HsOpTy lty1 op1 rty1) (HsOpTy lty2 op2 rty2)
  = eq_hsOp env op1 op2 && eq_hsType env lty1 lty2 && eq_hsType env rty1 rty2

eq_hsType env ty1 ty2 = False


eq_hsOp env (HsTyOp n1) (HsTyOp n2) = eq_hsVar env n1 n2
eq_hsOp env HsArrow     HsArrow     = True
eq_hsOp env op1		op2	    = False

-------------------
eq_hsContext env a b = eqListBy (eq_hsPred env) a b

-------------------
eq_hsPred env (HsClassP c1 tys1) (HsClassP c2 tys2)
  = c1 == c2 &&  eq_hsTypes env tys1 tys2
eq_hsPred env (HsIParam n1 ty1) (HsIParam n2 ty2)
  = n1 == n2 && eq_hsType env ty1 ty2
eq_hsPred env _ _ = False
\end{code}
