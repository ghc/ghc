]%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsTypes]{Abstract syntax: user-defined types}

\begin{code}
module HsTypes (
	  HsType(..), HsTyVarBndr(..), HsExplicitForAll(..),
	, HsContext, HsPred(..)

	, mkExplicitHsForAllTy, mkImplicitHsForAllTy, 
	, mkHsDictTy, mkHsIParamTy
	, hsTyVarName, hsTyVarNames, replaceTyVarName
	, splitHsInstDeclTy
	
	-- Type place holder
	, PostTcType, placeHolderType,

	-- Name place holder
	, SyntaxName, placeHolderName,

	-- Printing
	, pprParendHsType, pprHsForAll, pprHsContext, ppr_hs_context, pprHsTyVarBndr
    ) where

#include "HsVersions.h"

import TcType		( Type, Kind, liftedTypeKind, eqKind )
import Type		( {- instance Outputable Kind -}, pprParendKind, pprKind )
import Name		( Name, mkInternalName )
import OccName		( mkVarOcc )
import BasicTypes	( IPName, Boxity, tupleParens )
import PrelNames	( unboundKey )
import SrcLoc		( noSrcLoc )
import CmdLineOpts	( opt_PprStyle_Debug )
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
  = HsForAllTy	HsExplicitForAll	-- Renamer leaves this flag unchanged, to record the way
					-- the user wrote it originally, so that the printer can
					-- print it as the user wrote it
		[HsTyVarBndr name]	-- With ImplicitForAll, this is the empty list
					-- until the renamer fills in the variables
		(HsContext name)
		(HsType name)

  | HsTyVar		name		-- Type variable or type constructor

  | HsAppTy		(HsType name)
			(HsType name)

  | HsFunTy		(HsType name)   -- function type
			(HsType name)

  | HsListTy		(HsType name)	-- Element type

  | HsPArrTy		(HsType name)	-- Elem. type of parallel array: [:t:]

  | HsTupleTy		Boxity
			[HsType name]	-- Element types (length gives arity)

  | HsOpTy		(HsType name) name (HsType name)

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

data HsExplicitForAll = Explicit | Implicit

-----------------------
-- Combine adjacent for-alls. 
-- The following awkward situation can happen otherwise:
--	f :: forall a. ((Num a) => Int)
-- might generate HsForAll (Just [a]) [] (HsForAll Nothing [Num a] t)
-- Then a isn't discovered as ambiguous, and we abstract the AbsBinds wrt []
-- but the export list abstracts f wrt [a].  Disaster.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkImplicitHsForAllTy     ctxt ty = mkHsForAllTy Implicit [] ctxt ty
mkExplicitHsForAllTy tvs ctxt ty = mkHsForAllTy Explicit tvs ctxt ty

mkHsForAllTy :: HsExplicitForAll -> [HsTyVarBndr name] -> HsContext name -> HsType name -> HsType name
-- Smart constructor for HsForAllTy
mkHsForAllTy exp tvs []   ty = mk_forall_ty exp tvs ty
mkHsForAllTy exp tvs ctxt ty = HsForAllTy exp tvs ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty Explicit [] ty 			      = ty	-- Explicit for-all with no tyvars
mk_forall_ty exp  tvs  (HsParTy ty)		      = mk_forall_ty exp tvs ty
mk_forall_ty exp1 tvs1 (HsForAllTy exp2 tvs2 ctxt ty) = mkHsForAllTy (exp1 `plus` exp2) (tvs1 ++ tvs2) ctxt ty
mk_forall_ty exp  tvs  ty			      = HsForAllTy exp tvs [] ty

Implicit `plus` Implicit = Implicit
exp1     `plus` exp2     = Explicit

mkHsDictTy cls tys = HsPredTy (HsClassP cls tys)
mkHsIParamTy v ty  = HsPredTy (HsIParam v ty)

data HsTyVarBndr name
  = UserTyVar name
  | KindedTyVar name Kind
	-- *** NOTA BENE *** A "monotype" in a pragma can have
	-- for-alls in it, (mostly to do with dictionaries).  These
	-- must be explicitly Kinded.

hsTyVarName (UserTyVar n)     = n
hsTyVarName (KindedTyVar n _) = n

hsTyVarNames tvs = map hsTyVarName tvs

replaceTyVarName :: HsTyVarBndr name1 -> name2 -> HsTyVarBndr name2
replaceTyVarName (UserTyVar n)     n' = UserTyVar n'
replaceTyVarName (KindedTyVar n k) n' = KindedTyVar n' k
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
	HsForAllTy _ tvs cxt1 tau 	-- The type vars should have been
					-- computed by now, even if they were implicit
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

instance (Outputable name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar name)        = ppr name
    ppr (KindedTyVar name kind) = pprHsTyVarBndr name kind

instance Outputable name => Outputable (HsPred name) where
    ppr (HsClassP clas tys) = ppr clas <+> hsep (map pprParendHsType tys)
    ppr (HsIParam n ty)    = hsep [ppr n, dcolon, ppr ty]

pprHsTyVarBndr :: Outputable name => name -> Kind -> SDoc
pprHsTyVarBndr name kind | kind `eqKind` liftedTypeKind = ppr name
			 | otherwise 	  	        = hsep [ppr name, dcolon, pprParendKind kind]

pprHsForAll exp tvs cxt 
  | show_forall = forall_part <+> pprHsContext cxt
  | otherwise   = pprHsContext cxt
  where
    show_forall =  opt_PprStyle_Debug
		|| (not (null tvs) && is_explicit)
    is_explicit = case exp of {Explicit -> True; Implicit -> False}
    forall_part = ptext SLIT("forall") <+> interppSP tvs <> dot

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

pprHsType ty       = getPprStyle $ \sty -> ppr_mono_ty pREC_TOP (prepare sty ty)
pprParendHsType ty = ppr_mono_ty pREC_CON ty

-- Before printing a type
-- (a) Remove outermost HsParTy parens
-- (b) Drop top-level for-all type variables in user style
--     since they are implicit in Haskell
prepare sty (HsParTy ty)	  = prepare sty ty
prepare sty ty			  = ty

ppr_mono_ty ctxt_prec (HsForAllTy exp tvs ctxt ty)
  = maybeParen ctxt_prec pREC_FUN $
    sep [pprHsForAll exp tvs ctxt, ppr_mono_ty pREC_TOP ty]

ppr_mono_ty ctxt_prec (HsTyVar name)      = ppr name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   = ppr_fun_ty ctxt_prec ty1 ty2
ppr_mono_ty ctxt_prec (HsTupleTy con tys) = tupleParens con (interpp'SP tys)
ppr_mono_ty ctxt_prec (HsKindSig ty kind) = parens (ppr_mono_ty pREC_TOP ty <+> dcolon <+> pprKind kind)
ppr_mono_ty ctxt_prec (HsListTy ty)	  = brackets (ppr_mono_ty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPArrTy ty)	  = pabrackets (ppr_mono_ty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPredTy pred)     = braces (ppr pred)
ppr_mono_ty ctxt_prec (HsNumTy n)         = integer n  -- generics only

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_ty pREC_FUN fun_ty, ppr_mono_ty pREC_CON arg_ty]

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


