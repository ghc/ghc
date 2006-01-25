%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsTypes]{Abstract syntax: user-defined types}

\begin{code}
module HsTypes (
	HsType(..), LHsType, 
	HsTyVarBndr(..), LHsTyVarBndr,
	HsExplicitForAll(..),
	HsContext, LHsContext,
	HsPred(..), LHsPred,

	LBangType, BangType, HsBang(..), 
        getBangType, getBangStrictness, 
	
	mkExplicitHsForAllTy, mkImplicitHsForAllTy, 
	hsTyVarName, hsTyVarNames, replaceTyVarName,
	hsLTyVarName, hsLTyVarNames, hsLTyVarLocName, hsLTyVarLocNames,
	splitHsInstDeclTy, splitHsFunType,
	
	-- Type place holder
	PostTcType, placeHolderType,

	-- Printing
	pprParendHsType, pprHsForAll, pprHsContext, ppr_hs_context, pprHsTyVarBndr
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr ( HsSplice, pprSplice )

import Type		( Type )
import Kind		( {- instance Outputable Kind -}, Kind,
			  pprParendKind, pprKind, isLiftedTypeKind )
import BasicTypes	( IPName, Boxity, tupleParens )
import SrcLoc		( Located(..), unLoc, noSrcSpan )
import StaticFlags	( opt_PprStyle_Debug )
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
\end{code}

%************************************************************************
%*									*
\subsection{Bang annotations}
%*									*
%************************************************************************

\begin{code}
type LBangType name = Located (BangType name)
type BangType name  = HsType name	-- Bangs are in the HsType data type

data HsBang = HsNoBang	-- Only used as a return value for getBangStrictness,
			-- never appears on a HsBangTy
	    | HsStrict	-- ! 
	    | HsUnbox	-- {-# UNPACK #-} ! (GHC extension, meaning "unbox")

instance Outputable HsBang where
    ppr (HsNoBang) = empty
    ppr (HsStrict) = char '!'
    ppr (HsUnbox)  = ptext SLIT("!!")

getBangType :: LHsType a -> LHsType a
getBangType (L _ (HsBangTy _ ty)) = ty
getBangType ty                    = ty

getBangStrictness :: LHsType a -> HsBang
getBangStrictness (L _ (HsBangTy s _)) = s
getBangStrictness _                    = HsNoBang
\end{code}


%************************************************************************
%*									*
\subsection{Data types}
%*									*
%************************************************************************

This is the syntax for types as seen in type signatures.

\begin{code}
type LHsContext name = Located (HsContext name)

type HsContext name = [LHsPred name]

type LHsPred name = Located (HsPred name)

data HsPred name = HsClassP name [LHsType name]
		 | HsIParam (IPName name) (LHsType name)

type LHsType name = Located (HsType name)

data HsType name
  = HsForAllTy	HsExplicitForAll	-- Renamer leaves this flag unchanged, to record the way
					-- the user wrote it originally, so that the printer can
					-- print it as the user wrote it
		[LHsTyVarBndr name]	-- With ImplicitForAll, this is the empty list
					-- until the renamer fills in the variables
		(LHsContext name)
		(LHsType name)

  | HsTyVar		name		-- Type variable or type constructor

  | HsBangTy	HsBang (LHsType name)	-- Bang-style type annotations 

  | HsAppTy		(LHsType name)
			(LHsType name)

  | HsFunTy		(LHsType name)   -- function type
			(LHsType name)

  | HsListTy		(LHsType name)	-- Element type

  | HsPArrTy		(LHsType name)	-- Elem. type of parallel array: [:t:]

  | HsTupleTy		Boxity
			[LHsType name]	-- Element types (length gives arity)

  | HsOpTy		(LHsType name) (Located name) (LHsType name)

  | HsParTy		(LHsType name)   
	-- Parenthesis preserved for the precedence re-arrangement in RnTypes
	-- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!
	-- 
	-- However, NB that toHsType doesn't add HsParTys (in an effort to keep
	-- interface files smaller), so when printing a HsType we may need to
	-- add parens.  

  | HsNumTy             Integer		-- Generics only

  | HsPredTy		(HsPred name)	-- Only used in the type of an instance
					-- declaration, eg.  Eq [a] -> Eq a
					--			       ^^^^
					--                            HsPredTy
					-- Note no need for location info on the
					-- enclosed HsPred; the one on the type will do

  | HsKindSig		(LHsType name)	-- (ty :: kind)
			Kind		-- A type with a kind signature

  | HsSpliceTy		(HsSplice name)

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

mkHsForAllTy :: HsExplicitForAll -> [LHsTyVarBndr name] -> LHsContext name -> LHsType name -> HsType name
-- Smart constructor for HsForAllTy
mkHsForAllTy exp tvs (L _ []) ty = mk_forall_ty exp tvs ty
mkHsForAllTy exp tvs ctxt ty = HsForAllTy exp tvs ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty Explicit [] ty 			      = unLoc ty	-- Explicit for-all with no tyvars
mk_forall_ty exp  tvs  (L _ (HsParTy ty))		      = mk_forall_ty exp tvs ty
mk_forall_ty exp1 tvs1 (L _ (HsForAllTy exp2 tvs2 ctxt ty)) = mkHsForAllTy (exp1 `plus` exp2) (tvs1 ++ tvs2) ctxt ty
mk_forall_ty exp  tvs  ty			      = HsForAllTy exp tvs (L noSrcSpan []) ty

Implicit `plus` Implicit = Implicit
exp1     `plus` exp2     = Explicit

type LHsTyVarBndr name = Located (HsTyVarBndr name)

data HsTyVarBndr name
  = UserTyVar name
  | KindedTyVar name Kind
	--  *** NOTA BENE *** A "monotype" in a pragma can have
	-- for-alls in it, (mostly to do with dictionaries).  These
	-- must be explicitly Kinded.

hsTyVarName :: HsTyVarBndr name -> name
hsTyVarName (UserTyVar n)     = n
hsTyVarName (KindedTyVar n _) = n

hsLTyVarName :: LHsTyVarBndr name -> name
hsLTyVarName = hsTyVarName . unLoc

hsTyVarNames :: [HsTyVarBndr name] -> [name]
hsTyVarNames tvs = map hsTyVarName tvs

hsLTyVarNames :: [LHsTyVarBndr name] -> [name]
hsLTyVarNames = map hsLTyVarName

hsLTyVarLocName :: LHsTyVarBndr name -> Located name
hsLTyVarLocName = fmap hsTyVarName

hsLTyVarLocNames :: [LHsTyVarBndr name] -> [Located name]
hsLTyVarLocNames = map hsLTyVarLocName

replaceTyVarName :: HsTyVarBndr name1 -> name2 -> HsTyVarBndr name2
replaceTyVarName (UserTyVar n)     n' = UserTyVar n'
replaceTyVarName (KindedTyVar n k) n' = KindedTyVar n' k
\end{code}


\begin{code}
splitHsInstDeclTy 
    :: OutputableBndr name
    => HsType name 
    -> ([LHsTyVarBndr name], HsContext name, name, [LHsType name])
	-- Split up an instance decl type, returning the pieces

splitHsInstDeclTy inst_ty
  = case inst_ty of
	HsParTy (L _ ty)	      -> splitHsInstDeclTy ty
	HsForAllTy _ tvs cxt (L _ ty) -> split_tau tvs (unLoc cxt) ty
	other 			      -> split_tau []  []          other
    -- The type vars should have been computed by now, even if they were implicit
  where
    split_tau tvs cxt (HsPredTy (HsClassP cls tys)) = (tvs, cxt, cls, tys)
    split_tau tvs cxt (HsParTy (L _ ty))	    = split_tau tvs cxt ty

-- Splits HsType into the (init, last) parts
-- Breaks up any parens in the result type: 
--	splitHsFunType (a -> (b -> c)) = ([a,b], c)
splitHsFunType :: LHsType name -> ([LHsType name], LHsType name)
splitHsFunType (L l (HsFunTy x y)) = (x:args, res)
  where
  (args, res) = splitHsFunType y
splitHsFunType (L _ (HsParTy ty))  = splitHsFunType ty
splitHsFunType other 	   	   = ([], other)
\end{code}


%************************************************************************
%*									*
\subsection{Pretty printing}
%*									*
%************************************************************************

NB: these types get printed into interface files, so 
    don't change the printing format lightly

\begin{code}
instance (OutputableBndr name) => Outputable (HsType name) where
    ppr ty = pprHsType ty

instance (Outputable name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar name)        = ppr name
    ppr (KindedTyVar name kind) = pprHsTyVarBndr name kind

instance OutputableBndr name => Outputable (HsPred name) where
    ppr (HsClassP clas tys) = ppr clas <+> hsep (map (pprParendHsType.unLoc) tys)
    ppr (HsIParam n ty)    = hsep [ppr n, dcolon, ppr ty]

pprHsTyVarBndr :: Outputable name => name -> Kind -> SDoc
pprHsTyVarBndr name kind | isLiftedTypeKind kind = ppr name
			 | otherwise 	  	 = hsep [ppr name, dcolon, pprParendKind kind]

pprHsForAll exp tvs cxt 
  | show_forall = forall_part <+> pprHsContext (unLoc cxt)
  | otherwise   = pprHsContext (unLoc cxt)
  where
    show_forall =  opt_PprStyle_Debug
		|| (not (null tvs) && is_explicit)
    is_explicit = case exp of {Explicit -> True; Implicit -> False}
    forall_part = ptext SLIT("forall") <+> interppSP tvs <> dot

pprHsContext :: (OutputableBndr name) => HsContext name -> SDoc
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

pprHsType, pprParendHsType :: (OutputableBndr name) => HsType name -> SDoc

pprHsType ty       = getPprStyle $ \sty -> ppr_mono_ty pREC_TOP (prepare sty ty)
pprParendHsType ty = ppr_mono_ty pREC_CON ty

-- Before printing a type
-- (a) Remove outermost HsParTy parens
-- (b) Drop top-level for-all type variables in user style
--     since they are implicit in Haskell
prepare sty (HsParTy ty)	  = prepare sty (unLoc ty)
prepare sty ty			  = ty

ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty ctxt_prec (HsForAllTy exp tvs ctxt ty)
  = maybeParen ctxt_prec pREC_FUN $
    sep [pprHsForAll exp tvs ctxt, ppr_mono_lty pREC_TOP ty]

-- gaw 2004
ppr_mono_ty ctxt_prec (HsBangTy b ty)     = ppr b <> ppr ty
ppr_mono_ty ctxt_prec (HsTyVar name)      = ppr name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   = ppr_fun_ty ctxt_prec ty1 ty2
ppr_mono_ty ctxt_prec (HsTupleTy con tys) = tupleParens con (interpp'SP tys)
ppr_mono_ty ctxt_prec (HsKindSig ty kind) = parens (ppr_mono_lty pREC_TOP ty <+> dcolon <+> pprKind kind)
ppr_mono_ty ctxt_prec (HsListTy ty)	  = brackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPArrTy ty)	  = pabrackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPredTy pred)     = braces (ppr pred)
ppr_mono_ty ctxt_prec (HsNumTy n)         = integer n  -- generics only
ppr_mono_ty ctxt_prec (HsSpliceTy s)      = pprSplice s

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty, ppr_mono_lty pREC_CON arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2)  
  = maybeParen ctxt_prec pREC_OP $
    ppr_mono_lty pREC_OP ty1 <+> ppr op <+> ppr_mono_lty pREC_OP ty2

ppr_mono_ty ctxt_prec (HsParTy ty)
  = parens (ppr_mono_lty pREC_TOP ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --	toHsType doesn't put in any HsParTys, so we may still need them

--------------------------
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty pREC_FUN ty1
	p2 = ppr_mono_lty pREC_TOP ty2
    in
    maybeParen ctxt_prec pREC_FUN $
    sep [p1, ptext SLIT("->") <+> p2]

--------------------------
pabrackets p = ptext SLIT("[:") <> p <> ptext SLIT(":]")
\end{code}


