%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

HsTypes: Abstract syntax: user-defined types

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module HsTypes (
	HsType(..), LHsType, 
	HsTyVarBndr(..), LHsTyVarBndr,
	HsExplicitFlag(..),
	HsContext, LHsContext,
	HsPred(..), LHsPred,
	HsQuasiQuote(..),

	LBangType, BangType, HsBang(..), 
        getBangType, getBangStrictness, 

	ConDeclField(..), pprConDeclFields,
	
	mkExplicitHsForAllTy, mkImplicitHsForAllTy, hsExplicitTvs,
	hsTyVarName, hsTyVarNames, replaceTyVarName, replaceLTyVarName,
	hsTyVarKind, hsTyVarNameKind,
	hsLTyVarName, hsLTyVarNames, hsLTyVarLocName, hsLTyVarLocNames,
	splitHsInstDeclTy, splitHsFunType,
	splitHsAppTys, mkHsAppTys,
	
	-- Type place holder
	PostTcType, placeHolderType, PostTcKind, placeHolderKind,

	-- Printing
	pprParendHsType, pprHsForAll, pprHsContext, ppr_hs_context,
    ) where

import {-# SOURCE #-} HsExpr ( HsSplice, pprSplice )

import NameSet( FreeVars )
import Type
import HsDoc
import BasicTypes
import SrcLoc
import StaticFlags
import Outputable
import FastString

import Data.Data
\end{code}


%************************************************************************
%*									*
\subsection{Annotating the syntax}
%*									*
%************************************************************************

\begin{code}
type PostTcKind = Kind
type PostTcType = Type		-- Used for slots in the abstract syntax
				-- where we want to keep slot for a type
				-- to be added by the type checker...but
				-- before typechecking it's just bogus

placeHolderType :: PostTcType	-- Used before typechecking
placeHolderType  = panic "Evaluated the place holder for a PostTcType"

placeHolderKind :: PostTcKind	-- Used before typechecking
placeHolderKind  = panic "Evaluated the place holder for a PostTcKind"
\end{code}

%************************************************************************
%*									*
	Quasi quotes; used in types and elsewhere
%*									*
%************************************************************************

\begin{code}
data HsQuasiQuote id = HsQuasiQuote 
		       	   id		-- The quasi-quoter
		       	   SrcSpan	-- The span of the enclosed string
		       	   FastString	-- The enclosed string
  deriving (Data, Typeable)

instance OutputableBndr id => Outputable (HsQuasiQuote id) where
    ppr = ppr_qq

ppr_qq :: OutputableBndr id => HsQuasiQuote id -> SDoc
ppr_qq (HsQuasiQuote quoter _ quote) =
    char '[' <> ppr quoter <> ptext (sLit "|") <>
    ppr quote <> ptext (sLit "|]")
\end{code}


%************************************************************************
%*									*
\subsection{Bang annotations}
%*									*
%************************************************************************

\begin{code}
type LBangType name = Located (BangType name)
type BangType name  = HsType name	-- Bangs are in the HsType data type

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

data HsPred name = HsClassP name [LHsType name]		 -- class constraint
		 | HsEqualP (LHsType name) (LHsType name)-- equality constraint
		 | HsIParam (IPName name) (LHsType name)
		 deriving (Data, Typeable)

type LHsType name = Located (HsType name)

data HsType name
  = HsForAllTy	HsExplicitFlag   	-- Renamer leaves this flag unchanged, to record the way
					-- the user wrote it originally, so that the printer can
					-- print it as the user wrote it
		[LHsTyVarBndr name]	-- With ImplicitForAll, this is the empty list
					-- until the renamer fills in the variables
		(LHsContext name)
		(LHsType name)

  | HsTyVar		name		-- Type variable or type constructor

  | HsAppTy		(LHsType name)
			(LHsType name)

  | HsFunTy		(LHsType name)   -- function type
			(LHsType name)

  | HsListTy		(LHsType name)	-- Element type

  | HsPArrTy		(LHsType name)	-- Elem. type of parallel array: [:t:]

  | HsTupleTy		Boxity
			[LHsType name]	-- Element types (length gives arity)

  | HsOpTy		(LHsType name) (Located name) (LHsType name)

  | HsParTy		(LHsType name)   -- See Note [Parens in HsSyn] in HsExpr
	-- Parenthesis preserved for the precedence re-arrangement in RnTypes
	-- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!

  | HsPredTy		(HsPred name)	-- Only used in the type of an instance
					-- declaration, eg.  Eq [a] -> Eq a
					--			       ^^^^
					--                            HsPredTy
					-- Note no need for location info on the
					-- Enclosed HsPred; the one on the type will do

  | HsKindSig		(LHsType name)	-- (ty :: kind)
			Kind		-- A type with a kind signature

  | HsQuasiQuoteTy	(HsQuasiQuote name)

  | HsSpliceTy		(HsSplice name) 
                        FreeVars  	-- Variables free in the splice (filled in by renamer)
			PostTcKind

  | HsDocTy             (LHsType name) LHsDocString -- A documented type

  | HsBangTy	HsBang (LHsType name)	-- Bang-style type annotations 
  | HsRecTy [ConDeclField name]	        -- Only in data type declarations

  | HsCoreTy Type	-- An escape hatch for tunnelling a *closed* 
    	       		-- Core Type through HsSyn.  
					 
  deriving (Data, Typeable)

data HsExplicitFlag = Explicit | Implicit deriving (Data, Typeable)

data ConDeclField name	-- Record fields have Haddoc docs on them
  = ConDeclField { cd_fld_name :: Located name,
		   cd_fld_type :: LBangType name, 
		   cd_fld_doc  :: Maybe LHsDocString }
  deriving (Data, Typeable)

-----------------------
-- Combine adjacent for-alls. 
-- The following awkward situation can happen otherwise:
--	f :: forall a. ((Num a) => Int)
-- might generate HsForAll (Just [a]) [] (HsForAll Nothing [Num a] t)
-- Then a isn't discovered as ambiguous, and we abstract the AbsBinds wrt []
-- but the export list abstracts f wrt [a].  Disaster.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkImplicitHsForAllTy ::                        LHsContext name -> LHsType name -> HsType name
mkExplicitHsForAllTy :: [LHsTyVarBndr name] -> LHsContext name -> LHsType name -> HsType name
mkImplicitHsForAllTy     ctxt ty = mkHsForAllTy Implicit [] ctxt ty
mkExplicitHsForAllTy tvs ctxt ty = mkHsForAllTy Explicit tvs ctxt ty

mkHsForAllTy :: HsExplicitFlag -> [LHsTyVarBndr name] -> LHsContext name -> LHsType name -> HsType name
-- Smart constructor for HsForAllTy
mkHsForAllTy exp tvs (L _ []) ty = mk_forall_ty exp tvs ty
mkHsForAllTy exp tvs ctxt ty = HsForAllTy exp tvs ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty :: HsExplicitFlag -> [LHsTyVarBndr name] -> LHsType name -> HsType name
mk_forall_ty exp  tvs  (L _ (HsParTy ty))		    = mk_forall_ty exp tvs ty
mk_forall_ty exp1 tvs1 (L _ (HsForAllTy exp2 tvs2 ctxt ty)) = mkHsForAllTy (exp1 `plus` exp2) (tvs1 ++ tvs2) ctxt ty
mk_forall_ty exp  tvs  ty			            = HsForAllTy exp tvs (L noSrcSpan []) ty
	-- Even if tvs is empty, we still make a HsForAll!
	-- In the Implicit case, this signals the place to do implicit quantification
	-- In the Explicit case, it prevents implicit quantification	
	--	(see the sigtype production in Parser.y.pp)
	-- 	so that (forall. ty) isn't implicitly quantified

plus :: HsExplicitFlag -> HsExplicitFlag -> HsExplicitFlag
Implicit `plus` Implicit = Implicit
_        `plus` _        = Explicit

hsExplicitTvs :: LHsType name -> [name]
-- The explicitly-given forall'd type variables of a HsType
hsExplicitTvs (L _ (HsForAllTy Explicit tvs _ _)) = hsLTyVarNames tvs
hsExplicitTvs _                                   = []

---------------------
type LHsTyVarBndr name = Located (HsTyVarBndr name)

data HsTyVarBndr name
  = UserTyVar		-- No explicit kinding
         name 		-- See Note [Printing KindedTyVars]
         PostTcKind

  | KindedTyVar 
         name 
         Kind 
      --  *** NOTA BENE *** A "monotype" in a pragma can have
      -- for-alls in it, (mostly to do with dictionaries).  These
      -- must be explicitly Kinded.
  deriving (Data, Typeable)

hsTyVarName :: HsTyVarBndr name -> name
hsTyVarName (UserTyVar n _)   = n
hsTyVarName (KindedTyVar n _) = n

hsTyVarKind :: HsTyVarBndr name -> Kind
hsTyVarKind (UserTyVar _ k)   = k
hsTyVarKind (KindedTyVar _ k) = k

hsTyVarNameKind :: HsTyVarBndr name -> (name, Kind)
hsTyVarNameKind (UserTyVar n k)   = (n,k)
hsTyVarNameKind (KindedTyVar n k) = (n,k)

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
replaceTyVarName (UserTyVar _ k)   n' = UserTyVar n' k
replaceTyVarName (KindedTyVar _ k) n' = KindedTyVar n' k

replaceLTyVarName :: LHsTyVarBndr name1 -> name2 -> LHsTyVarBndr name2
replaceLTyVarName (L loc n1) n2 = L loc (replaceTyVarName n1 n2)
\end{code}


\begin{code}
splitHsAppTys :: LHsType n -> [LHsType n] -> (LHsType n, [LHsType n])
splitHsAppTys (L _ (HsAppTy f a)) as = splitHsAppTys f (a:as)
splitHsAppTys f          	  as = (f,as)

mkHsAppTys :: OutputableBndr n => LHsType n -> [LHsType n] -> HsType n
mkHsAppTys fun_ty [] = pprPanic "mkHsAppTys" (ppr fun_ty)
mkHsAppTys fun_ty (arg_ty:arg_tys)
  = foldl mk_app (HsAppTy fun_ty arg_ty) arg_tys
  where
    mk_app fun arg = HsAppTy (noLoc fun) arg	
       -- Add noLocs for inner nodes of the application; 
       -- they are never used 

splitHsInstDeclTy 
    :: OutputableBndr name
    => LHsType name 
    -> ([LHsTyVarBndr name], HsContext name, Located name, [LHsType name])
	-- Split up an instance decl type, returning the pieces

splitHsInstDeclTy linst_ty@(L _ inst_ty)
  = case inst_ty of
	HsParTy ty       	-> splitHsInstDeclTy ty
	HsForAllTy _ tvs cxt ty -> split_tau tvs (unLoc cxt) ty
	_ 			-> split_tau []  []          linst_ty
    -- The type vars should have been computed by now, even if they were implicit
  where
    split_tau tvs cxt (L loc (HsPredTy (HsClassP cls tys))) = (tvs, cxt, L loc cls, tys)
    split_tau tvs cxt (L _ (HsParTy ty))	            = split_tau tvs cxt ty
    split_tau _ _ _ = pprPanic "splitHsInstDeclTy" (ppr inst_ty)

-- Splits HsType into the (init, last) parts
-- Breaks up any parens in the result type: 
--	splitHsFunType (a -> (b -> c)) = ([a,b], c)
splitHsFunType :: LHsType name -> ([LHsType name], LHsType name)
splitHsFunType (L _ (HsFunTy x y)) = (x:args, res)
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

\begin{code}
instance (OutputableBndr name) => Outputable (HsType name) where
    ppr ty = pprHsType ty

instance (Outputable name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar name _)      = ppr name
    ppr (KindedTyVar name kind) = hsep [ppr name, dcolon, pprParendKind kind]

instance OutputableBndr name => Outputable (HsPred name) where
    ppr (HsClassP clas tys) = ppr clas <+> hsep (map pprLHsType tys)
    ppr (HsEqualP t1 t2)    = hsep [pprLHsType t1, ptext (sLit "~"), 
				    pprLHsType t2]
    ppr (HsIParam n ty)     = hsep [ppr n, dcolon, ppr ty]

pprLHsType :: OutputableBndr name => LHsType name -> SDoc
pprLHsType = pprParendHsType . unLoc

pprHsForAll :: OutputableBndr name => HsExplicitFlag -> [LHsTyVarBndr name] ->  LHsContext name -> SDoc
pprHsForAll exp tvs cxt 
  | show_forall = forall_part <+> pprHsContext (unLoc cxt)
  | otherwise   = pprHsContext (unLoc cxt)
  where
    show_forall =  opt_PprStyle_Debug
		|| (not (null tvs) && is_explicit)
    is_explicit = case exp of {Explicit -> True; Implicit -> False}
    forall_part = ptext (sLit "forall") <+> interppSP tvs <> dot

pprHsContext :: (OutputableBndr name) => HsContext name -> SDoc
pprHsContext []	        = empty
pprHsContext [L _ pred] 
   | noParenHsPred pred = ppr pred <+> darrow
pprHsContext cxt        = ppr_hs_context cxt <+> darrow

noParenHsPred :: HsPred name -> Bool
-- c.f. TypeRep.noParenPred
noParenHsPred (HsClassP {}) = True
noParenHsPred (HsEqualP {}) = True
noParenHsPred (HsIParam {}) = False

ppr_hs_context :: (OutputableBndr name) => HsContext name -> SDoc
ppr_hs_context []  = empty
ppr_hs_context cxt = parens (interpp'SP cxt)

pprConDeclFields :: OutputableBndr name => [ConDeclField name] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (ConDeclField { cd_fld_name = n, cd_fld_type = ty, 
			    cd_fld_doc = doc })
  	= ppr n <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
\end{code}

Note [Printing KindedTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Trac #3830 reminded me that we should really only print the kind
signature on a KindedTyVar if the kind signature was put there by the
programmer.  During kind inference GHC now adds a PostTcKind to UserTyVars,
rather than converting to KindedTyVars as before.

(As it happens, the message in #3830 comes out a different way now,
and the problem doesn't show up; but having the flag on a KindedTyVar
seems like the Right Thing anyway.)

\begin{code}
pREC_TOP, pREC_FUN, pREC_OP, pREC_CON :: Int
pREC_TOP = 0  -- type   in ParseIface.y
pREC_FUN = 1  -- btype  in ParseIface.y
              -- Used for LH arg of (->)
pREC_OP  = 2  -- Used for arg of any infix operator
              -- (we don't keep their fixities around)
pREC_CON = 3  -- Used for arg of type applicn:
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
prepare :: PprStyle -> HsType name -> HsType name
prepare sty (HsParTy ty)	  = prepare sty (unLoc ty)
prepare _   ty                    = ty

ppr_mono_lty :: (OutputableBndr name) => Int -> LHsType name -> SDoc
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty :: (OutputableBndr name) => Int -> HsType name -> SDoc
ppr_mono_ty ctxt_prec (HsForAllTy exp tvs ctxt ty)
  = maybeParen ctxt_prec pREC_FUN $
    sep [pprHsForAll exp tvs ctxt, ppr_mono_lty pREC_TOP ty]

ppr_mono_ty _    (HsBangTy b ty)     = ppr b <> ppr ty
ppr_mono_ty _    (HsQuasiQuoteTy qq) = ppr qq
ppr_mono_ty _    (HsRecTy flds)      = pprConDeclFields flds
ppr_mono_ty _    (HsTyVar name)      = ppr name
ppr_mono_ty prec (HsFunTy ty1 ty2)   = ppr_fun_ty prec ty1 ty2
ppr_mono_ty _    (HsTupleTy con tys) = tupleParens con (interpp'SP tys)
ppr_mono_ty _    (HsKindSig ty kind) = parens (ppr_mono_lty pREC_TOP ty <+> dcolon <+> pprKind kind)
ppr_mono_ty _    (HsListTy ty)	     = brackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty _    (HsPArrTy ty)	     = pabrackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty _    (HsPredTy pred)     = ppr pred
ppr_mono_ty _    (HsSpliceTy s _ _)  = pprSplice s
ppr_mono_ty _    (HsCoreTy ty)       = ppr ty

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty, ppr_mono_lty pREC_CON arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2)  
  = maybeParen ctxt_prec pREC_OP $
    ppr_mono_lty pREC_OP ty1 <+> ppr op <+> ppr_mono_lty pREC_OP ty2

ppr_mono_ty _         (HsParTy ty)
  = parens (ppr_mono_lty pREC_TOP ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --	toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty ctxt_prec (HsDocTy ty doc) 
  = maybeParen ctxt_prec pREC_OP $
    ppr_mono_lty pREC_OP ty <+> ppr (unLoc doc)
  -- we pretty print Haddock comments on types as if they were
  -- postfix operators

--------------------------
ppr_fun_ty :: (OutputableBndr name) => Int -> LHsType name -> LHsType name -> SDoc
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty pREC_FUN ty1
	p2 = ppr_mono_lty pREC_TOP ty2
    in
    maybeParen ctxt_prec pREC_FUN $
    sep [p1, ptext (sLit "->") <+> p2]

--------------------------
pabrackets :: SDoc -> SDoc
pabrackets p = ptext (sLit "[:") <> p <> ptext (sLit ":]")
\end{code}


