%
% (c) The AQUA Project, Glasgow University, 1996
%
%************************************************************************
%*									*
\section[PprCore]{Printing of Core syntax, including for interfaces}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module PprCore (
	pprCoreExpr,
	pprCoreBinding,
	pprBigCoreBinder,
	pprTypedCoreBinder
	
	-- these are here to make the instances go in 0.26:
#if __GLASGOW_HASKELL__ <= 30
	, GenCoreBinding, GenCoreExpr, GenCoreCaseAlts
	, GenCoreCaseDefault, GenCoreArg
#endif
    ) where

IMP_Ubiq(){-uitous-}

import CoreSyn
import CostCentre	( showCostCentre )
import Id		( idType, getIdInfo, getIdStrictness, isTupleCon,
			  nullIdEnv, SYN_IE(DataCon), GenId{-instances-}
			)
import IdInfo		( ppIdInfo, StrictnessInfo(..) )
import IdLoop		( Unfolding )	-- Needed by IdInfo.hi?
import Literal		( Literal{-instances-} )
import Name		( isSymLexeme )
import Outputable	-- quite a few things
import PprEnv
import PprType		( pprParendGenType, GenType{-instances-}, GenTyVar{-instance-} )
import PprStyle		( PprStyle(..) )
import Pretty
import PrimOp		( PrimOp{-instances-} )
import TyVar		( GenTyVar{-instances-} )
import Unique		( Unique{-instances-} )
import Usage		( GenUsage{-instances-} )
import Util		( panic{-ToDo:rm-} )
\end{code}

%************************************************************************
%*									*
\subsection{Public interfaces for Core printing (excluding instances)}
%*									*
%************************************************************************

@pprCoreBinding@ and @pprCoreExpr@ let you give special printing
function for ``major'' val_bdrs (those next to equal signs :-),
``minor'' ones (lambda-bound, case-bound), and bindees.  They would
usually be called through some intermediary.

The binder/occ printers take the default ``homogenized'' (see
@PprEnv@...) @Pretty@ and the binder/occ.  They can either use the
homogenized one, or they can ignore it completely.  In other words,
the things passed in act as ``hooks'', getting the last word on how to
print something.

@pprParendCoreExpr@ puts parens around non-atomic Core expressions.

\begin{code}
pprCoreBinding :: PprStyle -> CoreBinding -> Pretty

pprGenCoreBinding
	:: (Eq tyvar, Outputable tyvar,
	    Eq uvar,  Outputable uvar,
	    Outputable bndr,
	    Outputable occ)
	=> PprStyle
	-> (bndr -> Pretty)	-- to print "major" val_bdrs
	-> (bndr -> Pretty)	-- to print "minor" val_bdrs
	-> (occ  -> Pretty)	-- to print bindees
	-> GenCoreBinding bndr occ tyvar uvar
	-> Pretty

pprGenCoreBinding sty pbdr1 pbdr2 pocc bind
  = ppr_bind (init_ppr_env sty pbdr1 pbdr2 pocc) bind

init_ppr_env sty pbdr1 pbdr2 pocc
  = initPprEnv sty
	(Just (ppr sty)) -- literals
	(Just (ppr sty)) -- data cons
	(Just (ppr sty)) -- primops
	(Just (\ cc -> ppStr (showCostCentre sty True cc)))
	(Just (ppr sty)) -- tyvars
	(Just (ppr sty)) -- usage vars
	(Just pbdr1) (Just pbdr2) (Just pocc) -- value vars
	(Just (pprParendGenType sty)) -- types
	(Just (ppr sty)) -- usages

--------------
pprCoreBinding sty (NonRec binder expr)
  = ppHang (ppCat [pprBigCoreBinder sty binder, ppEquals])
    	 4 (pprCoreExpr sty (pprBigCoreBinder sty) (pprBabyCoreBinder sty) (ppr sty) expr)

pprCoreBinding sty (Rec binds)
  = ppAboves [ifPprDebug sty (ppStr "{- plain Rec -}"),
	      ppAboves (map ppr_bind binds),
	      ifPprDebug sty (ppStr "{- end plain Rec -}")]
  where
    ppr_bind (binder, expr)
      = ppHang (ppCat [pprBigCoreBinder sty binder, ppEquals])
	     4 (pprCoreExpr sty (pprBigCoreBinder sty) (pprBabyCoreBinder sty) (ppr sty) expr)
\end{code}

\begin{code}
pprCoreExpr
	:: PprStyle
	-> (Id -> Pretty) -- to print "major" val_bdrs
	-> (Id -> Pretty) -- to print "minor" val_bdrs
	-> (Id  -> Pretty) -- to print bindees
	-> CoreExpr
	-> Pretty
pprCoreExpr = pprGenCoreExpr

pprGenCoreExpr, pprParendCoreExpr
	:: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar,
	    Outputable bndr,
	    Outputable occ)
	=> PprStyle
	-> (bndr -> Pretty) -- to print "major" val_bdrs
	-> (bndr -> Pretty) -- to print "minor" val_bdrs
	-> (occ  -> Pretty) -- to print bindees
	-> GenCoreExpr bndr occ tyvar uvar
	-> Pretty

pprGenCoreExpr sty pbdr1 pbdr2 pocc expr
  = ppr_expr (init_ppr_env sty pbdr1 pbdr2 pocc) expr

pprParendCoreExpr sty pbdr1 pbdr2 pocc expr
  = let
	parenify
	  = case expr of
	      Var _ -> id	-- leave unchanged
	      Lit _ -> id
	      _	    -> ppParens	-- wraps in parens
    in
    parenify (pprGenCoreExpr sty pbdr1 pbdr2 pocc expr)

ppr_core_arg sty pocc arg
  = ppr_arg (init_ppr_env sty pocc pocc pocc) arg

ppr_core_alts sty pbdr1 pbdr2 pocc alts
  = ppr_alts (init_ppr_env sty pbdr1 pbdr2 pocc) alts

ppr_core_default sty pbdr1 pbdr2 pocc deflt
  = ppr_default (init_ppr_env sty pbdr1 pbdr2 pocc) deflt
\end{code}

%************************************************************************
%*									*
\subsection{Instance declarations for Core printing}
%*									*
%************************************************************************

\begin{code}
instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreBinding bndr occ tyvar uvar) where
    ppr sty bind = pprGenCoreBinding sty (ppr sty) (ppr sty) (ppr sty) bind

instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreExpr bndr occ tyvar uvar) where
    ppr sty expr = pprGenCoreExpr sty (ppr sty) (ppr sty) (ppr sty) expr

instance
  (Outputable occ, Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreArg occ tyvar uvar) where
    ppr sty arg = ppr_core_arg sty (ppr sty) arg

instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreCaseAlts bndr occ tyvar uvar) where
    ppr sty alts = ppr_core_alts sty (ppr sty) (ppr sty) (ppr sty) alts

instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreCaseDefault bndr occ tyvar uvar) where
    ppr sty deflt  = ppr_core_default sty (ppr sty) (ppr sty) (ppr sty) deflt
\end{code}

%************************************************************************
%*									*
\subsection{Workhorse routines (...????...)}
%*									*
%************************************************************************

\begin{code}
ppr_bind pe (NonRec val_bdr expr)
  = ppHang (ppCat [pMajBndr pe val_bdr, ppEquals])
	 4 (ppr_expr pe expr)

ppr_bind pe (Rec binds)
  = ppAboves [ ppStr "{- Rec -}",
	       ppAboves (map ppr_pair binds),
	       ppStr "{- end Rec -}" ]
  where
    ppr_pair (val_bdr, expr)
      = ppHang (ppCat [pMajBndr pe val_bdr, ppEquals])
	     4 (ppr_expr pe expr)
\end{code}

\begin{code}
ppr_parend_expr pe expr
  = let
	parenify
	  = case expr of
	      Var _ -> id	-- leave unchanged
	      Lit _ -> id
	      _	    -> ppParens	-- wraps in parens
    in
    parenify (ppr_expr pe expr)
\end{code}

\begin{code}
ppr_expr pe (Var name)   = pOcc pe name
ppr_expr pe (Lit lit)    = pLit pe lit
ppr_expr pe (Con con []) = pCon pe con

ppr_expr pe (Con con args)
  = ppHang (ppBesides [pCon pe con, ppChar '!'])
	 4 (ppSep (map (ppr_arg pe) args))

ppr_expr pe (Prim prim args)
  = ppHang (ppBesides [pPrim pe prim, ppChar '!'])
	 4 (ppSep (map (ppr_arg pe) args))

ppr_expr pe expr@(Lam _ _)
  = let
	(uvars, tyvars, vars, body) = collectBinders expr
    in
    ppHang (ppCat [pp_vars SLIT("_/u\\_") (pUVar    pe) uvars,
		   pp_vars SLIT("_/\\_")  (pTyVar   pe) tyvars,
		   pp_vars SLIT("\\")     (pMinBndr pe) vars])
	 4 (ppr_expr pe body)
  where
    pp_vars lam pp [] = ppNil
    pp_vars lam pp vs
      = ppCat [ppPStr lam, ppInterleave ppSP (map pp vs), ppStr "->"]

ppr_expr pe expr@(App _ _)
  = let
	(fun, uargs, targs, vargs) = collectArgs expr
    in
    ppHang (ppr_parend_expr pe fun)
	 4 (ppSep [ ppInterleave ppNil (map (pUse    pe) uargs)
		  , ppInterleave ppNil (map (pTy     pe) targs)
		  , ppInterleave ppNil (map (ppr_arg pe) vargs)
	          ])

ppr_expr pe (Case expr alts)
  | only_one_alt alts
    -- johan thinks that single case patterns should be on same line as case,
    -- and no indent; all sane persons agree with him.
  = let
	ppr_alt (AlgAlts  [] (BindDefault n _)) = ppBeside (pMinBndr pe n) (ppStr " ->")
	ppr_alt (PrimAlts [] (BindDefault n _)) = ppBeside (pMinBndr pe n) (ppStr " ->")
	ppr_alt (PrimAlts ((l, _):[]) NoDefault)= ppBeside (pLit pe l)	   (ppStr " ->")
	ppr_alt (AlgAlts  ((con, params, _):[]) NoDefault)
	  = ppCat [ppr_alt_con con (pCon pe con),
		   ppInterleave ppSP (map (pMinBndr pe) params),
		   ppStr "->"]

	ppr_rhs (AlgAlts [] (BindDefault _ expr))   = ppr_expr pe expr
	ppr_rhs (AlgAlts ((_,_,expr):[]) NoDefault) = ppr_expr pe expr
	ppr_rhs (PrimAlts [] (BindDefault _ expr))  = ppr_expr pe expr
	ppr_rhs (PrimAlts ((_,expr):[]) NoDefault)  = ppr_expr pe expr
    in 
    ppSep
    [ppSep [ppPStr SLIT("case"), ppNest 4 (ppr_parend_expr pe expr), ppStr "of {", ppr_alt alts],
	 ppBeside (ppr_rhs alts) (ppStr "}")]

  | otherwise -- default "case" printing
  = ppSep
    [ppSep [ppPStr SLIT("case"), ppNest 4 (ppr_parend_expr pe expr), ppStr "of {"],
     ppNest 2 (ppr_alts pe alts),
     ppStr "}"]

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

ppr_expr pe (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = ppAboves [
      ppCat [ppStr "let {", pMajBndr pe val_bdr, ppEquals],
      ppNest 2 (ppr_expr pe rhs),
      ppStr "} in",
      ppr_expr pe body ]

ppr_expr pe (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = ppAbove
      (ppHang (ppStr "let {")
	    2 (ppCat [ppHang (ppCat [pMajBndr pe val_bdr, ppEquals])
			   4 (ppr_expr pe rhs),
       ppStr "} in"]))
      (ppr_expr pe expr)

-- general case (recursive case, too)
ppr_expr pe (Let bind expr)
  = ppSep [ppHang (ppStr "let {") 2 (ppr_bind pe bind),
	   ppHang (ppStr "} in ") 2 (ppr_expr pe expr)]

ppr_expr pe (SCC cc expr)
  = ppSep [ppCat [ppPStr SLIT("_scc_"), pSCC pe cc],
	   ppr_parend_expr pe expr ]

ppr_expr pe (Coerce c ty expr)
  = ppSep [ppCat [ppPStr SLIT("_coerce_"), pp_coerce c],
	   pTy pe ty, ppr_parend_expr pe expr ]
  where
    pp_coerce (CoerceIn  v) = ppBeside (ppStr "{-in-}")  (ppr (pStyle pe) v)
    pp_coerce (CoerceOut v) = ppBeside (ppStr "{-out-}") (ppr (pStyle pe) v)

only_one_alt (AlgAlts []     (BindDefault _ _)) = True
only_one_alt (AlgAlts (_:[])  NoDefault) 	= True
only_one_alt (PrimAlts []    (BindDefault _ _)) = True
only_one_alt (PrimAlts (_:[]) NoDefault) 	= True
only_one_alt _					= False 

ppr_alt_con con pp_con
  = if isSymLexeme con then ppParens pp_con else pp_con
\end{code}

\begin{code}
ppr_alts pe (AlgAlts alts deflt)
  = ppAboves [ ppAboves (map ppr_alt alts), ppr_default pe deflt ]
  where
    ppr_alt (con, params, expr)
      = ppHang (if isTupleCon con then
		    ppCat [ppParens (ppInterleave ppComma (map (pMinBndr pe) params)),
			   ppStr "->"]
		else
		    ppCat [ppr_alt_con con (pCon pe con),
			   ppInterleave ppSP (map (pMinBndr pe) params),
			   ppStr "->"]
	       )
	     4 (ppr_expr pe expr)

ppr_alts pe (PrimAlts alts deflt)
  = ppAboves [ ppAboves (map ppr_alt alts), ppr_default pe deflt ]
  where
    ppr_alt (lit, expr)
      = ppHang (ppCat [pLit pe lit, ppStr "->"])
	     4 (ppr_expr pe expr)
\end{code}

\begin{code}
ppr_default pe NoDefault = ppNil

ppr_default pe (BindDefault val_bdr expr)
  = ppHang (ppCat [pMinBndr pe val_bdr, ppStr "->"])
	 4 (ppr_expr pe expr)
\end{code}

\begin{code}
ppr_arg pe (LitArg   lit) = pLit pe lit
ppr_arg pe (VarArg   v)	  = pOcc pe v
ppr_arg pe (TyArg    ty)  = pTy  pe ty
ppr_arg pe (UsageArg use) = pUse pe use
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
pprBigCoreBinder sty binder
  = ppAboves [sig, pragmas, ppr sty binder]
  where
    sig = ifnotPprShowAll sty (
	    ppHang (ppCat [ppr sty binder, ppStr "::"])
		 4 (ppr sty (idType binder)))

    pragmas =
	ifnotPprForUser sty
	 (ppIdInfo sty binder False{-no specs, thanks-} id nullIdEnv
	  (getIdInfo binder))

pprBabyCoreBinder sty binder
  = ppCat [ppr sty binder, pp_strictness]
  where
    pp_strictness
      = case (getIdStrictness binder) of
	  NoStrictnessInfo    -> ppNil
	  BottomGuaranteed    -> ppStr "{- _!_ -}"
	  StrictnessInfo xx _ ->
		panic "PprCore:pp_strictness:StrictnessInfo:ToDo"
		-- ppStr ("{- " ++ (showList xx "") ++ " -}")

pprTypedCoreBinder sty binder
  = ppBesides [ppLparen, ppCat [ppr sty binder,
	ppStr "::", ppr sty (idType binder)],
	ppRparen]
\end{code}
