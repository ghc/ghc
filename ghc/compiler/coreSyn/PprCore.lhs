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
	pprTypedCoreBinder,
	pprPlainCoreBinding
	
	-- these are here to make the instances go in 0.26:
#if __GLASGOW_HASKELL__ <= 26
	, GenCoreBinding, GenCoreExpr, GenCoreCaseAlts
	, GenCoreCaseDefault, GenCoreArg
#endif
    ) where

import Ubiq{-uitous-}

import CoreSyn
import CostCentre	( showCostCentre )
import Id		( idType, getIdInfo, getIdStrictness,
			  nullIdEnv, DataCon(..), GenId{-instances-}
			)
import IdInfo		( ppIdInfo, StrictnessInfo(..) )
import Literal		( Literal{-instances-} )
import Outputable	-- quite a few things
import PprType		( pprType_Internal,
			  GenType{-instances-}, GenTyVar{-instance-}
			)
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
@PrintEnv@...) @Pretty@ and the binder/occ.  They can either use the
homogenized one, or they can ignore it completely.  In other words,
the things passed in act as ``hooks'', getting the last word on how to
print something.

@pprParendCoreExpr@ puts parens around non-atomic Core expressions.

\begin{code}
pprPlainCoreBinding :: PprStyle -> CoreBinding -> Pretty

pprCoreBinding
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

pprCoreBinding sty pbdr1 pbdr2 pocc bind
  = ppr_bind (initial_pe sty (Left (pbdr1, pbdr2, pocc))) bind

pprPlainCoreBinding sty (NonRec binder expr)
  = ppHang (ppCat [pprBigCoreBinder sty binder, ppEquals])
    	 4 (pprCoreExpr sty (pprBigCoreBinder sty) (pprBabyCoreBinder sty) (ppr sty) expr)

pprPlainCoreBinding sty (Rec binds)
  = ppAboves [ifPprDebug sty (ppStr "{- plain Rec -}"),
	      ppAboves (map ppr_bind binds),
	      ifPprDebug sty (ppStr "{- end plain Rec -}")]
  where
    ppr_bind (binder, expr)
      = ppHang (ppCat [pprBigCoreBinder sty binder, ppEquals])
	     4 (pprCoreExpr sty (pprBigCoreBinder sty) (pprBabyCoreBinder sty) (ppr sty) expr)
\end{code}

\begin{code}
pprCoreExpr, pprParendCoreExpr
	:: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar,
	    Outputable bndr,
	    Outputable occ)
	=> PprStyle
	-> (bndr -> Pretty) -- to print "major" val_bdrs
	-> (bndr -> Pretty) -- to print "minor" val_bdrs
	-> (occ  -> Pretty) -- to print bindees
	-> GenCoreExpr bndr occ tyvar uvar
	-> Pretty

pprCoreExpr sty pbdr1 pbdr2 pocc expr
  = ppr_expr (initial_pe sty (Left (pbdr1, pbdr2, pocc))) expr

pprParendCoreExpr sty pbdr1 pbdr2 pocc expr
  = let
	parenify
	  = case expr of
	      Var _ -> id	-- leave unchanged
	      Lit _ -> id
	      _	    -> ppParens	-- wraps in parens
    in
    parenify (pprCoreExpr sty pbdr1 pbdr2 pocc expr)

ppr_core_arg sty pocc arg
  = ppr_arg (initial_pe sty (Left (pocc, pocc, pocc))) arg

ppr_core_alts sty pbdr1 pbdr2 pocc alts
  = ppr_alts (initial_pe sty (Left (pbdr1, pbdr2, pocc))) alts

ppr_core_default sty pbdr1 pbdr2 pocc deflt
  = ppr_default (initial_pe sty (Left (pbdr1, pbdr2, pocc))) deflt
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
    ppr sty bind = pprCoreBinding sty (ppr sty) (ppr sty) (ppr sty) bind

instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreExpr bndr occ tyvar uvar) where
    ppr sty expr = pprCoreExpr sty (ppr sty) (ppr sty) (ppr sty) expr

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
\subsection{Core printing environment (purely local)}
%*									*
%************************************************************************

Similar to @VE@ in @PprType@.  The ``values'' we print here
are locally-defined nested-scope names; callers to @pprCoreBinding@,
etc., can override these.

For tyvars and uvars, we {\em do} normally use these homogenized
names; for values, we {\em don't}.  In printing interfaces, though,
we use homogenized value names, so that interfaces don't wobble
uncontrollably from changing Unique-based names.

\begin{code}
data PrintEnv tyvar uvar bndr occ
  = PE	(Literal -> Pretty)	-- Doing these this way saves
	(DataCon -> Pretty)	-- carrying around a PprStyle
	(PrimOp  -> Pretty)
	(CostCentre -> Pretty)

	[Pretty]		-- Tyvar pretty names
	(tyvar -> Pretty)	-- Tyvar lookup function
        [Pretty]		-- Uvar  pretty names
	(uvar -> Pretty)	-- Uvar  lookup function

	(GenType tyvar uvar -> Pretty)
	(GenUsage uvar -> Pretty)

	(ValPrinters bndr occ)

data ValPrinters bndr occ
  = BOPE -- print binders/occs differently
	 (bndr -> Pretty)	-- to print "major" val_bdrs
	 (bndr -> Pretty)	-- to print "minor" val_bdrs
	 (occ  -> Pretty)	-- to print bindees

  | VPE  -- print all values the same way
	 [Pretty]		-- Value pretty names
	 (bndr -> Pretty)	-- Binder lookup function
	 (occ  -> Pretty)	-- Occurrence lookup function
\end{code}

\begin{code}
initial_pe :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar,
	       Outputable bndr, Outputable occ)
	   => PprStyle
	   -> Either
		(bndr -> Pretty, bndr -> Pretty, occ -> Pretty)
		()
	   -> PrintEnv tyvar uvar bndr occ

initial_pe sty val_printing
  = PE	(ppr sty)   -- for a Literal
	(ppr sty)   -- for a DataCon
	(ppr sty)   -- for a PrimOp
	(\ cc -> ppStr (showCostCentre sty True cc)) -- CostCentre

	tv_pretties ppr_tv -- for a TyVar
        uv_pretties ppr_uv -- for a UsageVar

	(\ ty -> pprType_Internal sty tv_pretties ppr_tv uv_pretties ppr_uv ty)
	(ppr sty) -- for a Usage

	val_printing_stuff
  where
    ppr_tv = ppr sty -- to print a tyvar
    ppr_uv = ppr sty -- to print a uvar

    tv_pretties = map (\ c -> ppChar c ) ['a' .. 'h']
		  ++
		  map (\ n -> ppBeside (ppChar 'a') (ppInt n))
		      ([0 .. ] :: [Int])	-- a0 ... aN
    
    uv_pretties = map (\ c -> ppChar c ) ['u' .. 'y']
		  ++
		  map (\ n -> ppBeside (ppChar 'u') (ppInt n))
		      ([0 .. ] :: [Int])	-- u0 ... uN
    
    val_pretties = map (\ c -> ppChar c ) ['i' .. 'k']
		++ map (\ n -> ppBeside (ppChar 'v') (ppInt n))
		       ([0 .. ] :: [Int])	-- v0 ... vN

    ------------------------
    val_printing_stuff
      = case val_printing of
	  Left  (pbdr1, pbdr2, pocc) -> BOPE pbdr1 pbdr2 pocc
	  Right () -> VPE val_pretties (ppr sty) (ppr sty)

\end{code}

\begin{code}
plit	 (PE pp  _  _  _ _  _ _  _  _  _ _) = pp
pcon	 (PE  _ pp  _  _ _  _ _  _  _  _ _) = pp
pprim	 (PE  _  _ pp  _ _  _ _  _  _  _ _) = pp
pscc	 (PE  _  _  _ pp _  _ _  _  _  _ _) = pp
ptyvar	 (PE  _  _  _  _ _ pp _  _  _  _ _) = pp
puvar	 (PE  _  _  _  _ _  _ _ pp  _  _ _) = pp
  
pty	 (PE  _  _  _  _ _  _ _  _ pp  _ _) = pp
puse	 (PE  _  _  _  _ _  _ _  _  _ pp _) = pp

pmaj_bdr (PE  _  _  _  _ _  _ _  _  _  _ (BOPE pp _ _)) = pp
pmaj_bdr (PE  _  _  _  _ _  _ _  _  _  _ (VPE  _ pp _)) = pp
				   
pmin_bdr (PE  _  _  _  _ _  _ _  _  _  _ (BOPE _ pp _)) = pp
pmin_bdr (PE  _  _  _  _ _  _ _  _  _  _ (VPE  _ pp _)) = pp
				   
pocc	 (PE  _  _  _  _ _  _ _  _  _  _ (BOPE _ _ pp)) = pp
pocc	 (PE  _  _  _  _ _  _ _  _  _  _ (VPE  _ _ pp)) = pp
\end{code}

%************************************************************************
%*									*
\subsection{Workhorse routines (...????...)}
%*									*
%************************************************************************

\begin{code}
ppr_bind pe (NonRec val_bdr expr)
  = ppHang (ppCat [pmaj_bdr pe val_bdr, ppEquals])
	 4 (ppr_expr pe expr)

ppr_bind pe (Rec binds)
  = ppAboves [ ppStr "{- Rec -}",
	       ppAboves (map ppr_pair binds),
	       ppStr "{- end Rec -}" ]
  where
    ppr_pair (val_bdr, expr)
      = ppHang (ppCat [pmaj_bdr pe val_bdr, ppEquals])
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
ppr_expr pe (Var name)   = pocc pe name
ppr_expr pe (Lit lit)    = plit pe lit
ppr_expr pe (Con con []) = pcon pe con

ppr_expr pe (Con con args)
  = ppHang (ppBesides [pcon pe con, ppChar '!'])
	 4 (ppSep (map (ppr_arg pe) args))

ppr_expr pe (Prim prim args)
  = ppHang (ppBesides [pprim pe prim, ppChar '!'])
	 4 (ppSep (map (ppr_arg pe) args))

ppr_expr pe expr@(Lam _ _)
  = let
	(uvars, tyvars, vars, body) = digForLambdas expr
    in
    ppHang (ppCat [pp_vars SLIT("_/u\\_") (puvar    pe) uvars,
		   pp_vars SLIT("_/\\_")  (ptyvar   pe) tyvars,
		   pp_vars SLIT("\\")     (pmin_bdr pe) vars])
	 4 (ppr_expr pe body)
  where
    pp_vars lam pp [] = ppNil
    pp_vars lam pp vs
      = ppCat [ppPStr lam, ppInterleave ppSP (map pp vs), ppStr "->"]

ppr_expr pe expr@(App _ _)
  = let
	(fun, args) = collectArgs expr
    in
    ppHang (ppr_parend_expr pe fun)
	 4 (ppSep (map (ppr_arg pe) args))

ppr_expr pe (Case expr alts)
  = ppSep
    [ppSep [ppPStr SLIT("case"), ppNest 4 (ppr_parend_expr pe expr), ppStr "of {"],
     ppNest 2 (ppr_alts pe alts),
     ppStr "}"]

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

ppr_expr pe (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = ppAboves [
      ppCat [ppStr "let {", pmaj_bdr pe val_bdr, ppEquals],
      ppNest 2 (ppr_expr pe rhs),
      ppStr "} in",
      ppr_expr pe body ]

ppr_expr pe (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = ppAbove
      (ppHang (ppStr "let {")
	    2 (ppCat [ppHang (ppCat [pmaj_bdr pe val_bdr, ppEquals])
			   4 (ppr_expr pe rhs),
       ppStr "} in"]))
      (ppr_expr pe expr)

-- general case (recursive case, too)
ppr_expr pe (Let bind expr)
  = ppSep [ppHang (ppStr "let {") 2 (ppr_bind pe bind),
	   ppHang (ppStr "} in ") 2 (ppr_expr pe expr)]

ppr_expr pe (SCC cc expr)
  = ppSep [ppCat [ppPStr SLIT("_scc_"), pscc pe cc],
	   ppr_parend_expr pe expr ]
\end{code}

\begin{code}
ppr_alts pe (AlgAlts alts deflt)
  = ppAboves [ ppAboves (map ppr_alt alts), ppr_default pe deflt ]
  where
    ppr_alt (con, params, expr)
      = ppHang (ppCat [ppr_con con (pcon pe con),
		       ppInterleave ppSP (map (pmin_bdr pe) params),
		       ppStr "->"])
	     4 (ppr_expr pe expr)
      where
    	ppr_con con pp_con
    	  = if isOpLexeme con then ppParens pp_con else pp_con

ppr_alts pe (PrimAlts alts deflt)
  = ppAboves [ ppAboves (map ppr_alt alts), ppr_default pe deflt ]
  where
    ppr_alt (lit, expr)
      = ppHang (ppCat [plit pe lit, ppStr "->"])
	     4 (ppr_expr pe expr)
\end{code}

\begin{code}
ppr_default pe NoDefault = ppNil

ppr_default pe (BindDefault val_bdr expr)
  = ppHang (ppCat [pmin_bdr pe val_bdr, ppStr "->"])
	 4 (ppr_expr pe expr)
\end{code}

\begin{code}
ppr_arg pe (LitArg   lit) = plit pe lit
ppr_arg pe (VarArg   v)	  = pocc pe v
ppr_arg pe (TyArg    ty)  = pty  pe ty
ppr_arg pe (UsageArg use) = puse pe use
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
	 (ppIdInfo sty binder True{-specs, please-} id nullIdEnv
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
