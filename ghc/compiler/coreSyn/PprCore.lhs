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
	pprCoreExpr, pprIfaceUnfolding, 
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
			  nullIdEnv, SYN_IE(DataCon), GenId{-instances-},
			  SYN_IE(Id)
			) 
import IdInfo		( ppIdInfo, StrictnessInfo(..) )
import Literal		( Literal{-instances-} )
import Name		( OccName, parenInCode )
import Outputable	-- quite a few things
import PprEnv
import PprType		( pprParendGenType, pprTyVarBndr, GenType{-instances-}, GenTyVar{-instance-} )
import PprStyle		( PprStyle(..), ifaceStyle )
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
@PprEnv@...) @Doc@ and the binder/occ.  They can either use the
homogenized one, or they can ignore it completely.  In other words,
the things passed in act as ``hooks'', getting the last word on how to
print something.

@pprParendCoreExpr@ puts parens around non-atomic Core expressions.

\begin{code}
pprCoreBinding :: PprStyle -> CoreBinding -> Doc

pprGenCoreBinding
	:: (Eq tyvar,  Outputable tyvar,
	    Eq uvar,  Outputable uvar,
	    Outputable bndr,
	    Outputable occ)
	=> PprStyle
	-> (bndr -> Doc)	-- to print "major" val_bdrs
	-> (bndr -> Doc)	-- to print "minor" val_bdrs
	-> (occ  -> Doc)	-- to print bindees
	-> GenCoreBinding bndr occ tyvar uvar
	-> Doc

pprGenCoreBinding sty pbdr1 pbdr2 pocc bind
  = ppr_bind (init_ppr_env sty (ppr sty) pbdr1 pbdr2 pocc) bind

init_ppr_env sty tvbndr pbdr1 pbdr2 pocc
  = initPprEnv sty
	(Just (ppr sty)) -- literals
	(Just ppr_con)		-- data cons
	(Just ppr_prim)		-- primops
	(Just (\ cc -> text (showCostCentre sty True cc)))
	(Just tvbndr)	 	-- tyvar binders
	(Just (ppr sty)) 	-- tyvar occs
	(Just (ppr sty))	-- usage vars
	(Just pbdr1) (Just pbdr2) (Just pocc) -- value vars
	(Just (pprParendGenType sty)) -- types
	(Just (ppr sty))	-- usages
  where

    ppr_con con = ppr sty con

{-	[We now use Con {a,b,c} for Con expressions. SLPJ March 97.]
	[We can't treat them as ordinary applications because the Con doesn't have
	 dictionaries in it, whereas the constructor Id does.]

	OLD VERSION: 
	-- ppr_con is used when printing Con expressions; we add a "!" 
	-- to distinguish them from ordinary applications.  But not when
	-- printing for interfaces, where they are treated as ordinary applications
    ppr_con con | ifaceStyle sty = ppr sty con
	        | otherwise	 = ppr sty con <> char '!'
-}

	-- We add a "!" to distinguish Primitive applications from ordinary applications.  
	-- But not when printing for interfaces, where they are treated 
	-- as ordinary applications
    ppr_prim prim | ifaceStyle sty = ppr sty prim
		  | otherwise	   = ppr sty prim <> char '!'

--------------
pprCoreBinding sty (NonRec binder expr)
  = hang (hsep [pprBigCoreBinder sty binder, equals])
    	 4 (pprCoreExpr sty (pprBigCoreBinder sty) (pprBabyCoreBinder sty) (ppr sty) expr)

pprCoreBinding sty (Rec binds)
  = vcat [ptext SLIT("Rec {"),
	      vcat (map ppr_bind binds),
	      ptext SLIT("end Rec }")]
  where
    ppr_bind (binder, expr)
      = hang (hsep [pprBigCoreBinder sty binder, equals])
	     4 (pprCoreExpr sty (pprBigCoreBinder sty) (pprBabyCoreBinder sty) (ppr sty) expr)
\end{code}

\begin{code}
pprCoreExpr
	:: PprStyle
	-> (Id -> Doc) -- to print "major" val_bdrs
	-> (Id -> Doc) -- to print "minor" val_bdrs
	-> (Id  -> Doc) -- to print bindees
	-> CoreExpr
	-> Doc
pprCoreExpr = pprGenCoreExpr

pprGenCoreExpr, pprParendCoreExpr
	:: (Eq tyvar, Outputable tyvar,
	    Eq uvar, Outputable uvar,
	    Outputable bndr,
	    Outputable occ)
	=> PprStyle
	-> (bndr -> Doc) -- to print "major" val_bdrs
	-> (bndr -> Doc) -- to print "minor" val_bdrs
	-> (occ  -> Doc) -- to print bindees
	-> GenCoreExpr bndr occ tyvar uvar
	-> Doc

pprGenCoreExpr sty pbdr1 pbdr2 pocc expr
  = ppr_expr (init_ppr_env sty (ppr sty) pbdr1 pbdr2 pocc) expr

pprParendCoreExpr sty pbdr1 pbdr2 pocc expr
  = let
	parenify
	  = case expr of
	      Var _ -> id	-- leave unchanged
	      Lit _ -> id
	      _	    -> parens	-- wraps in parens
    in
    parenify (pprGenCoreExpr sty pbdr1 pbdr2 pocc expr)

-- Printer for unfoldings in interfaces
pprIfaceUnfolding :: CoreExpr -> Doc
pprIfaceUnfolding = ppr_expr env 
  where
    env = init_ppr_env PprInterface (pprTyVarBndr PprInterface)
				    (pprTypedCoreBinder PprInterface)
				    (ppr PprInterface)
				    (ppr PprInterface)

ppr_core_arg sty pocc arg
  = ppr_arg (init_ppr_env sty (ppr sty) pocc pocc pocc) arg

ppr_core_alts sty pbdr1 pbdr2 pocc alts
  = ppr_alts (init_ppr_env sty (ppr sty) pbdr1 pbdr2 pocc) alts

ppr_core_default sty pbdr1 pbdr2 pocc deflt
  = ppr_default (init_ppr_env sty (ppr sty) pbdr1 pbdr2 pocc) deflt
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
    ppr sty bind = pprQuote sty $ \sty -> 
		   pprGenCoreBinding sty (ppr sty) (ppr sty) (ppr sty) bind

instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreExpr bndr occ tyvar uvar) where
    ppr sty expr = pprQuote sty $ \sty -> 
		   pprGenCoreExpr sty (ppr sty) (ppr sty) (ppr sty) expr

instance
  (Outputable occ, Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreArg occ tyvar uvar) where
    ppr sty arg = pprQuote sty $ \sty -> 
		  ppr_core_arg sty (ppr sty) arg

instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreCaseAlts bndr occ tyvar uvar) where
    ppr sty alts = pprQuote sty $ \sty -> 
		   ppr_core_alts sty (ppr sty) (ppr sty) (ppr sty) alts

instance
  (Outputable bndr, Outputable occ, Eq tyvar, Outputable tyvar,
   Eq uvar, Outputable uvar)
 =>
  Outputable (GenCoreCaseDefault bndr occ tyvar uvar) where
    ppr sty deflt  = pprQuote sty $ \sty -> 
		     ppr_core_default sty (ppr sty) (ppr sty) (ppr sty) deflt
\end{code}

%************************************************************************
%*									*
\subsection{Workhorse routines (...????...)}
%*									*
%************************************************************************

\begin{code}
ppr_bind pe (NonRec val_bdr expr)
  = hang (hsep [pMajBndr pe val_bdr, equals])
	 4 (ppr_expr pe expr)

ppr_bind pe (Rec binds)
  = vcat (map ppr_pair binds)
  where
    ppr_pair (val_bdr, expr)
      = hang (hsep [pMajBndr pe val_bdr, equals])
	     4 (ppr_expr pe expr <> semi)
\end{code}

\begin{code}
ppr_parend_expr pe expr
  = let
	parenify
	  = case expr of
	      Var _ -> id	-- leave unchanged
	      Lit _ -> id
	      _	    -> parens	-- wraps in parens
    in
    parenify (ppr_expr pe expr)
\end{code}

\begin{code}
ppr_expr pe (Var name)   = pOcc pe name
ppr_expr pe (Lit lit)    = pLit pe lit

ppr_expr pe (Con con args)
  = hang (pCon pe con)
	 4 (braces $ sep (map (ppr_arg pe) args))

ppr_expr pe (Prim prim args)
  = hang (pPrim pe prim)
	 4 (sep (map (ppr_arg pe) args))

ppr_expr pe expr@(Lam _ _)
  = let
	(uvars, tyvars, vars, body) = collectBinders expr
    in
    hang (hsep [pp_vars SLIT("/u\\") (pUVar    pe) uvars,
		   pp_vars SLIT("_/\\_")  (pTyVarB  pe) tyvars,
		   pp_vars SLIT("\\")   (pMajBndr pe) vars])
	 4 (ppr_expr pe body)
  where
    pp_vars lam pp [] = empty
    pp_vars lam pp vs
      = hsep [ptext lam, hsep (map pp vs), ptext SLIT("->")]

ppr_expr pe expr@(App fun arg)
  = let
	(final_fun, final_args)      = go fun [arg]
	go (App fun arg) args_so_far = go fun (arg:args_so_far)
	go fun		 args_so_far = (fun, args_so_far)
    in
    hang (ppr_parend_expr pe final_fun) 4 (sep (map (ppr_arg pe) final_args))

ppr_expr pe (Case expr alts)
  | only_one_alt alts
    -- johan thinks that single case patterns should be on same line as case,
    -- and no indent; all sane persons agree with him.
  = let

	ppr_alt (AlgAlts  [] (BindDefault n _)) = (<>) (pMinBndr pe n) ppr_arrow
	ppr_alt (PrimAlts [] (BindDefault n _)) = (<>) (pMinBndr pe n) ppr_arrow
	ppr_alt (PrimAlts ((l, _):[]) NoDefault)= (<>) (pLit pe l)	   ppr_arrow
	ppr_alt (AlgAlts  ((con, params, _):[]) NoDefault)
	  = hsep [pCon pe con,
		   hsep (map (pMinBndr pe) params),
		   ppr_arrow]

	ppr_rhs (AlgAlts [] (BindDefault _ expr))   = ppr_expr pe expr
	ppr_rhs (AlgAlts ((_,_,expr):[]) NoDefault) = ppr_expr pe expr
	ppr_rhs (PrimAlts [] (BindDefault _ expr))  = ppr_expr pe expr
	ppr_rhs (PrimAlts ((_,expr):[]) NoDefault)  = ppr_expr pe expr


        ppr_arrow = ptext SLIT(" ->")
    in 
    sep
    [sep [pp_keyword, nest 4 (ppr_expr pe expr), text "of {", ppr_alt alts],
	    (<>) (ppr_rhs alts) (text ";}")]

  | otherwise -- default "case" printing
  = sep
    [sep [pp_keyword, nest 4 (ppr_expr pe expr), ptext SLIT("of {")],
     nest 2 (ppr_alts pe alts),
     text "}"]
  where
    pp_keyword = case alts of
		  AlgAlts _ _  -> ptext SLIT("case")
		  PrimAlts _ _ -> ptext SLIT("case#")

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

ppr_expr pe (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = vcat [
      hsep [ptext SLIT("let {"), pMajBndr pe val_bdr, equals],
      nest 2 (ppr_expr pe rhs),
      ptext SLIT("} in"),
      ppr_expr pe body ]

ppr_expr pe (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = ($$)
      (hang (ptext SLIT("let {"))
	    2 (hsep [hang (hsep [pMajBndr pe val_bdr, equals])
			   4 (ppr_expr pe rhs),
       ptext SLIT("} in")]))
      (ppr_expr pe expr)

-- general case (recursive case, too)
ppr_expr pe (Let bind expr)
  = sep [hang (ptext keyword) 2 (ppr_bind pe bind),
	   hang (ptext SLIT("} in ")) 2 (ppr_expr pe expr)]
  where
    keyword = case bind of
		Rec _      -> SLIT("_letrec_ {")
		NonRec _ _ -> SLIT("let {")

ppr_expr pe (SCC cc expr)
  = sep [hsep [ptext SLIT("_scc_"), pSCC pe cc],
	   ppr_parend_expr pe expr ]

ppr_expr pe (Coerce c ty expr)
  = sep [pp_coerce c, pTy pe ty, ppr_expr pe expr]
  where
    pp_coerce (CoerceIn  v) = (<>) (ptext SLIT("_coerce_in_ "))  (ppr (pStyle pe) v)
    pp_coerce (CoerceOut v) = (<>) (ptext SLIT("_coerce_out_ ")) (ppr (pStyle pe) v)

only_one_alt (AlgAlts []     (BindDefault _ _)) = True
only_one_alt (AlgAlts (_:[])  NoDefault) 	= True
only_one_alt (PrimAlts []    (BindDefault _ _)) = True
only_one_alt (PrimAlts (_:[]) NoDefault) 	= True
only_one_alt _					= False 
\end{code}

\begin{code}
ppr_alts pe (AlgAlts alts deflt)
  = vcat [ vcat (map ppr_alt alts), ppr_default pe deflt ]
  where
    ppr_arrow = ptext SLIT("->")

    ppr_alt (con, params, expr)
      = hang (if isTupleCon con then
		    hsep [parens (hsep (punctuate comma (map (pMinBndr pe) params))),
			  ppr_arrow]
		else
		    hsep [pCon pe con,
			  hsep (map (pMinBndr pe) params),
			   ppr_arrow]
	       )
	     4 (ppr_expr pe expr <> semi)

ppr_alts pe (PrimAlts alts deflt)
  = vcat [ vcat (map ppr_alt alts), ppr_default pe deflt ]
  where
    ppr_alt (lit, expr)
      = hang (hsep [pLit pe lit, ptext SLIT("->")])
	     4 (ppr_expr pe expr <> semi)
\end{code}

\begin{code}
ppr_default pe NoDefault = empty

ppr_default pe (BindDefault val_bdr expr)
  = hang (hsep [pMinBndr pe val_bdr, ptext SLIT("->")])
	 4 (ppr_expr pe expr <> semi)
\end{code}

\begin{code}
ppr_arg pe (LitArg   lit) = pLit pe lit
ppr_arg pe (VarArg   v)	  = pOcc pe v
ppr_arg pe (TyArg    ty)  = ptext SLIT("_@_ ") <> pTy pe ty
ppr_arg pe (UsageArg use) = pUse pe use
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
pprBigCoreBinder sty binder
  = vcat [sig, pragmas, ppr sty binder]
  where
    sig = ifnotPprShowAll sty (
	    hang (hsep [ppr sty binder, ppDcolon])
		 4 (ppr sty (idType binder)))
    pragmas =
	ifnotPprForUser sty
	 (ppIdInfo sty False{-no specs, thanks-} (getIdInfo binder))

pprBabyCoreBinder sty binder
  = hsep [ppr sty binder, pp_strictness]
  where
    pp_strictness
      = case (getIdStrictness binder) of
	  NoStrictnessInfo    -> empty
	  BottomGuaranteed    -> ptext SLIT("{- _!_ -}")
	  StrictnessInfo xx _ ->
		panic "PprCore:pp_strictness:StrictnessInfo:ToDo"
		-- text ("{- " ++ (showList xx "") ++ " -}")

pprTypedCoreBinder sty binder
  = hcat [ppr sty binder, ppDcolon, pprParendGenType sty (idType binder)]

ppDcolon = ptext SLIT(" :: ")
		-- The space before the :: is important; it helps the lexer
		-- when reading inferfaces.  Otherwise it would lex "a::b" as one thing.
\end{code}
