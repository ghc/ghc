%
% (c) The AQUA Project, Glasgow University, 1996
%
%************************************************************************
%*									*
\section[PprCore]{Printing of Core syntax, including for interfaces}
%*									*
%************************************************************************

\begin{code}
module PprCore (
	pprCoreExpr, pprIfaceUnfolding, 
	pprCoreBinding, pprCoreBindings
    ) where

#include "HsVersions.h"

import CoreSyn
import CostCentre	( showCostCentre )
import Id		( idType, getIdInfo, getIdStrictness, isTupleCon,
			  nullIdEnv, DataCon, GenId{-instances-},
			  Id
			) 
import IdInfo		( ppIdInfo, ppStrictnessInfo )
import Literal		( Literal{-instances-} )
import Name		( OccName )
import Outputable	-- quite a few things
import PprEnv
import PprType		( pprParendGenType, pprTyVarBndr, GenType{-instances-}, GenTyVar{-instance-} )
import PrimOp		( PrimOp{-instances-} )
import TyVar		( GenTyVar{-instances-} )
import Unique		( Unique{-instances-} )
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
pprCoreBinding  :: CoreBinding   -> SDoc
pprCoreBindings :: [CoreBinding] -> SDoc

init_ppr_env tvbndr pbdr pocc
  = initPprEnv
	(Just ppr) -- literals
	(Just ppr_con)		-- data cons
	(Just ppr_prim)		-- primops
	(Just (\ cc -> text (showCostCentre True cc)))

	(Just tvbndr)	 	-- tyvar binders
	(Just ppr) 		-- tyvar occs
	(Just pprParendGenType) -- types

	(Just pbdr) (Just pocc) -- value vars
  where

    ppr_con con = ppr con

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
    ppr_prim prim = getPprStyle (\sty -> if ifaceStyle sty then
					    ppr prim
					 else
					    ppr prim <> char '!')

--------------
pprCoreBindings binds = vcat (map pprCoreBinding binds)

pprCoreBinding (NonRec binder expr) = ppr_binding (binder, expr)

pprCoreBinding (Rec binds)
  = vcat [ptext SLIT("Rec {"),
	  vcat (map ppr_binding binds),
	  ptext SLIT("end Rec }")]

ppr_binding (binder, expr)
 = sep [pprCoreBinder LetBind binder, 
        nest 2 (equals <+> pprCoreExpr expr)]
\end{code}

General expression printer

\begin{code}
pprCoreExpr :: CoreExpr	-> SDoc
pprCoreExpr = ppr_expr pprCoreEnv

pprCoreEnv = init_ppr_env ppr pprCoreBinder ppr
\end{code}

Printer for unfoldings in interfaces

\begin{code}
pprIfaceUnfolding :: CoreExpr -> SDoc
pprIfaceUnfolding = ppr_expr pprIfaceEnv

pprIfaceEnv = init_ppr_env pprTyVarBndr pprIfaceBinder  ppr
\end{code}

%************************************************************************
%*									*
\subsection{Instance declarations for Core printing}
%*									*
%************************************************************************

\begin{code}
pprGenEnv :: (Outputable bndr, Outputable occ) => PprEnv flexi bndr occ
pprGenEnv = init_ppr_env ppr (\_ -> ppr) ppr

pprGenArgEnv :: (Outputable occ) => PprEnv flexi bndr occ
pprGenArgEnv = init_ppr_env ppr (error "ppr_bndr") ppr

instance (Outputable bndr, Outputable occ) => Outputable (GenCoreBinding bndr occ flexi) where
    ppr bind = ppr_bind pprGenEnv bind

instance (Outputable bndr, Outputable occ) => Outputable (GenCoreExpr bndr occ flexi) where
    ppr expr = ppr_expr pprGenEnv expr

instance (Outputable occ) => Outputable (GenCoreArg occ flexi) where
    ppr arg = ppr_arg pprGenArgEnv arg

instance (Outputable bndr, Outputable occ) => Outputable (GenCoreCaseAlts bndr occ flexi) where
    ppr alts = ppr_alts pprGenEnv alts

instance (Outputable bndr, Outputable occ) => Outputable (GenCoreCaseDefault bndr occ flexi) where
    ppr deflt  = ppr_default pprGenEnv deflt
\end{code}

%************************************************************************
%*									*
\subsection{Workhorse routines (...????...)}
%*									*
%************************************************************************

\begin{code}
ppr_bind pe (NonRec val_bdr expr) = ppr_binding_pe pe (val_bdr, expr)
ppr_bind pe (Rec binds)  	  = vcat (map pp binds)
				  where
				    pp bind = ppr_binding_pe pe bind <> semi

ppr_binding_pe pe (val_bdr, expr)
  = sep [pValBndr pe LetBind val_bdr, 
	 nest 2 (equals <+> ppr_expr pe expr)]
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
  = pCon pe con <+> (braces $ sep (map (ppr_arg pe) args))

ppr_expr pe (Prim prim args)
  = pPrim pe prim <+> (sep (map (ppr_arg pe) args))

ppr_expr pe expr@(Lam _ _)
  = let
	(tyvars, vars, body) = collectBinders expr
    in
    hang (hsep [pp_vars SLIT("_/\\_") (pTyVarB  pe) tyvars,
		pp_vars SLIT("\\")    (pValBndr pe LambdaBind) vars])
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
	ppr_bndr = pValBndr pe CaseBind
	
	ppr_alt (AlgAlts  [] (BindDefault n _)) = (<>) (ppr_bndr n) ppr_arrow
	ppr_alt (PrimAlts [] (BindDefault n _)) = (<>) (ppr_bndr n) ppr_arrow
	ppr_alt (PrimAlts ((l, _):[]) NoDefault)= (<>) (pLit pe l)	   ppr_arrow
	ppr_alt (AlgAlts  ((con, params, _):[]) NoDefault)
	  = hsep [pCon pe con,
		   hsep (map ppr_bndr params),
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
      hsep [ptext SLIT("let {"), pValBndr pe LetBind val_bdr, equals],
      nest 2 (ppr_expr pe rhs),
      ptext SLIT("} in"),
      ppr_expr pe body ]

ppr_expr pe (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = ($$)
      (hang (ptext SLIT("let {"))
	    2 (hsep [hang (hsep [pValBndr pe LetBind val_bdr, equals])
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
    pp_coerce (CoerceIn  v) = (<>) (ptext SLIT("_coerce_in_ "))  (ppr v)
    pp_coerce (CoerceOut v) = (<>) (ptext SLIT("_coerce_out_ ")) (ppr v)

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
    ppr_bndr = pValBndr pe CaseBind

    ppr_alt (con, params, expr)
      = hang (if isTupleCon con then
		    hsep [parens (hsep (punctuate comma (map ppr_bndr params))),
			  ppr_arrow]
		else
		    hsep [pCon pe con,
			  hsep (map ppr_bndr params),
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
  = hang (hsep [pValBndr pe CaseBind val_bdr, ptext SLIT("->")])
	 4 (ppr_expr pe expr <> semi)
\end{code}

\begin{code}
ppr_arg pe (LitArg   lit) = pLit pe lit
ppr_arg pe (VarArg   v)	  = pOcc pe v
ppr_arg pe (TyArg    ty)  = ptext SLIT("_@_ ") <> pTy pe ty
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
-- Used for printing dump info
pprCoreBinder LetBind binder
  = vcat [sig, pragmas, ppr binder]
  where
    sig     = pprTypedBinder binder
    pragmas = ppIdInfo False{-no specs, thanks-} (getIdInfo binder)

pprCoreBinder LambdaBind binder = pprTypedBinder binder
pprCoreBinder CaseBind   binder = ppr binder


-- Used for printing interface-file unfoldings
pprIfaceBinder CaseBind binder = ppr binder
pprIfaceBinder other    binder = pprTypedBinder binder

pprTypedBinder binder
  = ppr binder <+> ptext SLIT("::") <+> pprParendGenType (idType binder)
	-- The space before the :: is important; it helps the lexer
	-- when reading inferfaces.  Otherwise it would lex "a::b" as one thing.
	--
	-- It's important that the type is parenthesised too, at least when
	-- printing interfaces, because we get \ x::(a->b) y::(c->d) -> ...
\end{code}
