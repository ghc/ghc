%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
%************************************************************************
%*									*
\section[PprCore]{Printing of Core syntax, including for interfaces}
%*									*
%************************************************************************

\begin{code}
module PprCore (
	pprCoreExpr, pprParendExpr, pprIfaceUnfolding, 
	pprCoreBinding, pprCoreBindings, pprIdBndr,
	pprCoreRules, pprCoreRule
    ) where

#include "HsVersions.h"

import CoreSyn
import CostCentre	( pprCostCentreCore )
import Id		( idType, idInfo, getInlinePragma, getIdDemandInfo, getIdOccInfo, Id )
import Var		( isTyVar )
import IdInfo		( IdInfo,
			  arityInfo, ppArityInfo, ppFlavourInfo, flavourInfo,
			  demandInfo, updateInfo, ppUpdateInfo, specInfo, 
			  strictnessInfo, ppStrictnessInfo, cafInfo, ppCafInfo,
			  cprInfo, ppCprInfo, lbvarInfo,
			  workerInfo, ppWorkerInfo
			)
import Const		( Con(..), DataCon )
import DataCon		( isTupleCon, isUnboxedTupleCon )
import PprType		( pprParendType, pprTyVarBndr )
import PprEnv
import Outputable
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

Un-annotated core dumps
~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
pprCoreBindings :: [CoreBind] -> SDoc
pprCoreBinding  :: CoreBind   -> SDoc
pprCoreExpr     :: CoreExpr   -> SDoc
pprParendExpr   :: CoreExpr   -> SDoc

pprCoreBindings = pprTopBinds pprCoreEnv
pprCoreBinding  = pprTopBind pprCoreEnv
pprCoreExpr     = ppr_expr pprCoreEnv
pprParendExpr   = ppr_parend_expr pprCoreEnv

pprCoreEnv = initCoreEnv pprCoreBinder
\end{code}

Printer for unfoldings in interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
pprIfaceUnfolding :: CoreExpr -> SDoc
pprIfaceUnfolding = ppr_parend_expr pprIfaceEnv
	-- Notice that it's parenthesised

pprIfaceArg = ppr_arg pprIfaceEnv

pprIfaceEnv = initCoreEnv pprIfaceBinder
\end{code}

\begin{code}
instance Outputable b => Outputable (Bind b) where
    ppr bind = ppr_bind pprGenericEnv bind

instance Outputable b => Outputable (Expr b) where
    ppr expr = ppr_expr pprGenericEnv expr

pprGenericEnv :: Outputable b => PprEnv b
pprGenericEnv = initCoreEnv (\site -> ppr)
\end{code}

%************************************************************************
%*									*
\subsection{Instance declarations for Core printing}
%*									*
%************************************************************************


\begin{code}
initCoreEnv pbdr
  = initPprEnv
	(Just ppr)			-- Constants
	(Just pprCostCentreCore)	-- Cost centres

	(Just ppr) 		-- tyvar occs
	(Just pprParendType)    -- types

	(Just pbdr) (Just ppr) -- value vars
	-- Use pprIdBndr for this last one as a debugging device.
\end{code}

%************************************************************************
%*									*
\subsection{The guts}
%*									*
%************************************************************************

\begin{code}
pprTopBinds pe binds = vcat (map (pprTopBind pe) binds)

pprTopBind pe (NonRec binder expr)
 = sep [ppr_binding_pe pe (binder,expr)] $$ text ""

pprTopBind pe (Rec binds)
  = vcat [ptext SLIT("Rec {"),
	  vcat (map (ppr_binding_pe pe) binds),
	  ptext SLIT("end Rec }"),
	  text ""]
\end{code}

\begin{code}
ppr_bind :: PprEnv b -> Bind b -> SDoc

ppr_bind pe (NonRec val_bdr expr) = ppr_binding_pe pe (val_bdr, expr)
ppr_bind pe (Rec binds)  	  = vcat (map pp binds)
				  where
				    pp bind = ppr_binding_pe pe bind <> semi

ppr_binding_pe :: PprEnv b -> (b, Expr b) -> SDoc
ppr_binding_pe pe (val_bdr, expr)
  = sep [pBndr pe LetBind val_bdr, 
	 nest 2 (equals <+> ppr_expr pe expr)]
\end{code}

\begin{code}
ppr_parend_expr pe expr
  | no_parens = ppr_expr pe expr
  | otherwise = parens (ppr_expr pe expr)
  where
    no_parens = case expr of
	 	  Var _     	     -> True
		  Con con []	     -> True
		  Con (DataCon dc) _ -> isTupleCon dc
		  _	             -> False
\end{code}

\begin{code}
ppr_expr :: PprEnv b -> Expr b -> SDoc

ppr_expr pe (Type ty)  = ptext SLIT("TYPE") <+> ppr ty	-- Wierd

ppr_expr pe (Var name) = pOcc pe name

ppr_expr pe (Con con [])
  = ppr con	-- Nullary constructors too

ppr_expr pe (Con (DataCon dc) args)
	-- Drop the type arguments and print in (a,b,c) notation
  | isTupleCon dc
  = parens (sep (punctuate comma (map (ppr_arg pe) (dropWhile isTypeArg args))))
  | isUnboxedTupleCon dc
  = text "(# " <> 
    hsep (punctuate comma (map (ppr_arg pe) (dropWhile isTypeArg args))) <>
    text " #)"

ppr_expr pe (Con con args)
  = pCon pe con <+> (braces $ sep (map (ppr_arg pe) args))

ppr_expr pe expr@(Lam _ _)
  = let
	(bndrs, body) = collectBinders expr
    in
    hang (ptext SLIT("\\") <+> sep (map (pBndr pe LambdaBind) bndrs) <+> arrow)
	 4 (ppr_expr pe body)

ppr_expr pe expr@(App fun arg)
  = let
	(final_fun, final_args)      = go fun [arg]
	go (App fun arg) args_so_far = go fun (arg:args_so_far)
	go fun		 args_so_far = (fun, args_so_far)
    in
    hang (ppr_parend_expr pe final_fun) 4 (sep (map (ppr_arg pe) final_args))

ppr_expr pe (Case expr var [(con,args,rhs)])
  = sep [sep [ptext SLIT("case") <+> ppr_expr pe expr,
	      hsep [ptext SLIT("of"),
		    ppr_bndr var,
		    char '{',
		    ppr_case_pat pe con args
	  ]],
	 ppr_expr pe rhs,
	 char '}'
    ]
  where
    ppr_bndr = pBndr pe CaseBind

ppr_expr pe (Case expr var alts)
  = sep [sep [ptext SLIT("case") <+> ppr_expr pe expr,
	      ptext SLIT("of") <+> ppr_bndr var <+> char '{'],
	 nest 4 (sep (punctuate semi (map ppr_alt alts))),
	 char '}'
    ]
  where
    ppr_bndr = pBndr pe CaseBind
 
    ppr_alt (con, args, rhs) = hang (ppr_case_pat pe con args)
			            4 (ppr_expr pe rhs)

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

ppr_expr pe (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = vcat [
      hsep [ptext SLIT("let {"), pBndr pe LetBind val_bdr, equals],
      nest 2 (ppr_expr pe rhs),
      ptext SLIT("} in"),
      ppr_expr pe body ]

ppr_expr pe (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = hang (ptext SLIT("let {"))
	  2 (hsep [hang (hsep [pBndr pe LetBind val_bdr, equals])
			   4 (ppr_expr pe rhs),
       ptext SLIT("} in")])
    $$
    ppr_expr pe expr

-- general case (recursive case, too)
ppr_expr pe (Let bind expr)
  = sep [hang (ptext keyword) 2 (ppr_bind pe bind),
	 hang (ptext SLIT("} in ")) 2 (ppr_expr pe expr)]
  where
    keyword = case bind of
		Rec _      -> SLIT("__letrec {")
		NonRec _ _ -> SLIT("let {")

ppr_expr pe (Note (SCC cc) expr)
  = sep [pSCC pe cc, ppr_expr pe expr]

#ifdef DEBUG
ppr_expr pe (Note (Coerce to_ty from_ty) expr)
 = getPprStyle $ \ sty ->
   if debugStyle sty && not (ifaceStyle sty) then
      sep [ptext SLIT("__coerce") <+> sep [pTy pe to_ty, pTy pe from_ty],
	   ppr_parend_expr pe expr]
   else
      sep [hsep [ptext SLIT("__coerce"), pTy pe to_ty],
	          ppr_parend_expr pe expr]
#else
ppr_expr pe (Note (Coerce to_ty from_ty) expr)
  = sep [sep [ptext SLIT("__coerce"), nest 4 (pTy pe to_ty)],
	 ppr_parend_expr pe expr]
#endif

ppr_expr pe (Note InlineCall expr)
  = ptext SLIT("__inline_call") <+> ppr_parend_expr pe expr

ppr_expr pe (Note InlineMe expr)
  = ptext SLIT("__inline_me") <+> ppr_parend_expr pe expr

ppr_expr pe (Note (TermUsg u) expr)
  = \ sty ->
    if ifaceStyle sty then
      ppr_expr pe expr sty
    else
      (ppr u <+> ppr_expr pe expr) sty

ppr_case_pat pe con@(DataCon dc) args
  | isTupleCon dc
  = parens (hsep (punctuate comma (map ppr_bndr args))) <+> arrow
  | isUnboxedTupleCon dc
  = hsep [text "(# " <> 
	  hsep (punctuate comma (map ppr_bndr args)) <>
	  text " #)",
	  arrow]
  where
    ppr_bndr = pBndr pe CaseBind

ppr_case_pat pe con args
  = pCon pe con <+> hsep (map ppr_bndr args) <+> arrow
  where
    ppr_bndr = pBndr pe CaseBind

ppr_arg pe (Type ty) = ptext SLIT("@") <+> pTy pe ty
ppr_arg pe expr      = ppr_parend_expr pe expr

arrow = ptext SLIT("->")
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
-- Used for printing dump info
pprCoreBinder LetBind binder
  = vcat [sig, pragmas, ppr binder]
  where
    sig     = pprTypedBinder binder
    pragmas = ppIdInfo (idInfo binder)

-- Lambda bound type variables are preceded by "@"
pprCoreBinder LambdaBind bndr = pprTypedBinder bndr

-- Case bound things don't get a signature or a herald
pprCoreBinder CaseBind bndr = pprUntypedBinder bndr

-- Used for printing interface-file unfoldings
pprIfaceBinder CaseBind binder = pprUntypedBinder binder
pprIfaceBinder other    binder = pprTypedBinder binder

pprUntypedBinder binder
  | isTyVar binder = ptext SLIT("@") <+> pprTyVarBndr binder
  | otherwise      = pprIdBndr binder

pprTypedBinder binder
  | isTyVar binder  = ptext SLIT("@") <+> pprTyVarBndr binder
  | otherwise	    = pprIdBndr binder <+> dcolon <+> pprParendType (idType binder)
	-- The space before the :: is important; it helps the lexer
	-- when reading inferfaces.  Otherwise it would lex "a::b" as one thing.
	--
	-- It's important that the type is parenthesised too, at least when
	-- printing interfaces, because we get \ x::(a->b) y::(c->d) -> ...

-- When printing any Id binder in debug mode, we print its inline pragma and one-shot-ness
pprIdBndr id = ppr id <+> ifPprDebug (ppr (getInlinePragma id) <+> ppr (getIdOccInfo id) <+> 
				      ppr (getIdDemandInfo id)) <+> ppr (lbvarInfo (idInfo id))
\end{code}


\begin{code}
ppIdInfo :: IdInfo -> SDoc
ppIdInfo info
  = hsep [
	    ppFlavourInfo (flavourInfo info),
	    ppArityInfo a,
	    ppUpdateInfo u,
	    ppWorkerInfo (workerInfo info),
	    ppStrictnessInfo s,
	    ppr d,
	    ppCafInfo c,
            ppCprInfo m,
	    ppr (lbvarInfo info),
	    pprIfaceCoreRules p
	-- Inline pragma printed out with all binders; see PprCore.pprIdBndr
	]
  where
    a = arityInfo info
    d = demandInfo info
    s = strictnessInfo info
    u = updateInfo info
    c = cafInfo info
    m = cprInfo info
    p = specInfo info
\end{code}


\begin{code}
pprCoreRules :: Id -> CoreRules -> SDoc
pprCoreRules var (Rules rules _) = vcat (map (pprCoreRule (Just var)) rules)

pprIfaceCoreRules :: CoreRules -> SDoc
pprIfaceCoreRules (Rules rules _) = vcat (map (pprCoreRule Nothing) rules)

pprCoreRule :: Maybe Id -> CoreRule -> SDoc
pprCoreRule maybe_fn (Rule name tpl_vars tpl_args rhs)
  = doubleQuotes (ptext name) <+> 
    sep [
	  ptext SLIT("__forall") <+> braces (sep (map pprTypedBinder tpl_vars)),
	  nest 4 (pp_fn <+> sep (map pprIfaceArg tpl_args)),
	  nest 4 (ptext SLIT("=") <+> pprIfaceUnfolding rhs)
    ]
  where
    pp_fn = case maybe_fn of
	    	Just id -> ppr id
		Nothing -> empty		-- Interface file
\end{code}
