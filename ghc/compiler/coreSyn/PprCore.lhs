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
	pprCoreExpr, pprParendExpr,
	pprCoreBinding, pprCoreBindings, pprIdBndr,
	pprCoreBinding, pprCoreBindings,
	pprCoreRules, pprCoreRule
    ) where

#include "HsVersions.h"

import CoreSyn
import CostCentre	( pprCostCentreCore )
import Id		( Id, idType, isDataConId_maybe, idLBVarInfo, idArity,
			  idInfo, idInlinePragma, idDemandInfo, idOccInfo
			)
import Var		( isTyVar )
import IdInfo		( IdInfo, megaSeqIdInfo, occInfo,
			  arityInfo, ppArityInfo, ppFlavourInfo, flavourInfo,
			  demandInfo, updateInfo, ppUpdateInfo, specInfo, 
			  strictnessInfo, ppStrictnessInfo, cafInfo, ppCafInfo,
			  cprInfo, ppCprInfo, lbvarInfo,
			  workerInfo, ppWorkerInfo
			)
import DataCon		( dataConTyCon )
import TyCon		( tupleTyConBoxity, isTupleTyCon )
import PprType		( pprParendType, pprTyVarBndr )
import BasicTypes	( tupleParens )
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
pprCoreExpr     = ppr_noparend_expr pprCoreEnv
pprParendExpr   = ppr_parend_expr   pprCoreEnv
pprArg 	        = ppr_arg pprCoreEnv

pprCoreEnv = initCoreEnv pprCoreBinder
\end{code}

Printer for unfoldings in interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
instance Outputable b => Outputable (Bind b) where
    ppr bind = ppr_bind pprGenericEnv bind

instance Outputable b => Outputable (Expr b) where
    ppr expr = ppr_noparend_expr pprGenericEnv expr

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
 = ppr_binding_pe pe (binder,expr) $$ text ""

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
	 nest 2 (equals <+> ppr_noparend_expr pe expr)]
\end{code}

\begin{code}
ppr_parend_expr   pe expr = ppr_expr parens pe expr
ppr_noparend_expr pe expr = ppr_expr noParens pe expr

noParens :: SDoc -> SDoc
noParens pp = pp
\end{code}

\begin{code}
ppr_expr :: (SDoc -> SDoc) -> PprEnv b -> Expr b -> SDoc
	-- The function adds parens in context that need
	-- an atomic value (e.g. function args)

ppr_expr add_par pe (Type ty)  = add_par (ptext SLIT("TYPE") <+> ppr ty)	-- Wierd
	           
ppr_expr add_par pe (Var name) = pOcc pe name
ppr_expr add_par pe (Lit lit)  = ppr lit

ppr_expr add_par pe expr@(Lam _ _)
  = let
	(bndrs, body) = collectBinders expr
    in
    add_par $
    hang (ptext SLIT("\\") <+> sep (map (pBndr pe LambdaBind) bndrs) <+> arrow)
	 4 (ppr_noparend_expr pe body)

ppr_expr add_par pe expr@(App fun arg)
  = case collectArgs expr of { (fun, args) -> 
    let
	pp_args     = sep (map (ppr_arg pe) args)
	val_args    = dropWhile isTypeArg args	 -- Drop the type arguments for tuples
	pp_tup_args = sep (punctuate comma (map (ppr_arg pe) val_args))
    in
    case fun of
	Var f -> case isDataConId_maybe f of
			-- Notice that we print the *worker*
			-- for tuples in paren'd format.
		   Just dc | saturated && isTupleTyCon tc
			   -> tupleParens (tupleTyConBoxity tc) pp_tup_args
			   where
			     tc	       = dataConTyCon dc
			     saturated = length val_args == idArity f

		   other -> add_par (hang (pOcc pe f) 4 pp_args)

	other -> add_par (hang (ppr_parend_expr pe fun) 4 pp_args)
    }

ppr_expr add_par pe (Case expr var [(con,args,rhs)])
  = add_par $
    sep [sep [ptext SLIT("case") <+> ppr_noparend_expr pe expr,
	      hsep [ptext SLIT("of"),
		    ppr_bndr var,
		    char '{',
		    ppr_case_pat pe con args
	  ]],
	 ppr_noparend_expr pe rhs,
	 char '}'
    ]
  where
    ppr_bndr = pBndr pe CaseBind

ppr_expr add_par pe (Case expr var alts)
  = add_par $
    sep [sep [ptext SLIT("case") <+> ppr_noparend_expr pe expr,
	      ptext SLIT("of") <+> ppr_bndr var <+> char '{'],
	 nest 4 (sep (punctuate semi (map ppr_alt alts))),
	 char '}'
    ]
  where
    ppr_bndr = pBndr pe CaseBind
 
    ppr_alt (con, args, rhs) = hang (ppr_case_pat pe con args)
			            4 (ppr_noparend_expr pe rhs)

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

ppr_expr add_par pe (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = add_par $
    vcat [
      hsep [ptext SLIT("let {"), pBndr pe LetBind val_bdr, equals],
      nest 2 (ppr_noparend_expr pe rhs),
      ptext SLIT("} in"),
      ppr_noparend_expr pe body ]

ppr_expr add_par pe (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = add_par
    (hang (ptext SLIT("let {"))
	  2 (hsep [hang (hsep [pBndr pe LetBind val_bdr, equals])
			   4 (ppr_noparend_expr pe rhs),
       ptext SLIT("} in")])
     $$
     ppr_noparend_expr pe expr)

-- general case (recursive case, too)
ppr_expr add_par pe (Let bind expr)
  = add_par $
    sep [hang (ptext keyword) 2 (ppr_bind pe bind),
	 hang (ptext SLIT("} in ")) 2 (ppr_noparend_expr pe expr)]
  where
    keyword = case bind of
		Rec _      -> SLIT("__letrec {")
		NonRec _ _ -> SLIT("let {")

ppr_expr add_par pe (Note (SCC cc) expr)
  = add_par (sep [pSCC pe cc, ppr_noparend_expr pe expr])

#ifdef DEBUG
ppr_expr add_par pe (Note (Coerce to_ty from_ty) expr)
 = add_par $
   getPprStyle $ \ sty ->
   if debugStyle sty && not (ifaceStyle sty) then
      sep [ptext SLIT("__coerce") <+> sep [pTy pe to_ty, pTy pe from_ty],
	   ppr_parend_expr pe expr]
   else
      sep [hsep [ptext SLIT("__coerce"), pTy pe to_ty],
	          ppr_parend_expr pe expr]
#else
ppr_expr add_par pe (Note (Coerce to_ty from_ty) expr)
  = add_par $
    sep [sep [ptext SLIT("__coerce"), nest 4 (pTy pe to_ty)],
	 ppr_parend_expr pe expr]
#endif

ppr_expr add_par pe (Note InlineCall expr)
  = add_par (ptext SLIT("__inline_call") <+> ppr_parend_expr pe expr)

ppr_expr add_par pe (Note InlineMe expr)
  = add_par $ ptext SLIT("__inline_me") <+> ppr_parend_expr pe expr

ppr_expr add_par pe (Note (TermUsg u) expr)
  = getPprStyle $ \ sty ->
    if ifaceStyle sty then
      ppr_expr add_par pe expr
    else
      add_par (ppr u <+> ppr_noparend_expr pe expr)

ppr_case_pat pe con@(DataAlt dc) args
  | isTupleTyCon tc
  = tupleParens (tupleTyConBoxity tc) (hsep (punctuate comma (map ppr_bndr args))) <+> arrow
  where
    ppr_bndr = pBndr pe CaseBind
    tc = dataConTyCon dc

ppr_case_pat pe con args
  = ppr con <+> hsep (map ppr_bndr args) <+> arrow
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
    pragmas = ppIdInfo binder (idInfo binder)

-- Lambda bound type variables are preceded by "@"
pprCoreBinder LambdaBind bndr = pprTypedBinder bndr

-- Case bound things don't get a signature or a herald
pprCoreBinder CaseBind bndr = pprUntypedBinder bndr

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
pprIdBndr id = ppr id <+> 
	       (megaSeqIdInfo (idInfo id) `seq`
			-- Useful for poking on black holes
	        ifPprDebug (ppr (idInlinePragma id) <+> ppr (idOccInfo id) <+> 
			    ppr (idDemandInfo id)) <+> ppr (idLBVarInfo id))
\end{code}


\begin{code}
ppIdInfo :: Id -> IdInfo -> SDoc
ppIdInfo b info
  = hsep [
	    ppFlavourInfo (flavourInfo info),
	    ppArityInfo a,
	    ppUpdateInfo u,
	    ppWorkerInfo (workerInfo info),
	    ppStrictnessInfo s,
	    ppCafInfo c,
            ppCprInfo m,
	    pprCoreRules b p
	-- Inline pragma, occ, demand, lbvar info
	-- printed out with all binders (when debug is on); 
	-- see PprCore.pprIdBndr
	]
  where
    a = arityInfo info
    s = strictnessInfo info
    u = updateInfo info
    c = cafInfo info
    m = cprInfo info
    p = specInfo info
\end{code}


\begin{code}
pprCoreRules :: Id -> CoreRules -> SDoc
pprCoreRules var (Rules rules _) = vcat (map (pprCoreRule (ppr var)) rules)

pprCoreRule :: SDoc -> CoreRule -> SDoc
pprCoreRule pp_fn (BuiltinRule _)
  = ifPprDebug (ptext SLIT("A built in rule"))

pprCoreRule pp_fn (Rule name tpl_vars tpl_args rhs)
  = doubleQuotes (ptext name) <+> 
    sep [
	  ptext SLIT("__forall") <+> braces (sep (map pprTypedBinder tpl_vars)),
	  nest 4 (pp_fn <+> sep (map pprArg tpl_args)),
	  nest 4 (ptext SLIT("=") <+> pprCoreExpr rhs)
    ] <+> semi
\end{code}
