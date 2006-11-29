%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1996-1998
%

Printing of Core syntax

\begin{code}
module PprCore (
	pprCoreExpr, pprParendExpr,
	pprCoreBinding, pprCoreBindings, pprCoreAlt,
	pprRules
    ) where

#include "HsVersions.h"

import CoreSyn
import CostCentre
import Var
import Id
import IdInfo
import NewDemand
#ifdef OLD_STRICTNESS
import Id
import IdInfo
#endif

import DataCon
import TyCon
import Type
import Coercion
import BasicTypes
import Util
import Outputable
import FastString
import Module
\end{code}

%************************************************************************
%*									*
\subsection{Public interfaces for Core printing (excluding instances)}
%*									*
%************************************************************************

@pprParendCoreExpr@ puts parens around non-atomic Core expressions.

\begin{code}
pprCoreBindings :: OutputableBndr b => [Bind b] -> SDoc
pprCoreBinding  :: OutputableBndr b => Bind b  -> SDoc
pprCoreExpr     :: OutputableBndr b => Expr b  -> SDoc
pprParendExpr   :: OutputableBndr b => Expr b  -> SDoc

pprCoreBindings = pprTopBinds
pprCoreBinding  = pprTopBind 

instance OutputableBndr b => Outputable (Bind b) where
    ppr bind = ppr_bind bind

instance OutputableBndr b => Outputable (Expr b) where
    ppr expr = pprCoreExpr expr
\end{code}


%************************************************************************
%*									*
\subsection{The guts}
%*									*
%************************************************************************

\begin{code}
pprTopBinds binds = vcat (map pprTopBind binds)

pprTopBind (NonRec binder expr)
 = ppr_binding (binder,expr) $$ text ""

pprTopBind (Rec binds)
  = vcat [ptext SLIT("Rec {"),
	  vcat (map ppr_binding binds),
	  ptext SLIT("end Rec }"),
	  text ""]
\end{code}

\begin{code}
ppr_bind :: OutputableBndr b => Bind b -> SDoc

ppr_bind (NonRec val_bdr expr) = ppr_binding (val_bdr, expr)
ppr_bind (Rec binds)  	       = vcat (map pp binds)
			       where
				 pp bind = ppr_binding bind <> semi

ppr_binding :: OutputableBndr b => (b, Expr b) -> SDoc
ppr_binding (val_bdr, expr)
  = pprBndr LetBind val_bdr $$ 
    hang (ppr val_bdr <+> equals) 2 (pprCoreExpr expr)
\end{code}

\begin{code}
pprParendExpr   expr = ppr_expr parens expr
pprCoreExpr expr = ppr_expr noParens expr

noParens :: SDoc -> SDoc
noParens pp = pp
\end{code}

\begin{code}
ppr_expr :: OutputableBndr b => (SDoc -> SDoc) -> Expr b -> SDoc
	-- The function adds parens in context that need
	-- an atomic value (e.g. function args)

ppr_expr add_par (Type ty)  = add_par (ptext SLIT("TYPE") <+> ppr ty)	-- Wierd
	           
ppr_expr add_par (Var name) = ppr name
ppr_expr add_par (Lit lit)  = ppr lit

ppr_expr add_par (Cast expr co) 
  = add_par $
    sep [pprParendExpr expr, 
	 ptext SLIT("`cast`") <+> parens (pprCo co)]
  where
    pprCo co = sep [ppr co, dcolon <+> ppr (coercionKindPredTy co)]
	 

ppr_expr add_par expr@(Lam _ _)
  = let
	(bndrs, body) = collectBinders expr
    in
    add_par $
    hang (ptext SLIT("\\") <+> sep (map (pprBndr LambdaBind) bndrs) <+> arrow)
	 2 (pprCoreExpr body)

ppr_expr add_par expr@(App fun arg)
  = case collectArgs expr of { (fun, args) -> 
    let
	pp_args     = sep (map pprArg args)
	val_args    = dropWhile isTypeArg args	 -- Drop the type arguments for tuples
	pp_tup_args = sep (punctuate comma (map pprCoreExpr val_args))
    in
    case fun of
	Var f -> case isDataConWorkId_maybe f of
			-- Notice that we print the *worker*
			-- for tuples in paren'd format.
		   Just dc | saturated && isTupleTyCon tc
			   -> tupleParens (tupleTyConBoxity tc) pp_tup_args
			   where
			     tc	       = dataConTyCon dc
			     saturated = val_args `lengthIs` idArity f

		   other -> add_par (hang (ppr f) 2 pp_args)

	other -> add_par (hang (pprParendExpr fun) 2 pp_args)
    }

ppr_expr add_par (Case expr var ty [(con,args,rhs)])
  = add_par $
    sep [sep [ptext SLIT("case") <+> pprCoreExpr expr,
	      ifPprDebug (braces (ppr ty)),
	      sep [ptext SLIT("of") <+> ppr_bndr var, 
		   char '{' <+> ppr_case_pat con args]
	  ],
	 pprCoreExpr rhs,
	 char '}'
    ]
  where
    ppr_bndr = pprBndr CaseBind

ppr_expr add_par (Case expr var ty alts)
  = add_par $
    sep [sep [ptext SLIT("case")
		<+> pprCoreExpr expr
		<+> ifPprDebug (braces (ppr ty)),
	      ptext SLIT("of") <+> ppr_bndr var <+> char '{'],
	 nest 2 (sep (punctuate semi (map pprCoreAlt alts))),
	 char '}'
    ]
  where
    ppr_bndr = pprBndr CaseBind
 

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

{-
ppr_expr add_par (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = add_par $
    vcat [
      hsep [ptext SLIT("let {"), (pprBndr LetBind val_bdr $$ ppr val_bndr), equals],
      nest 2 (pprCoreExpr rhs),
      ptext SLIT("} in"),
      pprCoreExpr body ]
-}

ppr_expr add_par (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = add_par
    (hang (ptext SLIT("let {"))
	  2 (hsep [ppr_binding (val_bdr,rhs),
		   ptext SLIT("} in")])
     $$
     pprCoreExpr expr)

-- general case (recursive case, too)
ppr_expr add_par (Let bind expr)
  = add_par $
    sep [hang (ptext keyword) 2 (ppr_bind bind),
	 hang (ptext SLIT("} in ")) 2 (pprCoreExpr expr)]
  where
    keyword = case bind of
		Rec _      -> SLIT("__letrec {")
		NonRec _ _ -> SLIT("let {")

ppr_expr add_par (Note (SCC cc) expr)
  = add_par (sep [pprCostCentreCore cc, pprCoreExpr expr])

ppr_expr add_par (Note InlineMe expr)
  = add_par $ ptext SLIT("__inline_me") <+> pprParendExpr expr

ppr_expr add_par (Note (CoreNote s) expr)
  = add_par $ 
    sep [sep [ptext SLIT("__core_note"), pprHsString (mkFastString s)],
         pprParendExpr expr]

pprCoreAlt (con, args, rhs) 
  = hang (ppr_case_pat con args) 2 (pprCoreExpr rhs)

ppr_case_pat con@(DataAlt dc) args
  | isTupleTyCon tc
  = tupleParens (tupleTyConBoxity tc) (hsep (punctuate comma (map ppr_bndr args))) <+> arrow
  where
    ppr_bndr = pprBndr CaseBind
    tc = dataConTyCon dc

ppr_case_pat con args
  = ppr con <+> sep (map ppr_bndr args) <+> arrow
  where
    ppr_bndr = pprBndr CaseBind

pprArg (Type ty) = ptext SLIT("@") <+> pprParendType ty
pprArg expr      = pprParendExpr expr
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
instance OutputableBndr Var where
  pprBndr = pprCoreBinder

pprCoreBinder :: BindingSite -> Var -> SDoc
pprCoreBinder LetBind binder
  = vcat [sig, pprIdDetails binder, pragmas]
  where
    sig     = pprTypedBinder binder
    pragmas = ppIdInfo binder (idInfo binder)

-- Lambda bound type variables are preceded by "@"
pprCoreBinder LambdaBind bndr = parens (pprTypedBinder bndr)

-- Case bound things don't get a signature or a herald, unless we have debug on
pprCoreBinder CaseBind bndr 
  = getPprStyle $ \ sty ->
    if debugStyle sty then
	parens (pprTypedBinder bndr)
    else
	pprUntypedBinder bndr

pprUntypedBinder binder
  | isTyVar binder = ptext SLIT("@") <+> ppr binder	-- NB: don't print kind
  | otherwise      = pprIdBndr binder

pprTypedBinder binder
  | isTyVar binder  = ptext SLIT("@") <+> pprTyVarBndr binder
  | otherwise	    = pprIdBndr binder <+> dcolon <+> pprType (idType binder)

pprTyVarBndr :: TyVar -> SDoc
pprTyVarBndr tyvar
  = getPprStyle $ \ sty ->
    if debugStyle sty then
        hsep [ppr tyvar, dcolon, pprParendKind kind]
		-- See comments with ppDcolon in PprCore.lhs
    else
        ppr tyvar
  where
    kind = tyVarKind tyvar

-- pprIdBndr does *not* print the type
-- When printing any Id binder in debug mode, we print its inline pragma and one-shot-ness
pprIdBndr id = ppr id <+> pprIdBndrInfo (idInfo id)

pprIdBndrInfo info 
  = megaSeqIdInfo `seq` doc -- The seq is useful for poking on black holes
  where
    prag_info = inlinePragInfo info
    occ_info  = occInfo info
    dmd_info  = newDemandInfo info
    lbv_info  = lbvarInfo info

    no_info = isAlwaysActive prag_info && isNoOcc occ_info && 
	      (case dmd_info of { Nothing -> True; Just d -> isTop d }) &&
	      hasNoLBVarInfo lbv_info

    doc | no_info = empty
 	| otherwise
        = brackets $ hsep [ppr prag_info, ppr occ_info, 
			   ppr dmd_info, ppr lbv_info
#ifdef OLD_STRICTNESS
			   , ppr (demandInfo id)
#endif
			  ]
\end{code}


\begin{code}
pprIdDetails :: Id -> SDoc
pprIdDetails id | isGlobalId id     = ppr (globalIdDetails id)
		| isExportedId id   = ptext SLIT("[Exported]")
		| otherwise	    = empty

ppIdInfo :: Id -> IdInfo -> SDoc
ppIdInfo b info
  = brackets $
    vcat [  ppArityInfo a,
	    ppWorkerInfo (workerInfo info),
	    ppCafInfo (cafInfo info),
#ifdef OLD_STRICTNESS
	    ppStrictnessInfo s,
            ppCprInfo m,
#endif
	    pprNewStrictness (newStrictnessInfo info),
	    if null rules then empty
	    else ptext SLIT("RULES:") <+> vcat (map pprRule rules)
	-- Inline pragma, occ, demand, lbvar info
	-- printed out with all binders (when debug is on); 
	-- see PprCore.pprIdBndr
	]
  where
    a = arityInfo info
#ifdef OLD_STRICTNESS
    s = strictnessInfo info
    m = cprInfo info
#endif
    rules = specInfoRules (specInfo info)
\end{code}


\begin{code}
instance Outputable CoreRule where
   ppr = pprRule

pprRules :: [CoreRule] -> SDoc
pprRules rules = vcat (map pprRule rules)

pprRule :: CoreRule -> SDoc
pprRule (BuiltinRule { ru_fn = fn, ru_name = name})
  = ptext SLIT("Built in rule for") <+> ppr fn <> colon <+> doubleQuotes (ftext name)

pprRule (Rule { ru_name = name, ru_act = act, ru_fn = fn,
		ru_bndrs = tpl_vars, ru_args = tpl_args,
		ru_rhs = rhs })
  = hang (doubleQuotes (ftext name) <+> ppr act)
       4 (sep [ptext SLIT("__forall") <+> braces (sep (map pprTypedBinder tpl_vars)),
	       nest 2 (ppr fn <+> sep (map pprArg tpl_args)),
	       nest 2 (ptext SLIT("=") <+> pprCoreExpr rhs)
	    ])
\end{code}
