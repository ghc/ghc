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
pprTopBinds :: OutputableBndr a => [Bind a] -> SDoc
pprTopBinds binds = vcat (map pprTopBind binds)

pprTopBind :: OutputableBndr a => Bind a -> SDoc
pprTopBind (NonRec binder expr)
 = ppr_binding (binder,expr) $$ text ""

pprTopBind (Rec binds)
  = vcat [ptext (sLit "Rec {"),
	  vcat (map ppr_binding binds),
	  ptext (sLit "end Rec }"),
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

ppr_expr add_par (Type ty)  = add_par (ptext (sLit "TYPE") <+> ppr ty)	-- Wierd
	           
ppr_expr _       (Var name) = ppr name
ppr_expr _       (Lit lit)  = ppr lit

ppr_expr add_par (Cast expr co) 
  = add_par $
    sep [pprParendExpr expr, 
	 ptext (sLit "`cast`") <+> parens (pprCo co)]
  where
    pprCo co = sep [ppr co, dcolon <+> ppr (coercionKindPredTy co)]
	 

ppr_expr add_par expr@(Lam _ _)
  = let
	(bndrs, body) = collectBinders expr
    in
    add_par $
    hang (ptext (sLit "\\") <+> sep (map (pprBndr LambdaBind) bndrs) <+> arrow)
	 2 (pprCoreExpr body)

ppr_expr add_par expr@(App {})
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

		   _ -> add_par (hang (ppr f) 2 pp_args)

	_ -> add_par (hang (pprParendExpr fun) 2 pp_args)
    }

ppr_expr add_par (Case expr var ty [(con,args,rhs)])
  = add_par $
    sep [sep [ptext (sLit "case") <+> pprCoreExpr expr,
	      ifPprDebug (braces (ppr ty)),
	      sep [ptext (sLit "of") <+> ppr_bndr var, 
		   char '{' <+> ppr_case_pat con args]
	  ],
	 pprCoreExpr rhs,
	 char '}'
    ]
  where
    ppr_bndr = pprBndr CaseBind

ppr_expr add_par (Case expr var ty alts)
  = add_par $
    sep [sep [ptext (sLit "case")
		<+> pprCoreExpr expr
		<+> ifPprDebug (braces (ppr ty)),
	      ptext (sLit "of") <+> ppr_bndr var <+> char '{'],
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
      hsep [ptext (sLit "let {"), (pprBndr LetBind val_bdr $$ ppr val_bndr), equals],
      nest 2 (pprCoreExpr rhs),
      ptext (sLit "} in"),
      pprCoreExpr body ]

ppr_expr add_par (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = add_par
    (hang (ptext (sLit "let {"))
	  2 (hsep [ppr_binding (val_bdr,rhs),
		   ptext (sLit "} in")])
     $$
     pprCoreExpr expr)
-}

-- General case (recursive case, too)
ppr_expr add_par (Let bind expr)
  = add_par $
    sep [hang (ptext keyword) 2 (ppr_bind bind <+> ptext (sLit "} in")),
	 pprCoreExpr expr]
  where
    keyword = case bind of
		Rec _      -> (sLit "letrec {")
		NonRec _ _ -> (sLit "let {")

ppr_expr add_par (Note (SCC cc) expr)
  = add_par (sep [pprCostCentreCore cc, pprCoreExpr expr])

ppr_expr add_par (Note InlineMe expr)
  = add_par $ ptext (sLit "__inline_me") <+> pprParendExpr expr

ppr_expr add_par (Note (CoreNote s) expr)
  = add_par $ 
    sep [sep [ptext (sLit "__core_note"), pprHsString (mkFastString s)],
         pprParendExpr expr]

pprCoreAlt :: OutputableBndr a => (AltCon, [a] , Expr a) -> SDoc
pprCoreAlt (con, args, rhs) 
  = hang (ppr_case_pat con args) 2 (pprCoreExpr rhs)

ppr_case_pat :: OutputableBndr a => AltCon -> [a] -> SDoc
ppr_case_pat (DataAlt dc) args
  | isTupleTyCon tc
  = tupleParens (tupleTyConBoxity tc) (hsep (punctuate comma (map ppr_bndr args))) <+> arrow
  where
    ppr_bndr = pprBndr CaseBind
    tc = dataConTyCon dc

ppr_case_pat con args
  = ppr con <+> sep (map ppr_bndr args) <+> arrow
  where
    ppr_bndr = pprBndr CaseBind

pprArg :: OutputableBndr a => Expr a -> SDoc
pprArg (Type ty) = ptext (sLit "@") <+> pprParendType ty
pprArg expr      = pprParendExpr expr
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
instance OutputableBndr Var where
  pprBndr = pprCoreBinder

pprCoreBinder :: BindingSite -> Var -> SDoc
pprCoreBinder LetBind binder
  | isTyVar binder = pprTypedBinder binder
  | otherwise
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

pprUntypedBinder :: Var -> SDoc
pprUntypedBinder binder
  | isTyVar binder = ptext (sLit "@") <+> ppr binder	-- NB: don't print kind
  | otherwise      = pprIdBndr binder

pprTypedBinder :: Var -> SDoc
pprTypedBinder binder
  | isTyVar binder  = ptext (sLit "@") <+> pprTyVarBndr binder
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
pprIdBndr :: Id -> SDoc
pprIdBndr id = ppr id <+> pprIdBndrInfo (idInfo id)

pprIdBndrInfo :: IdInfo -> SDoc
pprIdBndrInfo info 
  = megaSeqIdInfo info `seq` doc -- The seq is useful for poking on black holes
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
		| isExportedId id   = ptext (sLit "[Exported]")
		| otherwise	    = empty

ppIdInfo :: Id -> IdInfo -> SDoc
ppIdInfo _ info
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
	    else ptext (sLit "RULES:") <+> vcat (map pprRule rules)
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
  = ptext (sLit "Built in rule for") <+> ppr fn <> colon <+> doubleQuotes (ftext name)

pprRule (Rule { ru_name = name, ru_act = act, ru_fn = fn,
		ru_bndrs = tpl_vars, ru_args = tpl_args,
		ru_rhs = rhs })
  = hang (doubleQuotes (ftext name) <+> ppr act)
       4 (sep [ptext (sLit "forall") <+> braces (sep (map pprTypedBinder tpl_vars)),
	       nest 2 (ppr fn <+> sep (map pprArg tpl_args)),
	       nest 2 (ptext (sLit "=") <+> pprCoreExpr rhs)
	    ])
\end{code}
