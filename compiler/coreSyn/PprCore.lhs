%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1996-1998
%

Printing of Core syntax

\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PprCore (
        pprCoreExpr, pprParendExpr,
        pprCoreBinding, pprCoreBindings, pprCoreAlt,
        pprRules
    ) where

import CoreSyn
import Literal( pprLiteral )
import Name( pprInfixName, pprPrefixName )
import Var
import Id
import IdInfo
import Demand
import DataCon
import TyCon
import Type
import Coercion
import DynFlags
import BasicTypes
import Util
import Outputable
import FastString
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Public interfaces for Core printing (excluding instances)}
%*                                                                      *
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
%*                                                                      *
\subsection{The guts}
%*                                                                      *
%************************************************************************

\begin{code}
pprTopBinds :: OutputableBndr a => [Bind a] -> SDoc
pprTopBinds binds = vcat (map pprTopBind binds)

pprTopBind :: OutputableBndr a => Bind a -> SDoc
pprTopBind (NonRec binder expr)
 = ppr_binding (binder,expr) $$ blankLine

pprTopBind (Rec [])
  = ptext (sLit "Rec { }")
pprTopBind (Rec (b:bs))
  = vcat [ptext (sLit "Rec {"),
          ppr_binding b,
          vcat [blankLine $$ ppr_binding b | b <- bs],
          ptext (sLit "end Rec }"),
          blankLine]
\end{code}

\begin{code}
ppr_bind :: OutputableBndr b => Bind b -> SDoc

ppr_bind (NonRec val_bdr expr) = ppr_binding (val_bdr, expr)
ppr_bind (Rec binds)           = vcat (map pp binds)
                               where
                                 pp bind = ppr_binding bind <> semi

ppr_binding :: OutputableBndr b => (b, Expr b) -> SDoc
ppr_binding (val_bdr, expr)
  = pprBndr LetBind val_bdr $$
    hang (ppr val_bdr <+> equals) 2 (pprCoreExpr expr)
\end{code}

\begin{code}
pprParendExpr expr = ppr_expr parens expr
pprCoreExpr   expr = ppr_expr noParens expr

noParens :: SDoc -> SDoc
noParens pp = pp
\end{code}

\begin{code}
ppr_expr :: OutputableBndr b => (SDoc -> SDoc) -> Expr b -> SDoc
        -- The function adds parens in context that need
        -- an atomic value (e.g. function args)

ppr_expr _       (Var name)    = ppr name
ppr_expr add_par (Type ty)     = add_par (ptext (sLit "TYPE") <+> ppr ty)       -- Weird
ppr_expr add_par (Coercion co) = add_par (ptext (sLit "CO") <+> ppr co)
ppr_expr add_par (Lit lit)     = pprLiteral add_par lit

ppr_expr add_par (Cast expr co)
  = add_par $
    sep [pprParendExpr expr,
         ptext (sLit "`cast`") <+> pprCo co]
  where
    pprCo co = sdocWithDynFlags $ \dflags ->
               if gopt Opt_SuppressCoercions dflags
               then ptext (sLit "...")
               else parens $
                        sep [ppr co, dcolon <+> pprEqPred (coercionKind co)]


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
        val_args    = dropWhile isTypeArg args   -- Drop the type arguments for tuples
        pp_tup_args = sep (punctuate comma (map pprCoreExpr val_args))
    in
    case fun of
        Var f -> case isDataConWorkId_maybe f of
                        -- Notice that we print the *worker*
                        -- for tuples in paren'd format.
                   Just dc | saturated && isTupleTyCon tc
                           -> tupleParens (tupleTyConSort tc) pp_tup_args
                           where
                             tc        = dataConTyCon dc
                             saturated = val_args `lengthIs` idArity f

                   _ -> add_par (hang (ppr f) 2 pp_args)

        _ -> add_par (hang (pprParendExpr fun) 2 pp_args)
    }

ppr_expr add_par (Case expr var ty [(con,args,rhs)])
  = sdocWithDynFlags $ \dflags ->
    if gopt Opt_PprCaseAsLet dflags
    then add_par $  -- See Note [Print case as let]
         sep [ sep [ ptext (sLit "let! {") 
                     <+> ppr_case_pat con args
                     <+> ptext (sLit "~")
                     <+> ppr_bndr var
                   , ptext (sLit "<-") <+> ppr_expr id expr
                     <+> ptext (sLit "} in") ]
             , pprCoreExpr rhs
             ]
    else add_par $
         sep [sep [ptext (sLit "case") <+> pprCoreExpr expr,
                   ifPprDebug (braces (ppr ty)),
                   sep [ptext (sLit "of") <+> ppr_bndr var,
                        char '{' <+> ppr_case_pat con args <+> arrow]
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
         nest 2 (vcat (punctuate semi (map pprCoreAlt alts))),
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

ppr_expr add_par (Tick tickish expr)
  = add_par (sep [ppr tickish, pprCoreExpr expr])

pprCoreAlt :: OutputableBndr a => (AltCon, [a] , Expr a) -> SDoc
pprCoreAlt (con, args, rhs)
  = hang (ppr_case_pat con args <+> arrow) 2 (pprCoreExpr rhs)

ppr_case_pat :: OutputableBndr a => AltCon -> [a] -> SDoc
ppr_case_pat (DataAlt dc) args
  | isTupleTyCon tc
  = tupleParens (tupleTyConSort tc) (hsep (punctuate comma (map ppr_bndr args)))
  where
    ppr_bndr = pprBndr CaseBind
    tc = dataConTyCon dc

ppr_case_pat con args
  = ppr con <+> (fsep (map ppr_bndr args))
  where
    ppr_bndr = pprBndr CaseBind


-- | Pretty print the argument in a function application.
pprArg :: OutputableBndr a => Expr a -> SDoc
pprArg (Type ty)
 = sdocWithDynFlags $ \dflags ->
   if gopt Opt_SuppressTypeApplications dflags
   then empty
   else ptext (sLit "@") <+> pprParendType ty
pprArg (Coercion co) = ptext (sLit "@~") <+> pprParendCo co
pprArg expr          = pprParendExpr expr
\end{code}

Note [Print case as let]
~~~~~~~~~~~~~~~~~~~~~~~~
Single-branch case expressions are very common:
   case x of y { I# x' -> 
   case p of q { I# p' -> ... } }
These are, in effect, just strict let's, with pattern matching.
With -dppr-case-as-let we print them as such:
   let! { I# x' ~ y <- x } in
   let! { I# p' ~ q <- p } in ...

 
Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
instance OutputableBndr Var where
  pprBndr = pprCoreBinder
  pprInfixOcc  = pprInfixName  . varName
  pprPrefixOcc = pprPrefixName . varName

pprCoreBinder :: BindingSite -> Var -> SDoc
pprCoreBinder LetBind binder
  | isTyVar binder = pprKindedTyVarBndr binder
  | otherwise      = pprTypedLetBinder binder $$
                     ppIdInfo binder (idInfo binder)

-- Lambda bound type variables are preceded by "@"
pprCoreBinder bind_site bndr
  = getPprStyle $ \ sty ->
    pprTypedLamBinder bind_site (debugStyle sty) bndr

pprUntypedBinder :: Var -> SDoc
pprUntypedBinder binder
  | isTyVar binder = ptext (sLit "@") <+> ppr binder    -- NB: don't print kind
  | otherwise      = pprIdBndr binder

pprTypedLamBinder :: BindingSite -> Bool -> Var -> SDoc
-- For lambda and case binders, show the unfolding info (usually none)
pprTypedLamBinder bind_site debug_on var
  = sdocWithDynFlags $ \dflags ->
    case () of
    _
      | not debug_on            -- Even dead binders can be one-shot
      , isDeadBinder var        -> char '_' <+> ppWhen (isId var)
                                                (pprIdBndrInfo (idInfo var))

      | not debug_on            -- No parens, no kind info
      , CaseBind <- bind_site   -> pprUntypedBinder var

      | suppress_sigs dflags    -> pprUntypedBinder var

      | isTyVar var  -> parens (pprKindedTyVarBndr var)

      | otherwise    -> parens (hang (pprIdBndr var)
                                   2 (vcat [ dcolon <+> pprType (idType var)
                                           , pp_unf]))
  where
    suppress_sigs = gopt Opt_SuppressTypeSignatures

    unf_info = unfoldingInfo (idInfo var)
    pp_unf | hasSomeUnfolding unf_info = ptext (sLit "Unf=") <> ppr unf_info
           | otherwise                 = empty

pprTypedLetBinder :: Var -> SDoc
-- Print binder with a type or kind signature (not paren'd)
pprTypedLetBinder binder
  = sdocWithDynFlags $ \dflags ->
    case () of
    _
      | isTyVar binder                         -> pprKindedTyVarBndr binder
      | gopt Opt_SuppressTypeSignatures dflags -> pprIdBndr binder
      | otherwise                              -> hang (pprIdBndr binder) 2 (dcolon <+> pprType (idType binder))

pprKindedTyVarBndr :: TyVar -> SDoc
-- Print a type variable binder with its kind (but not if *)
pprKindedTyVarBndr tyvar
  = ptext (sLit "@") <+> pprTvBndr tyvar

-- pprIdBndr does *not* print the type
-- When printing any Id binder in debug mode, we print its inline pragma and one-shot-ness
pprIdBndr :: Id -> SDoc
pprIdBndr id = ppr id <+> pprIdBndrInfo (idInfo id)

pprIdBndrInfo :: IdInfo -> SDoc
pprIdBndrInfo info
  = sdocWithDynFlags $ \dflags ->
    if gopt Opt_SuppressIdInfo dflags
    then empty
    else megaSeqIdInfo info `seq` doc -- The seq is useful for poking on black holes
  where
    prag_info = inlinePragInfo info
    occ_info  = occInfo info
    dmd_info  = demandInfo info
    lbv_info  = oneShotInfo info

    has_prag  = not (isDefaultInlinePragma prag_info)
    has_occ   = not (isNoOcc occ_info)
    has_dmd   = not $ isTopDmd dmd_info 
    has_lbv   = not (hasNoOneShotInfo lbv_info)

    doc = showAttributes
          [ (has_prag, ptext (sLit "InlPrag=") <> ppr prag_info)
          , (has_occ,  ptext (sLit "Occ=") <> ppr occ_info)
          , (has_dmd,  ptext (sLit "Dmd=") <> ppr dmd_info)
          , (has_lbv , ptext (sLit "OS=") <> ppr lbv_info)
          ]
\end{code}


-----------------------------------------------------
--      IdDetails and IdInfo
-----------------------------------------------------

\begin{code}
ppIdInfo :: Id -> IdInfo -> SDoc
ppIdInfo id info
  = sdocWithDynFlags $ \dflags ->
    if gopt Opt_SuppressIdInfo dflags
    then empty
    else
    showAttributes
    [ (True, pp_scope <> ppr (idDetails id))
    , (has_arity,      ptext (sLit "Arity=") <> int arity)
    , (has_caf_info,   ptext (sLit "Caf=") <> ppr caf_info)
    , (True,           ptext (sLit "Str=") <> pprStrictness str_info)
    , (has_unf,        ptext (sLit "Unf=") <> ppr unf_info)
    , (not (null rules), ptext (sLit "RULES:") <+> vcat (map pprRule rules))
    ]   -- Inline pragma, occ, demand, one-shot info
        -- printed out with all binders (when debug is on);
        -- see PprCore.pprIdBndr
  where
    pp_scope | isGlobalId id   = ptext (sLit "GblId")
             | isExportedId id = ptext (sLit "LclIdX")
             | otherwise       = ptext (sLit "LclId")

    arity = arityInfo info
    has_arity = arity /= 0

    caf_info = cafInfo info
    has_caf_info = not (mayHaveCafRefs caf_info)

    str_info = strictnessInfo info

    unf_info = unfoldingInfo info
    has_unf = hasSomeUnfolding unf_info

    rules = specInfoRules (specInfo info)

showAttributes :: [(Bool,SDoc)] -> SDoc
showAttributes stuff
  | null docs = empty
  | otherwise = brackets (sep (punctuate comma docs))
  where
    docs = [d | (True,d) <- stuff]
\end{code}

-----------------------------------------------------
--      Unfolding and UnfoldingGuidance
-----------------------------------------------------

\begin{code}
instance Outputable UnfoldingGuidance where
    ppr UnfNever  = ptext (sLit "NEVER")
    ppr (UnfWhen unsat_ok boring_ok)
      = ptext (sLit "ALWAYS_IF") <>
        parens (ptext (sLit "unsat_ok=") <> ppr unsat_ok <> comma <>
                ptext (sLit "boring_ok=") <> ppr boring_ok)
    ppr (UnfIfGoodArgs { ug_args = cs, ug_size = size, ug_res = discount })
      = hsep [ ptext (sLit "IF_ARGS"),
               brackets (hsep (map int cs)),
               int size,
               int discount ]

instance Outputable UnfoldingSource where
  ppr InlineCompulsory  = ptext (sLit "Compulsory")
  ppr InlineStable      = ptext (sLit "InlineStable")
  ppr InlineRhs         = ptext (sLit "<vanilla>")

instance Outputable Unfolding where
  ppr NoUnfolding                = ptext (sLit "No unfolding")
  ppr (OtherCon cs)              = ptext (sLit "OtherCon") <+> ppr cs
  ppr (DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = args })
       = hang (ptext (sLit "DFun:") <+> ptext (sLit "\\") 
                <+> sep (map (pprBndr LambdaBind) bndrs) <+> arrow)
            2 (ppr con <+> sep (map ppr args))
  ppr (CoreUnfolding { uf_src = src
                     , uf_tmpl=rhs, uf_is_top=top, uf_is_value=hnf
                     , uf_is_conlike=conlike, uf_is_work_free=wf
                     , uf_expandable=exp, uf_guidance=g, uf_arity=arity})
        = ptext (sLit "Unf") <> braces (pp_info $$ pp_rhs)
    where
      pp_info = fsep $ punctuate comma
                [ ptext (sLit "Src=")        <> ppr src
                , ptext (sLit "TopLvl=")     <> ppr top
                , ptext (sLit "Arity=")      <> int arity
                , ptext (sLit "Value=")      <> ppr hnf
                , ptext (sLit "ConLike=")    <> ppr conlike
                , ptext (sLit "WorkFree=")   <> ppr wf
                , ptext (sLit "Expandable=") <> ppr exp
                , ptext (sLit "Guidance=")   <> ppr g ]
      pp_tmpl = ptext (sLit "Tmpl=") <+> ppr rhs
      pp_rhs | isStableSource src = pp_tmpl
             | otherwise          = empty
            -- Don't print the RHS or we get a quadratic
            -- blowup in the size of the printout!
\end{code}

-----------------------------------------------------
--      Rules
-----------------------------------------------------

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
       4 (sep [ptext (sLit "forall") <+>
                  sep (map (pprCoreBinder LambdaBind) tpl_vars) <> dot,
               nest 2 (ppr fn <+> sep (map pprArg tpl_args)),
               nest 2 (ptext (sLit "=") <+> pprCoreExpr rhs)
            ])
\end{code}

-----------------------------------------------------
--      Tickish
-----------------------------------------------------

\begin{code}
instance Outputable id => Outputable (Tickish id) where
  ppr (HpcTick modl ix) =
      hcat [ptext (sLit "tick<"),
            ppr modl, comma,
            ppr ix,
            ptext (sLit ">")]
  ppr (Breakpoint ix vars) =
      hcat [ptext (sLit "break<"),
            ppr ix,
            ptext (sLit ">"),
            parens (hcat (punctuate comma (map ppr vars)))]
  ppr (ProfNote { profNoteCC = cc,
                  profNoteCount = tick,
                  profNoteScope = scope }) =
      case (tick,scope) of
         (True,True)  -> hcat [ptext (sLit "scctick<"), ppr cc, char '>']
         (True,False) -> hcat [ptext (sLit "tick<"),    ppr cc, char '>']
         _            -> hcat [ptext (sLit "scc<"),     ppr cc, char '>']
\end{code}

-----------------------------------------------------
--      Vectorisation declarations
-----------------------------------------------------

\begin{code}
instance Outputable CoreVect where
  ppr (Vect     var e)               = hang (ptext (sLit "VECTORISE") <+> ppr var <+> char '=')
                                         4 (pprCoreExpr e)
  ppr (NoVect   var)                 = ptext (sLit "NOVECTORISE") <+> ppr var
  ppr (VectType False var Nothing)   = ptext (sLit "VECTORISE type") <+> ppr var
  ppr (VectType True  var Nothing)   = ptext (sLit "VECTORISE SCALAR type") <+> ppr var
  ppr (VectType False var (Just tc)) = ptext (sLit "VECTORISE type") <+> ppr var <+> char '=' <+>
                                       ppr tc
  ppr (VectType True var (Just tc))  = ptext (sLit "VECTORISE SCALAR type") <+> ppr var <+>
                                       char '=' <+> ppr tc
  ppr (VectClass tc)                 = ptext (sLit "VECTORISE class") <+> ppr tc
  ppr (VectInst var)                 = ptext (sLit "VECTORISE SCALAR instance") <+> ppr var
\end{code}
