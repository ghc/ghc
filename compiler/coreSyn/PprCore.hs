{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


Printing of Core syntax
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module PprCore (
        pprCoreExpr, pprParendExpr,
        pprCoreBinding, pprCoreBindings, pprCoreAlt,
        pprCoreBindingWithSize, pprCoreBindingsWithSize,
        pprRules, ppIdInfo
    ) where

import CoreSyn
import CoreStats (exprStats)
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
import SrcLoc      ( pprUserRealSpan )

{-
************************************************************************
*                                                                      *
\subsection{Public interfaces for Core printing (excluding instances)}
*                                                                      *
************************************************************************

@pprParendCoreExpr@ puts parens around non-atomic Core expressions.
-}

pprCoreBindings :: OutputableBndr b => [Bind b] -> SDoc
pprCoreBinding  :: OutputableBndr b => Bind b  -> SDoc
pprCoreExpr     :: OutputableBndr b => Expr b  -> SDoc
pprParendExpr   :: OutputableBndr b => Expr b  -> SDoc

pprCoreBindings = pprTopBinds noAnn
pprCoreBinding  = pprTopBind noAnn

pprCoreBindingsWithSize :: [CoreBind] -> SDoc
pprCoreBindingWithSize  :: CoreBind  -> SDoc

pprCoreBindingsWithSize = pprTopBinds sizeAnn
pprCoreBindingWithSize = pprTopBind sizeAnn

instance OutputableBndr b => Outputable (Bind b) where
    ppr bind = ppr_bind noAnn bind

instance OutputableBndr b => Outputable (Expr b) where
    ppr expr = pprCoreExpr expr

{-
************************************************************************
*                                                                      *
\subsection{The guts}
*                                                                      *
************************************************************************
-}

-- | A function to produce an annotation for a given right-hand-side
type Annotation b = Expr b -> SDoc

-- | Annotate with the size of the right-hand-side
sizeAnn :: CoreExpr -> SDoc
sizeAnn e = text "-- RHS size:" <+> ppr (exprStats e)

-- | No annotation
noAnn :: Expr b -> SDoc
noAnn _ = empty

pprTopBinds :: OutputableBndr a
            => Annotation a -- ^ generate an annotation to place before the
                            -- binding
            -> [Bind a]     -- ^ bindings to show
            -> SDoc         -- ^ the pretty result
pprTopBinds ann binds = vcat (map (pprTopBind ann) binds)

pprTopBind :: OutputableBndr a => Annotation a -> Bind a -> SDoc
pprTopBind ann (NonRec binder expr)
 = ppr_binding ann (binder,expr) $$ blankLine

pprTopBind _ (Rec [])
  = text "Rec { }"
pprTopBind ann (Rec (b:bs))
  = vcat [text "Rec {",
          ppr_binding ann b,
          vcat [blankLine $$ ppr_binding ann b | b <- bs],
          text "end Rec }",
          blankLine]

ppr_bind :: OutputableBndr b => Annotation b -> Bind b -> SDoc

ppr_bind ann (NonRec val_bdr expr) = ppr_binding ann (val_bdr, expr)
ppr_bind ann (Rec binds)           = vcat (map pp binds)
                                    where
                                      pp bind = ppr_binding ann bind <> semi

ppr_binding :: OutputableBndr b => Annotation b -> (b, Expr b) -> SDoc
ppr_binding ann (val_bdr, expr)
  = ann expr $$ pprBndr LetBind val_bdr $$
    hang (ppr val_bdr <+> equals) 2 (pprCoreExpr expr)

pprParendExpr expr = ppr_expr parens expr
pprCoreExpr   expr = ppr_expr noParens expr

noParens :: SDoc -> SDoc
noParens pp = pp

pprOptCo :: Coercion -> SDoc
pprOptCo co = sdocWithDynFlags $ \dflags ->
              if gopt Opt_SuppressCoercions dflags
              then text "..."
              else parens (sep [ppr co, dcolon <+> ppr (coercionType co)])

ppr_expr :: OutputableBndr b => (SDoc -> SDoc) -> Expr b -> SDoc
        -- The function adds parens in context that need
        -- an atomic value (e.g. function args)

ppr_expr _       (Var name)    = ppr name
ppr_expr add_par (Type ty)     = add_par (text "TYPE:" <+> ppr ty)       -- Weird
ppr_expr add_par (Coercion co) = add_par (text "CO:" <+> ppr co)
ppr_expr add_par (Lit lit)     = pprLiteral add_par lit

ppr_expr add_par (Cast expr co)
  = add_par $ sep [pprParendExpr expr, text "`cast`" <+> pprOptCo co]

ppr_expr add_par expr@(Lam _ _)
  = let
        (bndrs, body) = collectBinders expr
    in
    add_par $
    hang (text "\\" <+> sep (map (pprBndr LambdaBind) bndrs) <+> arrow)
         2 (pprCoreExpr body)

ppr_expr add_par expr@(App {})
  = sdocWithDynFlags $ \dflags ->
    case collectArgs expr of { (fun, args) ->
    let
        pp_args     = sep (map pprArg args)
        val_args    = dropWhile isTypeArg args   -- Drop the type arguments for tuples
        pp_tup_args = pprWithCommas pprCoreExpr val_args
        args'
          | gopt Opt_SuppressTypeApplications dflags = val_args
          | otherwise = args
        parens
          | null args' = id
          | otherwise  = add_par
    in
    case fun of
        Var f -> case isDataConWorkId_maybe f of
                        -- Notice that we print the *worker*
                        -- for tuples in paren'd format.
                   Just dc | saturated
                           , Just sort <- tyConTuple_maybe tc
                           -> tupleParens sort pp_tup_args
                           where
                             tc        = dataConTyCon dc
                             saturated = val_args `lengthIs` idArity f

                   _ -> parens (hang (ppr f) 2 pp_args)

        _ -> parens (hang (pprParendExpr fun) 2 pp_args)
    }

ppr_expr add_par (Case expr var ty [(con,args,rhs)])
  = sdocWithDynFlags $ \dflags ->
    if gopt Opt_PprCaseAsLet dflags
    then add_par $  -- See Note [Print case as let]
         sep [ sep [ text "let! {"
                     <+> ppr_case_pat con args
                     <+> text "~"
                     <+> ppr_bndr var
                   , text "<-" <+> ppr_expr id expr
                     <+> text "} in" ]
             , pprCoreExpr rhs
             ]
    else add_par $
         sep [sep [sep [ text "case" <+> pprCoreExpr expr
                       , ifPprDebug (text "return" <+> ppr ty)
                       , text "of" <+> ppr_bndr var
                       ]
                  , char '{' <+> ppr_case_pat con args <+> arrow
                  ]
              , pprCoreExpr rhs
              , char '}'
              ]
  where
    ppr_bndr = pprBndr CaseBind

ppr_expr add_par (Case expr var ty alts)
  = add_par $
    sep [sep [text "case"
                <+> pprCoreExpr expr
                <+> ifPprDebug (text "return" <+> ppr ty),
              text "of" <+> ppr_bndr var <+> char '{'],
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
      hsep [text "let {", (pprBndr LetBind val_bdr $$ ppr val_bndr), equals],
      nest 2 (pprCoreExpr rhs),
      text "} in",
      pprCoreExpr body ]

ppr_expr add_par (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = add_par
    (hang (text "let {")
          2 (hsep [ppr_binding (val_bdr,rhs),
                   text "} in"])
     $$
     pprCoreExpr expr)
-}

-- General case (recursive case, too)
ppr_expr add_par (Let bind expr)
  = add_par $
    sep [hang (ptext keyword) 2 (ppr_bind noAnn bind <+> text "} in"),
         pprCoreExpr expr]
  where
    keyword = case bind of
                Rec _      -> (sLit "letrec {")
                NonRec _ _ -> (sLit "let {")

ppr_expr add_par (Tick tickish expr)
  = sdocWithDynFlags $ \dflags ->
  if gopt Opt_PprShowTicks dflags
  then add_par (sep [ppr tickish, pprCoreExpr expr])
  else ppr_expr add_par expr

pprCoreAlt :: OutputableBndr a => (AltCon, [a] , Expr a) -> SDoc
pprCoreAlt (con, args, rhs)
  = hang (ppr_case_pat con args <+> arrow) 2 (pprCoreExpr rhs)

ppr_case_pat :: OutputableBndr a => AltCon -> [a] -> SDoc
ppr_case_pat (DataAlt dc) args
  | Just sort <- tyConTuple_maybe tc
  = tupleParens sort (pprWithCommas ppr_bndr args)
  where
    ppr_bndr = pprBndr CasePatBind
    tc = dataConTyCon dc

ppr_case_pat con args
  = ppr con <+> (fsep (map ppr_bndr args))
  where
    ppr_bndr = pprBndr CasePatBind


-- | Pretty print the argument in a function application.
pprArg :: OutputableBndr a => Expr a -> SDoc
pprArg (Type ty)
 = sdocWithDynFlags $ \dflags ->
   if gopt Opt_SuppressTypeApplications dflags
   then empty
   else text "@" <+> pprParendType ty
pprArg (Coercion co) = text "@~" <+> pprOptCo co
pprArg expr          = pprParendExpr expr

{-
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


Note [Binding-site specific printing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pprCoreBinder and pprTypedLamBinder receive a BindingSite argument to adjust
the information printed.

Let-bound binders are printed with their full type and idInfo.

Case-bound variables (both the case binder and pattern variables) are printed
without a type and without their unfolding.

Furthermore, a dead case-binder is completely ignored, while otherwise, dead
binders are printed as "_".
-}

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
  | isTyVar binder = text "@" <+> ppr binder    -- NB: don't print kind
  | otherwise      = pprIdBndr binder

pprTypedLamBinder :: BindingSite -> Bool -> Var -> SDoc
-- For lambda and case binders, show the unfolding info (usually none)
pprTypedLamBinder bind_site debug_on var
  = sdocWithDynFlags $ \dflags ->
    case () of
    _
      | not debug_on            -- Show case-bound wild bilders only if debug is on
      , CaseBind <- bind_site
      , isDeadBinder var        -> empty

      | not debug_on            -- Even dead binders can be one-shot
      , isDeadBinder var        -> char '_' <+> ppWhen (isId var)
                                                (pprIdBndrInfo (idInfo var))

      | not debug_on            -- No parens, no kind info
      , CaseBind <- bind_site   -> pprUntypedBinder var

      | not debug_on
      , CasePatBind <- bind_site    -> pprUntypedBinder var

      | suppress_sigs dflags    -> pprUntypedBinder var

      | isTyVar var  -> parens (pprKindedTyVarBndr var)

      | otherwise    -> parens (hang (pprIdBndr var)
                                   2 (vcat [ dcolon <+> pprType (idType var)
                                           , pp_unf]))
  where
    suppress_sigs = gopt Opt_SuppressTypeSignatures

    unf_info = unfoldingInfo (idInfo var)
    pp_unf | hasSomeUnfolding unf_info = text "Unf=" <> ppr unf_info
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
  = text "@" <+> pprTvBndr tyvar

-- pprIdBndr does *not* print the type
-- When printing any Id binder in debug mode, we print its inline pragma and one-shot-ness
pprIdBndr :: Id -> SDoc
pprIdBndr id = ppr id <+> pprIdBndrInfo (idInfo id)

pprIdBndrInfo :: IdInfo -> SDoc
pprIdBndrInfo info
  = sdocWithDynFlags $ \dflags ->
    ppUnless (gopt Opt_SuppressIdInfo dflags) $
    info `seq` doc -- The seq is useful for poking on black holes
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
          [ (has_prag, text "InlPrag=" <> ppr prag_info)
          , (has_occ,  text "Occ=" <> ppr occ_info)
          , (has_dmd,  text "Dmd=" <> ppr dmd_info)
          , (has_lbv , text "OS=" <> ppr lbv_info)
          ]

{-
-----------------------------------------------------
--      IdDetails and IdInfo
-----------------------------------------------------
-}

ppIdInfo :: Id -> IdInfo -> SDoc
ppIdInfo id info
  = sdocWithDynFlags $ \dflags ->
    ppUnless (gopt Opt_SuppressIdInfo dflags) $
    showAttributes
    [ (True, pp_scope <> ppr (idDetails id))
    , (has_arity,        text "Arity=" <> int arity)
    , (has_called_arity, text "CallArity=" <> int called_arity)
    , (has_caf_info,     text "Caf=" <> ppr caf_info)
    , (has_str_info,     text "Str=" <> pprStrictness str_info)
    , (has_unf,          text "Unf=" <> ppr unf_info)
    , (not (null rules), text "RULES:" <+> vcat (map pprRule rules))
    ]   -- Inline pragma, occ, demand, one-shot info
        -- printed out with all binders (when debug is on);
        -- see PprCore.pprIdBndr
  where
    pp_scope | isGlobalId id   = text "GblId"
             | isExportedId id = text "LclIdX"
             | otherwise       = text "LclId"

    arity = arityInfo info
    has_arity = arity /= 0

    called_arity = callArityInfo info
    has_called_arity = called_arity /= 0

    caf_info = cafInfo info
    has_caf_info = not (mayHaveCafRefs caf_info)

    str_info = strictnessInfo info
    has_str_info = not (isTopSig str_info)

    unf_info = unfoldingInfo info
    has_unf = hasSomeUnfolding unf_info

    rules = ruleInfoRules (ruleInfo info)

showAttributes :: [(Bool,SDoc)] -> SDoc
showAttributes stuff
  | null docs = empty
  | otherwise = brackets (sep (punctuate comma docs))
  where
    docs = [d | (True,d) <- stuff]

{-
-----------------------------------------------------
--      Unfolding and UnfoldingGuidance
-----------------------------------------------------
-}

instance Outputable UnfoldingGuidance where
    ppr UnfNever  = text "NEVER"
    ppr (UnfWhen { ug_arity = arity, ug_unsat_ok = unsat_ok, ug_boring_ok = boring_ok })
      = text "ALWAYS_IF" <>
        parens (text "arity="     <> int arity    <> comma <>
                text "unsat_ok="  <> ppr unsat_ok <> comma <>
                text "boring_ok=" <> ppr boring_ok)
    ppr (UnfIfGoodArgs { ug_args = cs, ug_size = size, ug_res = discount })
      = hsep [ text "IF_ARGS",
               brackets (hsep (map int cs)),
               int size,
               int discount ]

instance Outputable UnfoldingSource where
  ppr InlineCompulsory  = text "Compulsory"
  ppr InlineStable      = text "InlineStable"
  ppr InlineRhs         = text "<vanilla>"

instance Outputable Unfolding where
  ppr NoUnfolding                = text "No unfolding"
  ppr (OtherCon cs)              = text "OtherCon" <+> ppr cs
  ppr (DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = args })
       = hang (text "DFun:" <+> ptext (sLit "\\")
                <+> sep (map (pprBndr LambdaBind) bndrs) <+> arrow)
            2 (ppr con <+> sep (map ppr args))
  ppr (CoreUnfolding { uf_src = src
                     , uf_tmpl=rhs, uf_is_top=top, uf_is_value=hnf
                     , uf_is_conlike=conlike, uf_is_work_free=wf
                     , uf_expandable=exp, uf_guidance=g })
        = text "Unf" <> braces (pp_info $$ pp_rhs)
    where
      pp_info = fsep $ punctuate comma
                [ text "Src="        <> ppr src
                , text "TopLvl="     <> ppr top
                , text "Value="      <> ppr hnf
                , text "ConLike="    <> ppr conlike
                , text "WorkFree="   <> ppr wf
                , text "Expandable=" <> ppr exp
                , text "Guidance="   <> ppr g ]
      pp_tmpl = sdocWithDynFlags $ \dflags ->
                ppUnless (gopt Opt_SuppressUnfoldings dflags) $
                text "Tmpl=" <+> ppr rhs
      pp_rhs | isStableSource src = pp_tmpl
             | otherwise          = empty
            -- Don't print the RHS or we get a quadratic
            -- blowup in the size of the printout!

{-
-----------------------------------------------------
--      Rules
-----------------------------------------------------
-}

instance Outputable CoreRule where
   ppr = pprRule

pprRules :: [CoreRule] -> SDoc
pprRules rules = vcat (map pprRule rules)

pprRule :: CoreRule -> SDoc
pprRule (BuiltinRule { ru_fn = fn, ru_name = name})
  = text "Built in rule for" <+> ppr fn <> colon <+> doubleQuotes (ftext name)

pprRule (Rule { ru_name = name, ru_act = act, ru_fn = fn,
                ru_bndrs = tpl_vars, ru_args = tpl_args,
                ru_rhs = rhs })
  = hang (doubleQuotes (ftext name) <+> ppr act)
       4 (sep [text "forall" <+>
                  sep (map (pprCoreBinder LambdaBind) tpl_vars) <> dot,
               nest 2 (ppr fn <+> sep (map pprArg tpl_args)),
               nest 2 (text "=" <+> pprCoreExpr rhs)
            ])

{-
-----------------------------------------------------
--      Tickish
-----------------------------------------------------
-}

instance Outputable id => Outputable (Tickish id) where
  ppr (HpcTick modl ix) =
      hcat [text "hpc<",
            ppr modl, comma,
            ppr ix,
            text ">"]
  ppr (Breakpoint ix vars) =
      hcat [text "break<",
            ppr ix,
            text ">",
            parens (hcat (punctuate comma (map ppr vars)))]
  ppr (ProfNote { profNoteCC = cc,
                  profNoteCount = tick,
                  profNoteScope = scope }) =
      case (tick,scope) of
         (True,True)  -> hcat [text "scctick<", ppr cc, char '>']
         (True,False) -> hcat [text "tick<",    ppr cc, char '>']
         _            -> hcat [text "scc<",     ppr cc, char '>']
  ppr (SourceNote span _) =
      hcat [ text "src<", pprUserRealSpan True span, char '>']

{-
-----------------------------------------------------
--      Vectorisation declarations
-----------------------------------------------------
-}

instance Outputable CoreVect where
  ppr (Vect     var e)               = hang (text "VECTORISE" <+> ppr var <+> char '=')
                                         4 (pprCoreExpr e)
  ppr (NoVect   var)                 = text "NOVECTORISE" <+> ppr var
  ppr (VectType False var Nothing)   = text "VECTORISE type" <+> ppr var
  ppr (VectType True  var Nothing)   = text "VECTORISE SCALAR type" <+> ppr var
  ppr (VectType False var (Just tc)) = text "VECTORISE type" <+> ppr var <+> char '=' <+>
                                       ppr tc
  ppr (VectType True var (Just tc))  = text "VECTORISE SCALAR type" <+> ppr var <+>
                                       char '=' <+> ppr tc
  ppr (VectClass tc)                 = text "VECTORISE class" <+> ppr tc
  ppr (VectInst var)                 = text "VECTORISE SCALAR instance" <+> ppr var
