{-# LANGUAGE LambdaCase #-}

{-
   these are needed for the Outputable instance for GenTickish,
   since we need XTickishId to be Outputable. This should immediately
   resolve to something like Id.
 -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


Printing of Core syntax
-}

module GHC.Core.Ppr (
        pprCoreExpr, pprParendExpr,
        pprCoreBinding, pprCoreBindings, pprCoreAlt,
        pprCoreBindingWithSize, pprCoreBindingsWithSize,
        pprCoreBinder, pprCoreBinders,
        pprRules, pprOptCo
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Stats (exprStats)
import GHC.Types.Literal( pprLiteral )
import GHC.Types.Name( pprInfixName, pprPrefixName )
import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Ppr
import GHC.Core.Coercion
import GHC.Types.Basic
import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Types.SrcLoc ( pprUserRealSpan )
import GHC.Types.Tickish

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

instance OutputableBndr b => Outputable (Alt b) where
    ppr expr = pprCoreAlt expr

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
  = vcat [ ann expr
         , ppUnlessOption sdocSuppressTypeSignatures
             (pprBndr LetBind val_bdr)
         , pp_bind
         ]
  where
    pp_val_bdr = pprPrefixOcc val_bdr

    pp_bind = case bndrIsJoin_maybe val_bdr of
                Nothing -> pp_normal_bind
                Just ar -> pp_join_bind ar

    pp_normal_bind = hang pp_val_bdr 2 (equals <+> pprCoreExpr expr)

      -- For a join point of join arity n, we want to print j = \x1 ... xn -> e
      -- as "j x1 ... xn = e" to differentiate when a join point returns a
      -- lambda (the first rendering looks like a nullary join point returning
      -- an n-argument function).
    pp_join_bind join_arity
      | bndrs `lengthAtLeast` join_arity
      = hang (pp_val_bdr <+> sep (map (pprBndr LambdaBind) lhs_bndrs))
           2 (equals <+> pprCoreExpr rhs)
      | otherwise -- Yikes!  A join-binding with too few lambda
                  -- Lint will complain, but we don't want to crash
                  -- the pretty-printer else we can't see what's wrong
                  -- So refer to printing  j = e
      = pp_normal_bind
      where
        (bndrs, body) = collectBinders expr
        lhs_bndrs = take join_arity bndrs
        rhs       = mkLams (drop join_arity bndrs) body

pprParendExpr expr = ppr_expr parens expr
pprCoreExpr   expr = ppr_expr noParens expr

noParens :: SDoc -> SDoc
noParens pp = pp

pprOptCo :: Coercion -> SDoc
-- Print a coercion optionally; i.e. honouring -dsuppress-coercions
pprOptCo co = sdocOption sdocSuppressCoercions $ \case
              True  -> angleBrackets (text "Co:" <> int (coercionSize co))
              False -> parens $ sep [ppr co, dcolon <+> ppr (coercionType co)]

ppr_id_occ :: (SDoc -> SDoc) -> Id -> SDoc
ppr_id_occ add_par id
  | isJoinId id = add_par ((text "jump") <+> pp_id)
  | otherwise   = pp_id
  where
    pp_id = ppr id  -- We could use pprPrefixOcc to print (+) etc, but this is
                    -- Core where we don't print things infix anyway, so doing
                    -- so just adds extra redundant parens

ppr_expr :: OutputableBndr b => (SDoc -> SDoc) -> Expr b -> SDoc
        -- The function adds parens in context that need
        -- an atomic value (e.g. function args)

ppr_expr add_par (Var id)      = ppr_id_occ add_par id
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
  = sdocOption sdocSuppressTypeApplications $ \supp_ty_app ->
    case collectArgs expr of { (fun, args) ->
    let
        pp_args     = sep (map pprArg args)
        val_args    = dropWhile isTypeArg args   -- Drop the type arguments for tuples
        pp_tup_args = pprWithCommas pprCoreExpr val_args
        args'
          | supp_ty_app = val_args
          | otherwise   = args
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

                   _ -> parens (hang fun_doc 2 pp_args)
                   where
                     fun_doc = ppr_id_occ noParens f

        _ -> parens (hang (pprParendExpr fun) 2 pp_args)
    }

ppr_expr add_par (Case expr var ty [Alt con args rhs])
  = sdocOption sdocPrintCaseAsLet $ \case
      True -> add_par $  -- See Note [Print case as let]
               sep [ sep [ text "let! {"
                           <+> ppr_case_pat con args
                           <+> text "~"
                           <+> ppr_bndr var
                         , text "<-" <+> ppr_expr id expr
                           <+> text "} in" ]
                   , pprCoreExpr rhs
                   ]
      False -> add_par $
                sep [sep [sep [ text "case" <+> pprCoreExpr expr
                              , whenPprDebug (text "return" <+> ppr ty)
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
                <+> whenPprDebug (text "return" <+> ppr ty),
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
    sep [hang (keyword bind <+> char '{') 2 (ppr_bind noAnn bind <+> text "} in"),
         pprCoreExpr expr]
  where
    keyword (NonRec b _)
     | isJust (bndrIsJoin_maybe b) = text "join"
     | otherwise                   = text "let"
    keyword (Rec pairs)
     | ((b,_):_) <- pairs
     , isJust (bndrIsJoin_maybe b) = text "joinrec"
     | otherwise                   = text "letrec"

ppr_expr add_par (Tick tickish expr)
  = sdocOption sdocSuppressTicks $ \case
      True  -> ppr_expr add_par expr
      False -> add_par (sep [ppr tickish, pprCoreExpr expr])

pprCoreAlt :: OutputableBndr a => Alt a -> SDoc
pprCoreAlt (Alt con args rhs)
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
 = ppUnlessOption sdocSuppressTypeApplications
      (text "@" <> pprParendType ty)
pprArg (Coercion co) = text "@~" <> pprOptCo co
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

-- These instances are sadly orphans

instance OutputableBndr Var where
  pprBndr = pprCoreBinder
  pprInfixOcc  = pprInfixName  . varName
  pprPrefixOcc = pprPrefixName . varName
  bndrIsJoin_maybe = isJoinId_maybe

instance Outputable b => OutputableBndr (TaggedBndr b) where
  pprBndr _    b = ppr b   -- Simple
  pprInfixOcc  b = ppr b
  pprPrefixOcc b = ppr b
  bndrIsJoin_maybe (TB b _) = isJoinId_maybe b

pprCoreBinder :: BindingSite -> Var -> SDoc
pprCoreBinder LetBind binder
  | isTyVar binder = pprKindedTyVarBndr binder
  | otherwise      = pprTypedLetBinder binder $$
                     ppIdInfo binder (idInfo binder)

-- Lambda bound type variables are preceded by "@"
pprCoreBinder bind_site bndr
  = getPprDebug $ \debug ->
    pprTypedLamBinder bind_site debug bndr

pprCoreBinders :: [Var] -> SDoc
-- Print as lambda-binders, i.e. with their type
pprCoreBinders vs = sep (map (pprCoreBinder LambdaBind) vs)

pprUntypedBinder :: Var -> SDoc
pprUntypedBinder binder
  | isTyVar binder = text "@" <> ppr binder    -- NB: don't print kind
  | otherwise      = pprIdBndr binder

pprTypedLamBinder :: BindingSite -> Bool -> Var -> SDoc
-- For lambda and case binders, show the unfolding info (usually none)
pprTypedLamBinder bind_site debug_on var
  = sdocOption sdocSuppressTypeSignatures $ \suppress_sigs ->
    case () of
    _
      | not debug_on            -- Show case-bound wild binders only if debug is on
      , CaseBind <- bind_site
      , isDeadBinder var        -> empty

      | not debug_on            -- Even dead binders can be one-shot
      , isDeadBinder var        -> char '_' <+> ppWhen (isId var)
                                                (pprIdBndrInfo (idInfo var))

      | not debug_on            -- No parens, no kind info
      , CaseBind <- bind_site   -> pprUntypedBinder var

      | not debug_on
      , CasePatBind <- bind_site    -> pprUntypedBinder var

      | suppress_sigs -> pprUntypedBinder var

      | isTyVar var  -> parens (pprKindedTyVarBndr var)

      | otherwise    -> parens (hang (pprIdBndr var)
                                   2 (vcat [ dcolon <+> pprType (idType var)
                                           , pp_unf]))
  where
    unf_info = unfoldingInfo (idInfo var)
    pp_unf | hasSomeUnfolding unf_info = text "Unf=" <> ppr unf_info
           | otherwise                 = empty

pprTypedLetBinder :: Var -> SDoc
-- Print binder with a type or kind signature (not paren'd)
pprTypedLetBinder binder
  = sdocOption sdocSuppressTypeSignatures $ \suppress_sigs ->
    case () of
    _
      | isTyVar binder -> pprKindedTyVarBndr binder
      | suppress_sigs  -> pprIdBndr binder
      | otherwise      -> hang (pprIdBndr binder) 2 (dcolon <+> pprType (idType binder))

pprKindedTyVarBndr :: TyVar -> SDoc
-- Print a type variable binder with its kind (but not if *)
pprKindedTyVarBndr tyvar
  = text "@" <> pprTyVar tyvar

-- pprIdBndr does *not* print the type
-- When printing any Id binder in debug mode, we print its inline pragma and one-shot-ness
pprIdBndr :: Id -> SDoc
pprIdBndr id = pprPrefixOcc id <+> pprIdBndrInfo (idInfo id)

pprIdBndrInfo :: IdInfo -> SDoc
pprIdBndrInfo info
  = ppUnlessOption sdocSuppressIdInfo
      (info `seq` doc) -- The seq is useful for poking on black holes
  where
    prag_info = inlinePragInfo info
    occ_info  = occInfo info
    dmd_info  = demandInfo info
    lbv_info  = oneShotInfo info

    has_prag  = not (isDefaultInlinePragma prag_info)
    has_occ   = not (isNoOccInfo occ_info)
    has_dmd   = not $ isTopDmd dmd_info
    has_lbv   = not (hasNoOneShotInfo lbv_info)

    doc = showAttributes
          [ (has_prag, text "InlPrag=" <> pprInlineDebug prag_info)
          , (has_occ,  text "Occ=" <> ppr occ_info)
          , (has_dmd,  text "Dmd=" <> ppr dmd_info)
          , (has_lbv , text "OS=" <> ppr lbv_info)
          ]

instance Outputable IdInfo where
  ppr info = showAttributes
    [ (has_prag,         text "InlPrag=" <> pprInlineDebug prag_info)
    , (has_occ,          text "Occ=" <> ppr occ_info)
    , (has_dmd,          text "Dmd=" <> ppr dmd_info)
    , (has_lbv ,         text "OS=" <> ppr lbv_info)
    , (has_arity,        text "Arity=" <> int arity)
    , (has_called_arity, text "CallArity=" <> int called_arity)
    , (has_caf_info,     text "Caf=" <> ppr caf_info)
    , (has_str_info,     text "Str=" <> pprStrictness str_info)
    , (has_unf,          text "Unf=" <> ppr unf_info)
    , (has_rules,        text "RULES:" <+> vcat (map pprRule rules))
    ]
    where
      prag_info = inlinePragInfo info
      has_prag  = not (isDefaultInlinePragma prag_info)

      occ_info  = occInfo info
      has_occ   = not (isManyOccs occ_info)

      dmd_info  = demandInfo info
      has_dmd   = not $ isTopDmd dmd_info

      lbv_info  = oneShotInfo info
      has_lbv   = not (hasNoOneShotInfo lbv_info)

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
      has_rules = not (null rules)

{-
-----------------------------------------------------
--      IdDetails and IdInfo
-----------------------------------------------------
-}

ppIdInfo :: Id -> IdInfo -> SDoc
ppIdInfo id info
  = ppUnlessOption sdocSuppressIdInfo $
    showAttributes
    [ (True, pp_scope <> ppr (idDetails id))
    , (has_arity,        text "Arity=" <> int arity)
    , (has_called_arity, text "CallArity=" <> int called_arity)
    , (has_caf_info,     text "Caf=" <> ppr caf_info)
    , (has_str_info,     text "Str=" <> pprStrictness str_info)
    , (has_cpr_info,     text "Cpr=" <> ppr cpr_info)
    , (has_unf,          text "Unf=" <> ppr unf_info)
    , (not (null rules), text "RULES:" <+> vcat (map pprRule rules))
    ]   -- Inline pragma, occ, demand, one-shot info
        -- printed out with all binders (when debug is on);
        -- see GHC.Core.Ppr.pprIdBndr
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

    cpr_info = cprInfo info
    has_cpr_info = cpr_info /= topCprSig

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
  ppr BootUnfolding              = text "No unfolding (from boot)"
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
      pp_tmpl = ppUnlessOption sdocSuppressUnfoldings
                  (text "Tmpl=" <+> ppr rhs)
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
       4 (sep [text "forall" <+> pprCoreBinders tpl_vars <> dot,
               nest 2 (ppr fn <+> sep (map pprArg tpl_args)),
               nest 2 (text "=" <+> pprCoreExpr rhs)
            ])

{-
-----------------------------------------------------
--      Tickish
-----------------------------------------------------
-}

instance Outputable (XTickishId pass) => Outputable (GenTickish pass) where
  ppr (HpcTick modl ix) =
      hcat [text "hpc<",
            ppr modl, comma,
            ppr ix,
            text ">"]
  ppr (Breakpoint _ext ix vars) =
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
