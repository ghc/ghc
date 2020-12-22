{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring expressions.
-}

module GHC.HsToCore.Expr
   ( dsExpr, dsLExpr, dsLExprNoLP, dsLocalBinds
   , dsValBinds, dsLit, dsSyntaxExpr
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.HsToCore.Match
import GHC.HsToCore.Match.Literal
import GHC.HsToCore.Binds
import GHC.HsToCore.GuardedRHSs
import GHC.HsToCore.ListComp
import GHC.HsToCore.Utils
import GHC.HsToCore.Arrows
import GHC.HsToCore.Monad
import GHC.HsToCore.Pmc ( addTyCs, pmcGRHSs )
import GHC.Types.SourceText
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Core.FamInstEnv( topNormaliseType )
import GHC.HsToCore.Quote
import GHC.Hs

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Monad
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.Coercion( Coercion )
import GHC.Core
import GHC.Core.Utils
import GHC.Core.Make

import GHC.Driver.Session
import GHC.Types.CostCentre
import GHC.Types.Id
import GHC.Types.Id.Make
import GHC.Types.Var.Env
import GHC.Unit.Module
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.TyCo.Ppr( pprWithTYPE )
import GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Types.Basic
import GHC.Data.Maybe
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Data.Bag
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Core.PatSyn
import Control.Monad

import qualified GHC.LanguageExtensions as LangExt

{-
************************************************************************
*                                                                      *
                dsLocalBinds, dsValBinds
*                                                                      *
************************************************************************
-}

dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
dsLocalBinds (L _   (EmptyLocalBinds _))  body = return body
dsLocalBinds (L loc (HsValBinds _ binds)) body = putSrcSpanDs loc $
                                                 dsValBinds binds body
dsLocalBinds (L _ (HsIPBinds _ binds))    body = dsIPBinds  binds body

-------------------------
-- caller sets location
dsValBinds :: HsValBinds GhcTc -> CoreExpr -> DsM CoreExpr
dsValBinds (XValBindsLR (NValBinds binds _)) body
  = foldrM ds_val_bind body binds
dsValBinds (ValBinds {})       _    = panic "dsValBinds ValBindsIn"

-------------------------
dsIPBinds :: HsIPBinds GhcTc -> CoreExpr -> DsM CoreExpr
dsIPBinds (IPBinds ev_binds ip_binds) body
  = do  { ds_binds <- dsTcEvBinds ev_binds
        ; let inner = mkCoreLets ds_binds body
                -- The dict bindings may not be in
                -- dependency order; hence Rec
        ; foldrM ds_ip_bind inner ip_binds }
  where
    ds_ip_bind :: LIPBind GhcTc -> CoreExpr -> DsM CoreExpr
    ds_ip_bind (L _ (IPBind _ ~(Right n) e)) body
      = do e' <- dsLExpr e
           return (Let (NonRec n e') body)

-------------------------
-- caller sets location
ds_val_bind :: (RecFlag, LHsBinds GhcTc) -> CoreExpr -> DsM CoreExpr
-- Special case for bindings which bind unlifted variables
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE and SPECIALISE pragmas...
ds_val_bind (NonRecursive, hsbinds) body
  | [L loc bind] <- bagToList hsbinds
        -- Non-recursive, non-overloaded bindings only come in ones
        -- ToDo: in some bizarre case it's conceivable that there
        --       could be dict binds in the 'binds'.  (See the notes
        --       below.  Then pattern-match would fail.  Urk.)
  , isUnliftedHsBind bind
  = putSrcSpanDs loc $
     -- see Note [Strict binds checks] in GHC.HsToCore.Binds
    if is_polymorphic bind
    then errDsCoreExpr (poly_bind_err bind)
            -- data Ptr a = Ptr Addr#
            -- f x = let p@(Ptr y) = ... in ...
            -- Here the binding for 'p' is polymorphic, but does
            -- not mix with an unlifted binding for 'y'.  You should
            -- use a bang pattern.  #6078.

    else do { when (looksLazyPatBind bind) $
              warnIfSetDs Opt_WarnUnbangedStrictPatterns (unlifted_must_be_bang bind)
        -- Complain about a binding that looks lazy
        --    e.g.    let I# y = x in ...
        -- Remember, in checkStrictBinds we are going to do strict
        -- matching, so (for software engineering reasons) we insist
        -- that the strictness is manifest on each binding
        -- However, lone (unboxed) variables are ok


            ; dsUnliftedBind bind body }
  where
    is_polymorphic (AbsBinds { abs_tvs = tvs, abs_ev_vars = evs })
                     = not (null tvs && null evs)
    is_polymorphic _ = False

    unlifted_must_be_bang bind
      = hang (text "Pattern bindings containing unlifted types should use" $$
              text "an outermost bang pattern:")
           2 (ppr bind)

    poly_bind_err bind
      = hang (text "You can't mix polymorphic and unlifted bindings:")
           2 (ppr bind) $$
        text "Probable fix: add a type signature"

ds_val_bind (is_rec, binds) _body
  | anyBag (isUnliftedHsBind . unLoc) binds  -- see Note [Strict binds checks] in GHC.HsToCore.Binds
  = ASSERT( isRec is_rec )
    errDsCoreExpr $
    hang (text "Recursive bindings for unlifted types aren't allowed:")
       2 (vcat (map ppr (bagToList binds)))

-- Ordinary case for bindings; none should be unlifted
ds_val_bind (is_rec, binds) body
  = do  { MASSERT( isRec is_rec || isSingletonBag binds )
               -- we should never produce a non-recursive list of multiple binds

        ; (force_vars,prs) <- dsLHsBinds binds
        ; let body' = foldr seqVar body force_vars
        ; ASSERT2( not (any (isUnliftedType . idType . fst) prs), ppr is_rec $$ ppr binds )
          case prs of
            [] -> return body
            _  -> return (Let (Rec prs) body') }
        -- Use a Rec regardless of is_rec.
        -- Why? Because it allows the binds to be all
        -- mixed up, which is what happens in one rare case
        -- Namely, for an AbsBind with no tyvars and no dicts,
        --         but which does have dictionary bindings.
        -- See notes with GHC.Tc.Solver.inferLoop [NO TYVARS]
        -- It turned out that wrapping a Rec here was the easiest solution
        --
        -- NB The previous case dealt with unlifted bindings, so we
        --    only have to deal with lifted ones now; so Rec is ok

------------------
dsUnliftedBind :: HsBind GhcTc -> CoreExpr -> DsM CoreExpr
dsUnliftedBind (AbsBinds { abs_tvs = [], abs_ev_vars = []
               , abs_exports = exports
               , abs_ev_binds = ev_binds
               , abs_binds = lbinds }) body
  = do { let body1 = foldr bind_export body exports
             bind_export export b = bindNonRec (abe_poly export) (Var (abe_mono export)) b
       ; body2 <- foldlM (\body lbind -> dsUnliftedBind (unLoc lbind) body)
                            body1 lbinds
       ; ds_binds <- dsTcEvBinds_s ev_binds
       ; return (mkCoreLets ds_binds body2) }

dsUnliftedBind (FunBind { fun_id = L l fun
                        , fun_matches = matches
                        , fun_ext = co_fn
                        , fun_tick = tick }) body
               -- Can't be a bang pattern (that looks like a PatBind)
               -- so must be simply unboxed
  = do { (args, rhs) <- matchWrapper (mkPrefixFunRhs (L l $ idName fun))
                                     Nothing matches
       ; MASSERT( null args ) -- Functions aren't lifted
       ; MASSERT( isIdHsWrapper co_fn )
       ; let rhs' = mkOptTickBox tick rhs
       ; return (bindNonRec fun rhs' body) }

dsUnliftedBind (PatBind {pat_lhs = pat, pat_rhs = grhss
                        , pat_ext = ty }) body
  =     -- let C x# y# = rhs in body
        -- ==> case rhs of C x# y# -> body
    do { match_nablas <- pmcGRHSs PatBindGuards grhss
       ; rhs          <- dsGuarded grhss ty match_nablas
       ; let upat = unLoc pat
             eqn = EqnInfo { eqn_pats = [upat],
                             eqn_orig = FromSource,
                             eqn_rhs = cantFailMatchResult body }
       ; var    <- selectMatchVar Many upat
                    -- `var` will end up in a let binder, so the multiplicity
                    -- doesn't matter.
       ; result <- matchEquations PatBindRhs [var] [eqn] (exprType body)
       ; return (bindNonRec var rhs result) }

dsUnliftedBind bind body = pprPanic "dsLet: unlifted" (ppr bind $$ ppr body)

{-
************************************************************************
*                                                                      *
*              Variables, constructors, literals                       *
*                                                                      *
************************************************************************
-}


-- | Replace the body of the fucntion with this block to test the hsExprType
-- function in GHC.Tc.Utils.Zonk:
-- putSrcSpanDs loc $ do
--   { core_expr <- dsExpr e
--   ; MASSERT2( exprType core_expr `eqType` hsExprType e
--             , ppr e <+> dcolon <+> ppr (hsExprType e) $$
--                 ppr core_expr <+> dcolon <+> ppr (exprType core_expr) )
--   ; return core_expr }
dsLExpr :: LHsExpr GhcTc -> DsM CoreExpr
dsLExpr (L loc e) =
  putSrcSpanDs loc $ dsExpr e

-- | Variant of 'dsLExpr' that ensures that the result is not levity
-- polymorphic. This should be used when the resulting expression will
-- be an argument to some other function.
-- See Note [Levity polymorphism checking] in "GHC.HsToCore.Monad"
-- See Note [Levity polymorphism invariants] in "GHC.Core"
dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsLExprNoLP (L loc e)
  = putSrcSpanDs loc $
    do { e' <- dsExpr e
       ; dsNoLevPolyExpr e' (text "In the type of expression:" <+> ppr e)
       ; return e' }

dsExpr :: HsExpr GhcTc -> DsM CoreExpr
dsExpr (HsVar    _ (L _ id))           = dsHsVar id
dsExpr (HsRecFld _ (Unambiguous id _)) = dsHsVar id
dsExpr (HsRecFld _ (Ambiguous   id _)) = dsHsVar id
dsExpr (HsUnboundVar id _)             = dsHsVar id

dsExpr (HsPar _ e)            = dsLExpr e
dsExpr (ExprWithTySig _ e _)  = dsLExpr e

dsExpr (HsConLikeOut _ con)   = dsConLike con
dsExpr (HsIPVar {})           = panic "dsExpr: HsIPVar"
dsExpr (HsOverLabel{})        = panic "dsExpr: HsOverLabel"

dsExpr (HsLit _ lit)
  = do { warnAboutOverflowedLit lit
       ; dsLit (convertLit lit) }

dsExpr (HsOverLit _ lit)
  = do { warnAboutOverflowedOverLit lit
       ; dsOverLit lit }
dsExpr (XExpr (ExpansionExpr (HsExpanded _ b))) = dsExpr b
dsExpr hswrap@(XExpr (WrapExpr (HsWrap co_fn e)))
  = do { e' <- case e of
                 HsVar _ (L _ var) -> return $ varToCoreExpr var
                 HsConLikeOut _ (RealDataCon dc) -> return $ varToCoreExpr (dataConWrapId dc)
                 XExpr (WrapExpr (HsWrap _ _)) -> pprPanic "dsExpr: HsWrap inside HsWrap" (ppr hswrap)
                 HsPar _ _ -> pprPanic "dsExpr: HsPar inside HsWrap" (ppr hswrap)
                 _ -> addTyCs FromSource (hsWrapDictBinders co_fn) $
                      dsExpr e
               -- See Note [Detecting forced eta expansion]
       ; wrap' <- dsHsWrapper co_fn
       ; dflags <- getDynFlags
       ; let wrapped_e = wrap' e'
             wrapped_ty = exprType wrapped_e
       ; checkForcedEtaExpansion e (ppr hswrap) wrapped_ty -- See Note [Detecting forced eta expansion]
         -- Pass HsWrap, so that the user can see entire expression with -fprint-typechecker-elaboration
       ; warnAboutIdentities dflags e' wrapped_ty
       ; return wrapped_e }

dsExpr (NegApp _ (L loc
                    (HsOverLit _ lit@(OverLit { ol_val = HsIntegral i})))
                neg_expr)
  = do { expr' <- putSrcSpanDs loc $ do
          { warnAboutOverflowedOverLit
              (lit { ol_val = HsIntegral (negateIntegralLit i) })
          ; dsOverLit lit }
       ; dsSyntaxExpr neg_expr [expr'] }

dsExpr (NegApp _ expr neg_expr)
  = do { expr' <- dsLExpr expr
       ; dsSyntaxExpr neg_expr [expr'] }

dsExpr (HsLam _ a_Match)
  = uncurry mkLams <$> matchWrapper LambdaExpr Nothing a_Match

dsExpr (HsLamCase _ matches)
  = do { ([discrim_var], matching_code) <- matchWrapper CaseAlt Nothing matches
       ; return $ Lam discrim_var matching_code }

dsExpr e@(HsApp _ fun arg)
  = do { fun' <- dsLExpr fun
       ; dsWhenNoErrs (dsLExprNoLP arg)
                      (\arg' -> mkCoreAppDs (text "HsApp" <+> ppr e) fun' arg') }

dsExpr (HsAppType ty e _)
  = do { e' <- dsLExpr e
       ; return (App e' (Type ty)) }

{-
Note [Desugaring vars]
~~~~~~~~~~~~~~~~~~~~~~
In one situation we can get a *coercion* variable in a HsVar, namely
the support method for an equality superclass:
   class (a~b) => C a b where ...
   instance (blah) => C (T a) (T b) where ..
Then we get
   $dfCT :: forall ab. blah => C (T a) (T b)
   $dfCT ab blah = MkC ($c$p1C a blah) ($cop a blah)

   $c$p1C :: forall ab. blah => (T a ~ T b)
   $c$p1C ab blah = let ...; g :: T a ~ T b = ... } in g

That 'g' in the 'in' part is an evidence variable, and when
converting to core it must become a CO.


Note [Desugaring operator sections]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Desugaring left sections with -XPostfixOperators is straightforward: convert
(expr `op`) to (op expr).

Without -XPostfixOperators it's a bit more tricky. At first it looks as if we
can convert

    (expr `op`)

naively to

    \x -> op expr x

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider

    map (expr `op`) xs

for example. If expr were a redex then eta-expanding naively would
result in multiple evaluations where the user might only have expected one.

So we convert instead to

    let y = expr in \x -> op y x

Also, note that we must do this for both right and (perhaps surprisingly) left
sections. Why are left sections necessary? Consider the program (found in #18151),

    seq (True `undefined`) ()

according to the Haskell Report this should reduce to () (as it specifies
desugaring via eta expansion). However, if we fail to eta expand we will rather
bottom. Consequently, we must eta expand even in the case of a left section.

If `expr` is actually just a variable, say, then the simplifier
will inline `y`, eliminating the redundant `let`.

Note that this works even in the case that `expr` is unlifted. In this case
bindNonRec will automatically do the right thing, giving us:

    case expr of y -> (\x -> op y x)

See #18151.
-}

dsExpr e@(OpApp _ e1 op e2)
  = -- for the type of y, we need the type of op's 2nd argument
    do { op' <- dsLExpr op
       ; dsWhenNoErrs (mapM dsLExprNoLP [e1, e2])
                      (\exprs' -> mkCoreAppsDs (text "opapp" <+> ppr e) op' exprs') }

-- dsExpr (SectionL op expr)  ===  (expr `op`)  ~>  \y -> op expr y
--
-- See Note [Desugaring operator sections].
-- N.B. this also must handle postfix operator sections due to -XPostfixOperators.
dsExpr e@(SectionL _ expr op) = do
  postfix_operators <- xoptM LangExt.PostfixOperators
  if postfix_operators then
    -- Desugar (e !) to ((!) e)
    do { op' <- dsLExpr op
       ; dsWhenNoErrs (dsLExprNoLP expr) $ \expr' ->
         mkCoreAppDs (text "sectionl" <+> ppr expr) op' expr' }
  else do
    core_op <- dsLExpr op
    x_core <- dsLExpr expr
    case splitFunTys (exprType core_op) of
      -- Binary operator section
      (x_ty:y_ty:_, _) ->
        dsWhenNoErrs
          (newSysLocalsDsNoLP [x_ty, y_ty])
          (\[x_id, y_id] ->
            bindNonRec x_id x_core
            $ Lam y_id (mkCoreAppsDs (text "sectionl" <+> ppr e)
                                     core_op [Var x_id, Var y_id]))

      -- Postfix operator section
      (_:_, _) ->
        return $ mkCoreAppDs (text "sectionl" <+> ppr e) core_op x_core

      _ -> pprPanic "dsExpr(SectionL)" (ppr e)

-- dsExpr (SectionR op expr)  === (`op` expr)  ~>  \x -> op x expr
--
-- See Note [Desugaring operator sections].
dsExpr e@(SectionR _ op expr) = do
    core_op <- dsLExpr op
    let (x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
    y_core <- dsLExpr expr
    dsWhenNoErrs (newSysLocalsDsNoLP [x_ty, y_ty])
                 (\[x_id, y_id] -> bindNonRec y_id y_core $
                                   Lam x_id (mkCoreAppsDs (text "sectionr" <+> ppr e)
                                                          core_op [Var x_id, Var y_id]))

dsExpr (ExplicitTuple _ tup_args boxity)
  = do { let go (lam_vars, args) (L _ (Missing (Scaled mult ty)))
                    -- For every missing expression, we need
                    -- another lambda in the desugaring.
               = do { lam_var <- newSysLocalDsNoLP mult ty
                    ; return (lam_var : lam_vars, Var lam_var : args) }
             go (lam_vars, args) (L _ (Present _ expr))
                    -- Expressions that are present don't generate
                    -- lambdas, just arguments.
               = do { core_expr <- dsLExprNoLP expr
                    ; return (lam_vars, core_expr : args) }

       ; dsWhenNoErrs (foldM go ([], []) (reverse tup_args))
                -- The reverse is because foldM goes left-to-right
                      (\(lam_vars, args) ->
                        mkCoreLams lam_vars $
                          mkCoreTupBoxity boxity args) }
                        -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make

dsExpr (ExplicitSum types alt arity expr)
  = dsWhenNoErrs (dsLExprNoLP expr)
                 (\core_expr -> mkCoreConApps (sumDataCon alt arity)
                                (map (Type . getRuntimeRep) types ++
                                 map Type types ++
                                 [core_expr]) )

dsExpr (HsPragE _ prag expr) =
  ds_prag_expr prag expr

dsExpr (HsCase _ discrim matches)
  = do { core_discrim <- dsLExpr discrim
       ; ([discrim_var], matching_code) <- matchWrapper CaseAlt (Just discrim) matches
       ; return (bindNonRec discrim_var core_discrim matching_code) }

-- Pepe: The binds are in scope in the body but NOT in the binding group
--       This is to avoid silliness in breakpoints
dsExpr (HsLet _ binds body) = do
    body' <- dsLExpr body
    dsLocalBinds binds body'

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
dsExpr (HsDo res_ty ListComp (L _ stmts)) = dsListComp stmts res_ty
dsExpr (HsDo _ ctx@DoExpr{}      (L _ stmts)) = dsDo ctx stmts
dsExpr (HsDo _ ctx@GhciStmtCtxt  (L _ stmts)) = dsDo ctx stmts
dsExpr (HsDo _ ctx@MDoExpr{}     (L _ stmts)) = dsDo ctx stmts
dsExpr (HsDo _ MonadComp     (L _ stmts)) = dsMonadComp stmts

dsExpr (HsIf _ guard_expr then_expr else_expr)
  = do { pred <- dsLExpr guard_expr
       ; b1 <- dsLExpr then_expr
       ; b2 <- dsLExpr else_expr
       ; return $ mkIfThenElse pred b1 b2 }

dsExpr (HsMultiIf res_ty alts)
  | null alts
  = mkErrorExpr

  | otherwise
  = do { let grhss = GRHSs noExtField alts (noLoc emptyLocalBinds)
       ; rhss_nablas  <- pmcGRHSs IfAlt grhss
       ; match_result <- dsGRHSs IfAlt grhss res_ty rhss_nablas
       ; error_expr   <- mkErrorExpr
       ; extractMatchResult match_result error_expr }
  where
    mkErrorExpr = mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID res_ty
                               (text "multi-way if")

{-
\noindent
\underline{\bf Various data construction things}
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

dsExpr (ExplicitList elt_ty wit xs)
  = dsExplicitList elt_ty wit xs

dsExpr (ArithSeq expr witness seq)
  = case witness of
     Nothing -> dsArithSeq expr seq
     Just fl -> do { newArithSeq <- dsArithSeq expr seq
                   ; dsSyntaxExpr fl [newArithSeq] }

{-
Static Pointers
~~~~~~~~~~~~~~~

See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable for an overview.

    g = ... static f ...
==>
    g = ... makeStatic loc f ...
-}

dsExpr (HsStatic _ expr@(L loc _)) = do
    expr_ds <- dsLExprNoLP expr
    let ty = exprType expr_ds
    makeStaticId <- dsLookupGlobalId makeStaticName

    dflags <- getDynFlags
    let platform = targetPlatform dflags
    let (line, col) = case loc of
           RealSrcSpan r _ ->
                            ( srcLocLine $ realSrcSpanStart r
                            , srcLocCol  $ realSrcSpanStart r
                            )
           _             -> (0, 0)
        srcLoc = mkCoreConApps (tupleDataCon Boxed 2)
                     [ Type intTy              , Type intTy
                     , mkIntExprInt platform line, mkIntExprInt platform col
                     ]

    putSrcSpanDs loc $ return $
      mkCoreApps (Var makeStaticId) [ Type ty, srcLoc, expr_ds ]

{-
\noindent
\underline{\bf Record construction and update}
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
        T { op2 = e }
==>
        let err = /\a -> recConErr a
        T (recConErr t1 "M.hs/230/op1")
          e
          (recConErr t1 "M.hs/230/op3")
\end{verbatim}
@recConErr@ then converts its argument string into a proper message
before printing it as
\begin{verbatim}
        M.hs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.
-}

dsExpr (RecordCon { rcon_con  = L _ con_like
                  , rcon_flds = rbinds
                  , rcon_ext  = con_expr })
  = do { con_expr' <- dsExpr con_expr
       ; let
             (arg_tys, _) = tcSplitFunTys (exprType con_expr')
             -- A newtype in the corner should be opaque;
             -- hence TcType.tcSplitFunTys

             mk_arg (arg_ty, fl)
               = case findField (rec_flds rbinds) (flSelector fl) of
                   (rhs:rhss) -> ASSERT( null rhss )
                                 dsLExprNoLP rhs
                   []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (ppr (flLabel fl))
             unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty Outputable.empty

             labels = conLikeFieldLabels con_like

       ; con_args <- if null labels
                     then mapM unlabelled_bottom (map scaledThing arg_tys)
                     else mapM mk_arg (zipEqual "dsExpr:RecordCon" (map scaledThing arg_tys) labels)

       ; return (mkCoreApps con_expr' con_args) }

{-
Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
        data T = T1 {op1, op2, op3 :: Int}
               | T2 {op4, op2 :: Int}
               | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
        r { op2 = e }
===>
        let op2 = e in
        case r of
          T1 op1 _ op3 -> T1 op1 op2 op3
          T2 op4 _     -> T2 op4 op2
          other        -> recUpdError "M.hs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

Note [Update for GADTs]
~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T a b where
     MkT :: { foo :: a } -> T a Int

   upd :: T s t -> s -> T s t
   upd z y = z { foo = y}

We need to get this:
   $WMkT :: a -> T a Int
   MkT   :: (b ~# Int) => a -> T a b

   upd = /\s t. \(z::T s t) (y::s) ->
         case z of
            MkT (co :: t ~# Int) _ -> $WMkT @s y |> T (Refl s) (Sym co)

Note the final cast
   T (Refl s) (Sym co) :: T s Int ~ T s t
which uses co, bound by the GADT match.  This is the wrap_co coercion
in wrapped_rhs. How do we produce it?

* Start with raw materials
    tc, the tycon:                                       T
    univ_tvs, the universally quantified tyvars of MkT:  a,b
  NB: these are in 1-1 correspondence with the tyvars of tc

* Form univ_cos, a coercion for each of tc's args: (Refl s) (Sym co)
  We replaced
     a  by  (Refl s)    since 's' instantiates 'a'
     b  by  (Sym co)   since 'b' is in the data-con's EqSpec

* Then form the coercion T (Refl s) (Sym co)

It gets more complicated when data families are involved (#18809).
Consider
    data family F x
    data instance F (a,b) where
      MkF :: { foo :: Int } -> F (Int,b)

    bar :: F (s,t) -> Int -> F (s,t)
    bar z y = z { foo = y}

We have
    data R:FPair a b where
      MkF :: { foo :: Int } -> R:FPair Int b

    $WMkF :: Int -> F (Int,b)
    MkF :: forall a b. (a ~# Int) => Int -> R:FPair a b

    bar :: F (s,t) -> Int -> F (s,t)
    bar = /\s t. \(z::F (s,t)) \(y::Int) ->
         case z |> co1 of
            MkF (co2::s ~# Int) _ -> $WMkF @t y |> co3

(Side note: here (z |> co1) is built by typechecking the scrutinee, so
we ignore it here.  In general the scrutinee is an aribtrary expression.)

The question is: what is co3, the cast for the RHS?
      co3 :: F (Int,t) ~ F (s,t)
Again, we can construct it using co2, bound by the GADT match.
We do /exactly/ the same as the non-family case up to building
univ_cos.  But that gives us
     rep_tc:   R:FPair
     univ_cos: (Sym co2)   (Refl t)
But then we use mkTcFamilyTyConAppCo to "lift" this to the coercion
we want, namely
     F (Sym co2, Refl t) :: F (Int,t) ~ F (s,t)

-}

dsExpr expr@(RecordUpd { rupd_expr = record_expr, rupd_flds = fields
                       , rupd_ext = RecordUpdTc
                           { rupd_cons = cons_to_upd
                           , rupd_in_tys = in_inst_tys
                           , rupd_out_tys = out_inst_tys
                           , rupd_wrap = dict_req_wrap }} )
  | null fields
  = dsLExpr record_expr
  | otherwise
  = ASSERT2( notNull cons_to_upd, ppr expr )

    do  { record_expr' <- dsLExpr record_expr
        ; field_binds' <- mapM ds_field fields
        ; let upd_fld_env :: NameEnv Id -- Maps field name to the LocalId of the field binding
              upd_fld_env = mkNameEnv [(f,l) | (f,l,_) <- field_binds']

        -- It's important to generate the match with matchWrapper,
        -- and the right hand sides with applications of the wrapper Id
        -- so that everything works when we are doing fancy unboxing on the
        -- constructor arguments.
        ; alts <- mapM (mk_alt upd_fld_env) cons_to_upd
        ; ([discrim_var], matching_code)
                <- matchWrapper RecUpd (Just record_expr) -- See Note [Scrutinee in Record updates]
                                      (MG { mg_alts = noLoc alts
                                          , mg_ext = MatchGroupTc [unrestricted in_ty] out_ty
                                          , mg_origin = FromSource
                                          })
                                     -- FromSource is not strictly right, but we
                                     -- want incomplete pattern-match warnings

        ; return (add_field_binds field_binds' $
                  bindNonRec discrim_var record_expr' matching_code) }
  where
    ds_field :: LHsRecUpdField GhcTc -> DsM (Name, Id, CoreExpr)
      -- Clone the Id in the HsRecField, because its Name is that
      -- of the record selector, and we must not make that a local binder
      -- else we shadow other uses of the record selector
      -- Hence 'lcl_id'.  Cf #2735
    ds_field (L _ rec_field)
      = do { rhs <- dsLExpr (hsRecFieldArg rec_field)
           ; let fld_id = unLoc (hsRecUpdFieldId rec_field)
           ; lcl_id <- newSysLocalDs (idMult fld_id) (idType fld_id)
           ; return (idName fld_id, lcl_id, rhs) }

    add_field_binds [] expr = expr
    add_field_binds ((_,b,r):bs) expr = bindNonRec b r (add_field_binds bs expr)

        -- Awkwardly, for families, the match goes
        -- from instance type to family type
    (in_ty, out_ty) =
      case (head cons_to_upd) of
        RealDataCon data_con ->
          let tycon = dataConTyCon data_con in
          (mkTyConApp tycon in_inst_tys, mkFamilyTyConApp tycon out_inst_tys)
        PatSynCon pat_syn ->
          ( patSynInstResTy pat_syn in_inst_tys
          , patSynInstResTy pat_syn out_inst_tys)
    mk_alt upd_fld_env con
      = do { let (univ_tvs, ex_tvs, eq_spec,
                  prov_theta, _req_theta, arg_tys, _) = conLikeFullSig con
                 arg_tys' = map (scaleScaled Many) arg_tys
                   -- Record updates consume the source record with multiplicity
                   -- Many. Therefore all the fields need to be scaled thus.
                 user_tvs  = binderVars $ conLikeUserTyVarBinders con
                 in_subst  = zipTvSubst univ_tvs in_inst_tys
                 out_subst = zipTvSubst univ_tvs out_inst_tys

                -- I'm not bothering to clone the ex_tvs
           ; eqs_vars   <- mapM newPredVarDs (substTheta in_subst (eqSpecPreds eq_spec))
           ; theta_vars <- mapM newPredVarDs (substTheta in_subst prov_theta)
           ; arg_ids    <- newSysLocalsDs (substScaledTysUnchecked in_subst arg_tys')
           ; let field_labels = conLikeFieldLabels con
                 val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
                                         field_labels arg_ids
                 mk_val_arg fl pat_arg_id
                     = nlHsVar (lookupNameEnv upd_fld_env (flSelector fl) `orElse` pat_arg_id)

                 inst_con = noLoc $ mkHsWrap wrap (HsConLikeOut noExtField con)
                        -- Reconstruct with the WrapId so that unpacking happens
                 wrap = mkWpEvVarApps theta_vars                                <.>
                        dict_req_wrap                                           <.>
                        mkWpTyApps    [ lookupTyVar out_subst tv
                                          `orElse` mkTyVarTy tv
                                      | tv <- user_tvs ]
                          -- Be sure to use user_tvs (which may be ordered
                          -- differently than `univ_tvs ++ ex_tvs) above.
                          -- See Note [DataCon user type variable binders]
                          -- in GHC.Core.DataCon.
                 rhs = foldl' (\a b -> nlHsApp a b) inst_con val_args

                        -- Tediously wrap the application in a cast
                        -- Note [Update for GADTs]
                 wrapped_rhs =
                  case con of
                    RealDataCon data_con
                      | null eq_spec -> rhs
                      | otherwise    -> mkLHsWrap (mkWpCastN wrap_co) rhs
                                     -- This wrap is the punchline: Note [Update for GADTs]
                      where
                        rep_tc   = dataConTyCon data_con
                        wrap_co  = mkTcFamilyTyConAppCo rep_tc univ_cos
                        univ_cos = zipWithEqual "dsExpr:upd" mk_univ_co univ_tvs out_inst_tys

                        mk_univ_co :: TyVar   -- Universal tyvar from the DataCon
                                   -> Type    -- Corresponding instantiating type
                                   -> Coercion
                        mk_univ_co univ_tv inst_ty
                          = case lookupVarEnv eq_spec_env univ_tv of
                               Just co -> co
                               Nothing -> mkTcNomReflCo inst_ty

                        eq_spec_env :: VarEnv Coercion
                        eq_spec_env = mkVarEnv [ (eqSpecTyVar spec, mkTcSymCo (mkTcCoVarCo eqs_var))
                                               | (spec,eqs_var) <- zipEqual "dsExpr:upd2" eq_spec eqs_vars ]

                    -- eq_spec is always null for a PatSynCon
                    PatSynCon _ -> rhs


                 req_wrap = dict_req_wrap <.> mkWpTyApps in_inst_tys

                 pat = noLoc $ ConPat { pat_con = noLoc con
                                      , pat_args = PrefixCon [] $ map nlVarPat arg_ids
                                      , pat_con_ext = ConPatTc
                                        { cpt_tvs = ex_tvs
                                        , cpt_dicts = eqs_vars ++ theta_vars
                                        , cpt_binds = emptyTcEvBinds
                                        , cpt_arg_tys = in_inst_tys
                                        , cpt_wrap = req_wrap
                                        }
                                      }
           ; return (mkSimpleMatch RecUpd [pat] wrapped_rhs) }

{- Note [Scrutinee in Record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider #17783:

  data PartialRec = No
                  | Yes { a :: Int, b :: Bool }
  update No = No
  update r@(Yes {}) = r { b = False }

In the context of pattern-match checking, the occurrence of @r@ in
@r { b = False }@ is to be treated as if it was a scrutinee, as can be seen by
the following desugaring:

  r { b = False } ==> case r of Yes a b -> Yes a False

Thus, we pass @r@ as the scrutinee expression to @matchWrapper@ above.
-}

-- Here is where we desugar the Template Haskell brackets and escapes

-- Template Haskell stuff

dsExpr (HsRnBracketOut _ _ _)  = panic "dsExpr HsRnBracketOut"
dsExpr (HsTcBracketOut _ hs_wrapper x ps) = dsBracket hs_wrapper x ps
dsExpr (HsSpliceE _ s)         = pprPanic "dsExpr:splice" (ppr s)

-- Arrow notation extension
dsExpr (HsProc _ pat cmd) = dsProcExpr pat cmd

-- Hpc Support

dsExpr (HsTick _ tickish e) = do
  e' <- dsLExpr e
  return (Tick tickish e')

-- There is a problem here. The then and else branches
-- have no free variables, so they are open to lifting.
-- We need someway of stopping this.
-- This will make no difference to binary coverage
-- (did you go here: YES or NO), but will effect accurate
-- tick counting.

dsExpr (HsBinTick _ ixT ixF e) = do
  e2 <- dsLExpr e
  do { ASSERT(exprType e2 `eqType` boolTy)
       mkBinaryTickBox ixT ixF e2
     }

-- HsSyn constructs that just shouldn't be here:
dsExpr (HsBracket     {})  = panic "dsExpr:HsBracket"
dsExpr (HsDo          {})  = panic "dsExpr:HsDo"

ds_prag_expr :: HsPragE GhcTc -> LHsExpr GhcTc -> DsM CoreExpr
ds_prag_expr (HsPragSCC _ _ cc) expr = do
    dflags <- getDynFlags
    if sccProfilingEnabled dflags
      then do
        mod_name <- getModule
        count <- goptM Opt_ProfCountEntries
        let nm = sl_fs cc
        flavour <- ExprCC <$> getCCIndexDsM nm
        Tick (ProfNote (mkUserCC nm mod_name (getLoc expr) flavour) count True)
               <$> dsLExpr expr
      else dsLExpr expr

------------------------------
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsSyntaxExpr (SyntaxExprTc { syn_expr      = expr
                           , syn_arg_wraps = arg_wraps
                           , syn_res_wrap  = res_wrap })
             arg_exprs
  = do { fun            <- dsExpr expr
       ; core_arg_wraps <- mapM dsHsWrapper arg_wraps
       ; core_res_wrap  <- dsHsWrapper res_wrap
       ; let wrapped_args = zipWithEqual "dsSyntaxExpr" ($) core_arg_wraps arg_exprs
       ; dsWhenNoErrs (zipWithM_ dsNoLevPolyExpr wrapped_args [ mk_doc n | n <- [1..] ])
                      (\_ -> core_res_wrap (mkApps fun wrapped_args)) }
  where
    mk_doc n = text "In the" <+> speakNth n <+> text "argument of" <+> quotes (ppr expr)
dsSyntaxExpr NoSyntaxExprTc _ = panic "dsSyntaxExpr"

findField :: [LHsRecField GhcTc arg] -> Name -> [arg]
findField rbinds sel
  = [hsRecFieldArg fld | L _ fld <- rbinds
                       , sel == idName (unLoc $ hsRecFieldId fld) ]

{-
%--------------------------------------------------------------------

Note [Desugaring explicit lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Explicit lists are desugared in a cleverer way to prevent some
fruitless allocations.  Essentially, whenever we see a list literal
[x_1, ..., x_n] we generate the corresponding expression in terms of
build:

Explicit lists (literals) are desugared to allow build/foldr fusion when
beneficial. This is a bit of a trade-off,

 * build/foldr fusion can generate far larger code than the corresponding
   cons-chain (e.g. see #11707)

 * even when it doesn't produce more code, build can still fail to fuse,
   requiring that the simplifier do more work to bring the expression
   back into cons-chain form; this costs compile time

 * when it works, fusion can be a significant win. Allocations are reduced
   by up to 25% in some nofib programs. Specifically,

        Program           Size    Allocs   Runtime  CompTime
        rewrite          +0.0%    -26.3%      0.02     -1.8%
           ansi          -0.3%    -13.8%      0.00     +0.0%
           lift          +0.0%     -8.7%      0.00     -2.3%

At the moment we use a simple heuristic to determine whether build will be
fruitful: for small lists we assume the benefits of fusion will be worthwhile;
for long lists we assume that the benefits will be outweighted by the cost of
code duplication. This magic length threshold is @maxBuildLength@. Also, fusion
won't work at all if rewrite rules are disabled, so we don't use the build-based
desugaring in this case.

We used to have a more complex heuristic which would try to break the list into
"static" and "dynamic" parts and only build-desugar the dynamic part.
Unfortunately, determining "static-ness" reliably is a bit tricky and the
heuristic at times produced surprising behavior (see #11710) so it was dropped.
-}

{- | The longest list length which we will desugar using @build@.

This is essentially a magic number and its setting is unfortunate rather
arbitrary. The idea here, as mentioned in Note [Desugaring explicit lists],
is to avoid deforesting large static data into large(r) code. Ideally we'd
want a smaller threshold with larger consumers and vice-versa, but we have no
way of knowing what will be consuming our list in the desugaring impossible to
set generally correctly.

The effect of reducing this number will be that 'build' fusion is applied
less often. From a runtime performance perspective, applying 'build' more
liberally on "moderately" sized lists should rarely hurt and will often it can
only expose further optimization opportunities; if no fusion is possible it will
eventually get rule-rewritten back to a list). We do, however, pay in compile
time.
-}
maxBuildLength :: Int
maxBuildLength = 32

dsExplicitList :: Type -> Maybe (SyntaxExpr GhcTc) -> [LHsExpr GhcTc]
               -> DsM CoreExpr
-- See Note [Desugaring explicit lists]
dsExplicitList elt_ty Nothing xs
  = do { dflags <- getDynFlags
       ; xs' <- mapM dsLExprNoLP xs
       ; if xs' `lengthExceeds` maxBuildLength
                -- Don't generate builds if the list is very long.
         || null xs'
                -- Don't generate builds when the [] constructor will do
         || not (gopt Opt_EnableRewriteRules dflags)  -- Rewrite rules off
                -- Don't generate a build if there are no rules to eliminate it!
                -- See Note [Desugaring RULE left hand sides] in GHC.HsToCore
         then return $ mkListExpr elt_ty xs'
         else mkBuildExpr elt_ty (mk_build_list xs') }
  where
    mk_build_list xs' (cons, _) (nil, _)
      = return (foldr (App . App (Var cons)) (Var nil) xs')

dsExplicitList elt_ty (Just fln) xs
  = do { list <- dsExplicitList elt_ty Nothing xs
       ; dflags <- getDynFlags
       ; let platform = targetPlatform dflags
       ; dsSyntaxExpr fln [mkIntExprInt platform (length xs), list] }

dsArithSeq :: PostTcExpr -> (ArithSeqInfo GhcTc) -> DsM CoreExpr
dsArithSeq expr (From from)
  = App <$> dsExpr expr <*> dsLExprNoLP from
dsArithSeq expr (FromTo from to)
  = do fam_envs <- dsGetFamInstEnvs
       dflags <- getDynFlags
       warnAboutEmptyEnumerations fam_envs dflags from Nothing to
       expr' <- dsExpr expr
       from' <- dsLExprNoLP from
       to'   <- dsLExprNoLP to
       return $ mkApps expr' [from', to']
dsArithSeq expr (FromThen from thn)
  = mkApps <$> dsExpr expr <*> mapM dsLExprNoLP [from, thn]
dsArithSeq expr (FromThenTo from thn to)
  = do fam_envs <- dsGetFamInstEnvs
       dflags <- getDynFlags
       warnAboutEmptyEnumerations fam_envs dflags from (Just thn) to
       expr' <- dsExpr expr
       from' <- dsLExprNoLP from
       thn'  <- dsLExprNoLP thn
       to'   <- dsLExprNoLP to
       return $ mkApps expr' [from', thn', to']

{-
Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in GHC.HsToCore.ListComp).  Basically does the translation given in the
Haskell 98 report:
-}

dsDo :: HsStmtContext GhcRn -> [ExprLStmt GhcTc] -> DsM CoreExpr
dsDo ctx stmts
  = goL stmts
  where
    goL [] = panic "dsDo"
    goL ((L loc stmt):lstmts) = putSrcSpanDs loc (go loc stmt lstmts)

    go _ (LastStmt _ body _ _) stmts
      = ASSERT( null stmts ) dsLExpr body
        -- The 'return' op isn't used for 'do' expressions

    go _ (BodyStmt _ rhs then_expr _) stmts
      = do { rhs2 <- dsLExpr rhs
           ; warnDiscardedDoBindings rhs (exprType rhs2)
           ; rest <- goL stmts
           ; dsSyntaxExpr then_expr [rhs2, rest] }

    go _ (LetStmt _ binds) stmts
      = do { rest <- goL stmts
           ; dsLocalBinds binds rest }

    go _ (BindStmt xbs pat rhs) stmts
      = do  { body     <- goL stmts
            ; rhs'     <- dsLExpr rhs
            ; var   <- selectSimpleMatchVarL (xbstc_boundResultMult xbs) pat
            ; match <- matchSinglePatVar var Nothing (StmtCtxt ctx) pat
                         (xbstc_boundResultType xbs) (cantFailMatchResult body)
            ; match_code <- dsHandleMonadicFailure ctx pat match (xbstc_failOp xbs)
            ; dsSyntaxExpr (xbstc_bindOp xbs) [rhs', Lam var match_code] }

    go _ (ApplicativeStmt body_ty args mb_join) stmts
      = do {
             let
               (pats, rhss) = unzip (map (do_arg . snd) args)

               do_arg (ApplicativeArgOne fail_op pat expr _) =
                 ((pat, fail_op), dsLExpr expr)
               do_arg (ApplicativeArgMany _ stmts ret pat _) =
                 ((pat, Nothing), dsDo ctx (stmts ++ [noLoc $ mkLastStmt (noLoc ret)]))

           ; rhss' <- sequence rhss

           ; body' <- dsLExpr $ noLoc $ HsDo body_ty ctx (noLoc stmts)

           ; let match_args (pat, fail_op) (vs,body)
                   = do { var   <- selectSimpleMatchVarL Many pat
                        ; match <- matchSinglePatVar var Nothing (StmtCtxt ctx) pat
                                   body_ty (cantFailMatchResult body)
                        ; match_code <- dsHandleMonadicFailure ctx pat match fail_op
                        ; return (var:vs, match_code)
                        }

           ; (vars, body) <- foldrM match_args ([],body') pats
           ; let fun' = mkLams vars body
           ; let mk_ap_call l (op,r) = dsSyntaxExpr op [l,r]
           ; expr <- foldlM mk_ap_call fun' (zip (map fst args) rhss')
           ; case mb_join of
               Nothing -> return expr
               Just join_op -> dsSyntaxExpr join_op [expr] }

    go loc (RecStmt { recS_stmts = rec_stmts, recS_later_ids = later_ids
                    , recS_rec_ids = rec_ids, recS_ret_fn = return_op
                    , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op
                    , recS_ext = RecStmtTc
                        { recS_bind_ty = bind_ty
                        , recS_rec_rets = rec_rets
                        , recS_ret_ty = body_ty} }) stmts
      = goL (new_bind_stmt : stmts)  -- rec_ids can be empty; eg  rec { print 'x' }
      where
        new_bind_stmt = L loc $ BindStmt
          XBindStmtTc
            { xbstc_bindOp = bind_op
            , xbstc_boundResultType = bind_ty
            , xbstc_boundResultMult = Many
            , xbstc_failOp = Nothing -- Tuple cannot fail
            }
          (mkBigLHsPatTupId later_pats)
          mfix_app

        tup_ids      = rec_ids ++ filterOut (`elem` rec_ids) later_ids
        tup_ty       = mkBigCoreTupTy (map idType tup_ids) -- Deals with singleton case
        rec_tup_pats = map nlVarPat tup_ids
        later_pats   = rec_tup_pats
        rets         = map noLoc rec_rets
        mfix_app     = nlHsSyntaxApps mfix_op [mfix_arg]
        mfix_arg     = noLoc $ HsLam noExtField
                           (MG { mg_alts = noLoc [mkSimpleMatch
                                                    LambdaExpr
                                                    [mfix_pat] body]
                               , mg_ext = MatchGroupTc [unrestricted tup_ty] body_ty
                               , mg_origin = Generated })
        mfix_pat     = noLoc $ LazyPat noExtField $ mkBigLHsPatTupId rec_tup_pats
        body         = noLoc $ HsDo body_ty
                                ctx (noLoc (rec_stmts ++ [ret_stmt]))
        ret_app      = nlHsSyntaxApps return_op [mkBigLHsTupId rets]
        ret_stmt     = noLoc $ mkLastStmt ret_app
                     -- This LastStmt will be desugared with dsDo,
                     -- which ignores the return_op in the LastStmt,
                     -- so we must apply the return_op explicitly

    go _ (ParStmt   {}) _ = panic "dsDo ParStmt"
    go _ (TransStmt {}) _ = panic "dsDo TransStmt"

{-
************************************************************************
*                                                                      *
   Desugaring Variables
*                                                                      *
************************************************************************
-}

dsHsVar :: Id -> DsM CoreExpr
dsHsVar var
  | let bad_tys = badUseOfLevPolyPrimop var ty
  , not (null bad_tys)
  = do { levPolyPrimopErr (ppr var) ty bad_tys
       ; return unitExpr }  -- return something eminently safe

  | otherwise
  = return (varToCoreExpr var)   -- See Note [Desugaring vars]
  where
    ty = idType var

dsConLike :: ConLike -> DsM CoreExpr
dsConLike (RealDataCon dc) = dsHsVar (dataConWrapId dc)
dsConLike (PatSynCon ps)
  | Just (builder_name, _, add_void) <- patSynBuilder ps
  = do { builder_id <- dsLookupGlobalId builder_name
       ; return (if add_void
                 then mkCoreApp (text "dsConLike" <+> ppr ps)
                                (Var builder_id) (Var voidPrimId)
                 else Var builder_id) }
  | otherwise
  = pprPanic "dsConLike" (ppr ps)

{-
************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************
-}

-- Warn about certain types of values discarded in monadic bindings (#3263)
warnDiscardedDoBindings :: LHsExpr GhcTc -> Type -> DsM ()
warnDiscardedDoBindings rhs rhs_ty
  | Just (m_ty, elt_ty) <- tcSplitAppTy_maybe rhs_ty
  = do { warn_unused <- woptM Opt_WarnUnusedDoBind
       ; warn_wrong <- woptM Opt_WarnWrongDoBind
       ; when (warn_unused || warn_wrong) $
    do { fam_inst_envs <- dsGetFamInstEnvs
       ; let norm_elt_ty = topNormaliseType fam_inst_envs elt_ty

           -- Warn about discarding non-() things in 'monadic' binding
       ; if warn_unused && not (isUnitTy norm_elt_ty)
         then warnDs (Reason Opt_WarnUnusedDoBind)
                     (badMonadBind rhs elt_ty)
         else

           -- Warn about discarding m a things in 'monadic' binding of the same type,
           -- but only if we didn't already warn due to Opt_WarnUnusedDoBind
           when warn_wrong $
                case tcSplitAppTy_maybe norm_elt_ty of
                      Just (elt_m_ty, _)
                         | m_ty `eqType` topNormaliseType fam_inst_envs elt_m_ty
                         -> warnDs (Reason Opt_WarnWrongDoBind)
                                   (badMonadBind rhs elt_ty)
                      _ -> return () } }

  | otherwise   -- RHS does have type of form (m ty), which is weird
  = return ()   -- but at least this warning is irrelevant

badMonadBind :: LHsExpr GhcTc -> Type -> SDoc
badMonadBind rhs elt_ty
  = vcat [ hang (text "A do-notation statement discarded a result of type")
              2 (quotes (ppr elt_ty))
         , hang (text "Suppress this warning by saying")
              2 (quotes $ text "_ <-" <+> ppr rhs)
         ]

{-
************************************************************************
*                                                                      *
   Forced eta expansion and levity polymorphism
*                                                                      *
************************************************************************

Note [Detecting forced eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We cannot have levity polymorphic function arguments. See
Note [Levity polymorphism invariants] in GHC.Core. But we *can* have
functions that take levity polymorphic arguments, as long as these
functions are eta-reduced. (See #12708 for an example.)

However, we absolutely cannot do this for functions that have no
binding (i.e., say True to Id.hasNoBinding), like primops and unboxed
tuple constructors. These get eta-expanded in CorePrep.maybeSaturate.

Detecting when this is about to happen is a bit tricky, though. When
the desugarer is looking at the Id itself (let's be concrete and
suppose we have (#,#)), we don't know whether it will be levity
polymorphic. So the right spot seems to be to look after the Id has
been applied to its type arguments. To make the algorithm efficient,
it's important to be able to spot ((#,#) @a @b @c @d) without looking
past all the type arguments. We thus require that
  * The body of an HsWrap is not an HsWrap, nor an HsPar.
This invariant is checked in dsExpr.
With that representation invariant, we simply look inside every HsWrap
to see if its body is an HsVar whose Id hasNoBinding. Then, we look
at the wrapped type. If it has any levity polymorphic arguments, reject.
Because we might have an HsVar without a wrapper, we check in dsHsVar
as well. typecheck/should_fail/T17021 triggers this case.

Interestingly, this approach does not look to see whether the Id in
question will be eta expanded. The logic is this:
  * Either the Id in question is saturated or not.
  * If it is, then it surely can't have levity polymorphic arguments.
    If its wrapped type contains levity polymorphic arguments, reject.
  * If it's not, then it can't be eta expanded with levity polymorphic
    argument. If its wrapped type contains levity polymorphic arguments, reject.
So, either way, we're good to reject.

-}

-- | Takes an expression and its instantiated type. If the expression is an
-- HsVar with a hasNoBinding primop and the type has levity-polymorphic arguments,
-- issue an error. See Note [Detecting forced eta expansion]
checkForcedEtaExpansion :: HsExpr GhcTc -> SDoc -> Type -> DsM ()
checkForcedEtaExpansion expr expr_doc ty
  | Just var <- case expr of
                  HsVar _ (L _ var)               -> Just var
                  HsConLikeOut _ (RealDataCon dc) -> Just (dataConWrapId dc)
                  _                               -> Nothing
  , let bad_tys = badUseOfLevPolyPrimop var ty
  , not (null bad_tys)
  = levPolyPrimopErr expr_doc ty bad_tys
checkForcedEtaExpansion _ _ _ = return ()

-- | Is this a hasNoBinding Id with a levity-polymorphic type?
-- Returns the arguments that are levity polymorphic if they are bad;
-- or an empty list otherwise
-- See Note [Detecting forced eta expansion]
badUseOfLevPolyPrimop :: Id -> Type -> [Type]
badUseOfLevPolyPrimop id ty
  | hasNoBinding id
  = filter isTypeLevPoly arg_tys
  | otherwise
  = []
  where
    (binders, _) = splitPiTys ty
    arg_tys      = mapMaybe binderRelevantType_maybe binders

levPolyPrimopErr :: SDoc -> Type -> [Type] -> DsM ()
levPolyPrimopErr expr_doc ty bad_tys
  = errDs $ vcat
    [ hang (text "Cannot use function with levity-polymorphic arguments:")
         2 (expr_doc <+> dcolon <+> pprWithTYPE ty)
    , ppUnlessOption sdocPrintTypecheckerElaboration $ vcat
        [ text "(Note that levity-polymorphic primops such as 'coerce' and unboxed tuples"
        , text "are eta-expanded internally because they must occur fully saturated."
        , text "Use -fprint-typechecker-elaboration to display the full expression.)"
        ]
    , hang (text "Levity-polymorphic arguments:")
         2 $ vcat $ map
           (\t -> pprWithTYPE t <+> dcolon <+> pprWithTYPE (typeKind t))
           bad_tys
    ]
