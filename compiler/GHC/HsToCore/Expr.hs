{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# LANGUAGE LambdaCase #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring expressions.
-}

module GHC.HsToCore.Expr
   ( dsExpr, dsLExpr, dsLocalBinds
   , dsValBinds, dsLit, dsSyntaxExpr
   )
where

import GHC.Prelude

import GHC.HsToCore.Match
import GHC.HsToCore.Match.Literal
import GHC.HsToCore.Binds
import GHC.HsToCore.GuardedRHSs
import GHC.HsToCore.ListComp
import GHC.HsToCore.Utils
import GHC.HsToCore.Arrows
import GHC.HsToCore.Monad
import GHC.HsToCore.Pmc
import GHC.HsToCore.Pmc.Utils
import GHC.HsToCore.Errors.Types
import GHC.HsToCore.Quote
import GHC.HsToCore.Ticks (stripTicksTopHsExpr)
import GHC.Hs


-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Monad
import GHC.Tc.Instance.Class (lookupHasFieldLabel)

import GHC.Core
import GHC.Core.FVs( exprsFreeVarsList )
import GHC.Core.FamInstEnv( topNormaliseType )
import GHC.Core.Type
import GHC.Core.TyCo.Rep
import GHC.Core.Utils
import GHC.Core.Make
import GHC.Core.PatSyn

import GHC.Driver.Session

import GHC.Types.SourceText
import GHC.Types.Name hiding (varName)
import GHC.Types.CostCentre
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Id.Make
import GHC.Types.Var( isInvisibleAnonPiTyBinder )
import GHC.Types.Var.Set( isEmptyVarSet, elemVarSet )
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Tickish

import GHC.Unit.Module
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Builtin.Types
import GHC.Builtin.Names

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import Control.Monad

{-
************************************************************************
*                                                                      *
                dsLocalBinds, dsValBinds
*                                                                      *
************************************************************************
-}

dsLocalBinds :: HsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
dsLocalBinds (EmptyLocalBinds _)  body = return body
dsLocalBinds b@(HsValBinds _ binds) body = putSrcSpanDs (spanHsLocaLBinds b) $
                                           dsValBinds binds body
dsLocalBinds (HsIPBinds _ binds)  body = dsIPBinds  binds body

-------------------------
-- caller sets location
dsValBinds :: HsValBinds GhcTc -> CoreExpr -> DsM CoreExpr
dsValBinds (XValBindsLR (NValBinds binds _)) body
  = do { dflags <- getDynFlags
       ; foldrM (ds_val_bind dflags) body binds }
dsValBinds (ValBinds {})       _    = panic "dsValBinds ValBindsIn"

-------------------------
dsIPBinds :: HsIPBinds GhcTc -> CoreExpr -> DsM CoreExpr
dsIPBinds (IPBinds ev_binds ip_binds) body
  = do  { dsTcEvBinds ev_binds $ \ ds_binds -> do
        { let inner = mkCoreLets ds_binds body
                -- The dict bindings may not be in
                -- dependency order; hence Rec
        ; foldrM ds_ip_bind inner ip_binds } }
  where
    ds_ip_bind :: LIPBind GhcTc -> CoreExpr -> DsM CoreExpr
    ds_ip_bind (L _ (IPBind n _ e)) body
      = do e' <- dsLExpr e
           return (Let (NonRec n e') body)

-------------------------
-- caller sets location
ds_val_bind :: DynFlags -> (RecFlag, LHsBinds GhcTc) -> CoreExpr -> DsM CoreExpr
-- Special case for bindings which bind unlifted variables
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE and SPECIALISE pragmas...
ds_val_bind _ (NonRecursive, hsbinds) body
  | [L loc bind] <- hsbinds
        -- Non-recursive, non-overloaded bindings only come in ones
        -- ToDo: in some bizarre case it's conceivable that there
        --       could be dict binds in the 'binds'.  (See the notes
        --       below.  Then pattern-match would fail.  Urk.)
  , isUnliftedHsBind bind
  = putSrcSpanDs (locA loc) $
     -- see Note [Strict binds checks] in GHC.HsToCore.Binds
    if is_polymorphic bind
    then errDsCoreExpr (DsCannotMixPolyAndUnliftedBindings bind)
            -- data Ptr a = Ptr Addr#
            -- f x = let p@(Ptr y) = ... in ...
            -- Here the binding for 'p' is polymorphic, but does
            -- not mix with an unlifted binding for 'y'.  You should
            -- use a bang pattern.  #6078.

    else do { when (looksLazyPatBind bind) $
              diagnosticDs (DsUnbangedStrictPatterns bind)
        -- Complain about a binding that looks lazy
        --    e.g.    let I# y = x in ...
        -- Remember, in checkStrictBinds we are going to do strict
        -- matching, so (for software engineering reasons) we insist
        -- that the strictness is manifest on each binding
        -- However, lone (unboxed) variables are ok


            ; dsUnliftedBind bind body }
  where
    is_polymorphic (XHsBindsLR (AbsBinds { abs_tvs = tvs, abs_ev_vars = evs }))
                     = not (null tvs && null evs)
    is_polymorphic _ = False


ds_val_bind _ (is_rec, binds) _body
  | any (isUnliftedHsBind . unLoc) binds  -- see Note [Strict binds checks] in GHC.HsToCore.Binds
  = assert (isRec is_rec )
    errDsCoreExpr $ DsRecBindsNotAllowedForUnliftedTys binds

-- Special case: a non-recursive PatBind. No dancing about with lets and seqs,
-- we make a case immediately. Very important for linear types: let !pat can be
-- linear, but selectors as used in the general case aren't. So the general case
-- would transform a linear definition into a non-linear one. See Wrinkle 2
-- Note [Desugar Strict binds] in GHC.HsToCore.Binds.
ds_val_bind dflags (NonRecursive, hsbinds) body
  | [L _loc (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_mult = mult_ann
                     , pat_ext = (ty, (rhs_tick, _var_ticks))})] <- hsbinds
        -- Non-recursive, non-overloaded bindings only come in ones
  , pat' <- decideBangHood dflags pat
  , isBangedLPat pat'
  = do { rhss_nablas <- pmcGRHSs PatBindGuards grhss
        ; rhs_expr <- dsGuarded grhss ty rhss_nablas
        ; let rhs' = mkOptTickBox rhs_tick rhs_expr
        ; let body_ty = exprType body
        ; let mult = getTcMultAnn mult_ann
        ; error_expr <- mkErrorAppDs pAT_ERROR_ID body_ty (ppr pat')
        ; matchSimply rhs' PatBindRhs mult pat' body error_expr }
    -- This is the one place where matchSimply is given a non-ManyTy
    -- multiplicity argument.
    --
    -- In this form, there isn't a natural place for the var_ticks. In
    -- mkSelectorBinds, the ticks are around the selector function but there
    -- aren't any selection functions as we make a single pattern-match. Is this a
    -- problem?

-- Ordinary case for bindings; none should be unlifted
ds_val_bind _ (is_rec, binds) body
  = do  { massert (isRec is_rec || isSingleton binds)
               -- we should never produce a non-recursive list of multiple binds

        ; (force_vars,prs) <- dsLHsBinds binds
        ; let body' = foldr seqVar body force_vars
        ; assertPpr (not (any (isUnliftedType . idType . fst) prs)) (ppr is_rec $$ ppr binds) $
          -- NB: bindings have a fixed RuntimeRep, so it's OK to call isUnliftedType
          case prs of
            [] -> return body
            _  -> return (mkLets (mk_binds is_rec prs) body') }
            -- We can make a non-recursive let because we make sure to return
            -- the bindings in dependency order in dsLHsBinds,
            -- see Note [Return non-recursive bindings in dependency order] in
            -- GHC.HsToCore.Binds

-- | Helper function. You can use the result of 'mk_binds' with 'mkLets' for
-- instance.
--
--   * @'mk_binds' 'Recursive' binds@ makes a single mutually-recursive
--     bindings with all the rhs/lhs pairs in @binds@
--   * @'mk_binds' 'NonRecursive' binds@ makes one non-recursive binding
--     for each rhs/lhs pairs in @binds@
mk_binds :: RecFlag -> [(b, (Expr b))] -> [Bind b]
mk_binds Recursive binds = [Rec binds]
mk_binds NonRecursive binds = map (uncurry NonRec) binds

------------------
dsUnliftedBind :: HsBind GhcTc -> CoreExpr -> DsM CoreExpr
dsUnliftedBind (XHsBindsLR (AbsBinds { abs_tvs = [], abs_ev_vars = []
                                     , abs_exports = exports
                                     , abs_ev_binds = ev_binds
                                     , abs_binds = lbinds })) body
  = do { let body1 = foldr bind_export body exports
             bind_export export b = bindNonRec (abe_poly export) (Var (abe_mono export)) b
       ; body2 <- foldlM (\body lbind -> dsUnliftedBind (unLoc lbind) body)
                            body1 lbinds
       ; dsTcEvBinds_s ev_binds $ \ ds_binds -> do
       { return (mkCoreLets ds_binds body2) } }

dsUnliftedBind (FunBind { fun_id = L l fun
                        , fun_matches = matches
                        , fun_ext = (co_fn, tick)
                        }) body
               -- Can't be a bang pattern (that looks like a PatBind)
               -- so must be simply unboxed
  = do { (args, rhs) <- matchWrapper (mkPrefixFunRhs (L l $ idName fun) noAnn) Nothing matches
       ; massert (null args) -- Functions aren't unlifted
       ; dsHsWrapper co_fn $ \core_wrap ->  -- Can be non-identity (#21516)
    do { let rhs' = core_wrap (mkOptTickBox tick rhs)
       ; return (bindNonRec fun rhs' body) } }

dsUnliftedBind (PatBind { pat_lhs = pat, pat_rhs = grhss
                        , pat_ext = (ty, _) }) body
  =     -- let C x# y# = rhs in body
        -- ==> case rhs of C x# y# -> body
    do { match_nablas <- pmcGRHSs PatBindGuards grhss
       ; rhs          <- dsGuarded grhss ty match_nablas
       ; let eqn = EqnMatch { eqn_pat = pat, eqn_rest = EqnDone (cantFailMatchResult body) }
       ; var    <- selectMatchVar ManyTy (unLoc pat)
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

-- | Desugar a located typechecked expression.
dsLExpr :: LHsExpr GhcTc -> DsM CoreExpr
dsLExpr (L loc e) = putSrcSpanDsA loc $ dsExpr e

-- | Desugar a typechecked expression.
dsExpr :: HsExpr GhcTc -> DsM CoreExpr

dsExpr e@(HsVar {})                 = dsApp e
dsExpr e@(HsApp {})                 = dsApp e
dsExpr e@(HsAppType {})             = dsApp e

dsExpr (HsHole (_, (HER ref _ _))) = dsEvTerm =<< readMutVar ref
      -- See Note [Holes in expressions] in GHC.Tc.Types.Constraint.

dsExpr (HsPar _ e)            = dsLExpr e
dsExpr (ExprWithTySig _ e _)  = dsLExpr e

dsExpr (HsLit _ lit)
  = do { warnAboutOverflowedLit lit
       ; dsLit lit }

dsExpr (HsOverLit _ lit)
  = do { warnAboutOverflowedOverLit lit
       ; dsOverLit lit }

dsExpr e@(XExpr ext_expr_tc)
  = case ext_expr_tc of
      HsRecSelTc {} -> dsApp e
      WrapExpr {}   -> dsApp e
      ConLikeTc {}  -> dsApp e

      ExpandedThingTc _ e -> dsExpr e
      -- Hpc Support
      HsTick tickish e -> do
        e' <- dsLExpr e
        return (Tick tickish e')

      -- There is a problem here. The then and else branches
      -- have no free variables, so they are open to lifting.
      -- We need someway of stopping this.
      -- This will make no difference to binary coverage
      -- (did you go here: YES or NO), but will effect accurate
      -- tick counting.

      HsBinTick ixT ixF e -> do
        e2 <- dsLExpr e
        do { assert (exprType e2 `eqType` boolTy)
            mkBinaryTickBox ixT ixF e2
          }



-- Strip ticks due to #21701, need to be invariant about warnings we produce whether
-- this is enabled or not.
dsExpr (NegApp _ (L loc
                    (stripTicksTopHsExpr -> (ts, (HsOverLit _ lit@(OverLit { ol_val = HsIntegral i})))))
              neg_expr)
  = do { expr' <- putSrcSpanDsA loc $ do
          { warnAboutOverflowedOverLit
                -- See Note [Checking "negative literals"]
              (lit { ol_val = HsIntegral (negateIntegralLit i) })
          ; dsOverLit lit }
       ; dsSyntaxExpr neg_expr [mkTicks ts expr'] }

dsExpr (NegApp _ expr neg_expr)
  = do { expr' <- dsLExpr expr
       ; dsSyntaxExpr neg_expr [expr'] }

dsExpr (HsLam _ variant a_Match)
  = uncurry mkCoreLams <$> matchWrapper (LamAlt variant) Nothing a_Match


{-
Note [Checking "negative literals"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As observed in #13257 it's desirable to warn about overflowing negative literals
in some situations where the user thinks they are writing a negative literal (ie -1)
but without `-XNegativeLiterals` enabled.

This catches cases such as (-1 :: Word8) which overflow, because (negate 1 == 255) but
which we desugar to `negate (fromIntegral 1)`.

Notice it's crucial we still desugar to the correct (negate (fromIntegral ...)) despite
performing the negation in order to check whether the application of negate will overflow.
For a user written Integer instance we can't predict the interaction of negate and fromIntegral.

Also note that this works for detecting the right result for `-128 :: Int8`.. which is
in-range for Int8 but the correct result is achieved via two overflows.

negate (fromIntegral 128 :: Int8)
= negate (-128 :: Int8)
= -128 :: Int8

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
-}

dsExpr (ExplicitTuple _ tup_args boxity)
  = do { let go (lam_vars, args) (Missing st)
                    -- For every missing expression, we need
                    -- another lambda in the desugaring.
               = do { lam_var <- newSysLocalDs st
                    ; return (lam_var : lam_vars, Var lam_var : args) }
             go (lam_vars, args) (Present _ expr)
                    -- Expressions that are present don't generate
                    -- lambdas, just arguments.
               = do { core_expr <- dsLExpr expr
                    ; return (lam_vars, core_expr : args) }

       ; (lam_vars, args) <- foldM go ([], []) (reverse tup_args)
                -- The reverse is because foldM goes left-to-right
       ; return $ mkCoreLams lam_vars (mkCoreTupBoxity boxity args) }
                        -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make

dsExpr (ExplicitSum types alt arity expr)
  = mkCoreUnboxedSum arity alt types <$> dsLExpr expr

dsExpr (HsPragE _ (HsPragSCC _ cc) expr)
  = do { dflags <- getDynFlags
       ; if sccProfilingEnabled dflags && gopt Opt_ProfManualCcs dflags
         then do
            mod_name <- getModule
            count <- goptM Opt_ProfCountEntries
            let nm = sl_fs cc
            flavour <- mkExprCCFlavour <$> getCCIndexDsM nm
            Tick (ProfNote (mkUserCC nm mod_name (getLocA expr) flavour) count True)
                 <$> dsLExpr expr
         else dsLExpr expr }

dsExpr (HsCase ctxt discrim matches)
  = do { core_discrim <- dsLExpr discrim
       ; ([discrim_var], matching_code) <- matchWrapper ctxt (Just [discrim]) matches
       ; return (bindNonRec discrim_var core_discrim matching_code) }

-- Pepe: The binds are in scope in the body but NOT in the binding group
--       This is to avoid silliness in breakpoints
dsExpr (HsLet _ binds body) = do
    body' <- dsLExpr body
    dsLocalBinds binds body'

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
dsExpr (HsDo res_ty ListComp          (L _ stmts)) = dsListComp stmts res_ty
dsExpr (HsDo _      MonadComp         (L _ stmts)) = dsMonadComp stmts
dsExpr (HsDo res_ty ctx@DoExpr{}      (L _ stmts)) = dsDo ctx stmts res_ty
dsExpr (HsDo res_ty ctx@GhciStmtCtxt  (L _ stmts)) = dsDo ctx stmts res_ty
dsExpr (HsDo res_ty ctx@MDoExpr{}     (L _ stmts)) = dsDo ctx stmts res_ty

dsExpr (HsIf _ guard_expr then_expr else_expr)
  = do { pred <- dsLExpr guard_expr
       ; b1 <- dsLExpr then_expr
       ; b2 <- dsLExpr else_expr
       ; return $ mkIfThenElse pred b1 b2 }

dsExpr (HsMultiIf res_ty alts)
  = do { let grhss = GRHSs emptyComments  alts emptyLocalBinds
       ; rhss_nablas  <- pmcGRHSs IfAlt grhss
       ; match_result <- dsGRHSs IfAlt grhss res_ty rhss_nablas
       ; error_expr   <- mkErrorExpr
       ; extractMatchResult match_result error_expr }
  where
    mkErrorExpr = mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID res_ty
                               (text "multi-way if")

dsExpr (ExplicitList elt_ty xs) = dsExplicitList elt_ty xs

dsExpr (ArithSeq expr witness seq)
  = case witness of
     Nothing -> dsArithSeq expr seq
     Just fl -> do { newArithSeq <- dsArithSeq expr seq
                   ; dsSyntaxExpr fl [newArithSeq] }

{- Note [Desugaring static pointers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable
for an overview.
    g = ... static f ...
==>
    g = ... makeStatic loc f ...
-}

dsExpr (HsStatic (_, whole_ty) expr@(L loc _))
  = do { expr_ds <- dsLExpr expr
       ; let (_, [ty]) = splitTyConApp whole_ty
       ; makeStaticId <- dsLookupGlobalId makeStaticName

       ; dflags <- getDynFlags
       ;  let platform = targetPlatform dflags
              (line, col) = case locA loc of
                  RealSrcSpan r _ -> ( srcLocLine $ realSrcSpanStart r
                                     , srcLocCol  $ realSrcSpanStart r )
                  _               -> (0, 0)
              srcLoc = mkCoreTup [ mkIntExprInt platform line
                                 , mkIntExprInt platform col
                                 ]

       ; putSrcSpanDsA loc $ return $
         mkCoreApps (Var makeStaticId) [ Type ty, srcLoc, expr_ds ] }

{- Note [Desugaring record construction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
        T { op2 = e }
==>
        let err = /\a -> recConError a
        T (recConError t1 "M.hs/230/op1")
          e
          (recConError t1 "M.hs/230/op3")
\end{verbatim}
@recConError@ then converts its argument string into a proper message
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
-- See Note [Desugaring record construction]
  = do { con_expr' <- dsExpr con_expr
       ; let
             (arg_tys, _) = tcSplitFunTys (exprType con_expr')
             -- A newtype in the corner should be opaque;
             -- hence TcType.tcSplitFunTys

             mk_arg (arg_ty, fl)
               = case findField (rec_flds rbinds) (flSelector fl) of
                   (rhs:rhss) -> assert (null rhss)
                                 dsLExpr rhs
                   []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (ppr (flLabel fl))
             unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty Outputable.empty

             labels = conLikeFieldLabels con_like

       ; con_args <- if null labels
                     then mapM unlabelled_bottom (map scaledThing arg_tys)
                     else mapM mk_arg (zipEqual (map scaledThing arg_tys) labels)

       ; return (mkCoreApps con_expr' con_args) }


-- Here is where we desugar the Template Haskell brackets and escapes

-- Template Haskell stuff
-- See Note [The life cycle of a TH quotation]

dsExpr (HsTypedBracket   bracket_tc _) = dsBracket bracket_tc
dsExpr (HsUntypedBracket bracket_tc _) = dsBracket bracket_tc
dsExpr (HsTypedSplice   _   s) = pprPanic "dsExpr:typed splice" (pprTypedSplice Nothing s)
dsExpr (HsUntypedSplice ext _) = dataConCantHappen ext

-- Arrow notation extension
dsExpr (HsProc _ pat cmd) = dsProcExpr pat cmd

-- HsSyn constructs that just shouldn't be here, because
-- the renamer or typechecker removed them.  See GHC.Rename.Expr.
-- Note [Handling overloaded and rebindable constructs]
dsExpr (HsIPVar x _)      = dataConCantHappen x
dsExpr (HsGetField x _ _) = dataConCantHappen x
dsExpr (HsProjection x _) = dataConCantHappen x
dsExpr (RecordUpd x _ _)  = dataConCantHappen x
dsExpr (HsEmbTy x _)      = dataConCantHappen x
dsExpr (HsQual x _ _)     = dataConCantHappen x
dsExpr (HsForAll x _ _)   = dataConCantHappen x
dsExpr (HsFunArr x _ _ _) = dataConCantHappen x
dsExpr (HsOverLabel x _)  = dataConCantHappen x
dsExpr (OpApp x _ _ _)    = dataConCantHappen x
dsExpr (SectionL x _ _)   = dataConCantHappen x
dsExpr (SectionR x _ _)   = dataConCantHappen x


{- *********************************************************************
*                                                                      *
*              Desugaring applications
*                                                                      *
********************************************************************* -}

{- Note [Desugaring applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we come across an application (f e1 .. en) we collect up
all the desugared arguments, and then dispatch on the function f.
(Including the nullary case where n=0.)

There are several special cases to handle

* HsRecSel: a record selector gets warnings if it might fail.
* HsVar:    special magic for `noinline`
* HsVar:    special magic for `seq`

Note [Desugaring seq]
~~~~~~~~~~~~~~~~~~~~~
There are a few subtleties in the desugaring of `seq`, all
implemented in the `seqId` case of `ds_app_var`:

 1. (as described in #1031)

    Consider,
       f x y = x `seq` (y `seq` (# x,y #))

    Because the argument to the outer 'seq' has an unlifted type, we'll use
    call-by-value, and compile it as if we had

       f x y = case (y `seq` (# x,y #)) of v -> x `seq` v

    But that is bad, because we now evaluate y before x!

    Seq is very, very special!  So we recognise it right here, and desugar to
            case x of _ -> case y of _ -> (# x,y #)

 2. (as described in #2273)

    Consider
       let chp = case b of { True -> fst x; False -> 0 }
       in chp `seq` ...chp...
    Here the seq is designed to plug the space leak of retaining (snd x)
    for too long.

    If we rely on the ordinary inlining of seq, we'll get
       let chp = case b of { True -> fst x; False -> 0 }
       case chp of _ { I# -> ...chp... }

    But since chp is cheap, and the case is an alluring context, we'll
    inline chp into the case scrutinee.  Now there is only one use of chp,
    so we'll inline a second copy.  Alas, we've now ruined the purpose of
    the seq, by re-introducing the space leak:
        case (case b of {True -> fst x; False -> 0}) of
          I# _ -> ...case b of {True -> fst x; False -> 0}...

    We can try to avoid doing this by ensuring that the binder-swap in the
    case happens, so we get this at an early stage:
       case chp of chp2 { I# -> ...chp2... }
    But this is fragile.  The real culprit is the source program.  Perhaps we
    should have said explicitly
       let !chp2 = chp in ...chp2...

    But that's painful.  So the code here does a little hack to make seq
    more robust: a saturated application of 'seq' is turned *directly* into
    the case expression, thus:
       x  `seq` e2 ==> case x of x -> e2    -- Note shadowing!
       e1 `seq` e2 ==> case x of _ -> e2

    So we desugar our example to:
       let chp = case b of { True -> fst x; False -> 0 }
       case chp of chp { I# -> ...chp... }
    And now all is well.

    The reason it's a hack is because if you define mySeq=seq, the hack
    won't work on mySeq.

 3. (as described in #2409)

    The isInternalName ensures that we don't turn
            True `seq` e
    into
            case True of True { ... }
    which stupidly tries to bind the datacon 'True'.
-}

dsApp :: HsExpr GhcTc -> DsM CoreExpr
dsApp e = ds_app e [] []

----------------------
ds_lapp :: LHsExpr GhcTc -> [LHsExpr GhcTc] -> [CoreExpr] -> DsM CoreExpr
-- The [LHsExpr] args correspond to the [CoreExpr] args,
-- but there may be more of the latter because they include
-- type and dictionary arguments
ds_lapp (L loc e) hs_args core_args
  = putSrcSpanDsA loc $
    ds_app e hs_args core_args

ds_app :: HsExpr GhcTc -> [LHsExpr GhcTc] -> [CoreExpr] -> DsM CoreExpr
-- The work-horse
ds_app (HsPar _ e) hs_args core_args = ds_lapp e hs_args core_args

ds_app (HsApp _ fun arg) hs_args core_args
  = do { core_arg <- dsLExpr arg
       ; ds_lapp fun (arg : hs_args) (core_arg : core_args) }

ds_app (HsAppType arg_ty fun _) hs_args core_args
  = ds_lapp fun hs_args (Type arg_ty : core_args)

ds_app (XExpr (WrapExpr hs_wrap fun)) hs_args core_args
  = do { (fun_wrap, all_args) <- splitHsWrapperArgs hs_wrap core_args
       ; if isIdHsWrapper fun_wrap
         then ds_app fun hs_args all_args
         else do { core_fun <- dsHsWrapper fun_wrap $ \core_wrap ->
                               do { core_fun <- dsExpr fun
                                  ; return (core_wrap core_fun) }
                 ; return (mkCoreApps core_fun all_args) } }

ds_app (XExpr (ConLikeTc con tvs tys)) _hs_args core_args
-- Desugar desugars 'ConLikeTc': it eta-expands
-- data constructors to make linear types work.
-- See Note [Typechecking data constructors] in GHC.Tc.Gen.Head
  = do { ds_con <- dsHsConLike con
       ; ids    <- newSysLocalsDs tys
           -- NB: these 'Id's may be representation-polymorphic;
           -- see Wrinkle [Representation-polymorphic lambda] in
           -- Note [Typechecking data constructors] in GHC.Tc.Gen.Head.
       ; let core_fun = mkLams tvs $ mkLams ids $
                        ds_con `mkTyApps` mkTyVarTys tvs
                               `mkVarApps` ids
       ; return (mkApps core_fun core_args) }

ds_app (XExpr (HsRecSelTc (FieldOcc { foLabel = L _ sel_id }))) _hs_args core_args
  = ds_app_rec_sel sel_id sel_id core_args

ds_app (HsVar _ lfun) hs_args core_args
  = ds_app_var lfun hs_args core_args

ds_app e _hs_args core_args
  = do { core_e <- dsExpr e
       ; return (mkCoreApps core_e core_args) }

---------------
ds_app_var :: LocatedN Id -> [LHsExpr GhcTc] -> [CoreExpr] -> DsM CoreExpr
-- Desugar an application with HsVar at the head
ds_app_var (L loc fun_id) hs_args core_args

  -----------------------
  -- Deal with getField applications. General form:
  --   getField
  --     @GHC.Types.Symbol                        {k}
  --     @"sel"                                   x_ty
  --     @T                                       r_ty
  --     @Int                                     a_ty
  --     ($dHasField :: HasField "sel" T Int)     dict
  --     :: T -> Int
  -- where
  --  $dHasField = sel |> (co :: T -> Int ~R# HasField "sel" T Int)
  -- Alas, we cannot simply look at the unfolding of $dHasField below because it
  -- has not been set yet, so we have to reconstruct the selector Id from the types.
  | fun_id `hasKey` getFieldClassOpKey
  = do {  -- Look up the field named x/"sel" in the type r/T
         fam_inst_envs <- dsGetFamInstEnvs
       ; rdr_env       <- dsGetGlobalRdrEnv
       ; let core_arg_tys :: [Type] = [ty | Type ty <- core_args]
       ; case lookupHasFieldLabel fam_inst_envs rdr_env core_arg_tys of
           Just (sel_name,_,_,_)
             -> do { sel_id <- dsLookupGlobalId sel_name
                   ; tracePm "getfield2" (ppr sel_id)
                   ; ds_app_rec_sel sel_id fun_id core_args }
           _ -> ds_app_finish fun_id core_args }

  -----------------------
  -- Warn about identities for (fromInteger :: Integer -> Integer) etc
  -- They all have a type like:  forall <tvs>. <cxt> => arg_ty -> res_ty
  | idName fun_id `elem` numericConversionNames
  , let (conv_ty, _) = apply_invis_args fun_id core_args
  , Just (arg_ty, res_ty) <- splitVisibleFunTy_maybe conv_ty
  = do { dflags <- getDynFlags
       ; when (wopt Opt_WarnIdentities dflags
               && arg_ty `eqType` res_ty)  $
         -- So we are converting  ty -> ty
         diagnosticDs (DsIdentitiesFound fun_id conv_ty)

       ; ds_app_finish fun_id core_args }

  -----------------------
  -- Warn about unused return value in
  --    do { ...; e; ... } when e returns (say) an Int
  | fun_id `hasKey` thenMClassOpKey    -- It is the built-in Prelude.(>>)
    -- (>>) :: forall m. Monad m => forall a b. m a -> (b->m b) -> m b
  , Type m_ty : _dict : Type arg_ty : _ <- core_args
  , hs_arg : _ <- hs_args
  = do { tracePm ">>" (ppr loc $$ ppr arg_ty $$ ppr (isGeneratedSrcSpan (locA loc)))
       ; when (isGeneratedSrcSpan (locA loc)) $      -- It is a compiler-generated (>>)
         warnDiscardedDoBindings hs_arg m_ty arg_ty
       ; ds_app_finish fun_id core_args }

  -----------------------
  -- Deal with `noinline`
  -- See Note [noinlineId magic] in GHC.Types.Id.Make
  | fun_id `hasKey` noinlineIdKey
  , Type _ : arg1 : rest_args <- core_args
  , (inner_fun, inner_args) <- collectArgs arg1
  = return (Var fun_id `App` Type (exprType inner_fun) `App` inner_fun
            `mkCoreApps` inner_args `mkCoreApps` rest_args)

  -----------------------
  -- Deal with `seq`
  -- See Note [Desugaring seq], points (1) and (2)
  | fun_id `hasKey` seqIdKey
  , Type _r : Type ty1 : Type ty2 : arg1 : arg2 : rest_args <- core_args
  , let case_bndr = case arg1 of
            Var v1 | isInternalName (idName v1)
                  -> v1        -- Note [Desugaring seq], points (2) and (3)
            _     -> mkWildValBinder ManyTy ty1
  = return (Case arg1 case_bndr ty2 [Alt DEFAULT [] arg2]
            `mkCoreApps` rest_args)

  -----------------------
  -- Phew!  No more special cases.  Just build an applications
  | otherwise
  = ds_app_finish fun_id core_args

---------------
ds_app_finish :: Id -> [CoreExpr] -> DsM CoreExpr
-- We are about to construct an application that may include evidence applications
-- `f dict`.  If the dictionary is non-specialisable, instead construct
--     nospec f dict
-- See Note [nospecId magic] in GHC.Types.Id.Make for what `nospec` does.
-- See Note [Desugaring non-canonical evidence]
ds_app_finish fun_id core_args
  = do { mb_unspecables <- getUnspecables
       ; let fun_ty = idType fun_id
             free_dicts = exprsFreeVarsList
                            [ e | (e,pi_bndr) <- core_args `zip` fst (splitPiTys fun_ty)
                                , isInvisibleAnonPiTyBinder pi_bndr ]

             fun | Just unspecables <- mb_unspecables
                 , not (isEmptyVarSet unspecables)  -- Fast path
                 , any (`elemVarSet` unspecables) free_dicts
                 = Var nospecId `App` Type fun_ty `App` Var fun_id
                 | otherwise
                 = Var fun_id

       ; return (mkCoreApps fun core_args) }

---------------
ds_app_rec_sel :: Id             -- The record selector Id itself
               -> Id             -- The function at the the head
               -> [CoreExpr]     -- Its arguments
               -> DsM CoreExpr
-- Desugar an application with HsRecSelId at the head
ds_app_rec_sel sel_id fun_id core_args
  | RecSelId{ sel_cons = rec_sel_info } <- idDetails sel_id
  , RSI { rsi_undef = cons_wo_field } <- rec_sel_info
  = do { -- Record selectors are warned about if they are not present in all of the
         -- parent data type's constructors, or always in case of pattern synonym record
         -- selectors (regulated by a flag). However, this only produces a warning if
         -- it's not a part of a record selector application. For example:
         --         data T = T1 | T2 {s :: Bool}
         --         g y = map s y   -- Warn here
         --         f x = s x       -- No warning here
       ; let (fun_ty, val_args) = apply_invis_args fun_id core_args

       ; tracePm "ds_app_rec_sel" (ppr fun_ty $$ ppr val_args)
       ; case val_args of

           -- There is a value argument
           -- See (IRS2) of Note [Detecting incomplete record selectors] in GHC.HsToCore.Pmc
           (arg:_) -> pmcRecSel sel_id arg

           -- No value argument, but the selector is
           -- applied to all its type arguments
           -- See (IRS3) of Note [Detecting incomplete record selectors] in GHC.HsToCore.Pmc
           [] | Just (val_arg_ty, _) <- splitVisibleFunTy_maybe fun_ty
              -> do { dummy <- newSysLocalDs (Scaled ManyTy val_arg_ty)
                    ; pmcRecSel sel_id (Var dummy) }

           -- Not even applied to all its type args
           -- See (IRS4) of Note [Detecting incomplete record selectors] in GHC.HsToCore.Pmc
           _ -> unless (null cons_wo_field) $
                do { dflags <- getDynFlags
                   ; let maxCons = maxUncoveredPatterns dflags
                   ; diagnosticDs $ DsIncompleteRecordSelector (idName sel_id) cons_wo_field maxCons }

       ; ds_app_finish fun_id core_args }

  | otherwise
  = pprPanic "ds_app_rec_sel" (ppr sel_id $$ ppr (idDetails sel_id))
  where

apply_invis_args :: Id -> [CoreExpr] -> (Type, [CoreExpr])
-- Apply function to the initial /type/ args;
-- return the type of the instantiated function,
-- and the remaining args
--   e.g.  apply_type_args (++) [Type Int, Var xs]
--         = ([Int] -> [Int] -> [Int], [Var xs])
apply_invis_args fun_id args
  = (applyTypeToArgs fun_ty invis_args, rest_args)
  where
    fun_ty = idType fun_id
    (invis_args, rest_args) = splitAt (invisibleBndrCount fun_ty) args

------------------------------
splitHsWrapperArgs :: HsWrapper -> [CoreArg] -> DsM (HsWrapper, [CoreArg])
-- Splits the wrapper into the trailing arguments, and leftover bit
splitHsWrapperArgs wrap args = go wrap args
  where
    go (WpTyApp ty) args = return (WpHole, Type ty : args)
    go (WpEvApp tm) args = do { core_tm <- dsEvTerm tm
                              ; return (WpHole, core_tm : args)}
    go (WpCompose w1 w2) args
      = do { (w1', args') <- go w1 args
           ; if isIdHsWrapper w1'
             then go w2 args'
             else return (w1' <.> w2, args') }
    go wrap args = return (wrap, args)

------------------------------
dsHsConLike :: ConLike -> DsM CoreExpr
dsHsConLike (RealDataCon dc)
  = return (varToCoreExpr (dataConWrapId dc))
dsHsConLike (PatSynCon ps)
  | Just (builder_name, _, add_void) <- patSynBuilder ps
  = do { builder_id <- dsLookupGlobalId builder_name
       ; return (if add_void
                 then mkCoreApp (text "dsConLike" <+> ppr ps)
                                (Var builder_id) unboxedUnitExpr
                 else Var builder_id) }
  | otherwise
  = pprPanic "dsConLike" (ppr ps)

------------------------------
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsSyntaxExpr (SyntaxExprTc { syn_expr      = expr
                           , syn_arg_wraps = arg_wraps
                           , syn_res_wrap  = res_wrap })
             arg_exprs
  = do { fun <- dsExpr expr
       ; dsHsWrappers arg_wraps $ \core_arg_wraps ->
         dsHsWrapper res_wrap   $ \core_res_wrap ->
    do { let wrapped_args = zipWithEqual ($) core_arg_wraps arg_exprs
       ; return $ core_res_wrap (mkCoreApps fun wrapped_args) } }
dsSyntaxExpr NoSyntaxExprTc _ = panic "dsSyntaxExpr"

findField :: [LHsRecField GhcTc arg] -> Name -> [arg]
findField rbinds sel
  = [hfbRHS fld | L _ fld <- rbinds
                , sel == idName (hsRecFieldId fld) ]

{-
%--------------------------------------------------------------------

Note [Desugaring non-canonical evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When constructing an application
    f @ty1 ty2 .. dict1 dict2 .. arg1 arg2 ..
if the evidence `dict_i` is canonical, we simply build that application.
But if any of the `dict_i` are /non-canonical/, we wrap the application
in `nospec`, thus
    nospec @fty f @ty1 @ty2 .. dict1 dict2 .. arg1 arg2 ..
where  nospec :: forall a. a -> a  ensures that the typeclass specialiser
doesn't attempt to common up this evidence term with other evidence terms
of the same type (see Note [nospecId magic] in GHC.Types.Id.Make).

See Note [Coherence and specialisation: overview] in GHC.Core.InstEnv for
what a "non-canonical" dictionary is, and whe shouldn't specialise on it.

How do we decide if the arguments are non-canonical dictionaries?

* In `ds_app_finish` we look for dictionary arguments (invisible value args)

* In the DsM monad we track the "unspecables" (i.e. non-canonical dictionaries)
  in the `dsl_unspecable` field of `DsLclEnv`

* We extend that unspecable set via `addUnspecables`, in `dsEvBinds`.
  A dictionary is non-canonical if its own resolution was incoherent (see
  Note [Incoherent instances]), or if its definition refers to other non-canonical
  evidence. `dsEvBinds` is the convenient place to compute this, since it already
  needs to do inter-evidence dependency analysis to generate well-scoped
  bindings.

Wrinkle:

(NC1) We don't do this in the LHS of a RULE.  In particular, if we have
     f :: (Num a, HasCallStack) => a -> a
     {-# SPECIALISE f :: Int -> Int #-}
  then making a rule like
        RULE   forall d1:Num Int, d2:HasCallStack.
               f @Int d1 d2 = $sf
  is pretty dodgy, because $sf won't get the call stack passed in d2.
  But that's what you asked for in the SPECIALISE pragma, so we'll obey.

  We definitely can't desugar that LHS into this!
      nospec (f @Int d1) d2

  This is done by zapping the unspecables in `dsRule` to Nothing.  That `Nothing`
  says not to collect unspecables at all.


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
for long lists we assume that the benefits will be outweighed by the cost of
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

dsExplicitList :: Type -> [LHsExpr GhcTc]
               -> DsM CoreExpr
-- See Note [Desugaring explicit lists]
dsExplicitList elt_ty xs
  = do { dflags <- getDynFlags
       ; xs' <- mapM dsLExpr xs
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

dsArithSeq :: PostTcExpr -> (ArithSeqInfo GhcTc) -> DsM CoreExpr
dsArithSeq expr (From from)
  = App <$> dsExpr expr <*> dsLExpr from
dsArithSeq expr (FromTo from to)
  = do fam_envs <- dsGetFamInstEnvs
       dflags <- getDynFlags
       warnAboutEmptyEnumerations fam_envs dflags from Nothing to
       expr' <- dsExpr expr
       from' <- dsLExpr from
       to'   <- dsLExpr to
       return $ mkApps expr' [from', to']
dsArithSeq expr (FromThen from thn)
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn]
dsArithSeq expr (FromThenTo from thn to)
  = do fam_envs <- dsGetFamInstEnvs
       dflags <- getDynFlags
       warnAboutEmptyEnumerations fam_envs dflags from (Just thn) to
       expr' <- dsExpr expr
       from' <- dsLExpr from
       thn'  <- dsLExpr thn
       to'   <- dsLExpr to
       return $ mkApps expr' [from', thn', to']

{-
Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in GHC.HsToCore.ListComp).  Basically does the translation given in the
Haskell 98 report:
-}

dsDo :: HsDoFlavour -> [ExprLStmt GhcTc] -> Type -> DsM CoreExpr
-- This code path seems inactive for regular Do,
--     which is expanded in GHC.Tc.Gen.Do.
-- It is used only for ApplicativeDo (even the BindStmt case), which is *very*
--     annoying because it is a lot of duplicated code that is seldomly tested.
-- But we are on course to expane Applicative in GHC.Tc.Gen.Do, at which
-- point all this will go away
dsDo ctx stmts res_ty
  = goL stmts
  where
    goL [] = panic "dsDo"
    goL ((L loc stmt):lstmts) = putSrcSpanDsA loc (go loc stmt lstmts)

    go _ (LastStmt _ body _ _) stmts
      = assert (null stmts ) dsLExpr body
        -- The 'return' op isn't used for 'do' expressions

    go _ (BodyStmt _ rhs then_expr _) stmts
      = do { rhs2 <- dsLExpr rhs
           ; case  tcSplitAppTy_maybe (exprType rhs2) of
               Just (m_ty, elt_ty) -> warnDiscardedDoBindings rhs m_ty elt_ty
               Nothing             -> return ()  -- Odd, but not warning
           ; rest <- goL stmts
           ; dsSyntaxExpr then_expr [rhs2, rest] }

    go _ (LetStmt _ binds) stmts
      = do { rest <- goL stmts
           ; dsLocalBinds binds rest }

    go _ (BindStmt xbs pat rhs) stmts
      -- SG: As far as I can tell, this code path is only triggered when ApplicativeDo fails, e.g.
      --   do blah <- action1; action2 (blah * 2)
      -- It is reached when compiling GHC.Parser.PostProcess.Haddock.addHaddockToModule
      = do  { var   <- selectSimpleMatchVarL (xbstc_boundResultMult xbs) pat
            ; rhs'  <- dsLExpr rhs
            ; match <- matchSinglePatVar var Nothing (StmtCtxt (HsDoStmt ctx)) pat
                                 (xbstc_boundResultType xbs) (MR_Infallible $ goL stmts)
            -- NB: "goL stmts" needs to happen inside matchSinglePatVar, and not
            -- before it, so that long-distance information is properly threaded.
            -- See Note [Long-distance information in do notation].
            ; match_code <- dsHandleMonadicFailure ctx pat res_ty match (xbstc_failOp xbs)
            ; dsSyntaxExpr (xbstc_bindOp xbs) [rhs', Lam var match_code] }

    go loc (RecStmt { recS_stmts = L _ rec_stmts, recS_later_ids = later_ids
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
            { xbstc_bindOp          = bind_op
            , xbstc_boundResultType = bind_ty
            , xbstc_boundResultMult = ManyTy
            , xbstc_failOp          = Nothing -- Tuple cannot fail
            }
          (mkBigLHsPatTupId later_pats)
          mfix_app

        tup_ids      = rec_ids ++ filterOut (`elem` rec_ids) later_ids
        tup_ty       = mkBigCoreTupTy (map idType tup_ids) -- Deals with singleton case
        rec_tup_pats = map nlVarPat tup_ids
        later_pats   = rec_tup_pats
        rets         = map noLocA rec_rets
        mfix_app     = nlHsSyntaxApps mfix_op [mfix_arg]
        match_group  = MatchGroupTc [unrestricted tup_ty] body_ty (Generated OtherExpansion SkipPmc)
        mfix_arg     = noLocA $ HsLam noAnn LamSingle
                           (MG { mg_alts = noLocA [mkSimpleMatch
                                                    (LamAlt LamSingle)
                                                    (noLocA [mfix_pat]) body]
                               , mg_ext = match_group
                               })
        mfix_pat     = noLocA $ LazyPat noExtField $ mkBigLHsPatTupId rec_tup_pats
        body         = noLocA $ HsDo body_ty
                                ctx (noLocA (rec_stmts ++ [ret_stmt]))
        ret_app      = nlHsSyntaxApps return_op [mkBigLHsTupId rets]
        ret_stmt     = noLocA $ mkLastStmt ret_app
                     -- This LastStmt will be desugared with dsDo,
                     -- which ignores the return_op in the LastStmt,
                     -- so we must apply the return_op explicitly

    go _ (XStmtLR (ApplicativeStmt body_ty args mb_join)) stmts
      = do {
             let
               (pats, rhss) = unzip (map (do_arg . snd) args)

               do_arg (ApplicativeArgOne fail_op pat expr _) =
                 ((pat, fail_op), dsLExpr expr)
               do_arg (ApplicativeArgMany _ stmts ret pat _) =
                 ((pat, Nothing), dsDo ctx (stmts ++ [noLocA $ mkLastStmt (noLocA ret)]) res_ty)

           ; rhss' <- sequence rhss

           ; body' <- dsLExpr $ noLocA $ HsDo body_ty ctx (noLocA stmts)

           ; let match_args (pat, fail_op) (vs,body)
                   = putSrcSpanDs (getLocA pat) $
                     do { var   <- selectSimpleMatchVarL ManyTy pat
                        ; match <- matchSinglePatVar var Nothing (StmtCtxt (HsDoStmt ctx)) pat
                                   body_ty (cantFailMatchResult body)
                        ; match_code <- dsHandleMonadicFailure ctx pat body_ty match fail_op
                        ; return (var:vs, match_code)
                        }

           ; (vars, body) <- foldrM match_args ([],body') pats
           ; let fun' = mkLams vars body
           ; let mk_ap_call l (op,r) = dsSyntaxExpr op [l,r]
           ; expr <- foldlM mk_ap_call fun' (zip (map fst args) rhss')
           ; case mb_join of
               Nothing -> return expr
               Just join_op -> dsSyntaxExpr join_op [expr] }

    go _ (ParStmt   {}) _ = panic "dsDo ParStmt"
    go _ (TransStmt {}) _ = panic "dsDo TransStmt"

{- Note [Long-distance information in do notation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider T21360:

  data Foo = A Int | B

  swooble :: Foo -> Maybe Foo
  swooble foo = do
    bar@A{} <- Just foo
    return $ case bar of { A _ -> A 9 }

The pattern-match checker **should not** complain that the case statement
is incomplete, because we know that 'bar' is headed by the constructor 'A',
due to the pattern match in the line above. However, we need to ensure that we
propagate this long-distance information; failing to do so lead to #21360.

To do this, we use "matchSinglePatVar" to handle the first pattern match

  bar@A{} <- Just foo

"matchSinglePatVar" then threads through the long-distance information to the
desugaring of the remaining statements by using updPmNablasMatchResult.
This avoids any spurious pattern-match warnings when handling the case
statement on the last line.

Other places that requires from the same treatment:

  - monad comprehensions, e.g.

     blorble :: Foo -> Maybe Foo
     blorble foo = [ case bar of { A _ -> A 9 } | bar@A{} <- Just foo ]

     See GHC.HsToCore.ListComp.dsMcBindStmt. Also tested in T21360.

  - guards, e.g.

      giddy :: Maybe Char -> Char
      giddy x
        | y@(Just _) <- x
        , let z = case y of { Just w -> w }
        = z

    We don't want any inexhaustive pattern match warnings for the case statement,
    because we already know 'y' is of the form "Just ...".
    See test case T21360b.


************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************
-}

-- Warn about certain types of values discarded in monadic bindings (#3263)
warnDiscardedDoBindings :: LHsExpr GhcTc -> Type -> Type -> DsM ()
warnDiscardedDoBindings rhs@(L rhs_loc _) m_ty elt_ty
  = putSrcSpanDsA rhs_loc $ do { warn_unused <- woptM Opt_WarnUnusedDoBind
       ; warn_wrong <- woptM Opt_WarnWrongDoBind
       ; when (warn_unused || warn_wrong) $
    do { fam_inst_envs <- dsGetFamInstEnvs
       ; let norm_elt_ty = topNormaliseType fam_inst_envs elt_ty
             supressible_ty =
               isUnitTy norm_elt_ty || isAnyTy norm_elt_ty || isZonkAnyTy norm_elt_ty
         -- Warn about discarding things in 'monadic' binding,
         -- however few types are excluded:
         --   * Unit type `()`
         --   * `ZonkAny` or `Any` type see (Any8) of Note [Any types]
       ; if warn_unused && not supressible_ty
         then diagnosticDs (DsUnusedDoBind rhs elt_ty)
         else

           -- Warn about discarding m a things in 'monadic' binding of the same type,
           -- but only if we didn't already warn due to Opt_WarnUnusedDoBind
           -- Example:   do { return 3; blah }
           -- We get   (>>) @m d @(m Int) (return 3) blah
           when warn_wrong $
           case tcSplitAppTy_maybe norm_elt_ty of
             Just (elt_m_ty, _)
                | m_ty `eqType` topNormaliseType fam_inst_envs elt_m_ty
                -> diagnosticDs (DsWrongDoBind rhs elt_ty)
             _ -> return () } }
