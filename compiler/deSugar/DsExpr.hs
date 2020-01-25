{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring expressions.
-}

{-# LANGUAGE CPP, MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module DsExpr ( dsExpr, dsLExpr, dsLExprNoLP, dsLocalBinds
              , dsValBinds, dsLit, dsSyntaxExpr
              , dsHandleMonadicFailure ) where

#include "HsVersions.h"

import GhcPrelude

import Match
import MatchLit
import DsBinds
import DsGRHSs
import DsListComp
import DsUtils
import DsArrows
import DsMonad
import GHC.HsToCore.PmCheck ( checkGuardMatches )
import Name
import NameEnv
import FamInstEnv( topNormaliseType )
import DsMeta
import GHC.Hs

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import TcType
import TcEvidence
import TcRnMonad
import Type
import CoreSyn
import CoreUtils
import MkCore

import DynFlags
import CostCentre
import Id
import MkId
import Module
import ConLike
import DataCon
import TyCoPpr( pprWithTYPE )
import TysWiredIn
import PrelNames
import BasicTypes
import Maybes
import VarEnv
import SrcLoc
import Util
import Bag
import Outputable
import PatSyn

import Control.Monad

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
dsLocalBinds _                            _    = panic "dsLocalBinds"

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
    ds_ip_bind (L _ (IPBind _ ~(Right n) e)) body
      = do e' <- dsLExpr e
           return (Let (NonRec n e') body)
    ds_ip_bind _ _ = panic "dsIPBinds"
dsIPBinds (XHsIPBinds nec) _ = noExtCon nec

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
     -- see Note [Strict binds checks] in DsBinds
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
  | anyBag (isUnliftedHsBind . unLoc) binds  -- see Note [Strict binds checks] in DsBinds
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
        -- See notes with TcSimplify.inferLoop [NO TYVARS]
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
                        , pat_ext = NPatBindTc _ ty }) body
  =     -- let C x# y# = rhs in body
        -- ==> case rhs of C x# y# -> body
    do { rhs <- dsGuarded grhss ty
       ; checkGuardMatches PatBindGuards grhss
       ; let upat = unLoc pat
             eqn = EqnInfo { eqn_pats = [upat],
                             eqn_orig = FromSource,
                             eqn_rhs = cantFailMatchResult body }
       ; var    <- selectMatchVar upat
       ; result <- matchEquations PatBindRhs [var] [eqn] (exprType body)
       ; return (bindNonRec var rhs result) }

dsUnliftedBind bind body = pprPanic "dsLet: unlifted" (ppr bind $$ ppr body)

{-
************************************************************************
*                                                                      *
\subsection[DsExpr-vars-and-cons]{Variables, constructors, literals}
*                                                                      *
************************************************************************
-}

dsLExpr :: LHsExpr GhcTc -> DsM CoreExpr

dsLExpr (L loc e)
  = putSrcSpanDs loc $
    do { core_expr <- dsExpr e
   -- uncomment this check to test the hsExprType function in TcHsSyn
   --    ; MASSERT2( exprType core_expr `eqType` hsExprType e
   --              , ppr e <+> dcolon <+> ppr (hsExprType e) $$
   --                ppr core_expr <+> dcolon <+> ppr (exprType core_expr) )
       ; return core_expr }

-- | Variant of 'dsLExpr' that ensures that the result is not levity
-- polymorphic. This should be used when the resulting expression will
-- be an argument to some other function.
-- See Note [Levity polymorphism checking] in DsMonad
-- See Note [Levity polymorphism invariants] in CoreSyn
dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsLExprNoLP (L loc e)
  = putSrcSpanDs loc $
    do { e' <- dsExpr e
       ; dsNoLevPolyExpr e' (text "In the type of expression:" <+> ppr e)
       ; return e' }

dsExpr :: HsExpr GhcTc -> DsM CoreExpr
dsExpr = ds_expr False

ds_expr :: Bool   -- are we directly inside an HsWrap?
                  -- See Wrinkle in Note [Detecting forced eta expansion]
        -> HsExpr GhcTc -> DsM CoreExpr
ds_expr _ (HsPar _ e)            = dsLExpr e
ds_expr _ (ExprWithTySig _ e _)  = dsLExpr e
ds_expr w (HsVar _ (L _ var))    = dsHsVar w var
ds_expr _ (HsUnboundVar {})      = panic "dsExpr: HsUnboundVar" -- Typechecker eliminates them
ds_expr w (HsConLikeOut _ con)   = dsConLike w con
ds_expr _ (HsIPVar {})           = panic "dsExpr: HsIPVar"
ds_expr _ (HsOverLabel{})        = panic "dsExpr: HsOverLabel"

ds_expr _ (HsLit _ lit)
  = do { warnAboutOverflowedLit lit
       ; dsLit (convertLit lit) }

ds_expr _ (HsOverLit _ lit)
  = do { warnAboutOverflowedOverLit lit
       ; dsOverLit lit }

ds_expr _ (XExpr (HsWrap co_fn e))
  = do { e' <- ds_expr True e    -- This is the one place where we recurse to
                                 -- ds_expr (passing True), rather than dsExpr
       ; wrap' <- dsHsWrapper co_fn
       ; dflags <- getDynFlags
       ; let wrapped_e = wrap' e'
             wrapped_ty = exprType wrapped_e
       ; checkForcedEtaExpansion e wrapped_ty -- See Note [Detecting forced eta expansion]
       ; warnAboutIdentities dflags e' wrapped_ty
       ; return wrapped_e }

ds_expr _ (NegApp _ (L loc
                      (HsOverLit _ lit@(OverLit { ol_val = HsIntegral i})))
                  neg_expr)
  = do { expr' <- putSrcSpanDs loc $ do
          { warnAboutOverflowedOverLit
              (lit { ol_val = HsIntegral (negateIntegralLit i) })
          ; dsOverLit lit }
       ; dsSyntaxExpr neg_expr [expr'] }

ds_expr _ (NegApp _ expr neg_expr)
  = do { expr' <- dsLExpr expr
       ; dsSyntaxExpr neg_expr [expr'] }

ds_expr _ (HsLam _ a_Match)
  = uncurry mkLams <$> matchWrapper LambdaExpr Nothing a_Match

ds_expr _ (HsLamCase _ matches)
  = do { ([discrim_var], matching_code) <- matchWrapper CaseAlt Nothing matches
       ; return $ Lam discrim_var matching_code }

ds_expr _ e@(HsApp _ fun arg)
  = do { fun' <- dsLExpr fun
       ; dsWhenNoErrs (dsLExprNoLP arg)
                      (\arg' -> mkCoreAppDs (text "HsApp" <+> ppr e) fun' arg') }

ds_expr _ (HsAppType _ e _)
    -- ignore type arguments here; they're in the wrappers instead at this point
  = dsLExpr e

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

Operator sections.  At first it looks as if we can convert
\begin{verbatim}
        (expr op)
\end{verbatim}
to
\begin{verbatim}
        \x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
        map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
        let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.
-}

ds_expr _ e@(OpApp _ e1 op e2)
  = -- for the type of y, we need the type of op's 2nd argument
    do { op' <- dsLExpr op
       ; dsWhenNoErrs (mapM dsLExprNoLP [e1, e2])
                      (\exprs' -> mkCoreAppsDs (text "opapp" <+> ppr e) op' exprs') }

ds_expr _ (SectionL _ expr op)       -- Desugar (e !) to ((!) e)
  = do { op' <- dsLExpr op
       ; dsWhenNoErrs (dsLExprNoLP expr)
                      (\expr' -> mkCoreAppDs (text "sectionl" <+> ppr expr) op' expr') }

-- dsLExpr (SectionR op expr)   -- \ x -> op x expr
ds_expr _ e@(SectionR _ op expr) = do
    core_op <- dsLExpr op
    -- for the type of x, we need the type of op's 2nd argument
    let (x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
        -- See comment with SectionL
    y_core <- dsLExpr expr
    dsWhenNoErrs (mapM newSysLocalDsNoLP [x_ty, y_ty])
                 (\[x_id, y_id] -> bindNonRec y_id y_core $
                                   Lam x_id (mkCoreAppsDs (text "sectionr" <+> ppr e)
                                                          core_op [Var x_id, Var y_id]))

ds_expr _ (ExplicitTuple _ tup_args boxity)
  = do { let go (lam_vars, args) (L _ (Missing ty))
                    -- For every missing expression, we need
                    -- another lambda in the desugaring.
               = do { lam_var <- newSysLocalDsNoLP ty
                    ; return (lam_var : lam_vars, Var lam_var : args) }
             go (lam_vars, args) (L _ (Present _ expr))
                    -- Expressions that are present don't generate
                    -- lambdas, just arguments.
               = do { core_expr <- dsLExprNoLP expr
                    ; return (lam_vars, core_expr : args) }
             go _ _ = panic "ds_expr"

       ; dsWhenNoErrs (foldM go ([], []) (reverse tup_args))
                -- The reverse is because foldM goes left-to-right
                      (\(lam_vars, args) -> mkCoreLams lam_vars $
                                            mkCoreTupBoxity boxity args) }
                        -- See Note [Don't flatten tuples from HsSyn] in MkCore

ds_expr _ (ExplicitSum types alt arity expr)
  = do { dsWhenNoErrs (dsLExprNoLP expr)
                      (\core_expr -> mkCoreConApps (sumDataCon alt arity)
                                     (map (Type . getRuntimeRep) types ++
                                      map Type types ++
                                      [core_expr]) ) }

ds_expr _ (HsPragE _ prag expr) =
  ds_prag_expr prag expr

ds_expr _ (HsCase _ discrim matches)
  = do { core_discrim <- dsLExpr discrim
       ; ([discrim_var], matching_code) <- matchWrapper CaseAlt (Just discrim) matches
       ; return (bindNonRec discrim_var core_discrim matching_code) }

-- Pepe: The binds are in scope in the body but NOT in the binding group
--       This is to avoid silliness in breakpoints
ds_expr _ (HsLet _ binds body) = do
    body' <- dsLExpr body
    dsLocalBinds binds body'

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
ds_expr _ (HsDo res_ty ListComp (L _ stmts)) = dsListComp stmts res_ty
ds_expr _ (HsDo _ DoExpr        (L _ stmts)) = dsDo stmts
ds_expr _ (HsDo _ GhciStmtCtxt  (L _ stmts)) = dsDo stmts
ds_expr _ (HsDo _ MDoExpr       (L _ stmts)) = dsDo stmts
ds_expr _ (HsDo _ MonadComp     (L _ stmts)) = dsMonadComp stmts

ds_expr _ (HsIf _ fun guard_expr then_expr else_expr)
  = do { pred <- dsLExpr guard_expr
       ; b1 <- dsLExpr then_expr
       ; b2 <- dsLExpr else_expr
       ; case fun of  -- See Note [Rebindable if] in Hs.Expr
           (SyntaxExprTc {}) -> dsSyntaxExpr fun [pred, b1, b2]
           NoSyntaxExprTc    -> return $ mkIfThenElse pred b1 b2 }

ds_expr _ (HsMultiIf res_ty alts)
  | null alts
  = mkErrorExpr

  | otherwise
  = do { match_result <- liftM (foldr1 combineMatchResults)
                               (mapM (dsGRHS IfAlt res_ty) alts)
       ; checkGuardMatches IfAlt (GRHSs noExtField alts (noLoc emptyLocalBinds))
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

ds_expr _ (ExplicitList elt_ty wit xs)
  = dsExplicitList elt_ty wit xs

ds_expr _ (ArithSeq expr witness seq)
  = case witness of
     Nothing -> dsArithSeq expr seq
     Just fl -> do { newArithSeq <- dsArithSeq expr seq
                   ; dsSyntaxExpr fl [newArithSeq] }

{-
Static Pointers
~~~~~~~~~~~~~~~

See Note [Grand plan for static forms] in StaticPtrTable for an overview.

    g = ... static f ...
==>
    g = ... makeStatic loc f ...
-}

ds_expr _ (HsStatic _ expr@(L loc _)) = do
    expr_ds <- dsLExprNoLP expr
    let ty = exprType expr_ds
    makeStaticId <- dsLookupGlobalId makeStaticName

    dflags <- getDynFlags
    let (line, col) = case loc of
           RealSrcSpan r -> ( srcLocLine $ realSrcSpanStart r
                            , srcLocCol  $ realSrcSpanStart r
                            )
           _             -> (0, 0)
        srcLoc = mkCoreConApps (tupleDataCon Boxed 2)
                     [ Type intTy              , Type intTy
                     , mkIntExprInt dflags line, mkIntExprInt dflags col
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

ds_expr _ (RecordCon { rcon_flds = rbinds
                     , rcon_ext = RecordConTc { rcon_con_expr = con_expr
                                              , rcon_con_like = con_like }})
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
                     then mapM unlabelled_bottom arg_tys
                     else mapM mk_arg (zipEqual "dsExpr:RecordCon" arg_tys labels)

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
     T1 :: { f1 :: a } -> T a Int

Then the wrapper function for T1 has type
   $WT1 :: a -> T a Int
But if x::T a b, then
   x { f1 = v } :: T a b   (not T a Int!)
So we need to cast (T a Int) to (T a b).  Sigh.

-}

ds_expr _ expr@(RecordUpd { rupd_expr = record_expr, rupd_flds = fields
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
                <- matchWrapper RecUpd Nothing
                                      (MG { mg_alts = noLoc alts
                                          , mg_ext = MatchGroupTc [in_ty] out_ty
                                          , mg_origin = FromSource })
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
           ; lcl_id <- newSysLocalDs (idType fld_id)
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
                 user_tvs =
                   case con of
                     RealDataCon data_con -> dataConUserTyVars data_con
                     PatSynCon _          -> univ_tvs ++ ex_tvs
                       -- The order here is because of the order in `TcPatSyn`.
                 in_subst  = zipTvSubst univ_tvs in_inst_tys
                 out_subst = zipTvSubst univ_tvs out_inst_tys

                -- I'm not bothering to clone the ex_tvs
           ; eqs_vars   <- mapM newPredVarDs (substTheta in_subst (eqSpecPreds eq_spec))
           ; theta_vars <- mapM newPredVarDs (substTheta in_subst prov_theta)
           ; arg_ids    <- newSysLocalsDs (substTysUnchecked in_subst arg_tys)
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
                                      | tv <- user_tvs
                                      , not (tv `elemVarEnv` wrap_subst) ]
                          -- Be sure to use user_tvs (which may be ordered
                          -- differently than `univ_tvs ++ ex_tvs) above.
                          -- See Note [DataCon user type variable binders]
                          -- in DataCon.
                 rhs = foldl' (\a b -> nlHsApp a b) inst_con val_args

                        -- Tediously wrap the application in a cast
                        -- Note [Update for GADTs]
                 wrapped_rhs =
                  case con of
                    RealDataCon data_con ->
                      let
                        wrap_co =
                          mkTcTyConAppCo Nominal
                            (dataConTyCon data_con)
                            [ lookup tv ty
                              | (tv,ty) <- univ_tvs `zip` out_inst_tys ]
                        lookup univ_tv ty =
                          case lookupVarEnv wrap_subst univ_tv of
                            Just co' -> co'
                            Nothing  -> mkTcReflCo Nominal ty
                        in if null eq_spec
                             then rhs
                             else mkLHsWrap (mkWpCastN wrap_co) rhs
                    -- eq_spec is always null for a PatSynCon
                    PatSynCon _ -> rhs

                 wrap_subst =
                  mkVarEnv [ (tv, mkTcSymCo (mkTcCoVarCo eq_var))
                           | (spec, eq_var) <- eq_spec `zip` eqs_vars
                           , let tv = eqSpecTyVar spec ]

                 req_wrap = dict_req_wrap <.> mkWpTyApps in_inst_tys

                 pat = noLoc $ ConPat { pat_con = noLoc con
                                      , pat_args = PrefixCon $ map nlVarPat arg_ids
                                      , pat_con_ext = ConPatTc
                                        { pat_tvs = ex_tvs
                                        , pat_dicts = eqs_vars ++ theta_vars
                                        , pat_binds = emptyTcEvBinds
                                        , pat_arg_tys = in_inst_tys
                                        , pat_wrap = req_wrap
                                        }
                                      }
           ; return (mkSimpleMatch RecUpd [pat] wrapped_rhs) }

-- Here is where we desugar the Template Haskell brackets and escapes

-- Template Haskell stuff

ds_expr _ (HsRnBracketOut _ _ _)  = panic "dsExpr HsRnBracketOut"
ds_expr _ (HsTcBracketOut _ hs_wrapper x ps) = dsBracket hs_wrapper x ps
ds_expr _ (HsSpliceE _ s)         = pprPanic "dsExpr:splice" (ppr s)

-- Arrow notation extension
ds_expr _ (HsProc _ pat cmd) = dsProcExpr pat cmd

-- Hpc Support

ds_expr _ (HsTick _ tickish e) = do
  e' <- dsLExpr e
  return (Tick tickish e')

-- There is a problem here. The then and else branches
-- have no free variables, so they are open to lifting.
-- We need someway of stopping this.
-- This will make no difference to binary coverage
-- (did you go here: YES or NO), but will effect accurate
-- tick counting.

ds_expr _ (HsBinTick _ ixT ixF e) = do
  e2 <- dsLExpr e
  do { ASSERT(exprType e2 `eqType` boolTy)
       mkBinaryTickBox ixT ixF e2
     }

-- HsSyn constructs that just shouldn't be here:
ds_expr _ (HsBracket     {})  = panic "dsExpr:HsBracket"
ds_expr _ (HsDo          {})  = panic "dsExpr:HsDo"
ds_expr _ (HsRecFld      {})  = panic "dsExpr:HsRecFld"

ds_prag_expr :: HsPragE GhcTc -> LHsExpr GhcTc -> DsM CoreExpr
ds_prag_expr (HsPragSCC _ _ cc) expr = do
    dflags <- getDynFlags
    if gopt Opt_SccProfilingOn dflags
      then do
        mod_name <- getModule
        count <- goptM Opt_ProfCountEntries
        let nm = sl_fs cc
        flavour <- ExprCC <$> getCCIndexM nm
        Tick (ProfNote (mkUserCC nm mod_name (getLoc expr) flavour) count True)
               <$> dsLExpr expr
      else dsLExpr expr
ds_prag_expr (HsPragCore _ _ _) expr
  = dsLExpr expr
ds_prag_expr (HsPragTick _ _ _ _) expr = do
  dflags <- getDynFlags
  if gopt Opt_Hpc dflags
    then panic "dsExpr:HsPragTick"
    else dsLExpr expr
ds_prag_expr (XHsPragE x) _ = noExtCon x

------------------------------
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsSyntaxExpr (SyntaxExprTc { syn_expr      = expr
                           , syn_arg_wraps = arg_wraps
                           , syn_res_wrap  = res_wrap })
             arg_exprs
  = do { fun            <- dsExpr expr
       ; core_arg_wraps <- mapM dsHsWrapper arg_wraps
       ; core_res_wrap  <- dsHsWrapper res_wrap
       ; let wrapped_args = zipWith ($) core_arg_wraps arg_exprs
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
                -- See Note [Desugaring RULE left hand sides] in Desugar
         then return $ mkListExpr elt_ty xs'
         else mkBuildExpr elt_ty (mk_build_list xs') }
  where
    mk_build_list xs' (cons, _) (nil, _)
      = return (foldr (App . App (Var cons)) (Var nil) xs')

dsExplicitList elt_ty (Just fln) xs
  = do { list <- dsExplicitList elt_ty Nothing xs
       ; dflags <- getDynFlags
       ; dsSyntaxExpr fln [mkIntExprInt dflags (length xs), list] }

dsArithSeq :: PostTcExpr -> (ArithSeqInfo GhcTc) -> DsM CoreExpr
dsArithSeq expr (From from)
  = App <$> dsExpr expr <*> dsLExprNoLP from
dsArithSeq expr (FromTo from to)
  = do dflags <- getDynFlags
       warnAboutEmptyEnumerations dflags from Nothing to
       expr' <- dsExpr expr
       from' <- dsLExprNoLP from
       to'   <- dsLExprNoLP to
       return $ mkApps expr' [from', to']
dsArithSeq expr (FromThen from thn)
  = mkApps <$> dsExpr expr <*> mapM dsLExprNoLP [from, thn]
dsArithSeq expr (FromThenTo from thn to)
  = do dflags <- getDynFlags
       warnAboutEmptyEnumerations dflags from (Just thn) to
       expr' <- dsExpr expr
       from' <- dsLExprNoLP from
       thn'  <- dsLExprNoLP thn
       to'   <- dsLExprNoLP to
       return $ mkApps expr' [from', thn', to']

{-
Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in DsListComp).  Basically does the translation given in the
Haskell 98 report:
-}

dsDo :: [ExprLStmt GhcTc] -> DsM CoreExpr
dsDo stmts
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

    go _ (BindStmt res1_ty pat rhs bind_op fail_op) stmts
      = do  { body     <- goL stmts
            ; rhs'     <- dsLExpr rhs
            ; var   <- selectSimpleMatchVarL pat
            ; match <- matchSinglePatVar var (StmtCtxt DoExpr) pat
                                      res1_ty (cantFailMatchResult body)
            ; match_code <- dsHandleMonadicFailure pat match fail_op
            ; dsSyntaxExpr bind_op [rhs', Lam var match_code] }

    go _ (ApplicativeStmt body_ty args mb_join) stmts
      = do {
             let
               (pats, rhss) = unzip (map (do_arg . snd) args)

               do_arg (ApplicativeArgOne _ pat expr _ fail_op) =
                 ((pat, fail_op), dsLExpr expr)
               do_arg (ApplicativeArgMany _ stmts ret pat) =
                 ((pat, noSyntaxExpr), dsDo (stmts ++ [noLoc $ mkLastStmt (noLoc ret)]))
               do_arg (XApplicativeArg nec) = noExtCon nec

           ; rhss' <- sequence rhss

           ; body' <- dsLExpr $ noLoc $ HsDo body_ty DoExpr (noLoc stmts)

           ; let match_args (pat, fail_op) (vs,body)
                   = do { var   <- selectSimpleMatchVarL pat
                        ; match <- matchSinglePatVar var (StmtCtxt DoExpr) pat
                                   body_ty (cantFailMatchResult body)
                        ; match_code <- dsHandleMonadicFailure pat match fail_op
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
        new_bind_stmt = L loc $ BindStmt bind_ty (mkBigLHsPatTupId later_pats)
                                         mfix_app bind_op
                                         noSyntaxExpr  -- Tuple cannot fail

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
                               , mg_ext = MatchGroupTc [tup_ty] body_ty
                               , mg_origin = Generated })
        mfix_pat     = noLoc $ LazyPat noExtField $ mkBigLHsPatTupId rec_tup_pats
        body         = noLoc $ HsDo body_ty
                                DoExpr (noLoc (rec_stmts ++ [ret_stmt]))
        ret_app      = nlHsSyntaxApps return_op [mkBigLHsTupId rets]
        ret_stmt     = noLoc $ mkLastStmt ret_app
                     -- This LastStmt will be desugared with dsDo,
                     -- which ignores the return_op in the LastStmt,
                     -- so we must apply the return_op explicitly

    go _ (ParStmt   {}) _ = panic "dsDo ParStmt"
    go _ (TransStmt {}) _ = panic "dsDo TransStmt"
    go _ (XStmtLR nec)  _ = noExtCon nec

dsHandleMonadicFailure :: LPat GhcTc -> MatchResult -> SyntaxExpr GhcTc -> DsM CoreExpr
    -- In a do expression, pattern-match failure just calls
    -- the monadic 'fail' rather than throwing an exception
dsHandleMonadicFailure pat match fail_op
  | matchCanFail match
  = do { dflags <- getDynFlags
       ; fail_msg <- mkStringExpr (mk_fail_msg dflags pat)
       ; fail_expr <- dsSyntaxExpr fail_op [fail_msg]
       ; extractMatchResult match fail_expr }
  | otherwise
  = extractMatchResult match (error "It can't fail")

mk_fail_msg :: DynFlags -> Located e -> String
mk_fail_msg dflags pat = "Pattern match failure in do expression at " ++
                         showPpr dflags (getLoc pat)

{-
************************************************************************
*                                                                      *
   Desugaring Variables
*                                                                      *
************************************************************************
-}

dsHsVar :: Bool  -- are we directly inside an HsWrap?
                 -- See Wrinkle in Note [Detecting forced eta expansion]
        -> Id -> DsM CoreExpr
dsHsVar w var
  | not w
  , let bad_tys = badUseOfLevPolyPrimop var ty
  , not (null bad_tys)
  = do { levPolyPrimopErr var ty bad_tys
       ; return unitExpr }  -- return something eminently safe

  | otherwise
  = return (varToCoreExpr var)   -- See Note [Desugaring vars]

  where
    ty = idType var

dsConLike :: Bool  -- as in dsHsVar
          -> ConLike -> DsM CoreExpr
dsConLike w (RealDataCon dc) = dsHsVar w (dataConWrapId dc)
dsConLike _ (PatSynCon ps)   = return $ case patSynBuilder ps of
  Just (id, add_void)
    | add_void  -> mkCoreApp (text "dsConLike" <+> ppr ps) (Var id) (Var voidPrimId)
    | otherwise -> Var id
  _ -> pprPanic "dsConLike" (ppr ps)

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
                do { case tcSplitAppTy_maybe norm_elt_ty of
                         Just (elt_m_ty, _)
                            | m_ty `eqType` topNormaliseType fam_inst_envs elt_m_ty
                            -> warnDs (Reason Opt_WarnWrongDoBind)
                                      (badMonadBind rhs elt_ty)
                         _ -> return () } } }

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
Note [Levity polymorphism invariants] in CoreSyn. But we *can* have
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
  * The body of an HsWrap is not an HsWrap.
With that representation invariant, we simply look inside every HsWrap
to see if its body is an HsVar whose Id hasNoBinding. Then, we look
at the wrapped type. If it has any levity polymorphic arguments, reject.

Interestingly, this approach does not look to see whether the Id in
question will be eta expanded. The logic is this:
  * Either the Id in question is saturated or not.
  * If it is, then it surely can't have levity polymorphic arguments.
    If its wrapped type contains levity polymorphic arguments, reject.
  * If it's not, then it can't be eta expanded with levity polymorphic
    argument. If its wrapped type contains levity polymorphic arguments, reject.
So, either way, we're good to reject.

Wrinkle
~~~~~~~
Not all polymorphic Ids are wrapped in
HsWrap, due to the lazy instantiation of TypeApplications. (See "Visible type
application", ESOP '16.) But if we spot a levity-polymorphic hasNoBinding Id
without a wrapper, then that is surely problem and we can reject.

We thus have a parameter to `dsExpr` that tracks whether or not we are
directly in an HsWrap. If we find a levity-polymorphic hasNoBinding Id when
we're not directly in an HsWrap, reject.

-}

-- | Takes an expression and its instantiated type. If the expression is an
-- HsVar with a hasNoBinding primop and the type has levity-polymorphic arguments,
-- issue an error. See Note [Detecting forced eta expansion]
checkForcedEtaExpansion :: HsExpr GhcTc -> Type -> DsM ()
checkForcedEtaExpansion expr ty
  | Just var <- case expr of
                  HsVar _ (L _ var)               -> Just var
                  HsConLikeOut _ (RealDataCon dc) -> Just (dataConWrapId dc)
                  _                               -> Nothing
  , let bad_tys = badUseOfLevPolyPrimop var ty
  , not (null bad_tys)
  = levPolyPrimopErr var ty bad_tys
checkForcedEtaExpansion _ _ = return ()

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

levPolyPrimopErr :: Id -> Type -> [Type] -> DsM ()
levPolyPrimopErr primop ty bad_tys
  = errDs $ vcat
    [ hang (text "Cannot use function with levity-polymorphic arguments:")
         2 (ppr primop <+> dcolon <+> pprWithTYPE ty)
    , hang (text "Levity-polymorphic arguments:")
         2 $ vcat $ map
           (\t -> pprWithTYPE t <+> dcolon <+> pprWithTYPE (typeKind t))
           bad_tys
    ]
