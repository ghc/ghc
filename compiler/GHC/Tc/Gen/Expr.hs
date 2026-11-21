{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Tc.Gen.Expr
       ( tcCheckPolyExpr, tcCheckPolyExprNC,
         tcCheckMonoExpr, tcCheckMonoExprNC,
         tcInferExpr, tcInferSigma,
         tcInferRho, tcInferRhoNC,
         tcMonoLExpr, tcMonoLExprNC,
         tcInferRhoFRR, tcInferRhoFRRNC,
         tcPolyLExpr,  tcPolyLExprSig, tcPolyLExprNC,
         tcPolyExpr, tcExpr,
         tcSyntaxOp, tcSyntaxOpGen, SyntaxOpType(..), synKnownType,
         tcCheckId,
         ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Tc.Gen.Splice
  ( tcTypedSplice, tcTypedBracket, tcUntypedBracket, getUntypedSpliceBody )

import GHC.Hs
import GHC.Hs.Syn.Type
import GHC.Rename.Utils
import GHC.Rename.Env         ( addUsedGRE, getUpdFieldLbls )

import GHC.Tc.Gen.Expand( tcExpand )
import GHC.Tc.Gen.App
import GHC.Tc.Gen.Head
import GHC.Tc.Gen.Do
import GHC.Tc.Gen.SourceTable (sourceTableOccurrence, optionalSourceUses, registerOptionalSourceUses)
import GHC.Tc.Gen.Bind        ( tcLocalBinds, tcNonRecLets )
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Arrow
import GHC.Tc.Gen.Match( tcBody, tcScopedRuntimeBody, tcLambdaMatches, tcCaseMatches, tcIndexedCaseMatches, tcRetainedCase
                       , tcRetainedCallCase, tcEmptyRetainedScrutinee
                       , tcOpenImplicitPackage, tcOpenImplicitPackageAt
                       , implicitPayloadType, matchesImplicitPayloadPat
                       , tcGRHSNE, tcDoStmts )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs )
import GHC.Tc.Zonk.TcType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic, hasFixedRuntimeRep )
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Env
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Evidence
import GHC.Tc.Errors.Types hiding (HoleError)

import GHC.Core.Multiplicity
import qualified GHC.Core as Core
import GHC.Core.Dependent ( hasNativeDependentExpr, supportsQuickLookType )
import GHC.Core.Utils (applyTypeToArgs)
import GHC.Core.UsageEnv
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Class(classTyCon)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.TyCo.Rep (Type(ExistsTy, ForAllTy, TermAppTy, TermVarTy, TyConApp))
import GHC.Core.Coercion
import GHC.Core.Predicate( decomposeIPPred, mkNomEqPred )

import GHC.Types.Basic
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map
import GHC.Types.Unique.Set
import GHC.Types.Var ( ExistsTyFlag(ExplicitExists, ImplicitExists), mkTyVar, isTyVar
                     , binderFlag, binderVar, isInferredForAllTyFlag
                     , isSpecifiedForAllTyFlag, isVisibleForAllTyFlag )
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Name.Set (extendNameSet)
import GHC.Types.SrcLoc

import GHC.Types.Var.Env (emptyVarEnv)

import GHC.Builtin.WiredIn.Types
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs
import GHC.Builtin.Uniques ( mkBuiltinUnique )

import GHC.Driver.Config.HsToCore.Ticks (initTicksConfig)
import GHC.Driver.Session (sccProfilingEnabled)
import GHC.HsToCore.Ticks (TicksConfig(ticks_passes), TickishType(Breakpoints))
import GHC.Driver.DynFlags

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Data.FastString (fastStringToShortText, fsLit)

import Control.Monad
import Data.List (transpose)
import qualified Data.List.NonEmpty as NE

import qualified GHC.LanguageExtensions as LangExt

{-
************************************************************************
*                                                                      *
\subsection{Main wrappers}
*                                                                      *
************************************************************************
-}

tcCheckPolyExpr, tcCheckPolyExprNC
  :: LHsExpr GhcRn         -- Expression to type check
  -> TcSigmaType           -- Expected type (could be a polytype)
  -> TcM (LHsExpr GhcTc) -- Generalised expr with expected type

-- tcCheckPolyExpr is a convenient place (frequent but not too frequent)
-- place to add context information.
-- The NC version does not do so, usually because the caller wants
-- to do so themselves.

tcCheckPolyExpr   expr res_ty = tcPolyLExpr   expr (mkCheckExpType res_ty)
tcCheckPolyExprNC expr res_ty = tcPolyLExprNC expr (mkCheckExpType res_ty)

-----------------
-- These versions take an ExpType
tcPolyLExpr, tcPolyLExprNC :: LHsExpr GhcRn -> ExpSigmaType
                            -> TcM (LHsExpr GhcTc)

tcPolyLExpr (L loc expr) res_ty
  = setSrcSpanA loc   $  -- Set the error location context first
    addExprCtxt expr  $  -- Note [Error contexts in generated code]
    do { expr' <- tcPolyExpr expr res_ty
       ; return (L loc expr') }

tcPolyLExprNC (L loc expr) res_ty
  = setSrcSpanA loc    $
    do { expr' <- tcPolyExpr expr res_ty
       ; return (L loc expr') }

-----------------
tcPolyExpr :: HsExpr GhcRn -> ExpSigmaType -> TcM (HsExpr GhcTc)
tcPolyExpr e (Infer inf) = tcExpr e (Infer inf)
tcPolyExpr e (Check ty)  = tcPolyExprCheck e (Left ty)

-----------------
tcPolyLExprSig :: LHsExpr GhcRn -> TcCompleteSig -> TcM (LHsExpr GhcTc)
tcPolyLExprSig (L loc expr) sig
  = setSrcSpanA loc $
    -- No addExprCtxt.  For (e :: ty) we don't want generate
    --    In the expression e
    --    In the expression e :: ty
    -- We have already got an error-context for (e::ty), so when we
    -- get to `e`, just add the location
    do { traceTc "tcPolyLExprSig" (ppr loc $$ ppr expr)
       ; expr' <- tcPolyExprCheck expr (Right sig)
       ; return (L loc expr') }

-----------------
tcPolyExprCheck :: HsExpr GhcRn
                -> Either TcSigmaType TcCompleteSig
                -> TcM (HsExpr GhcTc)
-- tcPolyExpCheck deals with the special case for HsLam, in case the pushed-down
-- type is a forall-type.  E.g.    (\@a -> blah) :: forall b. b -> Int
--
-- The (Either TcSigmaType TcCompleteSig) deals with:
--   Left ty:    (f e) pushes f's argument type `ty` into `e`
--   Right sig:  (e :: sig) pushes `sig` into `e`
-- The Either stuff is entirely local to this function and its immediate callers.
--
-- See Note [Skolemisation overview] in GHC.Tc.Utils.Unify

tcPolyExprCheck expr res_ty
  | any (\(ma, _) -> not (ma `tcEqType` unmatchableDataConTy)) expected_bndrs
  , case res_ty of
      Left{}  -> True
      Right{} -> bare_variable expr
  = tc_existing_value expr
  | otherwise
  = tc_skolemise expr
  where
    expected_ty = case res_ty of
      Left ty   -> ty
      Right sig -> idType (sig_bndr sig)
    (expected_bndrs, _) = splitForAllForAllTyBinders expected_ty

    -- See Note [Checking existing values at non-U foralls].
    bare_variable (HsPar _ (L _ e)) = bare_variable e
    bare_variable HsVar{} = True
    bare_variable _ = False

    tc_existing_value (HsPar x (L loc e))
      = setSrcSpanA loc $
        do { e' <- tc_existing_value e
           ; return (HsPar x (L loc e')) }
    tc_existing_value (HsUntypedSplice splice_res _)
      = do { body <- getUntypedSpliceBody splice_res
           ; tc_existing_value body }
    tc_existing_value e
      = do { inferred <- tcInferAppHead_maybe e
           ; case inferred of
               Just (e', actual_ty) ->
                 do { wrap <- case res_ty of
                        Left{} -> tcSubTypeSigma (exprCtOrigin e) ctxt actual_ty expected_ty
                        Right sig -> setSrcSpan (sig_loc sig) $
                          tcSubTypeSigma (exprCtOrigin e) ctxt actual_ty expected_ty
                    ; return (mkHsWrap wrap e') }
               Nothing -> tc_skolemise e }

    tc_skolemise expr
      = outer_skolemise res_ty $ \pat_tys rho_ty ->
        let
          -- tc_body is a little loop that looks past parentheses
          tc_body (HsPar x (L loc e))
            = setSrcSpanA loc $
              do { e' <- tc_body e
                 ; return (HsPar x (L loc e')) }

          -- Look through any untyped splices (#24559)
          -- See Note [Type Checking Template Haskell Splices]
          tc_body (HsUntypedSplice splice_res _)
            = do { body <- getUntypedSpliceBody splice_res
                 ; tc_body body }

          -- The special case for lambda: go to tcLambdaMatches, passing pat_tys
          tc_body (HsLam x lam_variant matches)
            = tcLambdaExpr x lam_variant matches pat_tys (mkCheckExpType rho_ty)

          -- The general case: just do deep skolemisation if necessary,
          -- before handing off to tcExpr
          tc_body e = do { ds_flag <- getDeepSubsumptionFlag
                         ; inner_skolemise ds_flag rho_ty $ \rho_ty' ->
                           tcExpr e (mkCheckExpType rho_ty') }
        in tc_body expr

    -- `outer_skolemise` checks expressions that need runtime abstraction.
    -- It only does shallow skolemisation
    -- It always makes an implication constraint if deferred-errors is on
    outer_skolemise :: Either TcSigmaType TcCompleteSig
                    -> ([ExpPatType] -> TcRhoType -> TcM (HsExpr GhcTc))
                    -> TcM (HsExpr GhcTc)
    outer_skolemise (Left ty) thing_inside
      = do { (wrap, expr') <- tcSkolemiseExpectedType ty thing_inside
           ; traceTc "outer_skol" (vcat [ text "wrap" <+> ppr wrap
                                        , text "expr'" <+> ppr expr'
                                        , text "wrapped" <+> ppr (mkHsWrap wrap expr') ])
           ; return (mkHsWrap wrap expr') }
    outer_skolemise (Right sig) thing_inside
      = do { (wrap, expr') <- tcSkolemiseCompleteSig sig thing_inside
           ; return (mkHsWrap wrap expr') }

    -- inner_skolemise is used when we do not have a lambda
    -- With deep skolemisation we must remember to deeply skolemise
    -- after the (always-shallow) tcSkolemiseCompleteSig
    inner_skolemise :: DeepSubsumptionFlag -> TcRhoType
                    -> (TcRhoType -> TcM (HsExpr GhcTc)) -> TcM (HsExpr GhcTc)
    inner_skolemise Shallow rho_ty thing_inside
      = -- We have already done shallow skolemisation, so nothing further to do
        thing_inside rho_ty
    inner_skolemise deep rho_ty thing_inside
      = -- Try deep skolemisation
        do { (wrap, expr') <- tcSkolemise deep ctxt rho_ty thing_inside
           ; return (mkHsWrap wrap expr') }

    ctxt = case res_ty of
             Left {}   -> GenSigCtxt
             Right sig -> sig_ctxt sig


{- Note [Checking existing values at non-U foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An existing value can have an M or variable-annotated forall type. Checking
that value does not require a new type lambda. Try inferable application
heads before skolemising a leading forall prefix with a non-U annotation.
Use normal sigma subsumption for any adaptation. Identity subsumption can
then retain an existing value without emitting U-to-annotation obligations.
Lambdas and other expressions keep the runtime abstraction checks.

Look through parentheses and untyped splices. Preserve parenthesis annotations
and source spans. Consume splice finalizers once, including on the fallback
path. Known-U prefixes keep the ordinary checking and Quick Look path.

A complete signature also brings scoped type variables into scope during
skolemisation. Before that step, infer only a bare variable under parentheses.
Other signed expressions keep the existing scoped checking path. In particular,
an inner expression signature can refer to those scoped variables.
-}


{- *********************************************************************
*                                                                      *
        tcExpr: the main expression typechecker
*                                                                      *
********************************************************************* -}

tcInferSigma :: LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcSigmaType)
tcInferSigma = tcInferExpr IIF_Sigma

tcInferRho, tcInferRhoNC :: LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)
-- Infer a *rho*-type. The return type is always instantiated.
tcInferRho   = tcInferExpr   IIF_DeepRho
tcInferRhoNC = tcInferExprNC IIF_DeepRho

tcInferRhoFRR, tcInferRhoFRRNC :: FixedRuntimeRepContext -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)
-- Infer a *rho*-type. The return type is always instantiated.
tcInferRhoFRR   frr = tc_infer_expr    (IFRR_Check frr) IIF_DeepRho
tcInferRhoFRRNC frr = tc_infer_expr_NC (IFRR_Check frr) IIF_DeepRho

tcInferExpr, tcInferExprNC :: InferInstFlag -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcType)
tcInferExpr   = tc_infer_expr    IFRR_Any
tcInferExprNC = tc_infer_expr_NC IFRR_Any

tc_infer_expr, tc_infer_expr_NC :: InferFRRFlag -> InferInstFlag
                                -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcType)
tc_infer_expr ifrr iif (L loc expr)
  = setSrcSpanA loc  $  -- Set the error location context first
    addExprCtxt expr $  -- Note [Error contexts in generated code]
    do { (expr', rho) <- runInfer iif ifrr (tcExpr expr)
       ; return (L loc expr', rho) }

tc_infer_expr_NC ifrr iif (L loc expr)
  = setSrcSpanA loc  $
    do { (expr', rho) <- runInfer iif ifrr (tcExpr expr)
       ; return (L loc expr', rho) }

---------------
tcCheckMonoExpr, tcCheckMonoExprNC
    :: LHsExpr GhcRn     -- Expression to type check
    -> TcRhoType         -- Expected type
                         -- Definitely no foralls at the top
    -> TcM (LHsExpr GhcTc)
tcCheckMonoExpr   expr res_ty = tcMonoLExpr  expr (mkCheckExpType res_ty)
tcCheckMonoExprNC expr res_ty = tcMonoLExprNC expr (mkCheckExpType res_ty)

---------------
tcMonoLExpr, tcMonoLExprNC
    :: LHsExpr GhcRn     -- Expression to type check
    -> ExpRhoType        -- Expected type
                         -- Definitely no foralls at the top
    -> TcM (LHsExpr GhcTc)

tcMonoLExpr (L loc expr) res_ty
  = setSrcSpanA loc   $ -- Set the error location context first
    addExprCtxt expr $  -- Note [Error contexts in generated code]
    do  { expr' <- tcExpr expr res_ty
        ; return (L loc expr') }

tcMonoLExprNC (L loc expr) res_ty
  = setSrcSpanA loc $
    do  { expr' <- tcExpr expr res_ty
        ; return (L loc expr') }

---------------
tcCollectApp :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
tcCollectApp the_app res_ty
  = do { (fun, args) <- splitHsApps the_app
       ; tcApp the_app fun args res_ty }

-- | Check a lambda and introduce a dot package when its payload is an arrow.
-- See Note [Implicit existential lambda payloads].
tcLambdaExpr :: XLam GhcRn -> HsLamVariant -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> [ExpPatType] -> ExpSigmaType -> TcM (HsExpr GhcTc)
tcLambdaExpr x lam_variant matches pat_tys expected
  | Check schema <- expected
  , ExistsTy ImplicitExists binder body <- coreFullView schema
  , Just _ <- splitVisibleFunTy_maybe (implicitPayloadType body)
  = do { impred <- xoptM LangExt.ImpredicativeTypes
       ; chosen <- if impred then tcDeclaredLambdaTelescope schema matches else return Nothing
       ; case chosen of
           Just witnesses -> check_selected schema witnesses
           Nothing ->
             do { declared <- if impred
                              then tcDeclaredLambdaWitness binder body matches
                              else return Nothing
                ; witness <- maybe (newFlexiTyVarTy (tyVarKind binder)) return declared
                ; check_pack schema binder body witness $ \payload_ty ->
                    tcLambdaExpr x lam_variant matches pat_tys (mkCheckExpType payload_ty) } }
  | otherwise = check_lambda expected
  where
    check_lambda expected_ty
      = do { (wrap, matches') <- tcLambdaMatches (HsLam x lam_variant matches)
                                                lam_variant matches pat_tys expected_ty
           ; return (mkHsWrap wrap (HsLam x lam_variant matches')) }

    check_pack schema binder body witness check =
      do { payload <- check (substTyWith [binder] [witness] body)
         ; return (XExpr (NativeExistsPack schema witness (noLocA payload))) }

    check_selected schema [] = check_lambda (mkCheckExpType schema)
    check_selected schema (witness : rest)
      | ExistsTy ImplicitExists binder body <- coreFullView schema
      = check_pack schema binder body witness (\payload_ty -> check_selected payload_ty rest)
      | otherwise = pprPanic "tcLambdaExpr: witness telescope" (ppr schema)

-- | Read a declared application result before entering any lambda equation.
-- See Note [Implicit existential lambda payloads].
tcDeclaredLambdaWitness :: TyVar -> Type -> MatchGroup GhcRn (LHsExpr GhcRn)
                        -> TcM (Maybe Type)
tcDeclaredLambdaWitness binder body matches =
  firstJustsM (declaredLambdaResults False body matches match_witness)
  where
    match_witness formal consumed actual =
      do { guard (not (any (`elemVarSet` tyCoVarsOfType actual) consumed))
         ; matchPolytypeExistsWitness binder formal actual }

-- | Select closed witnesses for consecutive dot binders from independent result fields.
-- See Note [Implicit existential lambda payloads].
tcDeclaredLambdaTelescope :: Type -> MatchGroup GhcRn (LHsExpr GhcRn)
                          -> TcM (Maybe [Type])
tcDeclaredLambdaTelescope schema matches
  | (binders@(_ : _ : _), body) <- telescope schema
  , all (\v -> eqType (tyVarKind v) liftedTypeKind) binders
  = do { rows <- mapMaybeM id (declaredLambdaResults True body matches (match_row binders))
       ; return $ case rows of
           [] -> Nothing
           _ -> traverse agree (transpose rows) }
  | otherwise = return Nothing
  where
    telescope ty
      | ExistsTy ImplicitExists binder body <- coreFullView ty
      = let (binders, rest) = telescope body in (binder : binders, rest)
      | otherwise = ([], ty)

    match_row binders formal consumed actual
      | TyConApp tc fields <- coreFullView formal
      , TyConApp actual_tc values <- coreFullView actual
      , tc == actual_tc, isGenerativeTyCon tc Nominal
      , traverse getTyVar_maybe fields == Just binders
      , fields `equalLength` values
      = traverse (candidate consumed) values
      | otherwise = Nothing

    candidate consumed value
      | Just var <- getTyVar_maybe value, var `elem` consumed
      = Just Nothing
      | isEmptyVarSet (tyCoVarsOfType value), supportsQuickLookType value
      = Just (Just value)
      | otherwise = Nothing

    agree candidates
      | witness : rest <- catMaybes candidates
      , not (isTauTy witness), all (eqType witness) rest
      = Just witness
      | otherwise = Nothing

-- | Inspect named RHS applications using the ordinary result declaration.
declaredLambdaResults :: Bool -> Type -> MatchGroup GhcRn (LHsExpr GhcRn)
                      -> (Type -> [TyVar] -> Type -> Maybe a) -> [TcM (Maybe a)]
declaredLambdaResults skip_invisible body (MG { mg_alts = L _ matches }) select =
    concatMap match_results matches
  where
    match_results (L _ (Match { m_pats = L _ pats
                             , m_grhss = GRHSs _ rhss (L _ EmptyLocalBinds{}) }))
      | all isVisArgLPat pats
      , Just formal <- result_after (length pats) body
      = foldr (rhs_results formal) [] rhss
    match_results _ = []

    rhs_results formal (L _ (GRHS _ _ rhs)) = result_arms rhs
      where
        result_arms expr rest
          = case unLoc (stripParensLHsExpr expr) of
              HsIf _ _ yes no -> result_arms yes (result_arms no rest)
              HsMultiIf _ rhss -> foldr (rhs_results formal) rest rhss
              HsCase _ _ (MG { mg_alts = L _ alts }) ->
                foldr (case_results formal) rest alts
              _ -> rhs_result formal expr : rest

    case_results formal (L _ (Match { m_grhss = GRHSs _ rhss (L _ EmptyLocalBinds{}) })) rest
      = foldr (rhs_results formal) rest rhss
    case_results _ _ rest = rest

    rhs_result formal rhs
      | Just (name, ntypes, args) <- application 0 [] rhs
      = do { mb_id <- if isExternalName name
                     then tcLookupIdMaybe name
                     else fmap local_id (tcLookupLcl_maybe name)
           ; declared <- traverse (liftZonkM . zonkTcType . varType) mb_id
           ; formal' <- liftZonkM (zonkTcType formal)
           ; selected <- case declared >>= type_prefix ntypes of
               Just (consumed, residual) ->
                 let (skipped, result) = if skip_invisible
                                         then tcSplitForAllInvisTyVars residual
                                         else ([], residual)
                 in fmap (>>= uncurry (select formal'))
                         (result_from (map snd skipped ++ consumed) args result)
               Nothing -> return Nothing
           ; case selected of
               Just _ -> return selected
               Nothing -> do { actual <- tcPreviewQuickLookResult rhs
                             ; return (actual >>= select formal' []) } }
    rhs_result _ _ = return Nothing

    application ntypes args expr = case unLoc (stripParensLHsExpr expr) of
      HsApp _ fun arg | ntypes == 0 -> application ntypes (arg : args) fun
      HsAppType _ fun _             -> application (ntypes + 1) args fun
      HsVar _ name -> Just (unLocWithUserRdr name, ntypes, args)
      _ -> Nothing

    type_prefix :: Int -> Type -> Maybe ([TyVar], Type)
    type_prefix 0 ty = Just ([], ty)
    type_prefix n ty
      = do { let (inferred, sigma) = tcSplitSomeForAllTyVars isInferredForAllTyFlag ty
           ; (_, specified, rest) <- tcSplitForAllTyVarBinder_maybe sigma
           ; guard (isSpecifiedForAllTyFlag (binderFlag specified))
           ; (consumed, residual) <- type_prefix (n - 1) rest
           ; return (map snd inferred ++ binderVar specified : consumed, residual) }

    result_after :: Int -> Type -> Maybe Type
    result_after 0 ty = Just ty
    result_after n ty = splitVisibleFunTy_maybe ty >>= result_after (n - 1) . snd

    result_from consumed [] ty = return (Just (consumed, ty))
    result_from consumed (arg : args) ty
      | HsEmbTy _ written <- unLoc (stripParensLHsExpr arg)
      , ForAllTy ma bndr rest <- coreFullView ty
      , isVisibleForAllTyFlag (binderFlag bndr)
      , let var = binderVar bndr
      , isTyVar var, eqType ma unmatchableDataConTy
      = do { known <- required_synonym written
           ; case known of
               Just supplied | eqType (typeKind supplied) (tyVarKind var) ->
                 result_from consumed args (substTyWith [var] [supplied] rest)
               _ -> result_from (var : consumed) args rest }
      | Just (_, rest) <- splitVisibleFunTy_maybe ty
      = result_from consumed args rest
      | otherwise = return Nothing

    required_synonym :: LHsWcType GhcRn -> TcM (Maybe Type)
    required_synonym (HsWC [] ty)
      | HsTyVar _ NotPromoted (L _ (WithUserRdr _ name)) <- unLoc (ignoreParens ty)
      , isExternalName name, isTcOcc (nameOccName name)
      = do { thing <- tcLookup name
           ; return $ case thing of
               AGlobal (ATyCon tc) | tyConArity tc == 0 -> synTyConRhs_maybe tc
               _ -> Nothing }
    required_synonym _ = return Nothing

    local_id (Just (ATcId { tct_id = ident })) = Just ident
    local_id _ = Nothing

{- Note [Implicit existential lambda payloads]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An expected dot package can hold a function. Choose a fresh witness before
checking the lambda patterns. Check the lambda once against the instantiated
payload type. A witness introduced by a pattern cannot escape its implication
to fill that metavariable. The existential binder does not enter source scope.

With ImpredicativeTypes, a lambda can take a higher-rank witness from a
declared result type. For example, check (\b -> prepare b) against
  exists t. Bool -> Callback t
when prepare :: Bool -> Callback PolyId. Match the invariant result types
and check the lambda at Bool -> Callback PolyId.

Figure 10 of Eisenberg et al., "An Existential Crisis Resolved", uses the
expected payload to guide witness selection. Here the result declaration
fixes the choice before the lambda patterns enter scope. Read the head's
declared type and follow its written ordinary application arrows. The
ordinary check instantiates types, checks operands and records usage.
For a guarded or multi-equation lambda, read the first matching result
declaration in source order. An ordinary if adds its two result arms
in that order. It introduces no binders, so both arms can use the same
declaration lookup. The predicate and the result arms still go through
the ordinary expression check. Rebindable if is already an expanded
application at this stage. The selected declaration supplies no local
evidence. A head bound by a pattern or guard is unavailable at this
point. A multi-way if adds its result arms in guard order. Pattern guards
bring their binders and Givens into scope during the ordinary check.
A case adds the right-hand sides of its alternatives in source order.
Skip alternatives with local bindings. The probe uses the outer environment
for every lookup. Case patterns and their Givens enter scope during normal
checking, after the witness is selected. Every result checks with the same
witness. Exact invariant matching chooses a type without filling any
Quick Look variables. Runtime indices use their existing checks.

The head can have a leading visible type application prefix. For example,
  \b -> configured @Bool id b
can use the result of configured :: forall t. (t -> Bool) -> t -> Callback PolyId.
Walk the declared inferred and specified binders in IALL/ITYARG order.
Their choices can affect the domains of later value arguments. Require the
result type to be independent of those binders, including in its kinds.
The ordinary application checker instantiates the binders and checks each
written operand at its original location.
See Note [Visible type application and abstraction] in GHC.Tc.Gen.App.

If the declaration does not fix the witness, Quick Look can use the
call's arguments to find a result type. This covers
`\b -> id (prepare b)` with the same declaration for prepare.
See Note [Quick Look before lambda patterns] in GHC.Tc.Gen.App.

An explicitly marked required argument, such as (type PolyId), can
also fix the result type. Read only a closed, saturated type synonym
of the binder's kind. This inspects the declaration before entering
the lambda's pattern scope. An unread argument can still leave a
result that is independent of its binder. The ordinary checker
validates the type argument and checks the application in both cases.

For consecutive lifted dot binders, the same declared results can
determine separate witnesses. For example, one result can have the
form Callback PolyId r and another Callback l PolyMaybe. The
payload must use the binders as distinct direct parameters of one
nominally generative type constructor, in binder order. A bare
producer type variable supplies no candidate. Each selected field
must have one agreed closed polytype. This probe introduces no
constraints and does not check a lambda branch. Check the original
match group once at the selected payload type, and then build the
Packs in telescope order.

Keep the ordinary match checker, including its deep skolemisation and usage
checks. The outer forall wrapper remains outside the package when tcPolyExprCheck
skolemises the expected type. Pack erases to the checked lambda in CoreToStg.
The lambda body still waits for application.
-}

---------------
tcExpr :: HsExpr GhcRn
       -> ExpRhoType   -- DeepSubsumption <=> when checking, this type
                       --                     is deeply skolemised
       -> TcM (HsExpr GhcTc)

-- Use tcApp to typecheck applications, which are treated specially
-- by Quick Look.  Specifically:
--   - HsVar           lone variables, to ensure that they can get an
--                     impredicative instantiation (via Quick Look
--                     driven by res_ty (in checking mode)).
--   - ExprWithTySig   (e :: type)
--   - HsRecSel        overloaded record fields
--   - HsOpApp         operator applications
--   - HsOverLit       overloaded literals
--   - HsApp           value applications
--   - HsAppType       type applications
--   - HsAppRetained   explicit invisible retained applications

-- These constructors are the union of
--   - ones taken apart by GHC.Tc.Gen.Head.splitHsApps
--   - ones understood by GHC.Tc.Gen.Head.tcInferAppHead_maybe
-- HsType and HsTypeApp are a little special as
-- before calling tcApp on them we run splitHsApps on the
-- expression and try to break them up further
-- and then call tcApp on the maximal application chain.
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
tcExpr e@(HsVar {})              res_ty = tcApp e e [] res_ty
tcExpr e@(ExprWithTySig {})      res_ty = tcApp e e [] res_ty
tcExpr e@(XExpr (HsRecSelRn{}))  res_ty = tcApp e e [] res_ty
tcExpr e@(HsAppType {})          res_ty = tcCollectApp e res_ty
tcExpr e@(HsAppRetained {})      res_ty = tcCollectApp e res_ty
tcExpr e@(HsApp {})              res_ty = tcCollectApp e res_ty

-- Typecheck an occurrence of an unbound Id
--
-- Some of these started life as a true expression hole "_".
-- Others might simply be variables that accidentally have no binding site.
tcExpr (HsHole (HoleVar locc@(L _ occ))) res_ty
  = do { ty <- expTypeToType res_ty    -- Allow Int# etc (#12531)
       ; her <- emitNewExprHole occ ty
       ; tcEmitBindingUsage bottomUE   -- Holes fit any usage environment
                                       -- (#18491)
       ; return (HsHole (HoleVar locc, her))
       }
tcExpr (HsHole HoleError) _ =
  panic "GHC.Tc.Gen.Expr: tcExpr: HoleError: Not implemented"

tcExpr e@(HsLit x lit) res_ty
  = do { let lit_ty = hsLitType lit
       ; tcWrapResult e (HsLit x (convertLit lit)) lit_ty res_ty }

tcExpr (HsPar x expr) res_ty
  = do { expr' <- tcMonoLExprNC expr res_ty
       ; return (HsPar x expr') }

tcExpr (HsPragE x prag expr) res_ty
  = do { expr' <- tcMonoLExpr expr res_ty
       ; return (HsPragE x (tcExprPrag prag) expr') }

tcExpr (NegApp x expr neg_expr) res_ty
  = do  { (expr', neg_expr')
            <- tcSyntaxOp NegateOrigin neg_expr [SynAny] res_ty $
               \[arg_ty] [arg_mult] ->
               tcScalingUsage arg_mult $ tcCheckMonoExpr expr arg_ty
        ; return (NegApp x expr' neg_expr') }

tcExpr e@(HsIPVar _ x) res_ty
  = do { ip_ty <- newFlexiTyVarTy liftedTypeKind
          -- Create a unification type variable of kind 'Type'.
          -- (The type of an implicit parameter must have kind 'Type'.)
       ; let ip_name = mkStrLitTy (hsIPNameFS x)
             origin  = IPOccOrigin x
       ; ip_class <- tcLookupKnownKeyClass ipClassKey
       ; let ip_pred = mkClassPred ip_class [ip_name, ip_ty]
       ; ip_dict <- emitWantedEvVar origin ip_pred
       ; let (ip_op, _) = decomposeIPPred ip_pred
             wrap = mkWpEvVarApps [ip_dict] <.> mkWpTyApps [ip_name, ip_ty]
       ; tcWrapResult e
               (mkHsWrap wrap (mkHsVar (noLocA ip_op)))
               ip_ty res_ty }

tcExpr (HsLam x lam_variant matches) res_ty
  = tcLambdaExpr x lam_variant matches [] res_ty

{-
************************************************************************
*                                                                      *
                Overloaded literals
*                                                                      *
************************************************************************
-}

tcExpr e@(HsOverLit _ lit) res_ty
  = -- See Note [Typechecking overloaded literals]
    do { mb_res <- tcShortCutLit lit res_ty
         -- See Note [Short cut for overloaded literals] in GHC.Tc.Utils.TcMType
       ; case mb_res of
           Just lit' -> return (HsOverLit noExtField lit')
           Nothing   -> tcApp e e [] res_ty }
           -- Why go via tcApp? See Note [Typechecking overloaded literals]

{- Note [Typechecking overloaded literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, an overloaded literal like "3" typechecks as if you
had written (fromInteger (3 :: Integer)).   But in practice it's a little
tricky:

* Rebindable syntax (see #19154 and !4981). With rebindable syntax we might have
     fromInteger :: Integer -> forall a. Num a => a
  and then we might hope to use a Visible Type Application (VTA) to write
     3 @Int
  expecting it to expand to
     fromInteger (3::Integer) @Int dNumInt
  To achieve that, we need to
    * treat the application using `tcApp` to deal with the VTA
    * treat the overloaded literal as the "head" of an application;
      see `GHC.Tc.Gen.Head.tcInferAppHead`.

* Short-cutting.  If we have
     xs :: [Int]
     xs = [3,4,5,6... ]
  then it's a huge short-cut (in compile time) to just cough up the `Int` literal
  for `3`, rather than (fromInteger @Int d), with a wanted constraint `[W] Num Int`.
  See Note [Short cut for overloaded literals] in GHC.Tc.Utils.TcMType.

  We can only take this short-cut if rebindable syntax is off; see `tcShortCutLit`.
-}

tcExpr e@HsQualLit{} _ = pprPanic "tcExpr: HsQualLit" (ppr e)

{-
************************************************************************
*                                                                      *
                Explicit lists
*                                                                      *
************************************************************************
-}

-- Explicit lists [e1,e2,e3] have been expanded already in the renamer
-- The expansion includes an ExplicitList, but it is always the built-in
-- list type, so that's all we need concern ourselves with here.  See
-- GHC.Rename.Expr. Note [Handling overloaded and rebindable constructs]
tcExpr expr@(ExplicitList _ exprs) res_ty
  = do { anchor <- case res_ty of
             Infer _ -> do { enabled <- xoptM LangExt.ExperimentalDependentTypes
                           ; if enabled then find_native [] exprs
                                        else return Nothing }
             _ -> return Nothing
       ; case anchor of
           Just (before, element, after)
             -> do { (element', elt_ty) <- tcInferRho element
                   ; _ <- setSrcSpanA (getLoc element) $ addExprCtxt (unLoc element) $
                            unifyType Nothing (typeKind elt_ty) liftedTypeKind
                   ; before' <- mapM (`tcCheckPolyExpr` elt_ty) before
                   ; after' <- mapM (`tcCheckPolyExpr` elt_ty) after
                   ; tcWrapResultMono expr
                       (ExplicitList elt_ty (before' ++ (element' : after')))
                       (mkListTy elt_ty) res_ty }
           Nothing
             -> do { res_ty <- expTypeToType res_ty
                   ; (coi, elt_ty) <- matchExpectedListTy res_ty
                   ; exprs' <- mapM (`tcCheckPolyExpr` elt_ty) exprs
                   ; return $ mkHsWrapCo coi $ ExplicitList elt_ty exprs' } }
  where
    -- Select a declared type without checking an occurrence or collecting usage.
    find_native :: [LHsExpr GhcRn] -> [LHsExpr GhcRn]
                -> TcM (Maybe ([LHsExpr GhcRn], LHsExpr GhcRn, [LHsExpr GhcRn]))
    find_native before (element : after)
      = do { native <- native_element element
           ; if native then return (Just (reverse before, element, after))
                       else find_native (element : before) after }
    find_native _ [] = return Nothing

    native_element :: LHsExpr GhcRn -> TcM Bool
    native_element (L _ (HsPar _ element)) = native_element element
    native_element (L _ (HsVar _ lname))
      | not (isDataOcc (nameOccName (unLocWithUserRdr lname)))
      = do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
           ; case mb_id of
               Just var -> do
                 { ty <- liftZonkM (zonkTcType (varType var))
                 ; return (isTauTy ty && hasNativeDependentExpr (Core.Type ty)
                           && typeKind ty `eqType` liftedTypeKind) }
               Nothing -> return False }
    native_element _ = return False

{- Note [Inference of native list elements]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If vector :: Vec Int n, inferring [vector] must retain that exact element
type. The ordinary list path allocates an element meta-variable first.
It cannot contain the runtime Id n.

Under ExperimentalDependentTypes, an inferred list can use a declared
monomorphic native variable as its element type. Look through parentheses.
Read and zonk its declared type without checking its occurrence or emitting
usage. Do not expand splices or infer an application to select this type.
The type must be a lifted tau type that already contains native syntax.

Infer the selected occurrence once with tcInferRho. This retains its usual
usage and application checks. Check its lifted kind and check all other
elements against its exact inferred type. An earlier bottom element can then
use the same expected type. Keep every typed element in its original position.
This changes no runtime evaluation order and adds no sharing binding.

Use tcWrapResultMono with the original InferResult. Keep its level and
promotion checks. Surrounding application and callback scope checks remain
active. A list does not bind or hide a free runtime index. Do not write a
native type to an element meta-variable. Known expected types, empty lists
and lists without a declared native element keep ordinary list checking.
Polymorphic variables and checking-only expressions do not select this route.
-}

tcExpr expr@(ExplicitTuple x tup_args boxity) res_ty
  | all tupArgPresent tup_args
  = do { as_application <- case res_ty of
             Check expected | Boxed <- boxity
                            , ExistsTy ImplicitExists _ _ <- coreFullView expected
               -> return True
             Infer _ | Boxed <- boxity
               -> do { enabled <- xoptM LangExt.ExperimentalDependentTypes
                     ; if enabled
                       then snd <$> tcRetainedInferableArg (noLocA expr)
                       else return False }
             _ -> return False
       ; if as_application
         then do { checkTupSize (length tup_args)
                 ; let con = wrapGenSpan (mkHsVar
                               (noLocA (dataConName (tupleDataCon Boxed (length tup_args)))))
                       app = foldl' (\fun arg -> wrapGenSpan (HsApp noExtField fun arg))
                                    con [arg | Present _ arg <- tup_args]
                 -- See Note [Inference of native tuple fields].
                 ; tcCollectApp (mkExpandedExpr expr (unLoc app)) res_ty }
         else do
       { let arity  = length tup_args
             tup_tc = tupleTyCon boxity arity
               -- NB: tupleTyCon doesn't flatten 1-tuples
               -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make
       ; res_ty <- expTypeToType res_ty
       ; (coi, arg_tys) <- matchExpectedTyConApp tup_tc res_ty
                           -- Unboxed tuples have RuntimeRep vars, which we
                           -- don't care about here
                           -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
       ; let arg_tys' = case boxity of Unboxed -> drop arity arg_tys
                                       Boxed   -> arg_tys
       ; tup_args1 <- tcCheckExplicitTuple [] tup_args arg_tys'
       ; return $ mkHsWrapCo coi (ExplicitTuple x tup_args1 boxity) } }

  | otherwise
  = -- The tup_args are a mixture of Present and Missing (for tuple sections).
    do { known_tys <- case (boxity, res_ty) of
             (Boxed, Check expected)
               -> do { enabled <- xoptM LangExt.ExperimentalDependentTypes
                     ; if not enabled then return Nothing else
                  do { expected <- liftZonkM (zonkTcType expected)
                     ; let known
                             | hasNativeDependentExpr (Core.Type expected)
                             , Right (_mas, missing, result) <- tcSplitFunTysN
                                 (count (not . tupArgPresent) tup_args) expected
                             , Just (tc, tys) <- tcSplitTyConApp_maybe result
                             , tc == tupleTyCon Boxed (length tup_args)
                             = Just (map scaledMult missing, tys)
                             | otherwise = Nothing
                     ; return known } }
             _ -> return Nothing
       ; (tup_args1, arg_tys) <- case known_tys of
             Just (mults, tys)
               -> do { args <- tcCheckExplicitTuple mults tup_args tys
                     ; return (args, tys) }
             Nothing -> tcInferTupArgs boxity tup_args

       ; let expr'       = ExplicitTuple x tup_args1 boxity
             missing_tys = [Scaled mult ty | (Missing (Scaled mult _), ty) <- zip tup_args1 arg_tys]

             -- See Note [Typechecking data constructors] in GHC.Tc.Gen.Head
             -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make
             act_res_ty = mkScaledFunTysU missing_tys (mkTupleTy1 boxity arg_tys)

       ; traceTc "ExplicitTuple" (ppr act_res_ty $$ ppr res_ty)

       ; tcWrapResultMono expr expr' act_res_ty res_ty }

tcExpr (ExplicitSum _ alt arity expr) res_ty
  = do { let sum_tc = sumTyCon arity
       ; res_ty <- expTypeToType res_ty
       ; (coi, arg_tys) <- matchExpectedTyConApp sum_tc res_ty
       ; -- Drop levity vars, we don't care about them here
         let arg_tys' = drop arity arg_tys
             arg_ty   = arg_tys' `getNth` (alt - 1)
       ; expr' <- tcCheckPolyExpr expr arg_ty
       -- Check the whole res_ty, not just the arg_ty, to avoid #20277.
       -- Example:
       --   a :: TYPE rep (representation-polymorphic)
       --   (# 17# | #) :: (# Int# | a #)
       -- This should cause an error, even though (17# :: Int#)
       -- is not representation-polymorphic: we don't know how
       -- wide the concrete representation of the sum type will be.
       ; hasFixedRuntimeRep_syntactic (FRRUnboxedSum Nothing) res_ty
       ; return $ mkHsWrapCo coi (ExplicitSum arg_tys' alt arity expr' ) }


{-
************************************************************************
*                                                                      *
                Let, case, if, do
*                                                                      *
************************************************************************
-}

tcExpr original@(HsLet x local_binds@(L loc binds) body) res_ty
  = do { opened <- tcLexicalExistsLet original local_binds body res_ty
       ; case opened of
           Just expr -> return expr
           Nothing -> do
             { enabled <- xoptM LangExt.ExperimentalDependentTypes
             ; expansion <- case res_ty of
                   Check expected | enabled
                                  , hasNativeDependentExpr (Core.Type expected)
                                    || (isTauTy expected && not (anyFreeVarsOfType
                                                  (\v -> isTyVar v && isMetaTyVar v) expected)) ->
                     do { strict <- xoptM LangExt.Strict
                        ; if strict then return Nothing else tcNativeLet original local_binds body expected }
                   _ -> return Nothing
             ; case expansion of
                 Just expr -> return expr
                 Nothing -> do { let names = collectLocalBinders CollNoDictBinders binds
                               ; (binds', body') <- tcLocalBinds binds (tcScopedRuntimeBody names body res_ty)
                               ; return (HsLet x (L loc binds') body') } } }

tcExpr expr@(HsCase ctxt scrut matches) res_ty
  = do { opened <- tcExistsCase expr res_ty
       ; case opened of
           Just unpack -> return unpack
           Nothing -> do
             { mult <- newFlexiTyVarTy multiplicityTy
             ; head_app <- case unLoc (stripParensLHsExpr scrut) of
                 HsApp{} -> tcImplicitOperand scrut
                 _ -> return Nothing
             ; case head_app of
                 Just (package, Just rebuild) -> open_call mult package rebuild
                 _ -> ordinary_case mult scrut res_ty } }
  where
    -- See Note [Case on a call with an implicit package head].
    open_call mult package rebuild
      = do { (checked, schema) <- tcScalingUsage mult $ tcInferRho package
           ; result_ty <- expTypeToType res_ty
           ; opened <- tcOpenImplicitPackage (getLocA package) schema checked result_ty $ \payload ->
               let ref = nlHsVar (getName payload)
                   source = rebuild (fmap (\head_expr -> mkExpandedLExpr head_expr ref) package)
               in tcExtendIdEnv [payload] $
                    ordinary_case mult source (mkCheckExpType result_ty)
           ; return (preserve_origin opened) }

    ordinary_case mult source expected = do
               {  -- We used to typecheck the case alternatives first.
               -- The case patterns tend to give good type info to use
               -- when typechecking the scrutinee.  For example
               --   case (map f) of
               --     (x:xs) -> ...
               -- will report that map is applied to too few arguments
               --
               -- But now, in the GADT world, we need to typecheck the scrutinee
               -- first, to get type info that may be refined in the case alternatives
              -- Typecheck the scrutinee.  We use tcInferRho but tcInferSigma
              -- would also be possible (tcCaseMatches accepts sigma-types)
              -- Interesting litmus test: do these two behave the same?
              --     case id        of {..}
              --     case (\v -> v) of {..}
              -- This design choice is discussed in #17790
              (operand, operand_ty) <- tcScalingUsage mult $ tcInferRhoFRR FRRCase source
            ; (scrut', scrut_ty) <- if null (unLoc (mg_alts matches))
                                     then tcEmptyRetainedScrutinee operand operand_ty
                                     else pure (operand, operand_ty)
            ; dependent <- xoptM LangExt.ExperimentalDependentTypes
            ; special <- case unLoc (stripParensLHsExpr scrut') of
                _ | ExistsTy ImplicitExists _ _ <- coreFullView scrut_ty
                  -> dot_case scrut' scrut_ty mult expected
                HsVar _ (L _ index) | not dependent || isNativeIndexId index ->
                  tcRetainedCase dependent index matches expected
                _ | dependent ->
                  do { call_index <- checkedNativeCallIndex scrut' scrut_ty
                     ; case call_index of
                         Nothing -> return Nothing
                         Just index ->
                           fmap (fmap preserve_origin) $
                             tcRetainedCallCase index scrut' matches expected }
                _ -> return Nothing
            ; case special of
                Just expr -> return expr
                Nothing -> do
                  { -- See Note [Unrestricted data family case fields].
                    case unLoc (stripParensLHsExpr scrut') of
                      HsVar _ (L _ index)
                        | dependent, isNativeOpaqueValueId index
                        , Just (tc, _) <- splitTyConApp_maybe scrut_ty
                        , isDataFamilyTyCon tc ->
                            tcEqMult (OccurrenceOf (idName index)) mult ManyTy
                      _ -> return ()
                  ; let check_matches = case unLoc (stripParensLHsExpr scrut') of
                          HsVar _ (L _ index) | dependent, isNativeOpaqueValueId index
                            , not (isNativeIndexId index), Check result <- expected
                            , anyFreeVarsOfType (== index) result ->
                                tcIndexedCaseMatches index ctxt
                          _ -> tcCaseMatches ctxt tcBody
                  ; matches' <- check_matches (Scaled mult scrut_ty) matches expected
                  ; return (HsCase ctxt scrut' matches') } }

    preserve_origin expr' =
      XExpr (ExpandedThingTc (HSE { hse_ctxt = ExprCtxt expr, hse_exp = noLocA expr' }))

    dot_case scrut' schema mult expected
      | any (payload_alt schema) (unLoc (mg_alts matches))
      = do { result_ty <- expTypeToType expected
           ; opened <- tcOpenImplicitPackage (getLocA scrut) schema scrut' result_ty $ \payload ->
               do { payload_matches <- tcCaseMatches ctxt tcBody
                        (Scaled mult (varType payload)) matches (mkCheckExpType result_ty)
                  ; return (HsCase ctxt (nlHsVar payload) payload_matches) }
           ; return (Just (preserve_origin opened)) }
      | otherwise = return Nothing

    -- See Note [Matching an implicit existential payload].
    payload_alt schema (L _ Match{m_pats = L _ [pat]}) = matchesImplicitPayloadPat schema pat
    payload_alt _ _ = False

{- Note [Unrestricted data family case fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The selector in `case c of Stored x -> tag (pick x)` can use the
constructor's class Given. Its retained operand needs an unrestricted
field. If the case multiplicity is still a metavariable, the constructor
implication can make it untouchable. The retained operand check cannot
then establish Many with reflexive evidence.

For a checked local data family scrutinee whose binding is already
unrestricted, choose Many before checking the alternatives. The case
can use this variable at Many and scale its fields at Many, regardless
of whether the result names the scrutinee. Keep fresh multiplicity
inference for linear variables and compound scrutinees.
-}

{- Note [Case on a call with an implicit package head]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `case builder b of Callback value use -> use value`, builder has type
`exists t. Bool -> Callback t`. The call produces Callback t inside the
opening of builder. The result of the complete case can have a closed type.

Use the same declared call shape as implicit application operands. Check the
package head once. Open it around both the call on its payload and the case
alternatives. Allocate the case result type outside the opening so a field
that mentions the hidden witness cannot escape. Scale the checked head and
the rebuilt application at the original scrutinee multiplicity. The ordinary
case checker keeps the source patterns and their evaluation order.
-}

tcExpr (HsIf x pred b1 b2) res_ty
  = do { pred'    <- tcCheckMonoExpr pred boolTy
       ; let res_ty' = adjustExpTypeForCaseBranches res_ty [b1,b2]
       ; (u1,b1') <- tcCollectingUsage $ tcMonoLExpr b1 res_ty'
       ; (u2,b2') <- tcCollectingUsage $ tcMonoLExpr b2 res_ty'
       ; tcEmitBindingUsage (supUE u1 u2)
       ; return (HsIf x pred' b1' b2') }

{-
Note [MultiWayIf linearity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we'd like to compute the usage environment for

if | b1 -> e1
   | b2 -> e2
   | otherwise -> e3

and let u1, u2, v1, v2, v3 denote the usage env for b1, b2, e1, e2, e3
respectively.

Since a multi-way if is mere sugar for nested if expressions, the usage
environment should ideally be u1 + sup(v1, u2 + sup(v2, v3)).
However, currently we don't support linear guards (#19193). All variables
used in guards from u1 and u2 will have multiplicity Many.
But in that case, we have equality u1 + sup(x,y) = sup(u1 + x, y),
                      and likewise u2 + sup(x,y) = sup(u2 + x, y) for any x,y.
Using this identity, we can just compute sup(u1 + v1, u2 + v2, v3) instead.
This is simple to do, since we get u_i + v_i directly from tcGRHS.
If we add linear guards, this code will have to be revisited.
Not using 'sup' caused #23814.
-}

tcExpr (HsMultiIf _ alts) res_ty
  = do { alts' <- tcGRHSNE IfAlt tcBody alts res_ty
                  -- See Note [MultiWayIf linearity checking]
       ; res_ty <- readExpType res_ty
       ; return (HsMultiIf res_ty alts') }

tcExpr (HsDo _ do_or_lc stmts) res_ty
  | DoExpr{} <- do_or_lc
  -- ApplicativeDo are typechecked using tcDoStmts
  = do isApplicativeDo <- xoptM LangExt.ApplicativeDo
       if isApplicativeDo
         then tcDoStmts do_or_lc stmts res_ty
         -- Expand expression on the fly otherwise
         -- See Note [Typechecking by expansion: overview]
         else do { hse <- expandDoStmts do_or_lc stmts
                 ; tcHsExpansion hse res_ty }
  | MDoExpr{} <- do_or_lc
  = do hse <- expandDoStmts do_or_lc stmts
       tcHsExpansion hse res_ty
  | otherwise
  -- ListComp and MonadComp are handled by legacy tcDoStmts for now,
  -- The ultimate goal is to handle them via expandDoStmts.
  -- GHCiStmts are handled completely separate
  = tcDoStmts do_or_lc stmts res_ty

tcExpr (HsProc x pat cmd) res_ty
  = do  { (pat', cmd', coi) <- tcProc pat cmd res_ty
        ; return $ mkHsWrapCo coi (HsProc x pat' cmd') }

-- Typechecks the static form and wraps it with a call to 'fromStaticPtr'.
-- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable for an overview.
-- To type check
--      (static e) :: p a
-- we want to check (e :: a),
-- and wrap (static e) in a call to
--    fromStaticPtr :: IsStatic p => StaticPtr a -> p a

tcExpr (HsStatic free_names expr) res_ty
  = do  { res_ty          <- expTypeToType res_ty
        ; (co, (p_ty, expr_ty)) <- matchExpectedAppTy res_ty
        ; (expr', lie) <- captureConstraints $
                          updLclCtxt (\ctxt -> ctxt { tcl_lexical_givens = emptyLexicalGivens
                                                   , tcl_source_let_views = emptyVarEnv }) $
                          addErrCtxt (StaticFormCtxt expr) $
                          tcCheckPolyExprNC expr expr_ty

        -- Check that the free variables of the static form are top-level defined
        -- It's OK to use nonDetEltsUniqSet here as the only side effects of
        -- checkClosedInStaticForm are error messages.
        -- See (SF2) Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable
        ; mapM_ check_free_name (nonDetEltsUniqSet free_names)

        -- Emit an implication that captures the constraints of `expr`,
        -- but with a `ic_info` of StaticFormSkol
        -- See #13499 for an explanation of why this is the right thing to do:
        -- the enclosing skolems must be in scope.
        ; tc_lvl <- getTcLevel  -- No need to bump the level
        ; (implic, ev_binds) <- buildImplicationFor tc_lvl StaticFormSkol [] [] lie
        ; emitImplications implic
        ; let expr'' = mkLHsWrap (mkWpLet ev_binds) expr'

        -- Require the type of the argument to be Typeable.
        ; typeableClass <- tcLookupKnownKeyClass typeableClassKey
        ; typeable_ev <- emitWantedEvVar StaticOrigin $
                         mkTyConApp (classTyCon typeableClass)
                                    [liftedTypeKind, expr_ty]

        -- Wrap the static form with the 'fromStaticPtr' call.
        --   fromStaticPtr :: forall p. (IsStatic p) =>
        --                    forall a. (Typeable a) =>
        --                    StaticPtr a -> p a
        ; fromStaticPtr <- newKnownOccMethod StaticOrigin
                                    fromStaticPtrClassOpOcc [p_ty]
        ; static_ptr_ty_con <- tcLookupKnownOccTyCon staticPtrTyConOcc
        ; let wrap = mkWpEvVarApps [typeable_ev] <.> mkWpTyApps [expr_ty]
              static_expr_ty = mkTyConApp static_ptr_ty_con [expr_ty]
        ; return $ mkHsWrapCo co $
          HsStatic (static_expr_ty, mkHsWrap wrap fromStaticPtr)
                   expr''
        }
  where
    check_free_name :: Name -> TcM ()
    -- Check for free /term/ vars not defined at top level
    -- We use isExternalName as a proxy for top-level-defined
    check_free_name n
      = do { mb_thing <- tcLookupLcl_maybe n
           ; case mb_thing of
               Nothing  -> return ()  -- Imports, tycons, classes allowed
               Just (ATcId {})  -> unless (isExternalName n) $
                                   addErrTc (TcRnStaticFormNotClosed n)

               Just (ATyVar {}) -> return ()  -- Free type variables are allowed
               Just ATcTypeLet{} -> return ()
               Just ATcCaseLet{} -> return ()

                   -- Not really expecting these, but we'll get an error from
                   -- elsewhere, so don't produce an error here
               Just (ATcTyCon {})      -> return ()
               Just (APromotionErr {}) -> return ()
               Just (AGlobal {})       -> return () }

-- See Note [Reflecting a checked type operand].
tcExpr expr@(HsEmbTy _ written) expected
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; unless enabled $ failWith (TcRnIllegalTypeExpr TypeKeywordSyntax)
       ; case expected of
           Check ty
             | Just (tc, [kind, _]) <- splitTyConApp_maybe ty ->
               do { selector <- tcLookupKnownOccId typeRepIdOcc
                  ; let (_, _, rep) = tcSplitSigmaTy (idType selector)
                  ; case splitTyConApp_maybe rep of
                      Just (rep_tc, _) | tc == rep_tc ->
                        do { arg <- tcHsTypeApp written kind
                           ; let args = [kind, arg]
                                 (theta, actual) = tcSplitPhiTy (piResultTys (idType selector) args)
                           ; wrap <- instCall (exprCtOrigin expr) args theta
                           ; let call = mkHsWrap wrap (mkHsVar (noLocA selector))
                           ; result <- tcWrapResultMono expr call actual expected
                           ; return (XExpr (ExpandedThingTc (HSE (ExprCtxt expr) (noLocA result)))) }
                      _ -> wrong_context (Just ty) }
             | otherwise -> wrong_context (Just ty)
           Infer {} -> wrong_context Nothing }
  where
    wrong_context = failWith . TcRnRetainedQuantificationError . RetainedTypeOperandContext
tcExpr (HsQual _ _ _)     _ = failWith (TcRnIllegalTypeExpr ContextArrowSyntax)
tcExpr (HsForAll _ _ _)   _ = failWith (TcRnIllegalTypeExpr ForallTelescopeSyntax)
tcExpr (HsFunArr _ _ _ _) _ = failWith (TcRnIllegalTypeExpr FunctionArrowSyntax)
tcExpr (HsStar _)         _ = failWith (TcRnIllegalTypeExpr StarKindSyntax)

{- Note [Reflecting a checked type operand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an expression checked against TypeRep t, ExperimentalDependentTypes
allows (type t) to supply the runtime witness. Check the written type at
the expected kind and instantiate the same selector as typeRep. This
requires Typeable t, including when t is an abstract type variable.
Identify TypeRep from the selector's result so that aliases work and a
locally declared type with the same name keeps its ordinary meaning.

The caller can learn the expected witness type from an imported signature.
See Note [Explicit retained Type witnesses] in GHC.Tc.Gen.HsType.
A required forall argument takes a separate path in GHC.Tc.Gen.App.
It remains erased and requires no Typeable dictionary.
-}

{-
************************************************************************
*                                                                      *
                Record construction and update
*                                                                      *
************************************************************************
-}

tcExpr expr@(RecordCon { rcon_con = L loc qcon@(WithUserRdr _ con_name)
                       , rcon_flds = rbinds }) res_ty
  = do  { con_like <- tcLookupConLike qcon

        ; (con_expr, con_sigma) <- tcInferConLike con_like
        ; (con_wrap, con_tau)   <- topInstantiate orig con_sigma
              -- a shallow instantiation should really be enough for
              -- a data constructor.
        ; let arity = conLikeArity con_like
              Right (_mas, arg_tys, actual_res_ty) = tcSplitFunTysN arity con_tau

        ; checkTc (conLikeHasBuilder con_like) $
          nonBidirectionalErr (conLikeName con_like)

        ; rbinds' <- tcRecordBinds con_like arg_tys rbinds

        ; let rcon_tc = mkHsWrap con_wrap con_expr
              expr' = RecordCon { rcon_ext = rcon_tc
                                , rcon_con = L loc con_like
                                , rcon_flds = rbinds' }

        ; ret <- tcWrapResultMono expr expr' actual_res_ty res_ty

        -- Check for missing fields.  We do this after type-checking to get
        -- better types in error messages (cf #18869).  For example:
        --     data T a = MkT { x :: a, y :: a }
        --     r = MkT { y = True }
        -- Then we'd like to warn about a missing field `x :: True`, rather than `x :: a0`.
        --
        -- NB: to do this really properly we should delay reporting until typechecking is complete,
        -- via a new `HoleSort`.  But that seems too much work.
        ; checkMissingFields con_like rbinds arg_tys

        ; return ret }
  where
    orig = OccurrenceOf con_name

-- Record updates via dot syntax are replaced by expanded expressions
-- in the renamer. See Note [Overview of record dot syntax] in
-- GHC.Hs.Expr. This is why we match on 'rupd_flds = Left rbnds' here
-- and panic otherwise.
-- WIP: To be fixed soon expandRecordUpd needs to return HsExpansion and not a separate ds_res_ty
tcExpr expr@(RecordUpd { rupd_expr = record_expr
                       , rupd_flds =
                           RegularRecUpdFields
                             { xRecUpdFields = possible_parents
                             , recUpdFields  = rbnds }
                       })
       res_ty
  = assert (notNull rbnds) $ mkExpandedExprTc expr <$>
    do  { -- Expand the record update. See Note [Record Updates].

        ; (ds_expr, ds_res_ty, err_msg)
            <- expandRecordUpd record_expr possible_parents rbnds res_ty
        ; addErrCtxt err_msg $
          do { -- Typecheck the expanded expression.
               expr' <- tcExpr ds_expr (Check ds_res_ty)
               -- NB: it's important to use ds_res_ty and not res_ty here.
               -- Test case: T18802b.

             ; tcWrapResultMono expr expr' ds_res_ty res_ty
             -- We need to unify the result type of the expanded
             -- expression with the expected result type.
             --
             -- See Note [Unifying result types in tcRecordUpd].
             -- Test case: T10808.
             }
        }

tcExpr e@(RecordUpd { rupd_flds = OverloadedRecUpdFields {}}) _
  = pprPanic "tcExpr: unexpected overloaded-dot RecordUpd" $ ppr e

{-
************************************************************************
*                                                                      *
        Arithmetic sequences                    e.g. [a,b..]
        and their parallel-array counterparts   e.g. [: a,b.. :]

*                                                                      *
************************************************************************
-}

tcExpr (ArithSeq _ witness seq) res_ty
  = tcArithSeq witness seq res_ty

{-
************************************************************************
*                                                                      *
                Template Haskell
*                                                                      *
************************************************************************
-}

-- Here we get rid of it and add the finalizers to the global environment.
-- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
tcExpr (HsTypedSplice ext splice)   res_ty = tcTypedSplice ext splice res_ty
tcExpr e@(HsTypedBracket _ext body) res_ty = tcTypedBracket e body res_ty
tcExpr e@(HsUntypedBracket ps body) res_ty = tcUntypedBracket e body ps res_ty

{-
************************************************************************
*                                                                      *
                Catch-all
*                                                                      *
************************************************************************
-}

-- See Note [Typechecking by expansion: overview]
tcExpr e res_ty
  = do { mb_hse <- tcExpand e
       ; case mb_hse of
           Just hse -> tcHsExpansion hse res_ty
           Nothing  -> pprPanic "tcExpr: unhandled case:" (ppr e) }

-- | Open a dot package for one local pattern binding and its body.
-- See Note [Scope for a local erased package pattern].
tcLexicalExistsLet :: HsExpr GhcRn -> LHsLocalBinds GhcRn -> LHsExpr GhcRn
                   -> ExpRhoType -> TcM (Maybe (HsExpr GhcTc))
tcLexicalExistsLet original (L bind_loc (HsValBinds ann (XValBindsLR (HsVBG groups [])))) body expected
  | [(NonRecursive, [L pat_bind_loc binding@PatBind
      { pat_lhs = pat, pat_mods = [], pat_rhs = grhss }])] <- groups
  , tuple_pattern pat
  , GRHSs { grhssGRHSs = L rhs_loc (GRHS rhs_ann [] rhs) NE.:| []
          , grhssLocalBinds = L _ EmptyLocalBinds{} } <- grhss
  = runMaybeT $ do
      (schema, check_schema) <- MaybeT (tcLexicalPackageOperand rhs)
      case coreFullView schema of
        ExistsTy ImplicitExists _ _ -> guard (matchesImplicitPayloadPat schema pat)
        _ -> mzero
      liftMaybeT $ do
        mult <- newMultiplicityVar
        (checked, actual) <- tcScalingUsage mult $
          if check_schema
            then (, schema) <$> tcCheckMonoExpr rhs schema
            else tcInferRhoFRR FRRPatBind rhs
        result_ty <- expTypeToType expected
        opened <- tcOpenImplicitPackageAt mult (getLocA rhs) actual checked result_ty $ \payload -> do
          let name = getName payload
              replacement = fmap (\source -> mkExpandedLExpr source (nlHsVar name)) rhs
              binding' = binding
                { pat_ext = extendNameSet (pat_ext binding) name
                , pat_rhs = grhss { grhssGRHSs = L rhs_loc (GRHS rhs_ann [] replacement) NE.:| [] } }
              scope = HsValBinds ann (XValBindsLR (HsVBG [(NonRecursive, [L pat_bind_loc binding'])] []))
          (usage, (decls, body')) <- tcCollectingUsage $
            tcExtendGeneratedIdEnv payload $
              tcLocalBinds scope (tcMonoLExpr body (mkCheckExpType result_ty))
          let (payload_use, body_usage) = popUE usage payload
              rhs_mult = case payload_use of
                MUsage p -> p
                _ -> ManyTy
          tcEqMult (NonLinearPatternOrigin OtherPatternReason pat) rhs_mult mult
          tcEmitBindingUsage body_usage
          return (HsLet noExtField (L bind_loc decls) body')
        return (mkExpandedExprTc original opened)
  where
    tuple_pattern (L _ (ParPat _ nested)) = tuple_pattern nested
    tuple_pattern (L _ (LazyPat _ nested)) = plain_tuple nested
    tuple_pattern (L _ (BangPat _ nested)) = plain_tuple nested
    tuple_pattern pat = plain_tuple pat

    plain_tuple (L _ (ParPat _ nested)) = plain_tuple nested
    plain_tuple (L _ (TuplePat _ fields Boxed)) = all plain_field fields
    plain_tuple _ = False

    plain_field (L _ (ParPat _ nested)) = plain_field nested
    plain_field (L _ VarPat{}) = True
    plain_field (L _ WildPat{}) = True
    plain_field _ = False
tcLexicalExistsLet _ _ _ _ = return Nothing

{- Note [Scope for a local erased package pattern]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In let (value, reader) = package in reader value, the two fields mention
the same witness. Open the declared dot package once. Bind the witness
around the original pattern binding and its body. Replace the original
RHS with the shared payload Id. Then use tcLocalBinds to infer the pattern
binding and check its usage. Include the payload Id in the binding's
free variables, since it carries the local witness. Keep the result type
outside the opening. Keep the generated Id out of relevant bindings in
error messages so the report shows the user's fields and package.

The opening is lazy for every pattern. Keep the source pattern so ordinary
desugaring determines whether the binding forces the tuple before its body.
A source bang or the Strict default forces the tuple to WHNF. A source
tilde keeps it lazy, including under Strict. See Note [decideBangHood]
in GHC.HsToCore.Utils. The type of the complete let cannot mention the
hidden witness in any of these cases.

Allocate the payload multiplicity outside the opening. Use it to scale the
producer and annotate the payload Id. Equate it with the use of the synthetic
RHS from tcLocalBinds without using coercion evidence. This preserves the
ordinary restriction on lazy bindings. A strict match may reuse the payload
Id as its Core case binder
(see matchSinglePat in GHC.HsToCore.Match). Delete the synthetic Id from
the usage environment before leaving the opening.

Restrict this entry to an unsigned singleton nonrecursive binding with
an unguarded RHS, and a boxed tuple of variables or wildcards with at most
one outer bang or tilde. A recursive group, a signature, or a pattern
with its own evidence needs a separate rule. See also Note [Typechecking pattern bindings] in GHC.Tc.Gen.Bind.
-}

-- | Elaborate a nonrecursive index definition through dependent application.
-- See Note [Equality for lazy local definitions].
tcNativeLet :: HsExpr GhcRn -> LHsLocalBinds GhcRn -> LHsExpr GhcRn -> Type
            -> TcM (Maybe (HsExpr GhcTc))
tcNativeLet original (L loc (HsValBinds ann (XValBindsLR (HsVBG groups sigs)))) body expected
  | not (null groups), all isTypeLSig sigs
  , Just bindings <- traverse nonrec_fun groups
  = do { direct <- case (bindings, sigs) of
           ([L _ FunBind { fun_id = name, fun_matches = matches }], [])
             | Just rhs <- lazyLetRhs matches -> named_argument name rhs
           _ -> return Nothing
       ; case direct of
           Just expr -> return (Just (mkExpandedExprTc original expr))
           Nothing -> do
             { expr <- tcNonRecLets bindings sigs check_group (tcMonoLExpr body (Check expected))
             ; return (Just (mkExpandedExprTc original (unLoc expr))) } }
  where
    nonrec_fun (NonRecursive, [binding@(L _ FunBind{})]) = Just binding
    nonrec_fun _ = Nothing

    check_group checked rest = do
      refined <- tcOptionalLet checked rest expected
      case refined of
        Just expr -> return (L (noAnnSrcSpan (combineSrcSpans (locA loc) (getLocA body))) expr)
        Nothing -> do
          body' <- rest
          let binds = HsValBinds ann (XValBindsLR (HsVBG [(NonRecursive, checked)] []))
          return (L (getLoc body') (HsLet noExtField (L loc binds) body'))

    named_argument name rhs = runMaybeT $ do
        value <- candidate rhs
        index <- MaybeT (pure (Core.retainedArgIndex value))
        let domain = typeKind index
        guard (isNativeIndexType domain)
        template_name <- liftMaybeT (newSysName (mkTyVarOcc "let_index"))
        let template = mkTyVar template_name domain
        result <- MaybeT (pure (abstractNativeIndices [(index, template)] expected))
        guard (anyFreeVarsOfType (== template) result)
        liftMaybeT $ do
            (checked, runtime_rhs) <- tcRetainedArg domain rhs
            let binder = mkLocalId (unLoc name) ManyTy domain
                equality = mkNomEqPred (TermVarTy binder) index
                signature = mkDepFunTy binder (mkPhiTy [equality] expected)
                pat = L (l2l (getLoc name)) (VarPat noExtField name)
                row = genSimpleMatch (LamAlt LamSingle) [pat] body
                matches = mkMatchGroup (Generated OtherExpansion SkipPmc) noAnn (wrapGenSpan [row])
                lambda = wrapGenSpan (HsLam noAnn LamSingle matches)
            argument <- tcScalingUsage ManyTy (tcCheckMonoExpr runtime_rhs domain)
            fun <- tcWithSourceLetView binder index (tcCheckMonoExprNC lambda signature)
            let result_ty = applyTypeToArgs signature [checked]
                app = HsAppRetained (DepFunVisible, result_ty) fun argument
            tcWrapResult (unLoc body) app result_ty (Check expected)
    candidate :: LHsExpr GhcRn -> MaybeT TcM Core.CoreExpr
    candidate (L _ expr) = case expr of
      HsPar _ e -> candidate e
      HsApp _ f a -> Core.App <$> candidate f <*> candidate a
      OpApp _ a f b -> Core.mkApps <$> candidate f <*> traverse candidate [a, b]
      HsVar _ lname -> do
        thing <- liftMaybeT (tcLookup (unLocWithUserRdr lname))
        env <- liftMaybeT getGblEnv
        let occurrence var = MaybeT (pure (Core.Var <$> sourceTableOccurrence env var))
        case thing of
          ATcId { tct_id = var } -> occurrence var
          AGlobal (AnId var) -> occurrence var
          AGlobal (AConLike (RealDataCon dc)) -> return (Core.Var (dataConWorkId dc))
          _ -> mzero
      _ -> mzero
tcNativeLet _ _ _ _ = return Nothing

{- Note [Equality for lazy local definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose a result mentions times rows cols, and the body binds
cells = times rows cols. Check the body as a local function with type

  foreach (cells :: Nat) -> (cells ~ times rows cols) => result

Apply it to the original RHS once. The retained application checks the
actual function identities, operands, and domain. Substitution into the
equality makes the evidence argument reflexive. tcWrapResult still asks
the solver to discharge the actual predicate. The evidence argument is
an erased CoVar. The body can use cells twice or ignore it. Mark the
synthetic lambda and its match as generated. Keep the user's spans on
the binder, the RHS, and the body for diagnostics.

Select this rule from the result type and a read-only view of the renamed
RHS. Look up a same-module function in the completed type environment.
An earlier occurrence can lack its source table. The prepass registers no
equation use. The retained argument checker records the selected owners.
Neither the view nor the type match constructs evidence. Leave other
lets to ordinary binding checks, including recursive groups and Strict
bindings. A class selector alone supplies no checked index. Choosing a
global instance here could change the meaning of ordinary multiplication
under a local Num dictionary. Agda's let conversion motivates the equality.
The lazy value argument preserves Haskell sharing. See docs/dependent-haskell/REFERENCES.md.
-}

-- | Check a local definition in the scope of the preceding definitions.
-- See Note [Optional equality for an overloaded let].
tcOptionalLet :: LHsBinds GhcTc -> TcM (LHsExpr GhcTc) -> Type -> TcM (Maybe (HsExpr GhcTc))
tcOptionalLet checked rest expected
  | Just (binder, rhs) <- case checked of
      [L _ (XHsBindsLR (AbsBinds { abs_sig = True, abs_tvs = [], abs_ev_vars = [], abs_ev_binds = []
             , abs_exports = [ABE { abe_poly = public, abe_mono = mono, abe_wrap = WpHole
                                  , abe_prags = SpecPrags [] }], abs_binds = inner }))] ->
          do { (value, rhs) <- binding_rhs inner
             ; guard (value == mono && varType value `tcEqType` varType public)
             ; return (public, rhs) }
      _ -> binding_rhs checked
  = runMaybeT $ do
      initial <- liftMaybeT (liftZonkM (zonkId binder))
      let mult = idMult initial
          flexible = maybe False isMetaTyVar (getTyVar_maybe mult)
      guard (isNativeIndexId (setIdMult initial ManyTy) && (isManyTy mult || flexible))
      flags <- liftMaybeT getDynFlags
      guard (all (== Breakpoints) (ticks_passes (initTicksConfig flags))
             && not (sccProfilingEnabled flags))
      (canonical, owners, methods) <- MaybeT (tcOptionalLetOperand rhs)
      index <- MaybeT (pure (Core.retainedArgIndex canonical))
      guard (typeKind index `tcEqType` idType initial)
      let uses = [(dfun, sel, target) | (_, dfun, sel, target) <- methods]
          guards = [(wanted, dfun) | (wanted, dfun, _, _) <- methods]
      dependencies <- MaybeT (optionalSourceUses owners uses)
      liftMaybeT (tcSubMult (OccurrenceOf (idName initial)) ManyTy mult)
      value <- liftMaybeT (liftZonkM (zonkId binder))
      guard (isNativeIndexId value)
      liftMaybeT $ do
          proof <- newEvVar (mkNomEqPred (TermVarTy value) index)
          traceTc "tcOptionalLet" (ppr value <+> dcolon <+> ppr (idType proof) $$ ppr guards)
          (binds, body') <- setSrcSpan (nameSrcSpan (idName value)) $
                            checkSourceLetConstraints (idName value) guards proof $
                            tcWithSourceLetView value index rest
          registerOptionalSourceUses binds proof dependencies
          return (XExpr (NativeLetAlias rhs (PendingSourceLetRhs binds canonical) value proof expected (mkHsDictLet binds body')))
  | otherwise = return Nothing
  where
    binding_rhs [L _ FunBind { fun_ext = (WpHole, []), fun_id = L _ binder, fun_matches = matches }]
      | Just rhs <- lazyLetRhs matches = Just (binder, rhs)
    binding_rhs _ = Nothing

-- | Read a single lazy value equation without guards or local declarations.
lazyLetRhs :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> Maybe (LHsExpr (GhcPass p))
lazyLetRhs MG { mg_alts = L _ [L _ match] }
  | Match { m_ctxt = FunRhs { mc_strictness = NoSrcStrict }, m_pats = L _ []
          , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] rhs) NE.:| []
                            , grhssLocalBinds = L _ EmptyLocalBinds{} } } <- match
  = Just rhs
lazyLetRhs _ = Nothing

{- Note [Signed and grouped local indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check all written type signatures before checking the dependency-sorted
nonrecursive bindings. Check a binding before entering the implication for
its remaining siblings and body. Thus a later RHS may use an earlier alias
without making that equality available in its own definition.

tcPolyCheck represents a complete signature by an AbsBinds with one export.
The body uses the public Id. Its checked RHS binds a fresh inner Id. Read
the RHS only when the export and equation have identity wrappers and bind
no evidence, type variables or pragmas. A qualified or polymorphic signature
can abstract inside the FunBind wrapper even when the AbsBinds lists are empty.
Use ordinary let bindings for other checked forms.
-}

{- Note [Optional equality for an overloaded let]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For let cells = rows * cols, check the binding with the ordinary let
generalisation and usage rules. Inspect the checked RHS inside the scope
of the resulting binding. A simple monomorphic RHS can supply both the
dictionary Wanted for the written (*) and a candidate checked product.
An abstracted binding or a binding with wrappers keeps ordinary checking.
A nonrecursive binder can still have a flexible multiplicity. The recovered
RHS uses only unrestricted operands. Set the binder to Many in its original
scope before the body enters an implication. Preserve a rigid One.

Capture the body's constraints under an implication. Let the solver offer
cells ~# times rows cols only if the evidence for the original RHS selects
the dictionary that certifies times. A Given Num Nat can supply a different
method. The program may still use cells at runtime without its equality.
The body can also return a vector built independently at times rows cols.

When the body uses the proof, record the method and equation dependencies
and validate the eventual RULES. Desugaring must discharge that same proof
by retained application at the canonical product. Otherwise desugar the
written RHS once as an ordinary lazy let.
-}

-- | Open an explicit existential in a single case alternative.
-- See Note [Checking a lexical existential case].
tcExistsCase :: HsExpr GhcRn -> ExpRhoType -> TcM (Maybe (HsExpr GhcTc))
tcExistsCase expr@(HsCase ctxt scrut matches)
             expected
  | MG { mg_alts = L _ [L _ match] } <- matches
  , Match { m_pats = L _ [pat] } <- match
  , ConPat { pat_con = L con_loc (WithUserRdr _ con), pat_args = args } <- strip_pat pat
  = do { pack <- isExistsPackName con
       ; if pack then Just <$> check_pack con_loc con args
                 else return Nothing }
  where
    strip_pat (L _ (ParPat _ pat)) = strip_pat pat
    strip_pat (L _ pat) = pat

    check_pack con_loc con args
      = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
           ; unless enabled $ failWithTc (TcRnExistsPack ExistsPackExtension)
           ; case args of
               PrefixCon _ [witness_pat, payload_pat]
                 | EmbTyPat _ HsTP{ hstp_ext = HsTPRn [] [] [name], hstp_body = witness_ty }
                     <- strip_pat witness_pat
                 , HsTyVar{} <- unLoc (ignoreParens witness_ty)
                 -> open_pack con_loc con (Just name) payload_pat
                 | wildcard_witness (strip_pat witness_pat)
                 -> open_pack con_loc con Nothing payload_pat
                 | otherwise -> setSrcSpanA (getLoc witness_pat) $
                                  failWithTc (TcRnExistsPack ExistsPackInvalidContext)
               _ -> failWithTc (TcRnExistsPack ExistsPackInvalidContext) }

    wildcard_witness :: Pat GhcRn -> Bool
    wildcard_witness WildPat{} = True
    wildcard_witness (EmbTyPat _ HsTP{ hstp_body = ty })
      | HsWildCardTy (HoleVar (L _ hole)) <- unLoc (ignoreParens ty)
      = hole == unnamedHoleRdrName
    wildcard_witness _ = False

    open_pack con_loc con mb_name payload_pat
      = do { (scrut', scrut_ty) <- tcScalingUsage ManyTy $ tcInferRhoFRR FRRCase scrut
           ; schema <- liftZonkM (zonkTcType scrut_ty)
           ; result_ty <- expTypeToType expected
           ; let open_bar = open_explicit con_loc con mb_name payload_pat result_ty
           ; opened <- case coreFullView schema of
               ExistsTy ImplicitExists _ inner
                 | ExistsTy ExplicitExists _ _ <- implicitPayloadType inner
                 -> tcOpenImplicitPackage (getLocA scrut) schema scrut' result_ty $ \bar ->
                      open_bar (nlHsVar bar) (varType bar)
               _ -> open_bar scrut' schema
           ; return (XExpr (ExpandedThingTc (HSE (ExprCtxt expr) (noLocA opened)))) }

    open_explicit con_loc con mb_name payload_pat result_ty scrut' schema
      | ExistsTy ExplicitExists binder inner <- coreFullView schema
      = do { con_like <- RealDataCon <$> tcLookupDataCon con
           ; let origin = PatSkol con_like ctxt
           ; skol_info <- mkSkolemInfo origin
           ; name <- maybe (newSysName (getOccName binder)) return mb_name
           ; witness <- pushTcLevelM_ $ newSkolemTyVar skol_info name (tyVarKind binder)
           ; let payload_ty = substTyWith [binder] [mkTyVarTy witness] inner
                 replace_pat = fmap (\row -> row { m_pats = [payload_pat] <$ m_pats row })
                 payload_matches = matches { mg_alts = fmap (map replace_pat) (mg_alts matches) }
           ; payload <- newSysLocalId (fsLit "unpacked") ManyTy payload_ty
           ; (ev_binds, body_matches) <- setSrcSpanA con_loc $
               checkConstraints origin [witness] [] $
                 tcExtendNameTyVarEnv [(n, witness) | n <- maybeToList mb_name] $
                   tcCaseMatches ctxt tcBody (Scaled ManyTy payload_ty) payload_matches
                     (mkCheckExpType result_ty)
           ; let body = noLocA (HsCase ctxt (nlHsVar payload) body_matches)
           ; return (XExpr (NativeExistsUnpack witness payload result_ty scrut'
                              (mkHsDictLet ev_binds body))) }
      | otherwise = failWithTc (TcRnExistsPack (ExistsPackInvalidSchema schema))
tcExistsCase _ _ = return Nothing

{- Note [Matching an implicit existential payload]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A data constructor for the declared payload type can request an opening
in a case, as an application can request one with its formal domain.
An ordinary case first checks its scrutinee and multiplicity. Open the
checked package around the whole match, with the result type outside
the witness scope. Keep the ordinary case match checker. A constructor
demand, lazy pattern, and GADT refinement keep their ordinary meaning.

A variable or wildcard alone matches the package. A pattern synonym
can also match the package at its declared type. Require a real data
constructor of the payload type before selecting the opening. The
checked dot type is sufficient in a client that imports the schema.
-}

{- Note [Checking a lexical existential case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A sole Pack alternative opens the package with a fresh type skolem and a
payload. Prepare the result type at the outer level so it cannot mention the
skolem. Use the same implication machinery as constructor patterns for
constraints in the payload match and its right-hand side.

Translate the payload pattern to an ordinary case inside Unpack. A variable
or wildcard does not demand the payload in a lazy module. An ordinary strict
payload pattern, such as Just x, demands it in the usual way. The scope of the
skolem covers the payload pattern and the whole alternative. A wildcard
witness still needs this skolem to type the payload. Only a written witness
name enters the source type environment.

When the scrutinee has dot binders before the bar package, open the dot
binders around the explicit match. Keep the result type outside both
openings. The written Pack pattern still binds the bar witness, and its
payload pattern still determines demand.
-}

-- | Classify an opaque call after ordinary expression and usage checking.
-- See Note [Source opaque call refinement] in GHC.Tc.Gen.Match.
checkedNativeCallIndex :: LHsExpr GhcTc -> Type -> TcM (Maybe Type)
checkedNativeCallIndex expr result_ty
  | HsApp _ fun arg <- unLoc (stripParensLHsExpr expr)
  , HsVar _ (L _ head_id) <- unLoc (stripParensLHsExpr fun)
  , Just (domain, codomain) <- nativeIndexFunctionTys_maybe head_id
  = do { core_arg <- checkedNativeCallArgument emptyNameEnv arg
       ; return $ do
           arg_index <- core_arg >>= Core.exprToNativeIndex
           guard (eqType domain (typeKind arg_index) && eqType codomain result_ty)
           return (TermAppTy head_id arg_index) }
  | otherwise = return Nothing

-- | Reconstruct a checked scalar argument for classification, not execution.
-- See Note [Checked source call arguments] and Note [Signed source call beta].
checkedNativeCallArgument :: NameEnv Id -> LHsExpr GhcTc -> TcM (Maybe Core.CoreExpr)
checkedNativeCallArgument bound expr
  = case unLoc (stripParensLHsExpr expr) of
      HsVar _ (L _ arg_id)
        | let actual = fromMaybe arg_id (lookupNameEnv bound (idName arg_id))
        , isNativeIndexId actual -> return (Just (Core.Var actual))
      XExpr (ConLikeTc (RealDataCon con))
        -> return (Just (Core.Var (dataConWrapId con)))
      HsApp _ fun arg
        | ExprWithTySig _ inner signature <- unLoc (strip_checked fun)
        , Just domain_name <- simple_signature signature
        -> checked_beta domain_name inner arg
        | otherwise
        -> do { fun' <- checkedNativeCallArgument bound fun
              ; case fun' of
                  Nothing -> return Nothing
                  Just core_fun -> do
                    { arg' <- checkedNativeCallArgument bound arg
                    ; return (Core.App core_fun <$> arg') } }
      XExpr (WrapExpr wrapper inner)
        | Just tys <- type_arguments wrapper
        -> do { tys <- mapM (liftZonkM . zonkTcType) tys
              ; value <- checkedNativeCallArgument bound (noLocA inner)
              ; return ((`Core.mkTyApps` tys) <$> value) }
      ExplicitList element_ty elements
        -> do { element_ty <- liftZonkM (zonkTcType element_ty)
              ; values <- mapM (checkedNativeCallArgument bound) elements
              ; let nil = Core.mkConApp nilDataCon [Core.Type element_ty]
                    cons value rest = Core.mkConApp consDataCon [Core.Type element_ty, value, rest]
              ; return (foldr cons nil <$> sequence values) }
      XExpr (ExpandedThingTc (HSE _ inner))
        -> checkedNativeCallArgument bound inner
      _ -> return Nothing
  where
    -- Read only erased type application wrappers, in application order.
    type_arguments WpHole = Just []
    type_arguments (WpTyApp ty) = Just [ty]
    type_arguments (WpCompose outer inner)
      = (++) <$> type_arguments inner <*> type_arguments outer
    type_arguments _ = Nothing

    -- | Read only the existing transparent checked expression forms.
    strip_checked :: LHsExpr GhcTc -> LHsExpr GhcTc
    strip_checked expr = case unLoc (stripParensLHsExpr expr) of
      XExpr (WrapExpr WpHole inner) -> strip_checked (noLocA inner)
      XExpr (ExpandedThingTc (HSE _ inner)) -> strip_checked inner
      _ -> stripParensLHsExpr expr

    -- | Require a written, closed, plain A -> A signature with named domains.
    simple_signature :: LHsSigWcType GhcRn -> Maybe Name
    simple_signature (HsWC { hswc_ext = []
                          , hswc_body = L _ HsSig
                              { sig_bndrs = HsOuterImplicit { hso_ximplicit = [] }
                              , sig_body = signature } })
      | HsFunTy _ (HsModifiedFunArr _ [] (HsStandardArr _)) arg res <- strip_type signature
      , HsTyVar _ NotPromoted (L _ (WithUserRdr _ arg_name)) <- strip_type arg
      , HsTyVar _ NotPromoted (L _ (WithUserRdr _ res_name)) <- strip_type res
      , arg_name == res_name
      = Just arg_name
    simple_signature _ = Nothing

    -- | Remove parentheses without expanding type syntax or aliases.
    strip_type :: LHsType GhcRn -> HsType GhcRn
    strip_type (L _ (HsParTy _ ty)) = strip_type ty
    strip_type (L _ ty) = ty

    -- | Use the actual checked binder and read its existing assignments.
    checked_beta :: Name -> LHsExpr GhcTc -> LHsExpr GhcTc
                 -> TcM (Maybe Core.CoreExpr)
    checked_beta domain_name inner arg
      | HsLam _ LamSingle MG
          { mg_ext = MatchGroupTc { mg_arg_tys = [pat_ty], mg_res_ty = result_ty }
          , mg_alts = L _ [L _ Match
              { m_pats = L _ [L _ (VarPat _ (L _ binder))]
              , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] body) NE.:| []
                                , grhssLocalBinds = L _ (EmptyLocalBinds _) } }] }
          <- unLoc (strip_checked inner)
      = do { strict <- xoptM LangExt.Strict
           ; if strict then return Nothing else
             do { (actual, argument, result) <- liftZonkM $
                    (,,) <$> zonkId binder <*> zonkScaledTcType pat_ty
                         <*> zonkTcType result_ty
                ; case (idMult actual, scaledMult argument) of
                    (ManyTy, ManyTy)
                      | isNativeIndexId actual
                      , eqType (idType actual) (scaledThing argument)
                      , eqType (idType actual) result
                      , Just (tc, []) <- splitTyConApp_maybe (coreFullView (idType actual))
                      , tyConName tc == domain_name
                      -> do { body' <- checkedNativeCallArgument
                                         (extendNameEnv bound (idName actual) actual) body
                            ; case body' of
                                Nothing -> return Nothing
                                Just core_body -> do
                                  { arg' <- checkedNativeCallArgument bound arg
                                  ; return (Core.App (Core.Lam actual core_body) <$> arg') } }
                    _ -> return Nothing } }
    checked_beta _ _ _ = return Nothing

{- Note [Signed source call beta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A complete written A -> A signature can supply a checked Many lambda binder.
Require the surviving ExprWithTySig, one plain arrow and the same bare type
constructor Name on each side. Reject implicit or explicit forall binders,
wildcards, contexts, type applications, aliases and nontrivial wrappers.
Check the actual LamSingle, VarPat, unguarded RHS and empty local bindings.

Read existing assignments with the during-TC zonker. It leaves Flexi untouched;
only existing Indirect chains can be compressed. Require literal Many in the
actual Id and scaled MatchGroup argument. Check both domains and the result.
Preserve the binder Name and Unique. Use that zonked Id for bound occurrences,
without changing the existing policy for external index variables or heads.
Do not default, fill a meta, create a provisional Many Id or repeat usage.

The temporary Core.Lam is only classification data. exprToNativeIndex checks
its existing atomic beta grammar and domains. Keep the original checked call
for execution. No general lambda, residual opaque call or new Core rule is
admitted. Under Strict return Nothing: source VarPat demand is added later by
desugaring. Ordinary nondependent cases must retain their normal checking path.

Agda v2.8.0 Rules/Term.hs lines 468-486 and 542-587 check a lambda in its
target context. Rules/Application.hs lines 774-808 check and substitute the
argument. Their blobs are 02991c9fa0085315cc854d2833a9080af14a7740 and
82ba94ac864d19b947d4c92b9af252b0fe81347a. They supply scoped checking prior art,
not GHC multiplicity defaulting or Haskell demand laws. See
docs/dependent-haskell/REFERENCES.md#checked-constructor-call-arguments-prior-art
and Note [Source opaque call refinement] in GHC.Tc.Gen.Match.
-}

{- Note [Checked source call arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Classify a constructor argument only after ordinary expression and usage
checking. Reconstruct a temporary Core spine from the actual Ids and DataCons.
Use dataConWrapId, as the desugarer does. Never replace a real wrapper with its
worker. exprToNativeIndex checks the closed domain, saturation, field domains
and multiplicities. Closed native domains exclude constructor wrappers.

The temporary spine is not executed. The original checked scrutinee remains
in NativeCaseAlias and is desugared once. Parentheses, WpHole and a checked
source expansion preserve that expression. Other wrappers, casts, type or
evidence applications and opaque producers get no new admission. The separate
signed beta path is described in Note [Signed source call beta].
In particular, this classifier does not rerun tcRetainedArg on renamed syntax
or emit argument usage a second time.

Agda v2.8.0 Rules/Application.hs:774-808 checks the argument at the current
domain before substitution (blob 82ba94ac864d19b947d4c92b9af252b0fe81347a).
This supplies checking-order prior art, not a Haskell demand theorem.
See Note [Source opaque call refinement] in GHC.Tc.Gen.Match.
-}

{-
************************************************************************
*                                                                      *
                Expansion Expressions (XXExprGhcRn)
*                                                                      *
************************************************************************
-}

tcHsExpansion :: HsExpansion GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
tcHsExpansion (HSE { hse_ctxt = o, hse_exp = e }) res_ty
   = do { e' <- tcMonoLExpr e res_ty
        ; return $ XExpr $ ExpandedThingTc $
          HSE { hse_ctxt = o, hse_exp = e' } }


{-
************************************************************************
*                                                                      *
                Arithmetic sequences [a..b] etc
*                                                                      *
************************************************************************
-}

tcArithSeq :: Maybe (SyntaxExpr GhcRn) -> ArithSeqInfo GhcRn -> ExpRhoType
           -> TcM (HsExpr GhcTc)

tcArithSeq witness seq@(From expr) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr' <-tcScalingUsage elt_mult $ tcCheckPolyExpr expr elt_ty
       ; enum_from <- newKnownOccMethod (ArithSeqOrigin seq)
                              enumFromClassOpOcc [elt_ty]
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from wit' (From expr') }

tcArithSeq witness seq@(FromThen expr1 expr2) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr1 elt_ty
       ; expr2' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr2 elt_ty
       ; enum_from_then <- newKnownOccMethod (ArithSeqOrigin seq)
                              enumFromThenClassOpOcc [elt_ty]
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from_then wit' (FromThen expr1' expr2') }

tcArithSeq witness seq@(FromTo expr1 expr2) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr1 elt_ty
       ; expr2' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr2 elt_ty
       ; enum_from_to <- newKnownOccMethod (ArithSeqOrigin seq)
                              enumFromToClassOpOcc [elt_ty]
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from_to wit' (FromTo expr1' expr2') }

tcArithSeq witness seq@(FromThenTo expr1 expr2 expr3) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
        ; expr1' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr1 elt_ty
        ; expr2' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr2 elt_ty
        ; expr3' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr3 elt_ty
        ; eft <- newKnownOccMethod (ArithSeqOrigin seq)
                              enumFromThenToClassOpOcc [elt_ty]
        ; return $ mkHsWrap wrap $
          ArithSeq eft wit' (FromThenTo expr1' expr2' expr3') }

-----------------
arithSeqEltType :: Maybe (SyntaxExpr GhcRn) -> ExpRhoType
                -> TcM (HsWrapper, Mult, TcType, Maybe (SyntaxExpr GhcTc))
arithSeqEltType Nothing res_ty
  = do { res_ty <- expTypeToType res_ty
       ; (coi, elt_ty) <- matchExpectedListTy res_ty
       ; return (mkWpCastN coi, OneTy, elt_ty, Nothing) }
arithSeqEltType (Just fl) res_ty
  = do { ((elt_mult, elt_ty), fl')
           <- tcSyntaxOp ListOrigin fl [SynList] res_ty $
              \ [elt_ty] [elt_mult] -> return (elt_mult, elt_ty)
       ; return (idHsWrapper, elt_mult, elt_ty, Just fl') }

----------------

-- | Typecheck an explicit tuple @(a,b,c)@ or @(\#a,b,c\#)@.
--
-- The first argument gives missing-field multiplicities in source order.
tcCheckExplicitTuple :: [Mult]
                     -> [HsTupArg GhcRn]
                     -> [TcSigmaType]
                          -- ^ Argument types.
                          -- This function ensures they all have
                          -- a fixed runtime representation.
                     -> TcM [HsTupArg GhcTc]
tcCheckExplicitTuple mults args tys
  = do massert (equalLength args tys)
       checkTupSize (length args)
       go 1 mults args tys
  where
    -- | Check fields and consume missing-field multiplicities in source order.
    go :: Int -> [Mult] -> [HsTupArg GhcRn] -> [TcType] -> TcM [HsTupArg GhcTc]
    go _ [] [] [] = return []
    go i (mult : mults) (Missing{} : args) (arg_ty : tys)
      = do { hasFixedRuntimeRep_syntactic (FRRUnboxedTupleSection i) arg_ty
           ; args' <- go (i+1) mults args tys
           ; return (Missing (Scaled mult arg_ty) : args') }
    go i mults (Present x expr : args) (arg_ty : tys)
      = do { expr' <- tcCheckPolyExpr expr arg_ty
           ; (co, _) <- hasFixedRuntimeRep (FRRUnboxedTuple i) arg_ty
           ; args' <- go (i+1) mults args tys
           ; return (Present x (mkLHsWrap (mkWpCastN co) expr') : args') }
    go _ _ _ _ = panic "tcCheckExplicitTuple: field or multiplicity count"

-- | Typecheck an explicit tuple or tuple section by performing type inference.
tcInferTupArgs :: Boxity
               -> [HsTupArg GhcRn] -- ^ argument types
               -> TcM ([HsTupArg GhcTc], [TcSigmaTypeFRR])
tcInferTupArgs boxity args
  = do { checkTupSize (length args)
       ; zipWithAndUnzipM tc_infer_tup_arg [1,2..] args }
 where
  tc_infer_tup_arg :: Int -> HsTupArg GhcRn -> TcM (HsTupArg GhcTc, TcSigmaTypeFRR)
  tc_infer_tup_arg i (Missing {})
    = do { mult <- newFlexiTyVarTy multiplicityTy
         ; arg_ty <- new_arg_ty i
         ; return (Missing (Scaled mult arg_ty), arg_ty) }
  tc_infer_tup_arg i (Present x lexpr@(L l expr))
    = do { (expr', arg_ty) <- case boxity of
             Unboxed -> runInferRhoFRR (FRRUnboxedTuple i) (tcPolyExpr expr)
             Boxed   -> do { enabled <- xoptM LangExt.ExperimentalDependentTypes
                           ; (inferable, anchor) <- if enabled
                               then tcRetainedInferableArg lexpr
                               else return (False, False)
                           ; if inferable && anchor
                             then do { (expr', arg_ty) <- tcInferRhoFRR (FRRUnboxedTuple i) lexpr
                                     ; _ <- setSrcSpanA l $ addExprCtxt expr $
                                              unifyType Nothing (typeKind arg_ty) liftedTypeKind
                                     ; return (unLoc expr', arg_ty) }
                             else do { arg_ty <- newFlexiTyVarTy liftedTypeKind
                                     ; L _ expr' <- tcCheckPolyExpr lexpr arg_ty
                                     ; return (expr', arg_ty) } }
         ; return (Present x (L l expr'), arg_ty) }

  new_arg_ty :: Int -> TcM TcTypeFRR
  new_arg_ty i =
    case boxity of
      Unboxed -> newOpenFlexiFRRTyVarTy (FRRUnboxedTupleSection i)
      Boxed   -> newFlexiTyVarTy liftedTypeKind

{- Note [Inference of native tuple fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If xs :: Vec Int n, inferring (42, xs) must retain that exact second field
type. The ordinary tuple path first allocates component meta-variables.
Those variables cannot contain a runtime Id such as n.

Under ExperimentalDependentTypes, expand a complete boxed tuple to its
ordinary data constructor when the result is an InferResult and a field
has a native anchor. Use tcRetainedInferableArg to classify the fields
without checking them or running splices. The application checker infers
and caches native fields, chooses their types immutably, and checks the
other fields normally. It retains scope, kind, usage and subsumption checks.

Use the existing expansion wrapper and generated application spans. Keep
the original tuple context and the user-written field spans in diagnostics.
A known expected type still uses component checking, including higher-rank
fields. Unboxed tuples keep their existing paths.

Keep ExplicitTuple for sections. Infer an anchored native present field once
and retain its exact type. Check its lifted kind as well as its fixed runtime
representation. Other present fields and missing-field metas keep ordinary
inference. Missing fields supply no native anchor.

A native expected visible-arrow function with a boxed tuple result supplies
component types and hole multiplicities. Check present fields at those types.
Store a missing field's tuple component type and the corresponding arrow
multiplicity. Reconstruct the whole section function, then use the existing
tcWrapResultMono against the original expected type. It keeps any required
domain and result coercions. A tuple-result cast cannot wrap the section AST,
which denotes a function. Other expected shapes keep the ordinary path.

The existing desugaring creates missing-field lambdas in source order.
Present expressions stay inside their tuple body. Do not add a sharing let
or move an expression outside those lambdas. Named captured values retain
their original sharing. Keep field usage and scope checks unchanged.

The ordinary product does not bind a runtime index. Returning (n, xs) cannot
hide a free callback-local n. Use an explicit package for that purpose.
See docs/dependent-haskell/REFERENCES.md#boxed-tuple-inference-prior-art.
-}

---------------------------
-- See TcType.SyntaxOpType also for commentary
tcSyntaxOp :: CtOrigin
           -> SyntaxExprRn
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpRhoType               -- ^ overall result type
           -> ([TcSigmaType] -> [Mult] -> TcM a) -- ^ Type check any arguments,
                                                 -- takes a type per hole and a
                                                 -- multiplicity per arrow in
                                                 -- the shape.
           -> TcM (a, SyntaxExprTc)
-- ^ Typecheck a syntax operator
-- The operator is a variable or a lambda at this stage (i.e. renamer
-- output)t
tcSyntaxOp orig expr arg_tys res_ty
  = tcSyntaxOpGen orig expr arg_tys (SynType res_ty)

-- | Slightly more general version of 'tcSyntaxOp' that allows the caller
-- to specify the shape of the result of the syntax operator
tcSyntaxOpGen :: CtOrigin
              -> SyntaxExprRn
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a)
              -> TcM (a, SyntaxExprTc)
tcSyntaxOpGen orig (SyntaxExprRn op) arg_tys res_ty thing_inside
  = do { (expr, sigma) <- tcInferAppHead (op, noSrcSpan)
             -- Ugh!! But all this code is scheduled for demolition anyway
       ; traceTc "tcSyntaxOpGen" (ppr op $$ ppr expr $$ ppr sigma)
       ; (result, expr_wrap, arg_wraps, res_wrap)
           <- tcSynArgA orig op sigma arg_tys res_ty $
              thing_inside
       ; traceTc "tcSyntaxOpGen" (ppr op $$ ppr expr $$ ppr sigma )
       ; return (result, SyntaxExprTc { syn_expr = mkHsWrap expr_wrap expr
                                      , syn_arg_wraps = arg_wraps
                                      , syn_res_wrap  = res_wrap }) }
tcSyntaxOpGen _ NoSyntaxExprRn _ _ _ = panic "tcSyntaxOpGen"

{-
Note [tcSynArg]
~~~~~~~~~~~~~~~
Because of the rich structure of SyntaxOpType, we must do the
contra-/covariant thing when working down arrows, to get the
instantiation vs. skolemisation decisions correct (and, more
obviously, the orientation of the HsWrappers). We thus have
two tcSynArgs.
-}

-- works on "expected" types, skolemising where necessary
-- See Note [tcSynArg]
tcSynArgE :: CtOrigin
          -> HsExpr GhcRn -- ^ the operator to check (for error messages only)
          -> TcSigmaType
          -> SyntaxOpType                -- ^ shape it is expected to have
          -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a) -- ^ check the arguments
          -> TcM (a, HsWrapper)
           -- ^ returns a wrapper :: (type of right shape) ~~> (type passed in)
tcSynArgE orig op sigma_ty syn_ty thing_inside
  = do { (skol_wrap, (result, ty_wrapper))
           <- tcSkolemise Shallow GenSigCtxt sigma_ty $ \rho_ty ->
              go rho_ty syn_ty
       ; return (result, skol_wrap <.> ty_wrapper) }
    where
    go rho_ty SynAny
      = do { result <- thing_inside [rho_ty] []
           ; return (result, idHsWrapper) }

    go rho_ty SynRho   -- same as SynAny, because we skolemise eagerly
      = do { result <- thing_inside [rho_ty] []
           ; return (result, idHsWrapper) }

    go rho_ty SynList
      = do { (list_co, elt_ty) <- matchExpectedListTy rho_ty
           ; result <- thing_inside [elt_ty] []
           ; return (result, mkWpCastN list_co) }

    go rho_ty (SynFun arg_shape res_shape)
      = do { ( match_wrapper                         -- :: (arg_ty -> res_ty) ~~> rho_ty
             , ( ( (result, arg_ty, res_ty, op_mult)
                 , res_wrapper )                     -- :: res_ty_out ~~> res_ty
               , arg_wrapper1, [], arg_wrapper2 ) )  -- :: arg_ty ~~> arg_ty_out
               <- matchExpectedFunTys herald GenSigCtxt 1 (mkCheckExpType rho_ty) $
                  \ [ExpFunPatTy arg_ty] res_ty ->
                  do { arg_tc_ty <- expTypeToType (scaledThing arg_ty)
                     ; res_tc_ty <- expTypeToType res_ty

                         -- another nested arrow is too much for now,
                         -- but I bet we'll never need this
                     ; massertPpr (case arg_shape of
                                   SynFun {} -> False;
                                   _         -> True)
                                  (text "Too many nested arrows in SyntaxOpType" $$
                                   pprCtOrigin orig)

                     ; let arg_mult = scaledMult arg_ty
                     ; tcSynArgA orig op arg_tc_ty [] arg_shape $
                       \ arg_results arg_res_mults ->
                       tcSynArgE orig op res_tc_ty res_shape $
                       \ res_results res_res_mults ->
                       do { result <- thing_inside (arg_results ++ res_results) ([arg_mult] ++ arg_res_mults ++ res_res_mults)
                          ; return (result, arg_tc_ty, res_tc_ty, arg_mult) }}

           ; let fun_wrap = mkWpFun (arg_wrapper2 <.> arg_wrapper1) res_wrapper
                              (EqMultCo $ mkNomReflCo op_mult, arg_ty) res_ty
               -- NB: arg_ty comes from matchExpectedFunTys, so it has a
               -- fixed RuntimeRep, as needed to call mkWpFun.
           ; return (result, match_wrapper <.> fun_wrap) }
      where
        herald = ExpectedFunTySyntaxOp orig op

    go rho_ty (SynType the_ty)
      = do { wrap   <- tcSubTypePat orig GenSigCtxt the_ty rho_ty
           ; result <- thing_inside [] []
           ; return (result, wrap) }

-- works on "actual" types, instantiating where necessary
-- See Note [tcSynArg]
tcSynArgA :: CtOrigin
          -> HsExpr GhcRn -- ^ the operator we are checking (for error messages)
          -> TcSigmaType
          -> [SyntaxOpType]              -- ^ argument shapes
          -> SyntaxOpType                -- ^ result shape
          -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a) -- ^ check the arguments
          -> TcM (a, HsWrapper, [HsWrapper], HsWrapper)
            -- ^ returns a wrapper to be applied to the original function,
            -- wrappers to be applied to arguments
            -- and a wrapper to be applied to the overall expression
tcSynArgA orig op sigma_ty arg_shapes res_shape thing_inside
  = do { (match_wrapper, _mas, arg_tys, res_ty)
           <- matchActualFunTys herald orig (length arg_shapes) sigma_ty
              -- match_wrapper :: sigma_ty ~~> (arg_tys -> res_ty)
       ; ((result, res_wrapper), arg_wrappers)
           <- tc_syn_args_e (map scaledThing arg_tys) arg_shapes $ \ arg_results arg_res_mults ->
              tc_syn_arg    res_ty  res_shape  $ \ res_results ->
              thing_inside (arg_results ++ res_results) (map scaledMult arg_tys ++ arg_res_mults)
       ; return (result, match_wrapper, arg_wrappers, res_wrapper) }
  where
    herald = ExpectedFunTySyntaxOp orig op

    tc_syn_args_e :: [TcSigmaTypeFRR] -> [SyntaxOpType]
                  -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a)
                  -> TcM (a, [HsWrapper])
                    -- the wrappers are for arguments
    tc_syn_args_e (arg_ty : arg_tys) (arg_shape : arg_shapes) thing_inside
      = do { ((result, arg_wraps), arg_wrap)
               <- tcSynArgE     orig  op arg_ty  arg_shape  $ \ arg1_results arg1_mults ->
                  tc_syn_args_e          arg_tys arg_shapes $ \ args_results args_mults ->
                  thing_inside (arg1_results ++ args_results) (arg1_mults ++ args_mults)
           ; return (result, arg_wrap : arg_wraps) }
    tc_syn_args_e _ _ thing_inside = (, []) <$> thing_inside [] []

    tc_syn_arg :: TcSigmaTypeFRR -> SyntaxOpType
               -> ([TcSigmaTypeFRR] -> TcM a)
               -> TcM (a, HsWrapper)
                  -- the wrapper applies to the overall result
    tc_syn_arg res_ty SynAny thing_inside
      = do { result <- thing_inside [res_ty]
           ; return (result, idHsWrapper) }
    tc_syn_arg res_ty SynRho thing_inside
      = do { (inst_wrap, rho_ty) <- topInstantiate orig res_ty
               -- inst_wrap :: res_ty ~~> rho_ty
           ; result <- thing_inside [rho_ty]
           ; return (result, inst_wrap) }
    tc_syn_arg res_ty SynList thing_inside
      = do { (inst_wrap, rho_ty) <- topInstantiate orig res_ty
               -- inst_wrap :: res_ty ~~> rho_ty
           ; (list_co, elt_ty)   <- matchExpectedListTy rho_ty
               -- list_co :: [elt_ty] ~N rho_ty
           ; result <- thing_inside [elt_ty]
           ; return (result, mkWpCastN (mkSymCo list_co) <.> inst_wrap) }
    tc_syn_arg _ (SynFun {}) _
      = pprPanic "tcSynArgA hits a SynFun" (ppr orig)
    tc_syn_arg res_ty (SynType the_ty) thing_inside
      = do { wrap   <- addSubTypeCtxt res_ty the_ty $
                       tcSubType orig GenSigCtxt Nothing res_ty the_ty
           ; result <- thing_inside []
           ; return (result, wrap) }

{-
Note [Push result type in]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Unify with expected result before type-checking the args so that the
info from res_ty percolates to args.  This is when we might detect a
too-few args situation.  (One can think of cases when the opposite
order would give a better error message.)
experimenting with putting this first.

Here's an example where it actually makes a real difference

   class C t a b | t a -> b
   instance C Char a Bool

   data P t a = forall b. (C t a b) => MkP b
   data Q t   = MkQ (forall a. P t a)

   f1, f2 :: Q Char;
   f1 = MkQ (MkP True)
   f2 = MkQ (MkP True :: forall a. P Char a)

With the change, f1 will type-check, because the 'Char' info from
the signature is propagated into MkQ's argument. With the check
in the other order, the extra signature in f2 is reqd.
-}

{- *********************************************************************
*                                                                      *
                 Expanding record update
*                                                                      *
********************************************************************* -}

{- Note [Record Updates]
~~~~~~~~~~~~~~~~~~~~~~~~
To typecheck a record update, we expand it first.  Suppose we have
    data T p q = T1 { x :: Int, y :: Bool, z :: Char }
               | T2 { v :: Char }
               | T3 { x :: Int }
               | T4 { p :: Float, y :: Bool, x :: Int }
               | T5
Then the record update `e { x=e1, y=e2 }` expands as follows

       e { x=e1, y=e2 }
    ===>
       let { x' = e1; y' = e2 } in
       case e of
          T1 _ _ z -> T1 x' y' z
          T4 p _ _ -> T4 p y' x'
T2, T3 and T5 should not occur, so we omit them from the match.
The critical part of expansion is to identify T and then T1/T4.

Wrinkle [Disambiguating fields]

  As explained in Note [Disambiguating record updates] in GHC.Rename.Pat,
  to typecheck a record update we first need to disambiguate the field labels,
  in order to find a parent which has at least one constructor with all of the fields
  being updated.

  As mentioned in Note [Type-directed record disambiguation], we sometimes use
  type-directed disambiguation, although this mechanism is deprecated and
  scheduled for removal via the implementation of GHC proposal #366
  https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst.


All in all, this means that when typechecking a record update via expansion,
we take the following steps:

  (0) Perform a first typechecking pass on the record expression (`e` in the example above),
      to infer the type of the record being updated.
  (1) Disambiguate the record fields (potentially using the type obtained in (0)).
  (2) Expand the record update as described above, using an XXExprGhcRn.
      (a) Create a let-binding to share the record update right-hand sides.
      (b) Expand the record update to a case expression updating all the
          relevant constructors (those that have all of the fields being updated).
  (3) Typecheck the expanded code.

In (0), we call inferRho to infer the type of the record being updated. This returns the
inferred type of the record, together with a typechecked expression (of type HsExpr GhcTc)
and a collection of residual constraints.
We have no need for the latter two, because we will typecheck again in (D3). So, for
the time being (and until GHC proposal #366 is implemented), we simply drop them.

Wrinkle [Using IdSig]

  As noted above, we want to let-bind the updated fields to avoid code duplication:

    let { x' = e1; y' = e2 } in
    case e of
       T1 _ _ z -> T1 x' y' z
       T4 p _ _ -> T4 p y' x'

  However, doing so in a naive way would cause difficulties for type inference.
  For example:

    data R b = MkR { f :: (forall a. a -> a) -> (Int,b), c :: Int }
    foo r = r { f = \ k -> (k 3, k 'x') }

  If we expand to:

    ds_foo r =
      let f' = \ k -> (k 3, k 'x')
      in case r of
        MkR _ b -> MkR f' b

  then we are unable to infer an appropriately polymorphic type for f', because we
  never infer higher-rank types. To circumvent this problem, we proceed as follows:

    1. Obtain general field types by instantiating any of the constructors
       that contain all the necessary fields. (Note that the field type must be
       identical across different constructors of a given data constructor).
    2. Let-bind an 'IdSig' with this type. This amounts to giving the let-bound
       'Id's a partial type signature.

  In the above example, it's as if we wrote:

    ds_foo r =
      let f' :: (forall a. a -> a) -> (Int, _b)
          f' = \ k -> (k 3, k 'x')
      in case r of
        MkR _ b -> MkR f' b

  This allows us to compute the right type for f', and thus accept this record update.

Note [Type-directed record disambiguation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deprecation notice:
  The type-directed disambiguation mechanism for record updates described in
  this Note is deprecated, as per GHC proposal #366 (https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst).
  The removal of type-directed disambiguation for record updates is tracked
  in GHC ticket #19461, but progress towards this goal has stalled.

  Why? There are several suggested replacement mechanisms, such as:
    1. using module qualification to disambiguate,
    2. using OverloadedRecordUpdate for type-directed disambiguation
      (as described in Note [Overview of record dot syntax] in GHC.Hs.Expr).
  However, these solutions do not work in all situations:
    1. Module qualification doesn't work for fields defined in the current module,
       nor to disambiguate between constructors of different data family instances
       of a given parent data family TyCon.
    2. OverloadedRecordUpdate does not allow for type-changing record update,
       nor can it deal with fields with existentials or polytypes.
  There are also some avenues to improve the renamer's ability to disambiguate:
    - GHC ticket #23032 suggests using as-patterns to disambiguate in the renamer.
    - GHC proposal https://github.com/ghc-proposals/ghc-proposals/pull/537
      suggests a syntactic form of type-directed disambiguation that could be
      carried out in the renamer.
  Neither of these have been accepted/implemented at the time of writing (Sept 2025).
  This means that removal of type-directed disambiguation is currently stalled.

GHC tries to disambiguate record updates in the renamer, as described in
Note [Disambiguating record updates] in GHC.Rename.Pat. However, if the renamer
is unable to disambiguate, the renamer will defer to the typechecker: see
GHC.Tc.Gen.Expr.disambiguateRecordBinds, and in particular the auxiliary
function identifyParentLabels, which picks a parent for the record update
using the following additional mechanisms:

  (a) Use the type being pushed in, if it is already a TyConApp. The
      following are valid updates at type `R`:

        g :: R -> R
        g x = x { fld1 = 3 }

        g' x = x { fld1 = 3 } :: R

  (b) Use the type signature of the record expression, if it exists and
      is a TyConApp. Thus this is valid update at type `R`:

        h x = (x :: R) { fld1 = 3 }

Note that this type-directed disambiguation mechanism isn't very robust,
as it doesn't properly integrate with the rest of the typechecker.
For example, the following updates will all be rejected as ambiguous:

    let r :: R
        r = blah
    in r { foo = 3 }

    \r. (r { foo = 3 }, r :: R)

Record updates which require constraint-solving should instead use the
-XOverloadedRecordUpdate extension, as described in Note [Overview of record dot syntax].

Note [Unifying result types in tcRecordUpd]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After expanding and typechecking a record update in the way described in
Note [Record Updates], we must take care to unify the result types.

Example:

  type family F (a :: Type) :: Type where {}
  data D a = MkD { fld :: F a }

  f :: F Int -> D Bool -> D Int
  f i r = r { fld = i }

This record update expands to:

  let x :: F alpha -- metavariable
      x = i
  in case r of
    MkD _ -> MkD x

Because the type family F is not injective, our only hope for unifying the
metavariable alpha is through the result type of the record update, which tells
us that we should unify alpha := Int.

Test case: T10808.

Wrinkle [GADT result type in tcRecordUpd]

  When dealing with a GADT, we want to be careful about which result type we use.

  Example:

    data G a b where
      MkG :: { bar :: F a } -> G a Int

    g :: F Int -> G Float b -> G Int b
    g i r = r { bar = i }

    We **do not** want to use the result type from the constructor MkG, which would
    leave us with a result type "G alpha Int". Instead, we should use the result type
    from the GADT header, instantiating as above, to get "G alpha beta" which will get
    unified withy "G Int b".

    Test cases: T18809, HardRecordUpdate.

-}

-- | Expands a record update @record_expr { fld1 = e1, fld2 = e2 }@ into a case expression
-- that matches on the constructors of the record @r@, as described in
-- Note [Record Updates].
--
-- Returns a renamed but not-yet-typechecked expression, together with the
-- result type of this expanded record update.
expandRecordUpd :: LHsExpr GhcRn
                      -- ^ @record_expr@: expression to which the record update is applied
                 -> NE.NonEmpty (HsRecUpdParent GhcRn)
                      -- ^ Possible parent 'TyCon'/'PatSyn's for the record update,
                      -- with the associated constructors and field labels
                 -> [LHsRecUpdField GhcRn GhcRn]
                      -- ^ the record update fields
                 -> ExpRhoType
                      -- ^ the expected result type of the record update
                 -> TcM ( HsExpr GhcRn
                           -- Expanded record update expression
                        , TcType
                           -- result type of expanded record update
                        , HsCtxt
                           -- error context to push when typechecking
                           -- the expanded code
                        )
expandRecordUpd record_expr@(L lspan _) possible_parents rbnds res_ty
  = do {  -- STEP 0: typecheck the record_expr, the record to be updated.
          --
          -- Until GHC proposal #366 is implemented, we still use the type of
          -- the record to disambiguate its fields, so we must infer the record
          -- type here before we can expand. See Wrinkle [Disambiguating fields]
          -- in Note [Record Updates].
       ; ((_, record_rho), _lie) <- captureConstraints    $ -- see (1) below
                                    tcScalingUsage ManyTy $ -- see (2) below
                                    tcInferRho record_expr

            -- (1)
            -- Note that we capture, and then discard, the constraints.
            -- This `tcInferRho` is used *only* to identify the data type,
            -- so we can deal with field disambiguation.
            -- Then we are going to generate a expanded record update, including `record_expr`,
            -- and typecheck it from scratch.  We don't want to generate the constraints twice!

            -- (2)
            -- Record update drops some of the content of the record (namely the
            -- content of the field being updated). As a consequence, unless the
            -- field being updated is unrestricted in the record, we need an
            -- unrestricted record. Currently, we simply always require an
            -- unrestricted record.
            --
            -- Consider the following example:
            --
            -- data R a = R { self :: a }
            -- bad :: a ⊸ ()
            -- bad x = let r = R x in case r { self = () } of { R x' -> x' }
            --
            -- This should definitely *not* typecheck.

       -- STEP 1: disambiguate the record update by computing a single parent
       --         which has a constructor with all of the fields being updated.
       --
       -- See Note [Disambiguating record updates] in GHC.Rename.Pat.
       ; (cons, rbinds)
           <- disambiguateRecordBinds record_expr record_rho possible_parents rbnds res_ty
       ; let sel_ids       = map (unLoc . foLabel . unLoc . hfbLHS . unLoc) rbinds
             upd_fld_names = map idName sel_ids
             relevant_cons@(relevant_con NE.:| _) =
              case NE.nonEmpty $ nonDetEltsUniqSet cons of
                Just rel_cons -> rel_cons
                Nothing -> pprPanic "desugarRecordUpd: no relevant constructors" $
                              vcat [ text "record_expr:" <+> ppr record_expr ]

      -- STEP 2: expand the record update.
      --
      --  (a) Create new variables for the fields we are updating,
      --      so that we can share them across constructors.
      --
      --      Example:
      --
      --          e { x=e1, y=e2 }
      --
      --        We want to let-bind variables to `e1` and `e2`:
      --
      --          let x' :: Int
      --              x' = e1
      --              y' :: Bool
      --              y' = e2
      --          in ...

         -- Instantiate the type variables of any relevant constuctor
         -- with metavariables to obtain a type for each 'Id'.
         -- This will allow us to have 'Id's with polymorphic types
         -- by using 'IdSig'. See Wrinkle [Using IdSig] in Note [Record Updates].
       ; let (univ_tvs, ex_tvs, eq_spec, _, _, arg_tys, con_res_ty) = conLikeFullSig relevant_con
       ; (subst, tc_tvs) <- newMetaTyVars (univ_tvs ++ ex_tvs)
       ; let (actual_univ_tys, _actual_ex_tys) = splitAtList univ_tvs $ map mkTyVarTy tc_tvs

             -- See Wrinkle [GADT result type in tcRecordUpd]
             -- for an explanation of the following.
             ds_res_ty = case relevant_con of
               RealDataCon con
                 | not (null eq_spec) -- We only need to do this if we have actual GADT equalities.
                 -> mkFamilyTyConApp (dataConTyCon con) actual_univ_tys
               _ -> substTy subst con_res_ty

       -- Gather pairs of let-bound Ids and their right-hand sides,
       -- e.g. (x', e1), (y', e2), ...
       ; let mk_upd_id :: Name -> LHsFieldBind GhcTc fld (LHsExpr GhcRn) -> TcM (Name, (TcId, LHsExpr GhcRn))
             mk_upd_id fld_nm (L _ rbind)
               = do { let Scaled _ arg_ty = lookupNameEnv_NF arg_ty_env fld_nm
                          nm_occ = rdrNameOcc . nameRdrName $ fld_nm
                          actual_arg_ty = substTy subst arg_ty
                          rhs = hfbRHS rbind
                    ; (_co, actual_arg_ty) <- hasFixedRuntimeRep (FRRRecordUpdate fld_nm (unLoc rhs)) actual_arg_ty
                      -- We get a better error message by doing a (redundant) representation-polymorphism check here,
                      -- rather than delaying until the typechecker typechecks the let-bindings,
                      -- because the let-bound Ids have internal names.
                      -- (As we will typecheck the let-bindings later, we can drop this coercion here.)
                      -- See RepPolyRecordUpdate test.
                    ; nm <- newNameAt nm_occ generatedSrcSpan
                    ; let id = mkLocalId nm ManyTy actual_arg_ty
                      -- NB: create fresh names to avoid any accidental shadowing
                      -- occurring in the RHS expressions when creating the let bindings:
                      --
                      --  let x1 = e1; x2 = e2; ...
                      --
                      -- Above, we use multiplicity Many rather than the one associated to arg_ty.
                      -- Normally, there shouldn't be a difference, since it's a let binding.
                      -- But -XStrict can convert the let to a case, and this causes issues
                      -- in test LinearRecUpd. Since we don't support linear record updates,
                      -- using Many is simple and safe.
                    ; return (fld_nm, (id, rhs))
                    }
             arg_ty_env = mkNameEnv
                        $ zipWith (\ lbl arg_ty -> (flSelector lbl, arg_ty))
                            (conLikeFieldLabels relevant_con)
                            arg_tys

       ; traceTc "tcRecordUpd" $
           vcat [ text "upd_fld_names:" <+> ppr upd_fld_names
                , text "relevant_cons:" <+> ppr relevant_cons ]

       ; upd_ids <- zipWithM mk_upd_id upd_fld_names rbinds
       ; let updEnv :: UniqMap Name (Id, LHsExpr GhcRn)
             updEnv = listToUniqMap $ upd_ids

             make_pat :: ConLike -> LMatch GhcRn (LHsExpr GhcRn)
             -- As explained in Note [Record Updates], to expand
             --
             --   e { x=e1, y=e2 }
             --
             -- we generate a case statement, with an equation for
             -- each constructor of the record. For example, for
             -- the constructor
             --
             --   T1 :: { x :: Int, y :: Bool, z :: Char } -> T p q
             --
             -- we let-bind x' = e1, y' = e2 and generate the equation:
             --
             --   T1 _ _ z -> T1 x' y' z
             make_pat conLike = mkSimpleMatch RecUpd (noLocA [pat]) rhs
               where
                 (lhs_con_pats, rhs_con_args)
                    = zipWithAndUnzip mk_con_arg [1..] con_fields
                 pat = genSimpleConPat con lhs_con_pats
                 rhs = wrapGenSpan $ genHsApps con rhs_con_args
                 con = conLikeName conLike
                 con_fields = conLikeFieldLabels conLike

             mk_con_arg :: Int
                        -> FieldLabel
                        -> ( LPat GhcRn
                              -- LHS constructor pattern argument
                           , LHsExpr GhcRn )
                              -- RHS constructor argument
             mk_con_arg i fld_lbl =
               -- The following generates the pattern matches of the expanded `case` expression.
               -- For fields being updated (for example `x`, `y` in T1 and T4 in Note [Record Updates]),
               -- wildcards are used to avoid creating unused variables.
               case lookupUniqMap updEnv $ flSelector fld_lbl of
                 -- Field is being updated: LHS = wildcard pattern, RHS = appropriate let-bound Id.
                 Just (upd_id, _) -> (genWildPat, genLHsVar (idName upd_id))
                 -- Field is not being updated: LHS = variable pattern, RHS = that same variable.
                 _  -> let fld_nm = mkInternalName (mkBuiltinUnique i)
                                      (nameOccName $ flSelector $ fld_lbl)
                                      generatedSrcSpan
                       in (genVarPat fld_nm, genLHsVar fld_nm)

       -- STEP 2 (b): expand to HsCase, as per Note [Record Updates]
       ; let ds_expr :: HsExpr GhcRn
             ds_expr = HsLet noExtField (noLocA let_binds) (wrapGenSpan case_expr)

             case_expr :: HsExpr GhcRn
             case_expr = HsCase RecUpd (wrapGenSpan' (locA lspan) (unLoc record_expr))
                       $ mkMatchGroup (Generated OtherExpansion DoPmc) noAnn (wrapGenSpan matches)
             matches :: [LMatch GhcRn (LHsExpr GhcRn)]
             matches = map make_pat (NE.toList relevant_cons)

             let_binds :: HsLocalBindsLR GhcRn GhcRn
             let_binds = HsValBinds noAnn $ XValBindsLR
                       $ HsVBG upd_ids_lhs (map mk_idSig upd_ids)
             upd_ids_lhs :: [(RecFlag, LHsBindsLR GhcRn GhcRn)]
             upd_ids_lhs = [ (NonRecursive, [genSimpleFunBind (idName id) [] rhs])
                           | (_, (id, rhs)) <- upd_ids ]
             mk_idSig :: (Name, (Id, LHsExpr GhcRn)) -> LSig GhcRn
             mk_idSig (_, (id, _)) = wrapGenSpan (XSig $ IdSig id)
               -- We let-bind variables using 'IdSig' in order to accept
               -- record updates involving higher-rank types.
               -- See Wrinkle [Using IdSig] in Note [Record Updates].

        ; traceTc "expandRecordUpd" $
            vcat [ text "relevant_con:" <+> ppr relevant_con
                 , text "res_ty:" <+> ppr res_ty
                 , text "ds_res_ty:" <+> ppr ds_res_ty
                 , text "ds_expr:" <+> ppr ds_expr
                 ]
        ; return (ds_expr, ds_res_ty, RecordUpdCtxt relevant_cons upd_fld_names ex_tvs) }



{- *********************************************************************
*                                                                      *
                 Record bindings
*                                                                      *
**********************************************************************-}

-- | Disambiguate the fields in a record update.
--
-- Most of the disambiguation has been done by the renamer; this function
-- performs a final type-directed disambiguation pass, as explained in
-- Note [Type-directed record disambiguation].
disambiguateRecordBinds :: LHsExpr GhcRn -> TcRhoType
                        -> NE.NonEmpty (HsRecUpdParent GhcRn)
                        -> [LHsRecUpdField GhcRn GhcRn] -> ExpRhoType
                        -> TcM (UniqSet ConLike, [LHsRecUpdField GhcTc GhcRn])
disambiguateRecordBinds record_expr record_rho possible_parents rbnds res_ty
  = do { fam_inst_envs <- tcGetFamInstEnvs
         -- Identify a single parent, using type-directed disambiguation
         -- if necessary. (Note that type-directed disambiguation of
         -- record field updates is is scheduled for removal, as per
         -- Note [Type-directed record disambiguation].)
       ; TcRecUpdParent
           { tcRecUpdLabels = lbls
           , tcRecUpdCons   = cons }
             <- identifyParentLabels fam_inst_envs possible_parents
         -- Pick the right selector with that parent for each field
       ; rbnds' <- zipWithM lookupField (NE.toList lbls) rbnds
       ; return (cons, rbnds') }
  where

    -- Try to identify a single parent, using type-directed disambiguation.
    --
    -- Any non-type-directed disambiguation will have been done already.
    -- See GHC.Rename.Env.lookupRecUpdFields.
    identifyParentLabels :: FamInstEnvs
                         -> NE.NonEmpty (HsRecUpdParent GhcRn)
                         -> TcM (HsRecUpdParent GhcTc)
    identifyParentLabels fam_inst_envs possible_parents
      = case possible_parents of

        -- Exactly one possible parent for the record update!
        p NE.:| [] -> lookup_parent_flds p

        -- Multiple possible parents: try harder to disambiguate.
        -- Can we get a parent TyCon from the pushed-in type?
        --
        -- See (a) in Note [Type-directed record disambiguation] in GHC.Rename.Pat.
        _ NE.:| _ : _
          | Just tc <- tyConOfET fam_inst_envs res_ty
          -> do { reportAmbiguousUpdate possible_parents tc
                ; try_disambiguated_tycon tc possible_parents }

        -- Does the expression being updated have a type signature?
        -- If so, try to extract a parent TyCon from it.
        --
        -- See (b) inNote [Type-directed record disambiguation] in GHC.Rename.Pat.
          | Just {} <- obviousSig (unLoc record_expr)
          , Just tc <- tyConOf fam_inst_envs record_rho
          -> do { reportAmbiguousUpdate possible_parents tc
                ; try_disambiguated_tycon tc possible_parents }

        -- Nothing else we can try...
        p1 NE.:| p2 : ps
          -> do { p1 <- tcLookupRecSelParent p1
                ; p2 <- tcLookupRecSelParent p2
                ; ps <- mapM tcLookupRecSelParent ps
                ; failWithTc $ TcRnBadRecordUpdate (getUpdFieldLbls rbnds)
                             $ MultiplePossibleParents (p1, p2, ps) }

    -- Try to use the 'TyCon' we learned from type-directed disambiguation.
    -- This might not work, if it doesn't match up with any of the parents we had
    -- computed on the basis of the field labels.
    -- (See test cases overloadedrecfields01 and T21946.)
    try_disambiguated_tycon :: TyCon
                            -> NE.NonEmpty (HsRecUpdParent GhcRn)
                            -> TcM (HsRecUpdParent GhcTc)
    try_disambiguated_tycon tc pars
      = do { pars <- mapMaybeM (fmap (guard_parent tc) . lookup_parent_flds) (NE.toList pars)
           ; case pars of
               [par] -> return par
               []    -> do { pars <- mapM tcLookupRecSelParent possible_parents
                           ; failWithTc $ TcRnBadRecordUpdate (getUpdFieldLbls rbnds)
                                        $ InvalidTyConParent tc pars }
               _     -> pprPanic "try_disambiguated_tycon: more than 1 valid parent"
                          (ppr $ map tcRecUpdParent pars) }

    guard_parent :: TyCon -> HsRecUpdParent GhcTc -> Maybe (HsRecUpdParent GhcTc)
    guard_parent disamb_tc cand_parent@(TcRecUpdParent { tcRecUpdParent = cand_tc })
      = do { guard (RecSelData disamb_tc == cand_tc)
           ; return cand_parent }

    lookup_parent_flds :: HsRecUpdParent GhcRn
                       -> TcM (HsRecUpdParent GhcTc)
    lookup_parent_flds par@(RnRecUpdParent { rnRecUpdLabels = lbls, rnRecUpdCons = cons })
      = do { let cons' :: NonDetUniqFM ConLike ConLikeName
                 cons' = NonDetUniqFM $ unsafeCastUFMKey $ getUniqSet cons
                 lookup_one con = tcLookupConLike (noUserRdr $ getName con)
           ; cons <- traverse lookup_one cons'
           ; tc   <- tcLookupRecSelParent par
           ; return $
               TcRecUpdParent
                 { tcRecUpdParent = tc
                 , tcRecUpdLabels = lbls
                 , tcRecUpdCons   = unsafeUFMToUniqSet $ getNonDet cons } }

    lookupField :: FieldGlobalRdrElt
                -> LHsRecUpdField GhcRn GhcRn
                -> TcM (LHsRecUpdField GhcTc GhcRn)
    lookupField fld_gre (L l upd)
      = do { let L loc af = hfbLHS upd
                 lbl      = fieldOccRdrName af
                 mb_gre   = pickGREs lbl [fld_gre]
                      -- NB: this GRE can be 'Nothing' when in GHCi.
                      -- See test T10439.

             -- Mark the record fields as used, now that we have disambiguated.
             -- There is no risk of duplicate deprecation warnings, as we have
             -- not marked the GREs as used previously.
           ; setSrcSpanA loc $ mapM_ (addUsedGRE AllDeprecationWarnings) mb_gre
           ; sel <- tcLookupId (greName fld_gre)
           ; return $ L l HsFieldBind
               { hfbAnn = hfbAnn upd
               , hfbLHS = L (l2l loc) (FieldOcc lbl  (L (l2l loc) sel))
               , hfbRHS = hfbRHS upd
               , hfbPun = hfbPun upd
               } }

    -- The type-directed disambiguation mechanism is scheduled for removal,
    -- as per Note [Type-directed record disambiguation].
    -- So we emit a warning whenever the user relies on it.
    reportAmbiguousUpdate :: NE.NonEmpty (HsRecUpdParent GhcRn)
                          -> TyCon -> TcM ()
    reportAmbiguousUpdate parents parent_type =
        setSrcSpan loc $ addDiagnostic $ TcRnAmbiguousRecordUpdate rupd parent_type
      where
        rupd = RecordUpd { rupd_expr = record_expr
                         , rupd_flds =
                             RegularRecUpdFields
                              { xRecUpdFields = parents
                              , recUpdFields  = rbnds }
                         , rupd_ext = noExtField }
        loc  = getLocA (head rbnds)

{-
Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcCheckPolyExprNC (in tcRecordField),
   passing the field type as the expected argument type.

This extends OK when the field types are universally quantified.
-}

tcRecordBinds
        :: ConLike
        -> [Scaled TcType]     -- Expected type for each field
        -> HsRecordBinds GhcRn
        -> TcM (HsRecordBinds GhcTc)

tcRecordBinds con_like arg_tys (HsRecFields x rbinds dd)
  = do  { mb_binds <- mapM do_bind rbinds
        ; return (HsRecFields x (catMaybes mb_binds) dd) }
  where
    fields = map flSelector $ conLikeFieldLabels con_like
    flds_w_tys = zipEqual fields arg_tys

    do_bind :: LHsRecField GhcRn (LHsExpr GhcRn)
            -> TcM (Maybe (LHsRecField GhcTc (LHsExpr GhcTc)))
    do_bind (L l fld@(HsFieldBind { hfbLHS = f
                                 , hfbRHS = rhs }))

      = do { mb <- tcRecordField con_like flds_w_tys f rhs
           ; case mb of
               Nothing         -> return Nothing
               Just (f', rhs') -> return (Just (L l (HsFieldBind
                                                     { hfbAnn = hfbAnn fld
                                                     , hfbLHS = f'
                                                     , hfbRHS = rhs'
                                                     , hfbPun = hfbPun fld}))) }

tcRecordField :: ConLike -> Assoc Name (Scaled Type)
              -> LFieldOcc GhcRn -> LHsExpr GhcRn
              -> TcM (Maybe (LFieldOcc GhcTc, LHsExpr GhcTc))
tcRecordField con_like flds_w_tys (L loc (FieldOcc rdr (L l sel_name))) rhs
  | Just (Scaled field_mult field_ty) <- assocMaybe flds_w_tys sel_name
      = addErrCtxt (FieldCtxt field_lbl)$
        do { rhs' <- tcScalingUsage field_mult $ tcCheckPolyExprNC rhs field_ty
           ; hasFixedRuntimeRep_syntactic (FRRRecordCon rdr (unLoc rhs'))
                field_ty
           ; let field_id = mkUserLocal (nameOccName sel_name)
                                        (nameUnique sel_name)
                                        field_mult field_ty (locA loc)
                -- Yuk: the field_id has the *unique* of the selector Id
                --          (so we can find it easily)
                --      but is a LocalId with the appropriate type of the RHS
                --          (so the expansion knows the type of local binder to make)
           ; return (Just (L loc (FieldOcc rdr (L l field_id)), rhs')) }
      | otherwise
      = do { addErrTc (badFieldConErr (getName con_like) field_lbl)
           ; return Nothing }
  where
        field_lbl = FieldLabelString $ fastStringToShortText $ occNameFS $ rdrNameOcc rdr


checkMissingFields ::  ConLike -> HsRecordBinds GhcRn -> [Scaled TcType] -> TcM ()
checkMissingFields con_like rbinds arg_tys
  | null field_labels   -- Not declared as a record;
                        -- But C{} is still valid if no strict fields
  = if any isBanged field_strs then
        -- Illegal if any arg is strict
        addErrTc (TcRnMissingStrictFields con_like [])
    else do
        when (notNull field_strs && null field_labels) $ do
          let msg = TcRnMissingFields con_like []
          (diagnosticTc True msg)

  | otherwise = do              -- A record
    unless (null missing_s_fields) $ do
        fs <- liftZonkM $ zonk_fields missing_s_fields
        -- It is an error to omit a strict field, because
        -- we can't substitute it with (error "Missing field f")
        addErrTc (TcRnMissingStrictFields con_like fs)

    warn <- woptM Opt_WarnMissingFields
    when (warn && notNull missing_ns_fields) $ do
        fs <- liftZonkM $ zonk_fields missing_ns_fields
        -- It is not an error (though we may want) to omit a
        -- lazy field, because we can always use
        -- (error "Missing field f") instead.
        let msg = TcRnMissingFields con_like fs
        diagnosticTc True msg

  where
    -- we zonk the fields to get better types in error messages (#18869)
    zonk_fields fs = forM fs $ \(str,ty) -> do
        ty' <- zonkTcType ty
        return (str,ty')
    missing_s_fields
        = [ (flLabel fl, scaledThing ty) | (fl,str,ty) <- field_info,
                 isBanged str,
                 not (fl `elemField` field_names_used)
          ]
    missing_ns_fields
        = [ (flLabel fl, scaledThing ty) | (fl,str,ty) <- field_info,
                 not (isBanged str),
                 not (fl `elemField` field_names_used)
          ]

    field_names_used = hsRecFields rbinds
    field_labels     = conLikeFieldLabels con_like

    field_info = zip3 field_labels field_strs arg_tys

    field_strs = conLikeImplBangs con_like

    fl `elemField` flds = any (\ fl' -> flSelector fl == fl') flds
