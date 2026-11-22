{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE MonadComprehensions  #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures   #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typecheck some @Matches@
module GHC.Tc.Gen.Match
   ( tcFunBindMatches
   , tcCaseMatches, tcIndexedCaseMatches
   , tcRetainedCase
   , tcRetainedCallCase, tcEmptyRetainedScrutinee
   , tcLambdaMatches
   , tcMatches
   , tcOpenImplicitPackage
   , tcOpenImplicitPackageAt
   , implicitPayloadType
   , matchesImplicitPayloadPat
   , tcGRHSNE
   , tcGRHSsPat
   , TcStmtChecker
   , TcExprStmtChecker
   , TcCmdStmtChecker
   , tcStmts
   , tcStmtsAndThen
   , tcDoStmts
   , tcBody
   , tcScopedRuntimeBody, tcCloseRuntimeScope
   , tcDoStmt
   , tcGuardStmt
   , checkArgCounts
   )
where

import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcSyntaxOp, tcInferRho, tcInferRhoFRRNC
                                       , tcMonoLExprNC, tcExpr
                                       , tcCheckMonoExpr, tcCheckMonoExprNC
                                       , tcCheckPolyExpr, tcPolyLExpr )

import GHC.Rename.Utils ( bindLocalNames, wrapGenSpan' )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Solver ( tcCheckGivens, tcNormalise, tcRewriteFamilyAxioms )
import GHC.Tc.Solver.InertSet ( emptyInertSet )
import GHC.Tc.Solver.Monad ( runTcSEqualities )
import GHC.Tc.Solver.Rewrite ( rewriteFamilyAxioms )
import GHC.Tc.Zonk.TcType ( zonkTcType )
import GHC.Tc.Gen.Pat
import GHC.Tc.Gen.Head( tcCheckId )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Gen.Bind
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic )
import GHC.Tc.Utils.Unify
import GHC.Tc.Types.Origin
import GHC.Tc.Types.ErrCtxt( UserTypeCtxt( GenSigCtxt ), pprUserTypeCtxt )
import GHC.Tc.Types.Evidence
import GHC.Rename.Env ( irrefutableConLikeTc )

import GHC.Core.Multiplicity
import GHC.Core.UsageEnv
import GHC.Core.TyCon
import GHC.Core ( Expr(Var, App, Type), exprToNativeIndex )
import GHC.Core.TyCo.Rep ( Type(DepFunTy, ExistsTy, ForAllTy, FunTy, ft_af, TermVarTy, TyConApp) )
import GHC.Core.Subst ( extendIdSubstWithClone )
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Type ( coreFullView, expandTypeSynonyms, isManyTy, splitTyConApp_maybe
                     , anyFreeVarsOfType, typeKind, isNativeIndexFunctionId, isNativeOpaqueValueId
                     , isInvisibleForAllTyFlag, isNativeIndexId, nativeIndexDataConTyArgs
                     , splitFunTy_maybe, nativeCallSpine_maybe, substTyWith )
import GHC.Core.Coercion ( CoercionR, MCoercion(MRefl, MCo), Role(Representational), mkCoVarCo, mkCoCast, coercionType, isReflCo
                         , mkSymCo, mkSubCo, coercionKind, coercionLKind
                         , mkFunCo, mkFunResCo, mkHomoForAllCos
                         , mkTransCo, mkNomReflCo, mkRepReflCo, mkDepFunCoWithVis )
import GHC.Core.DataCon ( DataCon, dataConCannotMatch, dataConName, dataConSourceArity, dataConTyCon, dataConUnivTyVars, dataConWorkId )
import GHC.Core.ConLike ( ConLike(RealDataCon) )
import GHC.Core.Predicate ( isEqClassPred, getEqPredTys_maybe, mkNomEqPred, scopedSort )
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Utils ( applyTypeToArgs, exprType, isEmptyTy )
-- Create chunkified tuple types for monad comprehensions
import GHC.Core.Make.BigTuple

import GHC.Hs

import GHC.Driver.Config.HsToCore.Ticks ( initTicksConfig )
import GHC.Driver.Session ( getDynFlags, sccProfilingEnabled )
import GHC.HsToCore.Ticks ( TicksConfig(ticks_passes), TickishType(Breakpoints) )

import GHC.Builtin.WiredIn.Types
import GHC.Builtin.WiredIn.Prim

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Monad (anyM, mapAccumLM)

import GHC.Types.Name
import GHC.Types.Name.Env (anyNameEnv)
import GHC.Types.Name.Reader
import GHC.Types.Name.Set
import GHC.Types.Unique.Set (sizeUniqSet)
import GHC.Types.Id
import GHC.Types.Var ( binderFlag, EvVar, DepFunTyFlag(..), ExistsTyFlag(ImplicitExists), FunTyFlag, VarBndr(Bndr), varType, tyVarKind, isId, isCoVar, isTyVar, isVisibleFunArg, isInferredForAllTyFlag )
import GHC.Types.Var.Env ( mkInScopeSetList, unitVarEnv )
import GHC.Types.SrcLoc
import GHC.Types.Basic( VisArity, RecFlag(NonRecursive), TypeOrKind(TypeLevel), Origin(Generated), GenReason(OtherExpansion), DoPmc(SkipPmc) )
import GHC.Data.Bag ( listToBag, unionBags )
import GHC.Data.FastString ( fsLit )
import GHC.Data.Pair ( pSnd )
import GHC.Data.Maybe ( catMaybes, firstJustsM, fromMaybe, isJust, isNothing )

import qualified GHC.Data.List.NonEmpty as NE
import Data.Data ( Data, cast, gmapQ, gmapT )

import Control.Monad
import Control.Arrow ( second )
import Data.Foldable (toList)
import Data.List (find, partition, transpose, zipWith4)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe, maybeToList)

import qualified GHC.LanguageExtensions as LangExt


{-
************************************************************************
*                                                                      *
\subsection{tcFunBindMatches, tcCaseMatches}
*                                                                      *
************************************************************************

`tcFunBindMatches` typechecks a `[Match]` list which occurs in a
`FunBind`.  The second argument is the name of the function, which
is used in error messages.  It checks that all the equations have the
same number of arguments before using `tcMatches` to do the work.
-}

tcFunBindMatches :: UserTypeCtxt
                 -> Name            -- Function name
                 -> Mult            -- The multiplicity of the binder
                 -> MatchGroup GhcRn (LHsExpr GhcRn)
                 -> [ExpPatType]    -- Scoped skolemised binders
                 -> ExpRhoType      -- Expected type of function; caller
                                    -- has skolemised any outer forall's
                 -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
-- See Note [Skolemisation overview] in GHC.Tc.Utils.Unify
tcFunBindMatches ctxt fun_name mult source_matches invis_pat_tys source_exp_ty
  = assertPpr (funBindPrecondition source_matches) (pprMatches source_matches) $
    do  {  -- Check that they all have the same no of arguments
          source_arity <- checkArgCounts source_matches
        ; let (visibility_wrap, matches, exp_ty, arity)
                | Check ty <- source_exp_ty
                , (indices@(_:_), co) <- exposeInvisibleRetained ty
                = ( mkWpCastR co
                  , source_matches { mg_alts = fmap (map (addInvisibleRetainedPatterns indices)) (mg_alts source_matches) }
                  , mkCheckExpType (coercionLKind co)
                  , source_arity + length indices )
                | otherwise = (idHsWrapper, source_matches, source_exp_ty, source_arity)
              herald = ExpectedFunTyMatches (NameThing fun_name) source_matches
              tc_matches pat_tys rhs_ty = tcScalingUsage mult $
                do { traceTc "tcFunBindMatches 2" $
                     vcat [ text "ctxt:" <+> pprUserTypeCtxt ctxt
                          , text "arity:" <+> ppr arity
                          , text "invis_pat_tys:" <+> ppr invis_pat_tys
                          , text "pat_tys:" <+> ppr pat_tys
                          , text "rhs_ty:" <+> ppr rhs_ty ]
                   ; tcFunMatchesWithImplicitPackages mctxt (invis_pat_tys ++ pat_tys) rhs_ty matches }
        ; traceTc "tcFunBindMatches 1" (ppr fun_name $$ ppr mult $$ ppr exp_ty $$ ppr arity)

        ; (wrap_fun, r)
             <- case exp_ty of
                  Check ty | hasRetainedFunTy ty
                    -> tcScalingUsage mult $
                       tcRetainedMatches herald ctxt mctxt arity invis_pat_tys ty matches
                  _ -> matchExpectedFunTys herald ctxt arity exp_ty tc_matches

        ; return (visibility_wrap <.> wrap_fun, r) }
  where
    mctxt  = mkPrefixFunRhs (noLocA fun_name) noAnn

-- | Expose leading implicit runtime binders and preserve their scope in the cast.
exposeInvisibleRetained :: Type -> ([Id], CoercionR)
exposeInvisibleRetained ty
  | DepFunTy DepFunInvisible binder body <- coreFullView ty
  = let (binders, co) = exposeInvisibleRetained body
    in ( binder : binders
       , mkDepFunCoWithVis DepFunVisible DepFunInvisible binder MRefl co )
  | otherwise = ([], mkRepReflCo ty)

-- | Place generated runtime patterns after the written erased prefix.
addInvisibleRetainedPatterns :: [Id] -> LMatch GhcRn body -> LMatch GhcRn body
addInvisibleRetainedPatterns indices (L loc match@Match { m_pats = L pat_loc pats })
  = L loc (match { m_pats = L pat_loc (erased ++ generated ++ values) })
  where
    (erased, values) = span (not . isVisArgLPat) pats
    generated = map (noLocA . VarPat noExtField . noLocA . idName) indices

-- | Check the original equations beneath lazy openings of known package columns.
-- See Note [Implicit existential function patterns].
tcFunMatchesWithImplicitPackages
  :: HsMatchContextRn -> [ExpPatType] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcFunMatchesWithImplicitPackages ctxt pat_tys exp_ty group
  | Check result <- exp_ty
  , any is_dot_arg pat_tys
  , let columns = transpose (map (filter isVisArgLPat . hsLMatchPats) (unLoc (mg_alts group)))
        plan = select pat_tys columns
  , any (isJust . snd) plan
  = do { args <- mapM new_arg [scaled | ExpFunPatTy scaled <- pat_tys]
       ; body <- go result plan args [] []
       ; strict <- xoptM LangExt.Strict
       ; let arg_types = map (\arg -> Scaled (idMult arg) (idType arg)) args
             arg_pat arg = let var = noLocA (VarPat noExtField (noLocA arg))
                           in if strict then noLocA (LazyPat noExtField var) else var
             equation = mkSimpleMatch ctxt (noLocA (map arg_pat args)) (noLocA body)
       ; return (MG { mg_alts = noLocA [equation]
                    , mg_ext = MatchGroupTc arg_types result (Generated OtherExpansion SkipPmc) }) }
  | otherwise
  = tcExprMatchesWithWhereFrom id ctxt pat_tys exp_ty group
  where
    is_dot_arg (ExpFunPatTy (Scaled _ (Check ty)))
      | ExistsTy ImplicitExists _ _ <- coreFullView ty = True
    is_dot_arg _ = False

    select [] _ = []
    select (pat_ty:rest) columns
      | isVisibleExpPatType pat_ty, pats:later <- columns
      = (pat_ty, source_span pat_ty pats) : select rest later
      | otherwise
      = (pat_ty, Nothing) : select rest columns

    source_span (ExpFunPatTy (Scaled _ (Check ty))) pats =
      getLocA <$> find (matchesImplicitPayloadPat ty) pats
    source_span _ _ = Nothing

    new_arg (Scaled mult ty) = do { ty' <- readExpType ty
                                 ; newSysLocalId (fsLit "package_arg") mult ty' }

    go result ((ExpFunPatTy _, open):rest) (arg:args) checked_tys operands =
      case open of
        Just loc -> tcOpenImplicitPackage loc (idType arg) (nlHsVar arg) result $ \payload ->
          go result rest args (payload_pat payload : checked_tys) (nlHsVar payload : operands)
        Nothing -> go result rest args (payload_pat arg : checked_tys) (nlHsVar arg : operands)
      where
        payload_pat p = mkCheckExpFunPatTy (Scaled (idMult arg) (idType p))
    go result ((pat_ty@ExpForAllPatTy{}, _):rest) args checked_tys operands =
      go result rest args (pat_ty : checked_tys) operands
    go _ [] [] checked_tys operands =
      do { inner <- tcExprMatchesWithWhereFrom id ctxt (reverse checked_tys) exp_ty group
         ; return $ case operands of
             [operand] -> HsCase ctxt operand inner
             _ -> unLoc (mkHsApps (L (getLoc (mg_alts inner)) (HsLam noAnn LamCases inner))
                                  (reverse operands)) }
    go _ _ _ _ _ = panic "tcFunMatchesWithImplicitPackages"

{- Note [Implicit existential function patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A data constructor of a dot package's payload selects elimination in that
argument column. Check the original equations and guards together beneath
one opening for each selected argument. The normal matcher uses the original
arrow multiplicities. Its result type is outside every witness scope.

The outer equations bind only generated arguments. Apply the checked inner
match group to their payloads and to the other ordinary arguments. Unpack is
lazy, so the source match group still determines clause order and demand.
With Strict, an outer lazy pattern prevents a generated argument from
becoming strict before any source pattern runs. The inner matches retain
their FunRhs context for coverage checks and match failures.
-}

-- | Open checked dot binders with the result type outside every scope.
tcOpenImplicitPackage :: SrcSpan -> TcType -> LHsExpr GhcTc -> TcRhoType
                      -> (Id -> TcM (HsExpr GhcTc)) -> TcM (HsExpr GhcTc)
tcOpenImplicitPackage = tcOpenImplicitPackageAt ManyTy

-- | Open checked dot binders with the multiplicity of the payload pattern.
-- See Note [Type synonyms in implicit existential openings].
tcOpenImplicitPackageAt :: Mult -> SrcSpan -> TcType -> LHsExpr GhcTc -> TcRhoType
                        -> (Id -> TcM (HsExpr GhcTc)) -> TcM (HsExpr GhcTc)
tcOpenImplicitPackageAt mult loc schema checked result_ty check_body = open schema checked
  where
    open schema checked =
      case coreFullView schema of
        ExistsTy ImplicitExists binder payload_ty ->
          do { let origin = UnifyForAllSkol schema
             ; skol <- mkSkolemInfo origin
             ; uniq <- newUnique
             ; let name = mkSystemNameAt uniq (getOccName binder) loc
             ; witness <- pushTcLevelM_ $ newSkolemTyVar skol name (tyVarKind binder)
             ; let body_ty = substTyWith [binder] [mkTyVarTy witness]
                                         (expandTypeSynonyms payload_ty)
             ; payload <- newSysLocalId (fsLit "unpacked") mult body_ty
             ; (ev_binds, body) <- setSrcSpan loc $
                 checkConstraints origin [witness] [] $
                   case coreFullView body_ty of
                     ExistsTy ImplicitExists _ _ -> open body_ty (noLocA (mkHsVar (noLocA payload)))
                     _ -> check_body payload
             ; return (XExpr (NativeExistsUnpack witness payload result_ty checked
                                 (mkHsDictLet ev_binds (noLocA body)))) }
        _ -> failWithTc (TcRnExistsPack (ExistsPackInvalidSchema schema))

{- Note [Type synonyms in implicit existential openings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The result type of Unpack is outside the witness's scope. Suppose a
package contains Tile (Constant shape) shape, where Constant is a synonym
for Bool. A reader can infer Constant shape as its result element type.
The synonym is equal to Bool, but its argument still refers to shape in
Core. Expand synonyms in the payload before checking the reader. This
keeps discarded arguments out of the inferred result. Any genuine use of
the witness stays in scope for checking the reader.
-}

{- Note [Invisible retained definition patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The signature binds each leading invisible retained name in the definition.
See Note [Invisible retained definitions] in GHC.Rename.Bind. After outer
skolemisation, add an ordinary runtime variable pattern for each such name.
Keep written erased patterns first. Check the patterns against the visible
endpoints of the telescope, then cast the complete value to its declared
visibility. Keep the written equations for the arity check and its diagnostic.

This is a binding form, including when the equation has no written arguments.
The generated patterns follow ordinary Haskell argument demand, including
Strict. Applying the visibility cast itself adds no lambda or new demand.
Agda also adds implicit arguments to the clauses of a signed definition.
See docs/dependent-haskell/REFERENCES.md#invisible-retained-binders-next-candidate.
-}

-- | Check source patterns against an expected retained function telescope.
-- See Note [Checked retained lambdas].
tcRetainedMatches :: ExpectedFunTyCtxt -> UserTypeCtxt -> HsMatchContextRn
                  -> VisArity -> [ExpPatType] -> Type
                  -> MatchGroup GhcRn (LHsExpr GhcRn)
                  -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedMatches herald ctxt mctxt arity invis_pat_tys ty matches
  | DepFunTy DepFunVisible callback _ <- coreFullView ty
  , isNativeIndexFunctionId callback
  , [first_source, second_source] <- constructor_columns
  = do { (wrap, (pat_tys, plan, rhs_ty)) <- tcRetainedConArgs herald ctxt arity ty
       ; let visible = filter (isRetainedSourceArg . snd) (zip [0..] plan)
       ; case (drop first_source visible, drop second_source visible) of
           ((i, RetainedConIndex first) : _, (j, RetainedConIndex second) : _)
             | isNativeIndexId first, isNativeIndexId second
             -> do { checked <- tcRetainedMatrixMatches (i, first) (j, second)
                                 invis_pat_tys pat_tys plan rhs_ty matches
                   ; return (wrap, checked) }
           _ -> function_pattern_error }
  | has_function_binder ty
  , not (one_variable_equation matches)
  = function_pattern_error
  | otherwise
  = case unLoc (mg_alts matches) of
      [L loc match@(Match { m_pats = L pat_loc pats })]
        | not (first_retained_constructor pats) ->
          tcRetainedVarMatches herald ctxt ty pats $ \pat_tys rhs_ty boundary names ->
            let match' = match { m_pats = L pat_loc (bindRetainedWildcards names pats) }
                prepared = matches { mg_alts = L (getLoc (mg_alts matches)) [L loc match'] }
            in tcExprMatchesWithWhereFrom (const match) mctxt (invis_pat_tys ++ pat_tys) rhs_ty
                 (maybe prepared (\(prefix, indices) -> splitRetainedSuffix prefix indices prepared) boundary)
      _ | DepFunTy DepFunVisible binder _ <- coreFullView ty ->
          do { index <- newSysLocalId (occNameFS (nameOccName (idName binder))) ManyTy (varType binder)
             ; let body_ty = applyTypeToArgs ty [Var index]
             ; (wrap, (pat_tys, argument_plan, rhs_ty)) <-
                 tcRetainedConArgs herald ctxt (arity - 1) body_ty
             ; result <- tcRetainedConMatches index invis_pat_tys pat_tys argument_plan rhs_ty matches
             ; return (liftRetainedLambdaWrap index wrap, result) }
        | otherwise -> failWithTc $
            TcRnRetainedQuantificationError RetainedConstructorCoverage
  where
    function_pattern_error = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionPatterns

    constructor_columns =
      [ column
      | (column, pats) <- zip [0..] (transpose visible_rows)
      , any constructor_pat pats ]
    visible_rows = map (filter isVisArgLPat . hsLMatchPats) (unLoc (mg_alts matches))

    -- See Note [Function-valued retained binders] in GHC.Core.Type.
    has_function_binder :: Type -> Bool
    has_function_binder ty = case coreFullView ty of
      DepFunTy _ binder body -> isNativeIndexFunctionId binder || has_function_binder body
      ForAllTy _ _ body -> has_function_binder body
      FunTy _ _ _ _ body -> has_function_binder body
      _ -> False

    one_variable_equation :: MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
    one_variable_equation (MG { mg_alts = L _ [L _ (Match { m_pats = L _ pats })] })
      = variable_retained_patterns ty (filter isVisArgLPat pats)
    one_variable_equation _ = False

    variable_retained_patterns :: Type -> [LPat GhcRn] -> Bool
    variable_retained_patterns _ [] = True
    variable_retained_patterns ty (pat : pats) = case coreFullView ty of
      DepFunTy DepFunVisible _ body ->
        isRetainedVarPat pat && variable_retained_patterns body pats
      ForAllTy _ (Bndr _ Required) body -> variable_retained_patterns body pats
      FunTy af _ _ _ body
        | isVisibleFunArg af -> variable_retained_patterns body pats
        | otherwise -> variable_retained_patterns body (pat : pats)
      ForAllTy _ binder body
        | isInvisibleForAllTyFlag (binderFlag binder) ->
            variable_retained_patterns body (pat : pats)
      _ -> False

    first_retained_constructor pats
      | DepFunTy DepFunVisible _ _ <- coreFullView ty
      = case filter isVisArgLPat pats of
          pat : _ -> constructor_pat pat
          _ -> False
      | otherwise = False
    constructor_pat (L _ (ParPat _ pat)) = constructor_pat pat
    constructor_pat pat | ConPat{} <- unLoc (retainedConPat pat) = True
    constructor_pat _ = False

{- Note [Checked retained lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A checked lambda and a signed function binding use the same expected Pi type.
Both must open its runtime binder with the actual source pattern identity.
tcRetainedMatches shares this clause rule and its constructor evidence transport.
Agda also checks function clauses in the context of their expected telescope.
See docs/dependent-haskell/REFERENCES.md#checked-retained-lambdas.

Keep the source lambda variant and its value arity. A one-argument lambda-case
can inspect its argument before it returns a function. A multi-argument lambda
binds all its source arguments before it inspects their patterns. Do not change
one form into the other. Preserve the outer erased binders in invis_pat_tys.

The shared checker does not scale usage. Function bindings apply their binding
multiplicity around it. Lambdas keep the usage context of their caller. Ordinary
checking and inference still use matchExpectedFunTys, including its ordinary
DeepSubsumption wrappers. This rule adds no source type positions or inference
for a retained telescope.

An empty lambda-case on an empty index domain still binds its runtime index.
Use a generated variable clause whose body is an actual empty case on that Id.
An empty match group alone would make matchWrapper allocate a different Id.
See Note [Retained constructor matches].

Select the retained constructor rule only when the first expected argument is
a DepFunTy. A single ordinary constructor pattern can precede a retained result.
For example, unwrap (Box f) = f can have type Box QPi -> QPi. Consume its
ordinary arrow with tcRetainedVarMatches and check Box with the ordinary pattern
checker. When the source patterns end, keep the remaining Pi as the RHS type.
Do not add a runtime index pattern or eta-expand that result. The ordinary
pattern checker and desugarer retain newtype pattern laziness. Multiple rows
with ordinary leading constructor patterns remain separate work.

A leading required forall uses matchExpectedFunTys to scope the source type
pattern. Keep its wrapper around the checked retained match. The wrapper
binds the erased skolem and its evidence before any runtime argument. Thus
in f (type p) decide xs, xs still opens the retained binder with its source
Name. Do not move a wrapper across a value pattern. The rules for runtime
demand and retained constructor rows stay the same.
-}

-- | Find a retained binder after ordinary arrows and erased foralls.
hasRetainedFunTy :: Type -> Bool
hasRetainedFunTy ty = case coreFullView ty of
  DepFunTy DepFunVisible _ _ -> True
  ForAllTy _ binder result
    | isInferredForAllTyFlag (binderFlag binder)
    , DepFunTy DepFunInvisible _ _ <- coreFullView result -> True
    | otherwise -> hasRetainedFunTy result
  FunTy _ _ _ _ result -> hasRetainedFunTy result
  _ -> False

-- | Elaborate a checked dependent case through a retained pattern function.
-- See Note [Source retained case refinement].
tcRetainedCase :: Bool -> Id -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
               -> TcM (Maybe (HsExpr GhcTc))
tcRetainedCase _ _ (MG { mg_alts = L _ [] }) _ = return Nothing
tcRetainedCase dependent index matches@(MG { mg_alts = L loc alternatives }) (Check result_ty)
  = do { givens <- if dependent then dependent_givens else return []
       ; captures <- if dependent
                     then do { vars <- dependent_captures givens
                             ; mapM_ check_capture vars
                             ; return vars }
                     else return []
       ; let explicit = dependent &&
               (not (null captures) || not (null givens) || depends result_ty)
       ; application <- if explicit || not (isTauTy result_ty)
                           || not (isLiftedTypeKind (typeKind result_ty))
                           || anyFreeVarsOfType (const True) result_ty
                         then return False
                         else use_application
       ; if not (explicit || application)
         then return Nothing
         else setSrcSpanA loc $
           do { when (retainedCaseHasQuote matches) $
                  failWithTc (TcRnRetainedQuantificationError RetainedCaseQuotation)
              ; unless (isLiftedTypeKind (typeKind result_ty)) $
                  failWithTc (TcRnRetainedQuantificationError (RetainedResultKind result_ty))
              ; constructors <- mapM constructor alternatives
              ; let domain_cons = case splitTyConApp_maybe (coreFullView (varType index)) of
                      Just (tc, _) -> tyConDataCons tc
                      _ -> []
                    cons = map fst constructors
              ; unless (cons `equalLength` domain_cons && all (`elem` cons) domain_cons) case_error
              ; fresh_name <- newSysName (nameOccName (idName index))
              ; let fresh_index = mkLocalId fresh_name ManyTy (varType index)
                    in_scope = mkInScopeSetList
                      (fresh_index : tyCoVarsOfTypesList (result_ty : map varType (index : givens ++ captures)))
                    subst = Subst.mkIdSubst in_scope (unitVarEnv index (Var fresh_index))
                    mentions var ty = anyFreeVarsOfType (== var) ty
                    used_as_index var = any (mentions var) (result_ty : map varType (captures ++ givens))
                    bind_capture scope var = do
                      name <- newSysName (nameOccName (idName var))
                      let shared = mkLocalId name ManyTy (Subst.substTy scope (varType var))
                      return (extendIdSubstWithClone scope var shared, shared)
              ; (capture_subst, shared) <- mapAccumLM bind_capture subst captures
              ; let generalize = Subst.substTy capture_subst
              ; dictionaries <- newEvVars (map (generalize . varType) givens)
              ; let names = map idName shared
                    capture_names = zip (map idName captures) names
                    add_arguments (L match_loc match@(Match { m_pats = L pats_loc pats }), (con, fields))
                      = let extra = map (noLocA . VarPat noExtField . noLocA) names
                            subject_pats = map (bindRetainedConFields fields) pats
                            rewritten | application = match
                                      | otherwise = rewriteRetainedCase (idName index) con fields capture_names match
                            ctxt | application = m_ctxt match
                                 | otherwise = LamAlt LamSingle
                        in L match_loc (rewritten { m_ctxt = ctxt
                                                  , m_pats = L pats_loc (subject_pats ++ extra) })
                    matches' = matches { mg_alts = L loc (map add_arguments (zip alternatives constructors)) }
                    dictionary_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) dictionaries
                    capture_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) shared
                    arg_tys = capture_tys ++ dictionary_tys
                    capture_plan var binder
                      | used_as_index var = RetainedConIndex binder
                      | otherwise = RetainedConValue
                    argument_plan = zipWith capture_plan captures shared ++ map RetainedConDict dictionaries
              ; let check = tcRetainedConMatches fresh_index [] arg_tys argument_plan
                             (mkCheckExpType (generalize result_ty)) matches'
              ; checked <- if application
                           then tcCheckUsage (idName fresh_index) ManyTy $
                               tcExtendIdEnv2 [(idName index, fresh_index)
                                              , (idName fresh_index, fresh_index)] check
                           else check
              ; mapM_ (tcEmitBindingUsage . singleUsageUE) captures
              ; let fun = HsLam noAnn LamSingle checked
                    index_arg = noLocA (HsVar noExtField (noLocA index))
                    arguments = map (noLocA . HsVar noExtField . noLocA) captures
                    applied = foldl' (\f a -> HsApp noExtField (noLocA f) a)
                                     (HsApp noExtField (noLocA fun) index_arg) arguments
              ; return (Just (mkHsWrap (mkWpEvVarApps givens) applied)) } }
  where
    depends = anyFreeVarsOfType (== index)

    use_application
      = do { worker_in_scope <- anyNameEnv retained_worker <$> getLclTypeEnv
           ; if not worker_in_scope || not (dependent || isNativeIndexId index)
             then return False
             else do { complete <- completeRetainedCaseRows (varType index) alternatives
                     ; known <- if complete
                                then hasRetainedApplication known_local_app matches
                                else return False
                     ; if not known || retainedCaseHasQuote matches
                       then return False
                       else if dependent
                            then return True
                            else (&&) <$> (null <$> dependent_captures [])
                                      <*> (null <$> dependent_givens) } }

    retained_worker (ATcId { tct_id = worker })
      | DepFunTy DepFunVisible binder body <- coreFullView (varType worker)
      , varType binder `eqType` varType index
      = anyFreeVarsOfType (== binder) body
    retained_worker _ = False

    known_local_app (HsApp _ fun arg)
      | HsVar _ (L _ (WithUserRdr _ worker_name)) <- unLoc (stripParensLHsExpr fun)
      , HsVar _ (L _ (WithUserRdr _ argument_name)) <- unLoc (stripParensLHsExpr arg)
      , argument_name == idName index
      = maybe False retained_worker <$> tcLookupLcl_maybe worker_name
    known_local_app _ = return False

    dependent_captures givens = do
      named <- fmap catMaybes $ mapM capture (nameSetElemsStable (retainedCaseNames matches))
      let types = result_ty : map varType (givens ++ named)
          indices = [v | v <- tyCoVarsOfTypesList types, isId v, not (isCoVar v)
                       , isLocalId v, depends (varType v), v `notElem` named]
      return (scopedSort (named ++ indices))
    dependent_givens =
      filter (depends . varType) <$> getNativeGivens

    capture name
      = do { thing <- tcLookupLcl_maybe name
           ; case thing of
               Just (ATcId { tct_id = var })
                 | isLocalId var, not (isCoVar var), depends (varType var)
                 -> return (Just var)
               _ -> return Nothing }
    check_capture var = unless (isManyTy (idMult var)) $
      failWithTc (TcRnRetainedQuantificationError (RetainedPatternType (varType var)))

    constructor (L match_loc (Match { m_pats = L _ [pat]
                                   , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = setSrcSpanA match_loc $
        do { unless (case_pattern pat) case_error
           ; con <- tcRetainedCon (varType index) pat
           ; maybe case_error return con }
    constructor (L match_loc _) = setSrcSpanA match_loc case_error
    case_pattern pat = case unLoc (retainedConPat pat) of
      ParPat _ inner -> case_pattern inner
      ConPat { pat_args = PrefixCon _ fields }
        -> all isRetainedVarPat fields
      _ -> False
    case_error :: TcM a
    case_error = failWithTc (TcRnRetainedQuantificationError RetainedCasePatterns)
tcRetainedCase _ _ _ _ = return Nothing

-- | Refine a checked opaque call through a strict alias and a retained motive.
-- See Note [Source opaque call refinement].
tcRetainedCallCase :: Type -> LHsExpr GhcTc
                   -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
                   -> TcM (Maybe (HsExpr GhcTc))
tcRetainedCallCase call_ty@(nativeCallSpine_maybe -> Just (fun, indices, domain)) scrut
                  matches@(MG { mg_alts = L loc alternatives }) (Check result_ty)
  = do { captures <- fmap catMaybes $ mapM capture
                       (nameSetElemsStable (retainedCaseNames matches))
       ; native_givens <- getNativeGivens
       ; givens <- filterM (depends . varType) native_givens
       ; result_depends <- depends result_ty
       ; let existing_trigger = not (null captures) || not (null givens)
                                || result_depends || has_annotation matches
       ; metadata_trigger <- if existing_trigger || retainedCaseHasQuote matches
                             then return False
                             else do { complete <- complete_constructor_rows
                                     ; if complete then hasRetainedApplication named_application matches
                                       else return False }
       ; if not (existing_trigger || metadata_trigger)
         then return Nothing
         else setSrcSpanA loc $
           do { when (retainedCaseHasQuote matches) $
                  failWithTc (TcRnRetainedQuantificationError RetainedCaseQuotation)
              ; unless (typeKind result_ty `eqType` liftedTypeKind) $
                  failWithTc (TcRnRetainedQuantificationError (RetainedResultKind result_ty))
              ; constructors <- mapM constructor alternatives
              ; let domain_cons = case splitTyConApp_maybe (coreFullView domain) of
                      Just (tc, _) -> tyConDataCons tc
                      _ -> []
                    cons = map fst constructors
              ; unless (not (null cons) && cons `equalLength` domain_cons
                        && all (`elem` cons) domain_cons) case_error
              ; dflags <- getDynFlags
              ; unless (all (== Breakpoints) (ticks_passes (initTicksConfig dflags))
                        && not (sccProfilingEnabled dflags)) $
                  failWithTc (TcRnRetainedQuantificationError RetainedCallInstrumentation)
              ; alias <- newSysLocalId (fsLit "call_index") ManyTy domain
              ; proof <- newEvVar (mkNomEqPred (TermVarTy alias) call_ty)
              ; let alias_co = mkSymCo (mkCoVarCo proof)
                    transport ty = do
                      { Reduction family_co family_ty <- family_view ty
                      ; co <- tcRetainedCallCoercion call_ty alias_co family_ty
                      ; return (family_co `mkTransCo` co) }
              ; result_co <- transport result_ty
              ; capture_cos <- mapM (transport . varType) captures
              ; given_cos <- mapM (transport . varType) givens
              ; index <- newSysLocalId (fsLit "call_motive") ManyTy domain
              ; let in_scope = mkInScopeSetList
                      (alias : index : tyCoVarsOfTypesList
                        (call_ty : result_ty : map varType (givens ++ captures)))
                    subst = Subst.mkIdSubst in_scope (unitVarEnv alias (Var index))
                    generalize = Subst.substTy subst . pSnd . coercionKind
              ; motive_proof <- newEvVar (mkNomEqPred (TermVarTy index) call_ty)
              ; old_dictionaries <- newEvVars (map generalize given_cos)
              ; let dictionaries = motive_proof : old_dictionaries
              ; names <- mapM (newSysName . nameOccName . idName) captures
              ; let capture_names = zip (map idName captures) names
                    add_arguments (L match_loc match@(Match { m_pats = L pats_loc pats }), (con, fields))
                      = let extra = map (noLocA . VarPat noExtField . noLocA) names
                            subject_pats = map (bindRetainedConFields fields) pats
                            rewritten = renameRetainedMatch capture_names
                                          (rewrite_call con fields match)
                        in L match_loc (rewritten { m_ctxt = LamAlt LamSingle
                                                  , m_pats = L pats_loc (subject_pats ++ extra) })
                    matches' = matches { mg_alts = L loc (map add_arguments (zip alternatives constructors)) }
                    dictionary_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) dictionaries
                    capture_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . generalize) capture_cos
                    argument_plan = map RetainedConDict dictionaries
                                    ++ map (const RetainedConValue) captures
              ; checked <- tcRetainedConMatches index [] (dictionary_tys ++ capture_tys)
                             argument_plan (mkCheckExpType (generalize result_co)) matches'
              ; strict <- xoptM LangExt.Strict
              ; mapM_ (tcEmitBindingUsage . singleUsageUE) captures
              ; let protect_arguments :: MatchGroup GhcTc (LHsExpr GhcTc)
                                        -> MatchGroup GhcTc (LHsExpr GhcTc)
                    protect_arguments group@(MG { mg_alts = L group_loc rows })
                      | strict = group { mg_alts = L group_loc (map protect_row rows) }
                      | otherwise = group
                    protect_row :: LMatch GhcTc (LHsExpr GhcTc)
                                -> LMatch GhcTc (LHsExpr GhcTc)
                    protect_row (L row_loc row@(Match { m_pats = L pats_loc (first : rest) }))
                      = L row_loc (row { m_pats = L pats_loc
                          (first : map (\pat -> noLocA (LazyPat noExtField pat)) rest) })
                    protect_row row = row
                    motive = HsLam noAnn LamSingle (protect_arguments checked)
                    alias_arg = noLocA (HsVar noExtField (noLocA alias))
                    evidence = zipWith (\given co ->
                      if isCoVar given
                      then evCoercion (mkCoCast (mkCoVarCo given) (mkSubCo co))
                      else evCast (Var given) (mkSubCo co)) givens given_cos
                    with_evidence = mkHsWrap (mkWpEvApps (evCoercion (mkCoVarCo proof) : evidence))
                                      (HsApp noExtField (noLocA motive) alias_arg)
                    arguments = zipWith (\var co -> mkLHsWrapCo co
                                  (noLocA (HsVar noExtField (noLocA var)))) captures capture_cos
                    body = foldl' (\f a -> HsApp noExtField (noLocA f) a) with_evidence arguments
                    transported = mkHsWrapCo (mkSymCo result_co) body
              ; return (Just (XExpr (NativeCaseAlias scrut alias proof result_ty
                                      (noLocA transported)))) } }
  where
    call_ids = filter (\v -> isId v && not (isCoVar v)) (tyCoVarsOfTypeList call_ty)
    depends ty
      | retainedCallOccurs call_ty ty = return True
      | all (\v -> anyFreeVarsOfType (== v) ty) call_ids
      = do { Reduction _ view <- family_view ty
           ; return (retainedCallOccurs call_ty view) }
      | otherwise = return False
    -- | Reduce family axioms with the ordinary solver depth limit.
    family_view :: TcType -> TcM Reduction
    family_view ty
      = do { ty <- liftZonkM (zonkTcType ty)
           ; loc <- getCtLocM PatCheckOrigin Nothing
           ; runTcSEqualities (rewriteFamilyAxioms loc ty) }
    capture name
      = do { thing <- tcLookupLcl_maybe name
           ; case thing of
               Just (ATcId { tct_id = var })
                 | isLocalId var, not (isCoVar var) ->
                   do { dependent <- depends (varType var)
                      ; if not dependent then return Nothing else
                        do { unless (isManyTy (idMult var)) $
                               failWithTc (TcRnRetainedQuantificationError (RetainedPatternType (varType var)))
                           ; return (Just var) } }
               _ -> return Nothing }
    constructor (L match_loc (Match { m_pats = L _ [pat]
                                   , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = setSrcSpanA match_loc $
        do { unless (case_pattern pat) case_error
           ; con <- tcRetainedCon domain pat
           ; maybe case_error return con }
    constructor (L match_loc _) = setSrcSpanA match_loc case_error
    case_pattern pat = case unLoc (retainedConPat pat) of
      ParPat _ inner -> case_pattern inner
      ConPat { pat_args = PrefixCon _ fields }
        -> all isRetainedVarPat fields
      _ -> False
    case_error :: TcM a
    case_error = failWithTc (TcRnRetainedQuantificationError RetainedCasePatterns)

    is_call :: HsType GhcRn -> Bool
    is_call ty
      = let (head_ty, args) = split_apps [] ty
        in is_name (idName fun) head_ty && indices `equalLength` args
           && and (zipWith is_index indices args)
    is_name :: Name -> HsType GhcRn -> Bool
    is_name name (HsParTy _ ty) = is_name name (unLoc ty)
    is_name name (HsTyVar _ NotPromoted (L _ (WithUserRdr _ actual))) = name == actual
    is_name _ _ = False

    -- | Compare only checked scalar variables and promoted constructor trees.
    is_index :: Type -> HsType GhcRn -> Bool
    is_index index (HsParTy _ ty) = is_index index (unLoc ty)
    is_index (TermVarTy var) ty = is_name (idName var) ty
    is_index (TyConApp tc args) ty
      | Just con <- isPromotedDataCon_maybe tc
      = case split_apps [] ty of
          (HsTyVar _ _ (L _ (WithUserRdr _ name)), source_args)
            -> let indices = drop (length (dataConUnivTyVars con)) args
               in name == dataConName con && indices `equalLength` source_args
                  && and (zipWith is_index indices source_args)
          _ -> False
    is_index _ _ = False

    -- | Collect an ordinary type application spine without admitting wrappers.
    split_apps :: [HsType GhcRn] -> HsType GhcRn
               -> (HsType GhcRn, [HsType GhcRn])
    split_apps args (HsParTy _ ty) = split_apps args (unLoc ty)
    split_apps args (HsAppTy _ fun arg) = split_apps (unLoc arg : args) (unLoc fun)
    split_apps args (HsOpTy _ left op right)
      = split_apps (unLoc left : unLoc right : args) (unLoc op)
    split_apps args (HsExplicitListTy _ _ [])
      = (HsTyVar noAnn IsPromoted (noLocA (noUserRdr (dataConName nilDataCon))), args)
    split_apps args ty = (ty, args)

    call_names :: [Name]
    call_names = map getName (tyCoVarsOfTypeList call_ty)

    complete_constructor_rows = completeRetainedCaseRows domain alternatives

    -- | Open only the existing retained telescope of a known local worker.
    named_application :: HsExpr GhcRn -> TcM Bool
    named_application expr
      = case spine [] expr of
          Just (name, args) -> do
            { thing <- tcLookupLcl_maybe name
            ; case thing of
                Just (ATcId { tct_id = worker }) -> consume False (varType worker) args
                _ -> return False }
          Nothing -> return False
      where
        -- | Read the source application without checking its arguments.
        spine args (HsPar _ expr) = spine args (unLoc expr)
        spine args (HsApp _ fun arg) = spine (arg : args) (unLoc fun)
        spine args (OpApp _ left op right) = spine (left : right : args) (unLoc op)
        spine args (ExplicitList _ []) = Just (dataConName nilDataCon, args)
        spine args (HsVar _ (L _ (WithUserRdr _ name))) = Just (name, args)
        spine _ _ = Nothing

        -- | Substitute eligible actual arguments without allocating type choices.
        consume _ ty _
          | DepFunTy DepFunInvisible _ _ <- coreFullView ty
          = return False
        consume consumed ty args
          | DepFunTy DepFunVisible binder _ <- coreFullView ty
          = case args of
              arg : rest -> do
                { actual <- if isNativeIndexFunctionId binder
                            then do { var <- local_variable (unLoc arg)
                                    ; return $ case var of
                                        Just v | isNativeIndexFunctionId v
                                               , varType v `eqType` varType binder -> Just (Var v)
                                        _ -> Nothing }
                            else scalar_argument (varType binder) (unLoc arg)
                ; case actual of
                    Just value -> consume True (applyTypeToArgs ty [value]) rest
                    Nothing -> return False }
              [] -> return False
          | otherwise = if consumed && isTauTy ty then depends ty else return False

        -- | Reconstruct only resolved monomorphic index constructor trees.
        -- Function leaves keep their actual lexical Id and exact fixed type.
        -- This metadata query does not check or evaluate a source argument.
        scalar_argument expected expr
          | HsPar _ arg <- expr = scalar_argument expected (unLoc arg)
          | Just (name, args) <- spine [] expr
          , isDataOcc (nameOccName name)
          = do { con_like <- tcLookupConLike (noUserRdr name)
               ; case con_like of
                   RealDataCon con
                     | Just type_args <- nativeIndexDataConTyArgs expected con
                     , args `lengthIs` dataConSourceArity con
                     -> scalar_fields expected
                          (foldl' App (Var (dataConWorkId con)) (map Type type_args)) args
                   _ -> return Nothing }
          | HsVar{} <- expr
          = do { actual <- local_variable expr
               ; return $ case actual of
                   Just var | isNativeOpaqueValueId var
                            , varType var `eqType` expected -> Just (Var var)
                   _ -> Nothing }
          | otherwise = return Nothing

        -- | Open the worker telescope in order. Applying a checked witness
        -- substitutes it into the domain of the next field.
        scalar_fields expected worker [] = return $ do
          index <- exprToNativeIndex worker
          guard (typeKind index `eqType` expected)
          return worker
        scalar_fields expected worker (arg : args)
          | let sigma = exprType worker
          , Just domain <- case coreFullView sigma of
              DepFunTy DepFunVisible binder _ -> Just (varType binder)
              _ -> case splitFunTy_maybe sigma of
                Just (_, _, _, input, _) -> Just input
                Nothing -> Nothing
          = do { actual <- scalar_argument domain (unLoc arg)
               ; case actual of
                   Just value -> scalar_fields expected (App worker value) args
                   Nothing -> return Nothing }
          | otherwise = return Nothing

        -- | Use resolved lexical identities and decline unknown source locals.
        local_variable (HsPar _ expr) = local_variable (unLoc expr)
        local_variable (HsVar _ (L _ (WithUserRdr _ name)))
          = do { thing <- tcLookupLcl_maybe name
               ; return $ case thing of
                   Just (ATcId { tct_id = var })
                     | isLocalId var, not (isExternalName (idName var)) -> Just var
                   _ -> Nothing }
        local_variable _ = return Nothing

    has_annotation :: Data a => a -> Bool
    has_annotation thing
      | Just HsTypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = False
      | Just HsUntypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = False
      | Just ty <- (cast thing :: Maybe (HsType GhcRn)), is_call ty = True
      | Just _ <- (cast thing :: Maybe Name) = False
      | Just _ <- (cast thing :: Maybe NameSet) = False
      | Just _ <- (cast thing :: Maybe Type) = False
      | otherwise = or (gmapQ has_annotation thing)

    rewrite_call :: Data a => DataCon -> [Id] -> a -> a
    rewrite_call con fields = go
      where
        con_type :: HsType GhcRn
        con_type = foldl' (\f a -> HsAppTy noExtField (noLocA f) a)
          (HsTyVar noAnn IsPromoted (noLocA (noUserRdr (dataConName con))))
          (map (noLocA . HsTyVar noAnn NotPromoted . noLocA . noUserRdr . idName) fields)
        go :: Data b => b -> b
        go thing
          | Just ty <- (cast thing :: Maybe (HsType GhcRn)), is_call ty
          = fromMaybe thing (cast con_type)
          | Just names <- (cast thing :: Maybe NameSet)
          , all (`elemNameSet` names) call_names
          = fromMaybe thing (cast (names `unionNameSet`
              mkNameSet (dataConName con : map idName fields)))
          | Just _ <- (cast thing :: Maybe Name) = thing
          | Just _ <- (cast thing :: Maybe Type) = thing
          | otherwise = gmapT go thing
tcRetainedCallCase _ _ _ _ = return Nothing

{- Note [Source opaque call refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A checked call f n has an opaque index. A constructor case can refine this
whole index without assuming that f is injective. Keep n and every Vec n
capture unchanged. Generalize only the exact call in the result, captures,
and incoming evidence. Abstract it to an immutable template and lift the
checked alias proof with ordinary nominal FC congruence.

An existing saturated family can expose the exact call. Normalize the family
view when the original type contains every runtime dependency of that call.
Use the view for activation and capture discovery. The solver rewrite enforces
the ordinary reduction-depth limit. Compose the nominal family reduction with
the alias transport from tcRetainedCallCoercion in GHC.Tc.Utils.Unify.
Keep its complete head evidence. See Note [Native function-value
application] in GHC.Core.TyCo.Make.

Introduce a strict alias r with q :: r ~# f n. Check the constructor rows
with the existing retained motive checker, applied to the scalar index r.
Transport the original captures and Givens into that motive, then transport
its result back. Under Strict, protect only the generated capture and evidence
patterns with LazyPat. Desugaring removes this marker instead of adding a bang.
Keep the original constructor and field patterns unchanged.
Replace exact call annotations in each row with its checked constructor type.
For a constructor-tree argument, compare the renamed application spine with
that checked tree. Require the same promoted DataCons, arities and variable
Names at every field. Ignore parentheses. Do not expand synonyms or compare
printed names. Keep f n, f (S n), f (S m) and shadowed heads distinct.
Seed the transport substitution with all free variables of the whole call,
including constructor fields. Reject quotations before any source rewrite
can change a quoted Name.

Require complete, unguarded constructor rows with variable or wildcard fields.
A source wildcard or variable case can ignore its scrutinee in Haskell.
Do not introduce this strict alias on that ordinary path. Breakpoint insertion
keeps the checked scrutinee intact. See Note [Breakpoints in retained operands]
in GHC.HsToCore.Ticks. Other instrumentation can obscure that call.
The dispatcher checks the original scrutinee and its usage once. It also
keeps the original case for diagnostics with ExpandedThingTc.

If the original activation conditions are absent, inspect only complete
bare-constructor rows without quotations. Read an already checked local
worker type and its actual arguments. Keep function heads as bare local Vars.
Reconstruct scalar arguments only from local Vars, parentheses and saturated
monomorphic real constructor trees. Validate these trees with exprToNativeIndex.
Open only leading retained binders with applyTypeToArgs. Require a whole tau
residual with the exact free call index. This is metadata discovery, not
argument checking. Do not infer types, allocate metas, collect usage, or
expand splices here. Decline
unknown workers, erased choices and unsupported source forms. Keep the
ordinary case fallback on this path. Existing activation keeps its diagnostics.

Pass an ordinary evidence parameter q_motive :: i ~# f n after the motive
index. Create it after whole-call generalization. Apply the motive to the
outer alias r, then q :: r ~# f n, then the old evidence and captures.
The outer alias r and motive index i remain distinct.

The existing constructor transport uses cv :: i ~# K to give the row
q_branch :: K ~# f n. checkConstraints scopes this Given over the complete
row, including its where bindings. Ordinary argument and result checking can
emit a Wanted f n ~# K. The solver matches the exact or symmetric irreducible
Given. It does not decompose the opaque call or equate its inputs or heads.
Coercion-hole filling records the used evidence, so the solver retains the
required q_branch binding. See Note [Delete dead Given evidence bindings]
in GHC.Tc.Solver.Solve. No separate application cast or lexical proof list
is needed for this path. Keep q_motive in the ordinary evidence-token ABI;
only cv occupies the virtual constructor-proof slot.

Agda with-abstraction supplies prior art for whole-term generalization and
checking a helper in its dependent context. It supplies no Haskell demand
law or FC proof. Agda v2.8.0 Rules/Application.hs lines 774-808 checks the
argument before substituting it into the codomain. This is checking and scope prior
art, not the algorithm for our structural source matcher.
See docs/dependent-haskell/REFERENCES.md#strict-opaque-call-refinement-prior-art
and Note [Checked opaque case alias] in GHC.Hs.Expr.
-}

-- | Decline unsupported ordinary cases without adding a new diagnostic.
completeRetainedCaseRows :: Type -> [LMatch GhcRn (LHsExpr GhcRn)] -> TcM Bool
completeRetainedCaseRows domain alternatives
  = do { rows <- mapM row_constructor alternatives
       ; let expected = case splitTyConApp_maybe (coreFullView domain) of
               Just (tc, _) -> tyConDataCons tc
               _ -> []
       ; return $ case sequence rows of
           Just cons -> not (null cons) && cons `equalLength` expected
                        && all (`elem` cons) expected
           Nothing -> False }
  where
    row_constructor (L _ (Match { m_pats = L _ [pat]
                                , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = pattern_constructor pat
    row_constructor _ = return Nothing
    pattern_constructor pat = case unLoc (retainedConPat pat) of
      ParPat _ inner -> pattern_constructor inner
      ConPat { pat_con = L _ name, pat_args = PrefixCon _ fields }
        | all isRetainedVarPat fields
        -> do { con_like <- tcLookupConLike name
              ; return $ case con_like of
                  RealDataCon con
                    | Just _ <- nativeIndexDataConTyArgs domain con
                    , fields `lengthIs` dataConSourceArity con -> Just con
                  _ -> Nothing }
      _ -> return Nothing

-- | Inspect existing checked worker types without elaborating source values.
hasRetainedApplication :: Data a => (HsExpr GhcRn -> TcM Bool) -> a -> TcM Bool
hasRetainedApplication check thing
  | Just expr <- (cast thing :: Maybe (HsExpr GhcRn))
  = case expr of
      HsTypedBracket{} -> return False
      HsUntypedBracket{} -> return False
      HsTypedSplice{} -> return False
      HsUntypedSplice{} -> return False
      XExpr{} -> return False
      HsApp{} -> do { found <- check expr
                    ; if found then return True else children }
      _ -> children
  | Just _ <- (cast thing :: Maybe Name) = return False
  | Just _ <- (cast thing :: Maybe NameSet) = return False
  | Just _ <- (cast thing :: Maybe Type) = return False
  | otherwise = children
  where
    children = anyM id (gmapQ (hasRetainedApplication check) thing)

-- | Collect resolved names outside quotations and stored free-name sets.
retainedCaseNames :: Data a => a -> NameSet
retainedCaseNames thing
  | Just HsTypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = emptyNameSet
  | Just HsUntypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = emptyNameSet
  | Just name <- cast thing = unitNameSet name
  | Just _ <- (cast thing :: Maybe NameSet) = emptyNameSet
  | otherwise = unionNameSets (gmapQ retainedCaseNames thing)

-- | Detect quotations before a source substitution can change quoted Names.
retainedCaseHasQuote :: Data a => a -> Bool
retainedCaseHasQuote thing
  | Just HsTypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = True
  | Just HsUntypedBracket{} <- (cast thing :: Maybe (HsExpr GhcRn)) = True
  | Just _ <- (cast thing :: Maybe Name) = False
  | Just _ <- (cast thing :: Maybe NameSet) = False
  | otherwise = or (gmapQ retainedCaseHasQuote thing)

-- | Apply the checked constructor substitution and rename generalized locals.
rewriteRetainedCase :: Data a => Name -> DataCon -> [Id] -> [(Name, Name)] -> a -> a
rewriteRetainedCase index con fields captures = go
  where
    rename name = fromMaybe name (lookup name captures)
    con_expr :: HsExpr GhcRn
    con_expr = foldl' (\f a -> HsApp noExtField (noLocA f) a)
                 (HsVar noExtField (noLocA (noUserRdr (dataConName con))))
                 (map (noLocA . HsVar noExtField . noLocA . noUserRdr . idName) fields)
    con_type :: HsType GhcRn
    con_type = foldl' (\f a -> HsAppTy noExtField (noLocA f) a)
                 (HsTyVar noAnn IsPromoted (noLocA (noUserRdr (dataConName con))))
                 (map (noLocA . HsTyVar noAnn NotPromoted . noLocA . noUserRdr . idName) fields)
    replace_name name
      | name == index = dataConName con : map idName fields
      | otherwise = [rename name]
    go :: Data b => b -> b
    go thing
      | Just (HsVar _ (L _ (WithUserRdr _ name))) <- (cast thing :: Maybe (HsExpr GhcRn))
      , name == index
      = fromMaybe thing (cast con_expr)
      | Just (HsTyVar _ _ (L _ (WithUserRdr _ name))) <- (cast thing :: Maybe (HsType GhcRn))
      , name == index
      = fromMaybe thing (cast con_type)
      | Just name <- (cast thing :: Maybe Name)
      = fromMaybe thing (cast (rename name))
      | Just names <- (cast thing :: Maybe NameSet)
      = fromMaybe thing (cast (mkNameSet (concatMap replace_name (nameSetElemsStable names))))
      | otherwise = gmapT go thing

-- | Rename resolved row Names and free-name sets after checking quotations.
renameRetainedMatch :: Data a => [(Name, Name)] -> a -> a
renameRetainedMatch names = go
  where
    rename name = fromMaybe name (lookup name names)
    go :: Data b => b -> b
    go thing
      | Just name <- (cast thing :: Maybe Name)
      = fromMaybe thing (cast (rename name))
      | Just free <- (cast thing :: Maybe NameSet)
      = fromMaybe thing (cast (mkNameSet (map rename (nameSetElemsStable free))))
      | Just _ <- (cast thing :: Maybe Type) = thing
      | otherwise = gmapT go thing

-- | Find a quotation's source span without traversing compiler type graphs.
retainedMatchQuotation :: Data a => a -> Maybe SrcSpan
retainedMatchQuotation thing
  | Just (L loc expr) <- (cast thing :: Maybe (LHsExpr GhcRn))
  , is_quote expr = Just (locA loc)
  | Just _ <- (cast thing :: Maybe Name) = Nothing
  | Just _ <- (cast thing :: Maybe NameSet) = Nothing
  | Just _ <- (cast thing :: Maybe Type) = Nothing
  | otherwise = msum (gmapQ retainedMatchQuotation thing)
  where
    is_quote :: HsExpr GhcRn -> Bool
    is_quote HsTypedBracket{} = True
    is_quote HsUntypedBracket{} = True
    is_quote _ = False

{- Note [Source retained case refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check a dependent source case by abstracting its expected result over a fresh
runtime index. Generalize each referenced local whose type depends on that
index as a later value argument. Check the constructor clauses with the existing
retained match rule, then apply the checked function to the scrutinee and those
locals. The constructor rule transports both later arguments and the result.

Generalize incoming evidence whose predicate depends on the index, even when no
source Name refers to that evidence. The lexical native Given scope records the
actual parameters. A captured value can itself occur in the result, in a
later argument type, or in an evidence predicate. Sort the captures in
dependency order and substitute each fresh binder into the rest of the
telescope. Give a capture a retained argument when its identity occurs in
one of these types. Bind the fresh evidence after the captures so that its
predicates refer to the new binders. Pass the original evidence with an
evidence wrapper. This includes primitive equality CoVars. Substitute into
predicates directly. Their kind is Constraint, not a retained result kind.
Transport the original dictionaries and let the ordinary solver derive their
superclasses in each branch. Static forms exclude enclosing evidence.

Agda's case_returning_of_ uses an explicit motive and a pattern function applied
to its scrutinee. Its with-abstraction also generalizes dependent context
arguments. See the pinned references in docs/dependent-haskell/REFERENCES.md.

Use fresh Names for the motive index and generalized locals. Resolved source
Names distinguish shadowed fields from the original scrutinee. Substitute the
checked constructor for direct references to that original scrutinee in each
branch. Preserve source locations and update stored free-name sets.
Use a lambda match context for generated clauses. The enclosing source case
supplies the error context; generated argument patterns must not appear there.
Keep the case context when the generated clause has only the source pattern.
Reject Template Haskell quotations before substitution. Quoted source Names
must not become the fresh generalized Names.

Complete unguarded constructor clauses preserve inspection of the scrutinee.
The generated application supplies every generalized argument. Wildcards and
defaults need a separate forcing rule; do not turn a strict case into an unused
function argument. A checked local retained worker can also make a case
relevant when it takes the scrutinee itself. Require a closed result and
complete unguarded constructor rows. Check the local environment before
looking for an application in the clauses. Most ordinary cases have no
retained worker in scope. With the extension off, keep ordinary
checking when a referenced local or Given already depends on the scrutinee.
For example, a local cached result still has the original scrutinee in its
type. The ordinary path keeps the same index in both applications.

Bind the source name to the fresh motive parameter in the clauses. The
constructor proof then refines the actual index of the application. The
application checker transports its result type using that proof. See
tcRetainedResultCoercion in GHC.Tc.Gen.App. Pass the original runtime value
to the motive parameter. No equality comes from a definition or unfolding.
-}

-- | Introduce runtime index identities before checking the remaining patterns.
-- See Note [Erased suffixes in retained matches].
tcRetainedVarMatches :: ExpectedFunTyCtxt -> UserTypeCtxt -> Type
                     -> [LPat GhcRn]
                     -> ([ExpPatType] -> ExpRhoType -> Maybe (Int, [Id]) -> [Maybe Name] -> TcM a)
                     -> TcM (HsWrapper, a)
tcRetainedVarMatches herald ctxt initial_ty source_pats = go [] initial_ty initial_pats
  where
    initial_pats = filter isVisArgLPat source_pats
    trailing_erased = not (null initial_pats) && not (isVisArgLPat (last source_pats))

    finish acc rhs_ty boundary thing_inside =
      do { let args = reverse acc
         ; result <- thing_inside (map fst args) rhs_ty boundary
                       [name | (pat_ty, name) <- args, isVisibleExpPatType pat_ty]
         ; return (idHsWrapper, result) }

    go acc ty pats thing_inside
      | (theta, _) <- tcSplitPhiTy ty, not (null theta)
      = split_suffix acc ty pats RetainedContextPattern thing_inside
      | ForAllTy{} <- coreFullView ty
      , not (leading_required ty acc)
      , not (null pats) || trailing_erased
      = split_suffix acc ty pats RetainedResultWrapper thing_inside
    go acc ty [] thing_inside = finish acc (mkCheckExpType ty) Nothing thing_inside
    go acc ty (pat : pats) thing_inside
      | DepFunTy DepFunVisible binder _ <- coreFullView ty
      = setSrcSpanA (getLoc pat) $
        do { name <- case unLoc pat of
               VarPat _ (L _ name) -> return name
               WildPat{} -> newSysName (getOccName binder)
               _ -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
           ; let index = mkLocalId name ManyTy (varType binder)
                 pat_ty = mkCheckExpFunPatTy (Scaled ManyTy (varType binder))
           ; (wrap, result) <- go ((pat_ty, Just name) : acc)
                                  (applyTypeToArgs ty [Var index]) pats thing_inside
           ; return (liftRetainedLambdaWrap index wrap, result) }
      | FunTy { ft_af = af } <- coreFullView ty
      = do { (ma_co, arg_ty, rhs_ty) <- setSrcSpanA (getLoc pat) $
                  matchRetainedLambdaArrow herald (length initial_pats, initial_ty) ty
           ; (wrap, result) <- go ((mkCheckExpFunPatTy arg_ty, Nothing) : acc) rhs_ty pats thing_inside
           ; wrap' <- liftOrdinaryLambdaWrap af ma_co arg_ty rhs_ty wrap
           ; return (wrap', result) }
      | otherwise
      = do { (wrap, (rest_wrap, result)) <- matchExpectedFunTys herald ctxt 1 (mkCheckExpType ty) $
               \pat_tys rhs_ty ->
                 do { rhs_ty <- readExpType rhs_ty
                    ; go (map (, Nothing) (reverse pat_tys) ++ acc) rhs_ty pats thing_inside }
           ; unless (isIdHsWrapper wrap || leading_required ty acc) $
               failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
           ; return (wrap <.> rest_wrap, result) }

    leading_required ty acc
      | ForAllTy _ (Bndr _ Required) _ <- coreFullView ty
      = not (any (isExpFunPatType . fst) acc)
      | otherwise = False

    split_suffix acc ty pats err thing_inside
      = do { strict <- xoptM LangExt.Strict
           ; let visible = filter isVisibleExpPatType (map fst (reverse acc))
                 can_split ty pat = not (isExpFunPatType ty) ||
                                    (not strict && nonforcing_pattern pat)
           ; unless (null pats || and (zipWith can_split visible initial_pats)) $
               failWithTc (TcRnRetainedQuantificationError err)
           ; let (indices, co) = expose_suffix ty
           ; (wrap, result) <- finish acc (mkCheckExpType (coercionLKind co))
                                 (Just (length visible, indices)) thing_inside
           ; return (mkWpCastR co <.> wrap, result) }

    expose_suffix ty
      | ForAllTy ma binder body <- coreFullView ty
      , isInferredForAllTyFlag (binderFlag binder)
      , (indices@(_:_), co) <- exposeInvisibleRetained body
      = (indices, mkHomoForAllCos [(mkNomReflCo ma, binder)] co)
      | otherwise = ([], mkRepReflCo ty)

    nonforcing_pattern (L _ pat) = case pat of
      VarPat{} -> True
      WildPat{} -> True
      LazyPat{} -> True
      ParPat _ inner -> nonforcing_pattern inner
      AsPat _ _ inner -> nonforcing_pattern inner
      SigPat _ inner _ -> nonforcing_pattern inner
      _ -> False

-- | Check an ordinary arrow consumed by a new runtime lambda.
-- See Note [Ordinary arrows in retained matches].
matchRetainedLambdaArrow :: ExpectedFunTyCtxt -> (VisArity, Type) -> Type
                         -> TcM (TcCoercionN, Scaled TcSigmaTypeFRR, TcSigmaType)
matchRetainedLambdaArrow herald arity ty
  = do { (arg_co, ma, arg_ty, rhs_ty) <- matchActualFunTy herald Nothing arity ty
       ; unless (isReflCo arg_co) $
           failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
       ; ma_co <- unifyType Nothing unmatchableDataConTy ma
       ; return (ma_co, arg_ty, rhs_ty) }

-- | Lift a result cast through a retained runtime binder.
liftRetainedLambdaWrap :: Id -> HsWrapper -> HsWrapper
liftRetainedLambdaWrap index (WpCast co) =
  mkWpCastR (mkDepFunCoWithVis DepFunVisible DepFunVisible index MRefl co)
liftRetainedLambdaWrap _ wrap = wrap

-- | Build the cast for an ordinary runtime lambda and its result.
liftOrdinaryLambdaWrap :: FunTyFlag -> TcCoercionN -> Scaled TcSigmaTypeFRR
                       -> TcSigmaType -> HsWrapper -> TcM HsWrapper
liftOrdinaryLambdaWrap af ma_co (Scaled mult arg_ty) res_ty wrap
  | isReflCo ma_co, isIdHsWrapper wrap = return idHsWrapper
  | Just res_co <- hsWrapperCast_maybe wrap =
      let body_co = case res_co of
            MRefl -> mkRepReflCo res_ty
            MCo co -> co
          co = mkFunCo Representational af ma_co (mkNomReflCo mult)
                 (mkRepReflCo arg_ty) body_co
      in return (mkWpCastR co)
  | otherwise = failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)

{- Note [Ordinary arrows in retained matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Core Lam introduces an ordinary arrow with unmatchable metadata. Check
that metadata when a value pattern consumes an ordinary arrow in a
retained telescope. This uses the same equality as matchExpectedFunTys.
Let the solver resolve the equality in its original scope. A matchability
variable introduced at the call can be untouchable inside an erased forall.

Require reflexive argument representation. Carry the matchability proof in a
result cast. Lift it through the preceding binders without introducing another
lambda or moving any pattern match. A preceding erased binder uses its existing
wrapper outside this cast. In a constructor group, build the cast while
collecting shared arguments, before entering the scope of any pattern evidence.
A Given introduced by a pattern cannot justify the arrow for that pattern.
An arrow with no corresponding pattern keeps its complete type, so an
existing matchable function can pass through as the result.
-}

-- | Give each retained wildcard its column's internal Name.
-- See Note [Wildcard retained binders].
bindRetainedWildcards :: [Maybe Name] -> [LPat GhcRn] -> [LPat GhcRn]
bindRetainedWildcards (name : names) (pat : pats)
  | isVisArgLPat pat = maybe pat (`bindRetainedWildcard` pat) name
                       : bindRetainedWildcards names pats
  | otherwise = pat : bindRetainedWildcards (name : names) pats
bindRetainedWildcards _ pats = pats

{- Note [Wildcard retained binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In keep :: foreach (n :: Nat) -> Vec a n -> Vec a n, the clause
keep _ xs = xs discards the source name. The type of xs still refers to
the first runtime argument. A checked WildPat would make selectMatchVar
allocate a second Id during desugaring.

Bind a fresh system Name for each anonymous retained parameter in a
single equation. Use that Name to open the function type and to check
the variable pattern. Keep the signature binder's occurrence. A type
error can then identify a mismatch between p x and q x when the
equation used wildcard patterns for p, q and x. The fresh Unique keeps
the wildcard distinct from any source binding with the same spelling.
A constructor row already has a shared index Id
for each retained column. Use that Id's Name for a wildcard in its
column. A retained package field likewise needs the Id substituted into
the remaining field types. No branch equality follows from a wildcard.

The outer pattern keeps the source span of the wildcard. The internal
binder has no source occurrence. The renamer has already resolved the
RHS and its quotations. Ordinary variable-pattern desugaring preserves
the argument's demand, including the module's Strict setting.

A source wildcard has no name that the user can refer to. Keep its
generated Id out of the relevant bindings in an error message. A named
pattern such as `_first` is a source binding and can still be reported.
-}

-- | Check an erased suffix as an expression inside its runtime prefix.
-- See Note [Erased suffixes in retained matches].
splitRetainedSuffix :: Int -> [Id] -> MatchGroup GhcRn (LHsExpr GhcRn)
                     -> MatchGroup GhcRn (LHsExpr GhcRn)
splitRetainedSuffix prefix indices group@(MG { mg_alts = L loc [L match_loc match]
                                     , mg_ext = ext })
  = group { mg_alts = L loc [L match_loc outer] }
  where
    Match { m_pats = L pats_loc pats } = match
    (before, after) = split_prefix prefix pats
    inner = match { m_ctxt = LamAlt LamSingle, m_pats = L pats_loc after }
    lambda = L match_loc (HsLam noAnn LamSingle
               (MG { mg_alts = L loc [addInvisibleRetainedPatterns indices (L match_loc inner)]
                   , mg_ext = ext }))
    outer = match { m_pats = L pats_loc before
                  , m_grhss = unguardedGRHSs (locA match_loc) lambda noAnn }

    split_prefix 0 ps = ([], ps)
    split_prefix n (pat : ps)
      = let (before, after) = split_prefix (if isVisArgLPat pat then n-1 else n) ps
        in (pat : before, after)
    split_prefix n [] = pprPanic "splitRetainedSuffix: missing prefix" (ppr n)
splitRetainedSuffix _ _ _ = panic "splitRetainedSuffix: multiple equations"

{- Note [Erased suffixes in retained matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a single equation, a forall or context can occur between runtime
parameters. Check the suffix as a lambda with the declared polytype. This
puts type and evidence lambdas after the preceding runtime arguments.
The remaining patterns, guards and local bindings stay inside the lambda.
A written @a pattern binds the erased type variable at that position.
The signature's inner forall alone does not scope a name over the equation.

A retained Type binder elaborates to an inferred forall followed by an
invisible TypeRep argument. Expose the runtime argument in the suffix and
insert the same variable pattern as for a leading invisible binder.
Lift its visibility cast through the inferred forall and the preceding
checked arguments. The renamer puts the signature's runtime name in
scope before it renames the remaining source patterns and body.
See Note [Invisible retained definitions] in GHC.Rename.Bind.

A MatchGroupTc stores a list of runtime argument types and one result type.
It cannot store a type or evidence lambda between runtime arguments.
Partition the match only when there are no later runtime patterns, or
when the prefix cannot force an argument. Count a required type pattern
in a visible source position. It has no runtime demand. A split at a
strict value pattern would force that pattern too early.
See docs/dependent-haskell/REFERENCES.md#alternating-retained-and-erased-binders.
-}

-- | Distinguish transported payloads from shared evidence and runtime indices.
data RetainedConArg
  = RetainedConValue
  | RetainedConDict EvVar
  | RetainedConIndex Id

-- | Test whether the argument occupies a written column.
isRetainedSourceArg :: RetainedConArg -> Bool
isRetainedSourceArg RetainedConDict{} = False
isRetainedSourceArg _ = True

-- | Collect shared binders in telescope order without counting evidence as values.
tcRetainedConArgs :: ExpectedFunTyCtxt -> UserTypeCtxt -> Int -> Type
                  -> TcM (HsWrapper, ([ExpPatType], [RetainedConArg], ExpRhoType))
tcRetainedConArgs herald ctxt = go [] []
  where
    go acc argument_plan n ty
      | (theta, tau) <- tcSplitPhiTy ty, not (null theta)
      = do { givens <- newEvVars theta
           ; let pat_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) givens
           ; (wrap, result) <- go (reverse pat_tys ++ acc)
                (reverse (map RetainedConDict givens) ++ argument_plan) n tau
           ; let wrap' = case wrap of
                   WpCast co -> mkWpCastR (foldr (mkFunResCo Representational unmatchableDataConTy) co givens)
                   _ -> wrap
           ; return (wrap', result) }
      | n == 0
      = return (idHsWrapper, (reverse acc, reverse argument_plan, mkCheckExpType ty))
      | ForAllTy _ binder _ <- coreFullView ty
      , isInvisibleForAllTyFlag (binderFlag binder)
      = failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
      | DepFunTy DepFunVisible binder _ <- coreFullView ty
      = do { name <- newSysName (nameOccName (idName binder))
           ; let index = mkLocalId name ManyTy (varType binder)
                 pat_ty = mkCheckExpFunPatTy (Scaled ManyTy (varType binder))
           ; (wrap, result) <- go (pat_ty : acc) (RetainedConIndex index : argument_plan) (n-1)
                                  (applyTypeToArgs ty [Var index])
           ; return (liftRetainedLambdaWrap index wrap, result) }
      | FunTy { ft_af = af } <- coreFullView ty
      = do { (ma_co, arg_ty, rhs_ty) <- matchRetainedLambdaArrow herald (n, ty) ty
           ; (wrap, result) <- go (mkCheckExpFunPatTy arg_ty : acc) (RetainedConValue : argument_plan) (n-1) rhs_ty
           ; wrap' <- liftOrdinaryLambdaWrap af ma_co arg_ty rhs_ty wrap
           ; return (wrap', result) }
      | otherwise
      = do { (wrap, result) <- matchExpectedFunTys herald ctxt 1 (mkCheckExpType ty) $
               \pat_tys rhs_ty ->
                 do { rhs_ty <- readExpType rhs_ty
                    ; go (reverse pat_tys ++ acc)
                         (map (const RetainedConValue) pat_tys ++ argument_plan) (n-1) rhs_ty }
           ; unless (isIdHsWrapper wrap) $
               failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
           ; return result }

{- Note [Retained argument telescopes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Introduce every retained argument with the Name of its actual variable pattern.
Instantiate its codomain before reading later argument types. Only visible
retained binders consume source value patterns. An invisible retained binder
uses the ordinary expected-function fallback. No implicit runtime argument is
introduced. See Note [Retained function visibility] in GHC.Core.TyCo.Rep.
Check the complete pattern list only after these identities have been chosen.
tcPatBndr and selectMatchVar preserve their Unique values. Outer erased forall binders keep
their ordinary skolemisation and wrapper; do not lift a nonidentity wrapper
through a retained binder.

A context after a retained binder must use these actual runtime identities.
For a single variable-prefix equation, stop at the first context. Keep the
remaining source patterns, guards, and local bindings in a generated HsLam.
The ordinary expression checker introduces the context before it checks this
whole suffix. Moving only the final RHS expression would leave its guards,
view patterns, and local bindings outside the Givens. An empty suffix still
uses this rule: its zero-pattern match adds no value lambda. Require
nonforcing patterns in the prefix when value patterns remain. This split must
not force a prefix pattern before an omitted suffix argument. Strict makes the
retained variable pattern forcing, so reject that case too. With no remaining
value patterns, the split moves no value lambda. Preserve the old prefix shapes
in that case. Checked suffix lambdas use the same clause rule;
see Note [Checked retained lambdas].

Use matchActualFunTy to consume a known ordinary arrow without skolemising its
result. matchExpectedFunTys skolemises a following context even after its last
requested value argument. That wrapper cannot move across a retained binder.
Keep the following context for the next telescope step instead. Require the
argument representation coercion to be reflexive, as on the existing path.

mkDepFunTy turns an unused binder into an ordinary arrow. A retained telescope
can therefore contain both FunTy and DepFunTy. Walk both forms in source order.
Agda checks a telescope by extending its context before checking the remainder.
See the pinned Rules.Term and LHSResult sources in
docs/dependent-haskell/REFERENCES.md#agda-provenance.
-}

-- | Names shared by the visible runtime index columns of constructor equations.
retainedColumnNames :: [RetainedConArg] -> [Maybe Name]
retainedColumnNames = foldr column []
  where
    column (RetainedConIndex index) rest = Just (idName index) : rest
    column RetainedConValue rest = Nothing : rest
    column RetainedConDict{} rest = rest

-- | Bind implicit dictionaries in their original argument positions.
insertRetainedDictionaries :: [RetainedConArg] -> [LPat GhcRn] -> [LPat GhcRn]
insertRetainedDictionaries [] pats = pats
insertRetainedDictionaries (RetainedConDict dict : rest) pats
  = noLocA (VarPat noExtField (noLocA (idName dict))) : insertRetainedDictionaries rest pats
insertRetainedDictionaries plan@(_ : rest) (pat : pats)
  | isVisArgLPat pat = pat : insertRetainedDictionaries rest pats
  | otherwise = pat : insertRetainedDictionaries plan pats
insertRetainedDictionaries _ [] = panic "insertRetainedDictionaries: missing source argument"

-- | Check one equation with its constructor proofs and transported dictionaries.
withRetainedRowGivens :: SkolemInfoAnon -> [EvVar] -> [RetainedConArg] -> [TcCoercionN]
                      -> TcM a -> TcM (TcEvBinds, a)
withRetainedRowGivens origin proofs plan arg_cos action
  = do { let dictionary_cos = [(dict, co) | (RetainedConDict dict, co) <- zip plan arg_cos]
             domain_cos = [co | (RetainedConIndex _, co) <- zip plan arg_cos
                              , not (isReflCo co)]
       ; refined_givens <- newEvVars (map (pSnd . coercionKind . snd) dictionary_cos)
       ; domain_givens <- newEvVars (map coercionType domain_cos)
       ; (ev_binds, result) <-
           checkConstraints origin []
             (proofs ++ refined_givens ++ domain_givens) action
       ; let given_binds = zipWith (\given (dict, co) ->
                               mkGivenEvBind given
                                 (if isCoVar dict
                                  then evCoercion (mkCoCast (mkCoVarCo dict) (mkSubCo co))
                                  else evCast (Var dict) (mkSubCo co)))
                             refined_givens dictionary_cos
                          ++ zipWith (\given co -> mkGivenEvBind given (evCoercion co))
                                     domain_givens domain_cos
       ; branch_binds <- case ev_binds of
           TcEvBinds binds -> do { mapM_ (addTcEvBind binds) given_binds
                                 ; return ev_binds }
           EvBinds binds -> return (EvBinds (listToBag given_binds `unionBags` binds))
       ; return (branch_binds, result) }

-- | Select a later retained constructor column without counting dictionaries.
retainedMatrixColumn :: [RetainedConArg]
                     -> MatchGroup GhcRn (LHsExpr GhcRn) -> Maybe (Int, Id)
retainedMatrixColumn plan (MG { mg_alts = L _ rows })
  = case [(column, shared)
         | L _ (Match { m_pats = L _ pats }) <- rows
         , ((column, RetainedConIndex shared), pat) <-
             zip (filter (isRetainedSourceArg . snd) (zip [0..] plan))
                 (drop 1 (filter isVisArgLPat pats))
         , constructor_pattern pat] of
      first : _ -> Just first
      [] -> Nothing
  where
    constructor_pattern :: LPat GhcRn -> Bool
    constructor_pattern (L _ (ParPat _ pat)) = constructor_pattern pat
    constructor_pattern pat | ConPat{} <- unLoc (retainedConPat pat) = True
    constructor_pattern _ = False

-- | Check a two-index matrix in left-to-right order.
-- See Note [Retained constructor matrices].
tcRetainedMatrixMatches :: (Int, Id) -> (Int, Id) -> [ExpPatType] -> [ExpPatType]
                       -> [RetainedConArg] -> ExpRhoType
                       -> MatchGroup GhcRn (LHsExpr GhcRn)
                       -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedMatrixMatches (first_column, first_index) (second_column, second_index) invis_pat_tys
                       pat_tys plan rhs_ty
                       (MG { mg_alts = L loc matches, mg_ext = (origin, _) })
  = setSrcSpanA loc $
    do { rows <- zipWithM constructors [1..] matches
       ; _ <- foldM unique_row [] (zip rows matches)
       ; let expected = [(first, second) | first <- domain_cons first_index
                                         , second <- second_cons]
       ; unless (any isNothing rows) $
           forM_ (filter (\pair -> not (any (covers pair) rows)) expected) check_missing
       ; arg_tys <- mapM argument_type pat_tys
       ; result_ty <- readExpType rhs_ty
       ; (usages, checked) <- mapAndUnzipM
           (\(row, match) -> tcCollectingUsage (check_row row arg_tys result_ty match))
           (zip rows matches)
       ; tcEmitBindingUsage (supUEs usages)
       ; return (MG { mg_alts = L loc checked
                    , mg_ext = MatchGroupTc arg_tys result_ty origin }) }
  where
    dictionaries = [d | RetainedConDict d <- plan]
    first_source = source_column first_column
    second_source = source_column second_column
    source_column column = length (filter isRetainedSourceArg (take column plan))

    -- An omitted constructor pair must contradict a declared equality.
    check_missing (one, two)
      | any (anyFreeVarsOfType (\v -> v == first_index || v == second_index)) predicates
      = do { level <- getTcLevel
           ; ambient <- filter (isCoverageEquality . varType) <$> getLexicalGivens
           ; let satisfiable tys = do
                   givens <- newEvVars tys
                   tcCheckGivens (emptyInertSet level) (listToBag (ambient ++ givens))
           ; before <- satisfiable predicates
           ; when (isNothing before) missing_pair
           ; co1 <- hypothetical first_index one
           ; co2 <- hypothetical second_index two
           ; specialised <- mapM (fmap (pSnd . coercionKind) . transport_pair co1 co2) predicates
           ; after <- satisfiable specialised
           ; when (isJust after) missing_pair }
      | otherwise = missing_pair
      where
        missing_pair :: TcM a
        missing_pair = failWithTc (TcRnRetainedQuantificationError
                         (RetainedMissingConstructorPair (dataConName one) (dataConName two)))
        predicates = [varType d | RetainedConDict d <- plan, isCoverageEquality (varType d)]
        hypothetical index con = do
          let pat = nlConPatName (dataConName con) (replicate (dataConSourceArity con) nlWildPatName)
          resolved <- tcRetainedCon (varType index) pat
          case resolved of
            Just row -> mkCoVarCo <$> constructor_proof index row
            Nothing -> missing_pair

    transport_pair first_co second_co ty = do
      one <- tcRetainedCoercion first_index first_co ty
      two <- tcRetainedCoercion second_index second_co (pSnd (coercionKind one))
      return (mkTransCo one two)
    domain_cons index = case splitTyConApp_maybe (coreFullView (varType index)) of
      Just (tc, _) -> tyConDataCons tc
      _ -> []
    second_cons = domain_cons second_index
    coverage_error = failWithTc (TcRnRetainedQuantificationError RetainedMatrixCoverage)
    pattern_error = failWithTc (TcRnRetainedQuantificationError RetainedMatrixPattern)
    argument_error = failWithTc (TcRnRetainedQuantificationError RetainedConstructorArguments)

    row_patterns (Match { m_pats = L _ pats })
      = case (drop first_source visible, drop second_source visible) of
          (first : _, second : _) -> return (first, second)
          _ -> pattern_error
      where
        visible = filter isVisArgLPat pats

    constructors position (L match_loc match@(Match
      { m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = setSrcSpanA match_loc $
        do { (first, second) <- row_patterns match
           ; if fallback_pattern first && fallback_pattern second
             then do { unless (position == length matches) $
                         setSrcSpanA (getLoc first) $
                           failWithTc (TcRnRetainedQuantificationError RetainedMatrixFallback)
                     ; return Nothing }
             else do { one <- resolve first_index first
                     ; two <- if fallback_pattern second
                              then return Nothing else Just <$> resolve second_index second
                     ; return (Just (one, two)) } }
    constructors _ (L match_loc _) = setSrcSpanA match_loc coverage_error

    fallback_pattern :: LPat GhcRn -> Bool
    fallback_pattern (L _ VarPat{}) = True
    fallback_pattern (L _ WildPat{}) = True
    fallback_pattern _ = False

    resolve index pat = setSrcSpanA (getLoc pat) $
      do { con <- tcRetainedCon (varType index) pat
         ; maybe pattern_error return con }

    covers _ Nothing = True
    covers (first, second) (Just ((con, _), later)) =
      first == con && maybe True ((== second) . fst) later

    unique_row seen (Nothing, _) = return seen
    unique_row seen (row@(Just (first, second)), L match_loc match)
      = setSrcSpanA match_loc $
        do { let seconds = maybe second_cons (pure . fst) second
           ; when (all (\con -> any (covers (fst first, con)) seen) seconds) $
               do { (_, pat) <- row_patterns match
                  ; setSrcSpanA (getLoc pat) $
                      failWithTc (TcRnRetainedQuantificationError RetainedMatrixDuplicate) }
           ; return (row : seen) }

    argument_type (ExpFunPatTy ty)
      = do { scaled@(Scaled mult _) <- readScaledExpType ty
           ; unless (isManyTy mult) argument_error
           ; return scaled }
    argument_type _ = argument_error

    check_row row arg_tys result_ty (L match_loc match@(Match { m_pats = L _ pats }))
      = setSrcSpanA match_loc $
        do { names <- check_columns 0 plan (filter isVisArgLPat pats)
           ; when (isNothing row || not (null names)) $
               case retainedMatchQuotation match of
                 Nothing -> return ()
                 Just span -> setSrcSpan span $
                   failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
           ; cvs <- case row of
               Nothing -> return Nothing
               Just (first, second) ->
                 do { first_cv <- constructor_proof first_index first
                    ; second_cv <- traverse (constructor_proof second_index) second
                    ; return (Just (first_cv, second_cv)) }
           ; let transport ty = case cvs of
                   Nothing -> return (mkNomReflCo ty)
                   Just (first_cv, Nothing) -> tcRetainedCoercion first_index (mkCoVarCo first_cv) ty
                   Just (first_cv, Just second_cv) ->
                     transport_pair (mkCoVarCo first_cv) (mkCoVarCo second_cv) ty
           ; arg_cos <- mapM (transport . scaledThing) arg_tys
           ; result_co <- transport result_ty
           -- Keep later retained arguments at their incoming types.
           -- See Note [Refining later retained domains].
           ; let refined_args = zipWith3 (\argument (Scaled mult ty) co ->
                                   mkCheckExpFunPatTy (Scaled mult (case argument of
                                     RetainedConValue -> pSnd (coercionKind co)
                                     _ -> ty))) plan arg_tys arg_cos
                 renamed@(Match { m_pats = L renamed_loc renamed_pats })
                   = renameRetainedMatch names match
                 constructor_columns = case row of
                   Just ((_, first_fields), second) ->
                     (first_source, first_fields) :
                       [(second_source, fields) | (_, fields) <- maybeToList second]
                   Nothing -> []
                 prepared = renamed { m_pats = L renamed_loc
                   (insertRetainedDictionaries plan $ retainedConPats constructor_columns $
                    bindRetainedWildcards (retainedColumnNames plan) renamed_pats) }
                 check = do
                   clause <- if length matches == 1
                     then prepareUnguardedWhere (mkCheckExpType (pSnd (coercionKind result_co)))
                            (L match_loc prepared)
                     else return (L match_loc prepared)
                   tcMatchWithSource tcBody (invis_pat_tys ++ refined_args)
                     (mkCheckExpType (pSnd (coercionKind result_co))) match clause
           ; (ev_binds, L checked_loc checked) <- case (row, cvs) of
               (Nothing, Nothing)
                 | null dictionaries -> do { checked <- check; return (emptyTcEvBinds, checked) }
                 | otherwise -> checkConstraints (SigTypeSkol GenSigCtxt) [] dictionaries check
               (Just ((first_con, _), _), Just (first_cv, second_cv)) ->
                 withRetainedRowGivens (PatSkol (RealDataCon first_con) (m_ctxt match))
                   (first_cv : maybeToList second_cv) plan arg_cos check
               _ -> panic "tcRetainedMatrixMatches: inconsistent constructor proofs"
           ; let Match { m_pats = L pats_loc checked_pats, m_grhss = grhss } = checked
                 wrap_index _ Nothing _ pat = pat
                 wrap_index index (Just cv) binds (L pat_loc pat)
                   = L pat_loc (AsPat noExtField (L (noAnnSrcSpan generatedSrcSpan) index)
                       (L pat_loc (addRetainedConProof cv binds pat)))
                 second_cv = cvs >>= snd
                 first_binds = if isJust second_cv then emptyTcEvBinds else ev_binds
                 wrap_column (column, argument) co (Scaled _ ty) pat@(L pat_loc inner)
                   | column == first_column = wrap_index first_index (fst <$> cvs) first_binds pat
                   | column == second_column = wrap_index second_index second_cv ev_binds pat
                   | RetainedConValue <- argument = L pat_loc (XPat (CoPat (mkWpCastN co) inner ty))
                   | otherwise = pat
                 checked_pats' = zipWith4 wrap_column (zip [0..] plan) arg_cos arg_tys checked_pats
                 cast_rhs (L rhs_loc (GRHS x guards rhs))
                   = L rhs_loc (GRHS x guards (mkLHsWrapCo (mkSymCo result_co) rhs))
                 fallback_binds = isNothing cvs && not (null dictionaries)
                 where_binds = grhssLocalBinds grhss
                 wrap_fallback (L rhs_loc (GRHS x guards rhs))
                   | fallback_binds =
                       let body = case unLoc where_binds of
                             EmptyLocalBinds{} -> rhs
                             _ -> L (getLoc rhs) (HsLet noExtField where_binds rhs)
                       in L rhs_loc (GRHS x guards (mkHsDictLet ev_binds body))
                   | otherwise = cast_rhs (L rhs_loc (GRHS x guards rhs))
                 locals | fallback_binds = fmap (const emptyLocalBinds) where_binds
                        | otherwise = where_binds
           ; return (L checked_loc (checked { m_pats = L pats_loc checked_pats'
                       , m_grhss = grhss { grhssGRHSs = fmap wrap_fallback (grhssGRHSs grhss)
                                         , grhssLocalBinds = locals } })) }

    constructor_proof index (con, fields) =
      newEvVar (mkNomEqPred (TermVarTy index) (retainedConIndex (varType index) con fields))

    check_columns _ [] [] = return []
    check_columns column (RetainedConDict _ : args) pats = check_columns (column+1) args pats
    check_columns column (argument : args) (pat@(L pat_loc inner) : pats)
      | RetainedConIndex shared <- argument
      = setSrcSpanA pat_loc $
        case inner of
          VarPat _ (L _ name) ->
            do { names <- check_columns (column+1) args pats
               ; return ((name, idName shared) : names) }
          WildPat{} -> check_columns (column+1) args pats
          _ | column == first_column || column == second_column
            -> check_columns (column+1) args pats
            | otherwise -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
      | otherwise
      = do { unless (isRetainedVarPat pat) (setSrcSpanA pat_loc argument_error)
           ; check_columns (column+1) args pats }
    check_columns _ _ _ = argument_error

{- Note [Retained constructor matrices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor row in a two-index matrix needs one proof for each matched index.
Keep both shared whole-argument Ids. Resolve their constructor patterns with the
actual source field Ids. Lift the first nominal constructor equality through a
payload or result type, then lift the second equality through that result.
Compose the two coercions. Check the specialised RHS and cast it back to the
original result. No equality follows from an alias or a shared source spelling.

Attach each proof to its own ConPat, inside an outer AsPat for its whole Id.
Keep the original MatchGroupTc types and all source argument lambdas. Ordinary
payload CoPats preserve their outer types. A payload before the second index
cannot mention that binder; its second transport is reflexive. Keep retained domains at their incoming types. The branch implication
contains their transported equalities and the specialised dictionaries.
Attach its evidence bindings to the last matched constructor.
When the second pattern is a bare variable or wildcard, keep its shared
index at the incoming type. Use only the first constructor proof. The
second VarPat binds the shared index without a case. A variable fallback
uses the original dictionaries and keeps its evidence outside its local
definitions and RHS.

When the telescope starts with a retained callback, select the two written
constructor columns. Each column must have a structural retained binder.
Use a bare variable or wildcard in the other columns. Collect callbacks
with the rest of the telescope and rename each row's callback Names to
the shared Ids. Keep all source patterns in order. The ordinary matcher
uses the original arity and applies Strict to the original positions.
Use the same coverage check as for a matrix without a callback prefix.

For an omitted pair, instantiate each constructor with fresh fields.
Specialise the declared nominal equalities to that pair. Check them with
the enclosing nominal Givens. Accept the omission only if the ordinary
Given solver refutes the specialised equalities. A stuck family can leave
the pair possible. See also
Note [Retained equalities in uncovered patterns] in GHC.HsToCore.Pmc.Solver.

Check each possible constructor pair. A row with a bare second pattern
covers the remaining pairs for its first constructor. Reject a later row
that earlier rows already cover. The final fallback described in
Note [Retained variable fallback] covers all remaining pairs.
Keep bare subfields. Ordinary grouping of
repeated constructors would alias distinct native fields and proofs. Desugar
proof-bearing rows separately as described in GHC.HsToCore.Match.Constructor,
Note [Retained constructor equation scope]. This preserves row failure and
left-before-right demand. Agda supplies the constructor substitution and split
tree precedents in docs/dependent-haskell/REFERENCES.md#two-index-constructor-equations.
-}

{- Note [Retained variable fallback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Allow one final equation with bare variables or wildcards in both selected
index columns.
Constructor rows retain their two actual-field proofs. The variable row has
zero constructor proofs. Start its transports with nominal reflexivity; do not
pass reflexivity to tcRetainedCoercion, which requires a constructor endpoint.
Check the fallback RHS at the original result type. Earlier failed rows supply
no equality, field Id or Given to that RHS.

Rename both source variable Names to the shared whole-index Names before TC.
Also rename any other retained variable columns, as in M19. Rebuild NameSets
and reject quotations before renaming. Replace each selected WildPat with a
VarPat carrying that column's existing shared Name and source span. A raw
WildPat would not bind the Id needed by later payload and result types. Do not
allocate a fresh Id or expose a source name for an anonymous column. Keep the
original source Match for diagnostics. Check quotations even if both columns
are anonymous. Keep the checked index VarPats unwrapped.
A lazy let alias is not native index equality. Repackaging a fallback payload
with its original index must work without an alias proof.

GHC.HsToCore.Match already keeps the final PgAny block after the PgCon block.
Its matchVariables drops a column without forcing it. combineMatchResults and
shareFailureHandler put fallback execution outside failed constructor proofs.
Keep M20's separate constructor rows. No backend change is needed. In particular,
a missing first constructor can reach fallback without demanding the second
index. Keep all source argument lambdas before this discrimination.

Agda's variable terminal problems and ordered catchall compilation supply the
precedents. See docs/dependent-haskell/REFERENCES.md#ordered-variable-fallback-equations
for pinned source ranges and GHC failure-continuation provenance. These sources
do not establish Haskell demand; the source probes must check it separately.
Agda's anonymous context names and GHC's variable environment and wildcard
tidying rules supply the anonymous binder precedents. See
docs/dependent-haskell/REFERENCES.md#anonymous-terminal-fallback-equations.
-}

-- | Resolve a constructor and its actual closed runtime field identities.

tcRetainedConMatches :: Id -> [ExpPatType] -> [ExpPatType] -> [RetainedConArg] -> ExpRhoType
                    -> MatchGroup GhcRn (LHsExpr GhcRn)
                    -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedConMatches index invis_pat_tys pat_tys argument_plan rhs_ty
                    group@(MG { mg_alts = L loc matches, mg_ext = (origin, _) })
  | Just (second_column, second) <- matrix_column
  , not split_final
  = tcRetainedMatrixMatches (0, index) (second_column + 1, second) invis_pat_tys
      (mkCheckExpFunPatTy (Scaled ManyTy (varType index)) : pat_tys) all_arguments rhs_ty group
  | otherwise
  = setSrcSpanA loc $
    do { constructors <- mapM constructor matches
       ; let cons = map fst constructors
             domain_cons = case splitTyConApp_maybe (coreFullView (varType index)) of
               Just (tc, _) -> tyConDataCons tc
               _ -> []
             columns = retainedColumnNames all_arguments
             has_value_pattern (L _ (Match { m_pats = L _ pats })) =
               or [not (isRetainedVarPat pat) | (Nothing, pat) <- zip columns (filter isVisArgLPat pats)]
       -- See Note [Ordinary patterns after a retained constructor].
       ; let represented = filter (`elem` cons) domain_cons
             missing = filter (`notElem` cons) domain_cons
       ; unless (cons `equalLength` represented || any has_value_pattern matches) coverage_error
       ; arg_tys <- mapM argument_type pat_tys
       ; let written = zip constructors matches
       ; rows <- if any (not . isManyTy . scaledMult) arg_tys
                 then group_linear_rows written
                 else return [(row, Nothing) | row <- written]
       ; absent <- mapM (absurd_row arg_tys) missing
       ; result_ty <- readExpType rhs_ty
       ; (usages, checked) <- mapAndUnzipM
           (\((con, match), args) -> tcCollectingUsage (check_match args con arg_tys result_ty match))
           (rows ++ [(row, Just ([], [])) | row <- absent])
       ; tcEmitBindingUsage (supUEs usages)
       ; if null matches
         then empty_matches arg_tys result_ty
         else return (MG { mg_alts = L loc checked
                         , mg_ext = MatchGroupTc (Scaled ManyTy (varType index) : arg_tys) result_ty origin }) }
  where
    matrix_column = retainedMatrixColumn argument_plan group
    all_arguments = RetainedConIndex index : argument_plan

    -- See Note [Split the final retained column inside its first branch].
    split_final
      | Just (0, _) <- matrix_column
      , [RetainedConIndex _] <- argument_plan
      , Just (first, second) <- unzip <$> traverse split_columns matches
      , sizeUniqSet (mkNameSet first) == length first
      = any isNothing second && any isJust second
      | otherwise = False

    split_columns (L _ Match { m_pats = L _ pats })
      | second : prefix <- reverse pats
      , isVisArgLPat second
      , [first] <- filter isVisArgLPat prefix
      = do { con <- simple_con first
           ; later <- if isRetainedVarPat second
                      then Just Nothing else Just <$> simple_con second
           ; return (unwrapUserRdr con, later) }
      | otherwise = Nothing

    simple_con :: LPat GhcRn -> Maybe (WithUserRdr Name)
    simple_con pat = case unLoc (case_pattern pat) of
      ConPat { pat_con = L _ name, pat_args = PrefixCon _ fields }
        | all isRetainedVarPat fields -> Just name
      _ -> Nothing

    -- The indexed case checker reads the constructor at the pattern head.
    case_pattern :: LPat GhcRn -> LPat GhcRn
    case_pattern (L _ (ParPat _ inner)) = case_pattern inner
    case_pattern pat = retainedConPat pat

    terminal_case arg_cos match@(Match { m_pats = L pats_loc pats, m_grhss = grhss })
      | split_final
      , [domain_co] <- arg_cos
      , (second : prefix) <- reverse pats
      , Just name <- simple_con second
      = setSrcSpanA (getLoc second) $
        do { con_like <- tcLookupConLike name
           ; domain <- liftZonkM (zonkTcType (pSnd (coercionKind domain_co)))
           ; loc <- getCtLocM PatCheckOrigin Nothing
           ; Reduction _ domain <- runTcSEqualities (rewriteFamilyAxioms loc domain)
           ; case (con_like, splitTyConApp_maybe (coreFullView domain)) of
               (RealDataCon con, Just (tc, args))
                 | tc == dataConTyCon con
                 , let possible = filter (not . dataConCannotMatch args) (tyConDataCons tc)
                 -> case possible of
                      [only] | only == con -> return ()
                      _ | missing@(_ : _) <- filter (/= con) possible ->
                            failWithTc (TcRnRetainedQuantificationError
                              (RetainedUncoveredConstructors (dataConName con) (map dataConName missing)))
                        | otherwise -> failWithTc (TcRnRetainedQuantificationError RetainedMatrixCoverage)
               _ -> failWithTc (TcRnRetainedQuantificationError RetainedMatrixCoverage)
           ; name <- newSysName (mkVarOcc "retained_case")
           ; let arg_pat = L (getLoc second) (VarPat noExtField (noLocA name))
                 guarded = L (getLoc second) (HsMultiIf noExtField (grhssGRHSs grhss))
                 inner_grhss
                   | is_unguarded grhss = grhss
                   | otherwise = grhss { grhssGRHSs = noLocA (GRHS noAnn [] guarded) :| [] }
                 alt = L (getLoc second)
                     (match { m_ctxt = CaseAlt, m_pats = L pats_loc [case_pattern second]
                            , m_grhss = inner_grhss })
                 scrut = noLocA (HsVar noExtField (noLocA (noUserRdr name)))
                 cases = MG { mg_ext = (Generated OtherExpansion SkipPmc, snd (mg_ext group))
                            , mg_alts = L (getLoc (mg_alts group)) [alt] }
                 body = L (getLoc second) (HsCase CaseAlt scrut cases)
           ; return (match { m_pats = L pats_loc (reverse prefix ++ [arg_pat])
                           , m_grhss = unguardedGRHSs (getLocA second) body noAnn }) }
      | otherwise = return match

    -- See Note [Linear fields after a retained constructor].
    group_linear_rows [] = return []
    group_linear_rows (row@((dc, fields), L row_loc first@Match { m_pats = L pats_loc pats }) : rest)
      = do { let (same, others) = partition ((== dc) . fst . fst) rest
           ; combined <- case same of
               [] -> return (row, Nothing)
               _ | con_pat : args@(first_arg : _) <- pats
                 , let written_args = filter visible argument_plan
                 , all is_value written_args
                 , args `equalLength` written_args
                 -> do { names <- mapM (const (newSysName (mkVarOcc "arg"))) args
                       ; let ctxt = case args of [_] -> CaseAlt; _ -> LamAlt LamCases
                       ; alts <- mapM (value_alt fields ctxt) (row : same)
                       ; let cases = group { mg_alts = L loc alts }
                             body = case names of
                               [n] -> L (getLoc first_arg) (HsCase CaseAlt (nlHsVar n) cases)
                               _ -> L row_loc (HsLam noAnn LamCases cases)
                             outer = first { m_pats = L pats_loc (bindRetainedConFields fields con_pat : map nlVarPat names)
                                           , m_grhss = unguardedGRHSs (locA row_loc) body noAnn }
                       ; return (((dc, fields), L row_loc outer), Just (names, map (unLoc . snd) (row : same))) }
               _ -> setSrcSpanA row_loc argument_error
           ; (combined :) <$> group_linear_rows others }
      where
        is_value RetainedConValue = True
        is_value _ = False

    value_alt shared ctxt ((_, fields), L row_loc source@Match { m_pats = L pats_loc (_ : pats) })
      = do { let names = [(idName own, idName common) | (own, common) <- zipEqual fields shared, own /= common]
           ; case retainedMatchQuotation source of
               Just span | not (null names) -> setSrcSpan span $
                 failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
               _ -> return ()
           ; return (L row_loc (renameRetainedMatch names
                       (source { m_ctxt = ctxt, m_pats = L pats_loc pats }))) }
    value_alt _ _ (_, L row_loc _) = setSrcSpanA row_loc argument_error

    -- See Note [Omitted impossible retained equations].
    absurd_row arg_tys con
      | L _ first : _ <- matches
      = do { let con_pat = nlConPatName (dataConName con)
                             (replicate (dataConSourceArity con) nlWildPatName)
           ; resolved <- tcRetainedCon (varType index) con_pat
           ; (dc, fields) <- maybe (missing_equation con) return resolved
           ; eq <- newEvVar (mkNomEqPred (TermVarTy index)
                                         (retainedConIndex (varType index) dc fields))
           ; let arguments = [(arg, ty) | (arg, ty) <- zip argument_plan arg_tys, visible arg]
                 rows = [drop 1 (filter isVisArgLPat (hsLMatchPats row)) | row <- matches]
                 columns = zip3 [0 :: Int ..] arguments (transpose rows)
           ; unless (all (equalLength arguments) rows) (missing_equation con)
           ; let inspected = [(n, ty) | (n, (RetainedConValue, Scaled _ ty), pats) <- columns
                                     , any constructor_pattern pats]
                 branch = mkCoVarCo eq
           ; direct <- firstJustsM [empty_field branch n ty | (n, ty) <- inspected
                                     , anyFreeVarsOfType (== index) ty]
           ; witness <- case direct of
               Just _ -> return direct
               Nothing -> empty_under_givens branch inspected
           ; case witness of
               Nothing -> missing_equation con
               Just n -> do
                 { name <- newSysName (mkVarOcc "impossible")
                 ; pats <- forM columns $ \(i, (_, Scaled mult _), _) ->
                     if i == n then return (nlVarPat name)
                     else if isManyTy mult then return nlWildPatName
                     else nlVarPat <$> newSysName (mkVarOcc "unused")
                 ; let empty = MG (Generated OtherExpansion SkipPmc, snd (mg_ext group)) (L loc [])
                       rhs = noLocA (HsCase CaseAlt (nlHsVar name) empty)
                 ; return ((dc, fields), mkSimpleMatch (m_ctxt first) (noLocA (con_pat : pats)) rhs) } }
      | otherwise = missing_equation con

    missing_equation con = failWithTc (TcRnRetainedQuantificationError
                                         (RetainedMissingEquation (dataConName con)))

    visible RetainedConDict{} = False
    visible _ = True

    constructor_pattern pat = case unLoc (case_pattern pat) of
      ConPat{} -> True
      _ -> False

    empty_field eq n ty
      = do { co <- tcRetainedCoercion index eq ty
           ; loc <- getCtLocM PatCheckOrigin Nothing
           ; Reduction _ domain <- runTcSEqualities $
               rewriteFamilyAxioms loc (pSnd (coercionKind co))
           ; return (if isEmptyTy domain then Just n else Nothing) }

    -- See Note [Empty positions under an index equality].
    empty_under_givens eq inspected
      | not (null inspected)
      , let predicates = [varType dict | RetainedConDict dict <- argument_plan
                                       , isCoverageEquality (varType dict)]
      , any (anyFreeVarsOfType (== index)) predicates
      = do { level <- getTcLevel
           ; let solve tys = newEvVars tys >>= tcCheckGivens (emptyInertSet level) . listToBag
                 specialise ty = pSnd . coercionKind <$> tcRetainedCoercion index eq ty
           ; before <- solve predicates
           ; after <- mapM specialise predicates >>= solve
           ; case (before, after) of
               (Just outer, Just local) -> firstJustsM
                 [ do { original <- tcNormalise outer ty
                      ; refined <- specialise ty >>= tcNormalise local
                      ; return (if not (isEmptyTy original) && isEmptyTy refined
                                then Just n else Nothing) } | (n, ty) <- inspected ]
               _ -> return Nothing }
      | otherwise = return Nothing

    empty_matches arg_tys result_ty
      = do { arguments <- zipWithM (\argument (Scaled mult ty) ->
                               case argument of
                                 RetainedConValue -> newSysLocalId (fsLit "retained_arg") mult ty
                                 RetainedConDict dict -> return dict
                                 RetainedConIndex shared -> return shared)
                             argument_plan arg_tys
           ; let index_ty = Scaled ManyTy (varType index)
                 empty_case = MG { mg_alts = L loc []
                                 , mg_ext = MatchGroupTc [index_ty] result_ty origin }
                 rhs = L (noAnnSrcSpan (locA loc))
                         (HsCase CaseAlt (nlHsVar index) empty_case)
                 pats = map (noLocA . VarPat noExtField . noLocA) (index : arguments)
                 match = mkSimpleMatch (LamAlt LamSingle) (noLocA pats) rhs
           ; return (MG { mg_alts = L loc [match]
                        , mg_ext = MatchGroupTc (index_ty : arg_tys) result_ty
                                     (Generated OtherExpansion SkipPmc) }) }

    constructor (L match_loc Match { m_pats = L _ pats, m_grhss = grhss })
      | split_final || is_unguarded grhss
      = setSrcSpanA match_loc $
        case filter isVisArgLPat pats of
          pat : _ -> do { mb_con <- tcRetainedCon (varType index) pat
                        ; maybe coverage_error return mb_con }
          _ -> coverage_error
    constructor (L match_loc _) = setSrcSpanA match_loc coverage_error

    is_unguarded GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } = True
    is_unguarded _ = False

    coverage_error = failWithTc (TcRnRetainedQuantificationError RetainedConstructorCoverage)
    argument_error = failWithTc (TcRnRetainedQuantificationError RetainedConstructorArguments)

    argument_type (ExpFunPatTy ty)
      = readScaledExpType ty
    argument_type _ = argument_error

    check_match operands (con, fields) arg_tys result_ty (L match_loc source_match)
      = setSrcSpanA match_loc $
        do { let constructor_index = retainedConIndex (varType index) con fields
           ; cv <- newEvVar (mkNomEqPred (TermVarTy index) constructor_index)
           ; arg_cos <- mapM (tcRetainedCoercion index (mkCoVarCo cv) . scaledThing) arg_tys
           ; result_co <- tcRetainedCoercion index (mkCoVarCo cv) result_ty
           ; match@Match { m_pats = L _ pats } <- terminal_case arg_cos source_match
           ; names <- retained_names argument_plan (drop 1 (filter isVisArgLPat pats))
           ; unless (null names) $
               case retainedMatchQuotation match of
                 Nothing -> return ()
                 Just span -> setSrcSpan span $
                   failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
           -- Keep later retained arguments at their incoming types.
           -- See Note [Refining later retained domains].
           ; let refined_domains = zipWith3 (\dictionary (Scaled mult ty) co ->
                                   Scaled mult (case dictionary of
                                     RetainedConValue -> pSnd (coercionKind co)
                                     _ -> ty))
                                 argument_plan arg_tys arg_cos
                 refined_args = map mkCheckExpFunPatTy refined_domains
                 index_arg = mkCheckExpFunPatTy (Scaled ManyTy (varType index))
                 renamed = renameRetainedMatch names match
                 prepare = insertRetainedDictionaries all_arguments . retainedConPats [(0, fields)]
                         . bindRetainedWildcards (retainedColumnNames all_arguments)
                 match' = renamed { m_pats = fmap prepare (m_pats renamed)
                                  , m_ctxt = case operands of
                                      Just (_ : _ : _, _) -> LamAlt LamSingle
                                      _ -> m_ctxt renamed }
                 check_body (L body_loc (HsLam _ LamCases cases)) expected
                   | Just (names@(_ : _ : _), originals) <- operands = do
                       let domains = [dom | (RetainedConValue, dom) <- zip argument_plan refined_domains]
                       checked <- tcMatchesFromWithIndex Nothing (const originals)
                                    (LamAlt LamCases) tcBody (map mkCheckExpFunPatTy domains) expected cases
                       args <- zipWithM (\name (Scaled mult ty) ->
                                  tcScalingUsage mult (tcBody (nlHsVar name) (mkCheckExpType ty)))
                                  names domains
                       return (mkHsApps (L body_loc (HsLam noAnn LamCases checked)) args)
                 check_body body expected = tcBody body expected
                 original = case operands of
                   Just (_, first : _) -> first
                   _ -> source_match
                 check = do
                   clause <- if length matches == 1
                     then prepareUnguardedWhere (mkCheckExpType (pSnd (coercionKind result_co)))
                            (L match_loc match')
                     else return (L match_loc match')
                   tcMatchWithSource check_body (invis_pat_tys ++ index_arg : refined_args)
                     (mkCheckExpType (pSnd (coercionKind result_co))) original clause
           ; (branch_binds, L checked_loc checked) <-
               withRetainedRowGivens (PatSkol (RealDataCon con) (m_ctxt match))
                 [cv] argument_plan arg_cos check
           ; strict <- xoptM LangExt.Strict
           ; let Match { m_pats = L pats_loc checked_pats, m_grhss = grhss } = checked
                 generated pat@(L l _) | strict && isJust operands = L l (LazyPat noExtField pat)
                                       | otherwise = pat
                 checked_pats' = case checked_pats of
                   L first_loc first : rest ->
                     L first_loc (AsPat noExtField (L (noAnnSrcSpan generatedSrcSpan) index)
                                   (L first_loc (addRetainedConProof cv branch_binds first)))
                     : zipWith4 (\dictionary co (Scaled _ ty) (L pat_loc pat) ->
                                   case dictionary of
                                     RetainedConValue -> generated (L pat_loc (XPat (CoPat (mkWpCastN co) pat ty)))
                                     _ -> L pat_loc pat)
                                argument_plan arg_cos arg_tys rest
                   [] -> panic "tcRetainedConMatches: no constructor pattern"
                 cast_rhs (L rhs_loc (GRHS x guards rhs)) =
                   L rhs_loc (GRHS x guards (mkLHsWrapCo (mkSymCo result_co) rhs))
                 grhss' = grhss { grhssGRHSs = fmap cast_rhs (grhssGRHSs grhss) }
           ; return (L checked_loc (checked { m_pats = L pats_loc checked_pats'
                                          , m_ctxt = m_ctxt source_match
                                          , m_grhss = grhss' })) }

    -- Match shared retained positions to the row's resolved variable Names.
    retained_names :: [RetainedConArg] -> [LPat GhcRn] -> TcM [(Name, Name)]
    retained_names [] [] = return []
    retained_names (RetainedConDict _ : args) pats = retained_names args pats
    retained_names (RetainedConIndex shared : args) (L pat_loc pat : pats)
      = setSrcSpanA pat_loc $
        case pat of
          VarPat _ (L _ name) -> ((name, idName shared) :) <$> retained_names args pats
          WildPat{} -> retained_names args pats
          _ -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
    retained_names (RetainedConValue : args) (_ : pats) = retained_names args pats
    retained_names _ _ = argument_error


{- Note [Split the final retained column inside its first branch]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a complete set of distinct first constructors, split a later constructor
pattern inside its own first branch. Use this rule for two retained arguments
when at least one row has a bare second pattern. Keep the source arity. The
first pattern still runs before the second. A bare second pattern keeps its
usual demand. No other row can match after a guard fails in the first branch.

Use the first constructor proof to read the specialised second domain. Reduce
family axioms at the usual depth limit. Require every omitted constructor to
be impossible according to dataConCannotMatch. Move the final pattern and
its where bindings into a checked case. The indexed case rule sees the
actual constructor fields. Keep the source equation as the outer error context
and the written pattern's span on the generated alternative.

The generated case is structurally exhaustive. Its argument keeps the
incoming type in Core, so ordinary pattern checking would report a missing
constructor in that wider type. Check the original guard sequence in the
constructor scope with a multi-way if. This keeps guard coverage and where
bindings in scope while omitting a second check of constructor coverage.

See Agda's indexed coverage and case tree rules. Haskell's argument demand
also requires the original value lambdas before the first branch.
-}

{- Note [Linear fields after a retained constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained list index can consume an ordinary position of type Fin n
linearly. For the two cons equations, split on the list once, then case
on the position in the cons branch. Independent cons cases would consume
the position again in the failure continuation of the first equation.

Use the field identities of the first cons pattern throughout that branch.
Rename each subsequent row's field occurrences before checking its ordinary
position alternative. Keep the source order and span of each alternative.
The ordinary pattern checker checks field usage and missing alternatives.

With multiple ordinary columns, check all the alternatives at their
refined argument types. For example, indexThen takes both a linear Fin
and a linear continuation after the retained list. Inferring the type of
a fresh lambda here can lose the length of the pattern-bound tail.

Check each alternative with its original equation as the error context.
The generated outer match binds the arguments without testing them.
Under Strict, suppress bangs on those generated binders after checking
their usage. The written patterns still determine demand in each row.
On an omitted impossible branch, bind each linear operand even if the
branch ends in an empty case. A wildcard would require Many before GHC
checks that the branch is impossible.
-}

-- | Give an explicit empty case its checked domain in a retained constructor branch.
tcEmptyRetainedScrutinee :: LHsExpr GhcTc -> Type -> TcM (LHsExpr GhcTc, Type)
tcEmptyRetainedScrutinee scrut ty = do
  givens <- getLexicalGivens
  let retained cv = isCoVar cv && case getEqPredTys_maybe (varType cv) of
        Just (Nominal, TermVarTy v, _) -> isRetainedConstructorProof v (mkCoVarCo cv)
        _ -> False
  if not (any retained givens) || isEmptyTy ty then pure (scrut, ty) else do
    level <- getTcLevel
    inerts <- tcCheckGivens (emptyInertSet level) (listToBag (filter (isCoverageEquality . varType) givens))
    case inerts of
      Just local -> do
        refined <- tcNormalise local ty
        if isEmptyTy refined then do
          co <- unifyType Nothing ty refined
          pure (mkLHsWrapCo co scrut, refined)
        else pure (scrut, ty)
      Nothing -> pure (scrut, ty)

-- | Select the equality assumptions available to the coverage check.
isCoverageEquality :: Type -> Bool
isCoverageEquality ty = isEqClassPred ty || case getEqPredTys_maybe ty of
  Just (Nominal, _, _) -> True
  _ -> False

{- Note [Empty positions under an index equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In foreach xs -> (n ~ sourceLength xs) => Fin n %1 -> a the position
mentions n. Its type has no direct occurrence of xs. Reuse the ordinary
pattern checker's Given solver to normalise the inspected position before
and after specialising the declared equalities to the missing constructor.
At [], Fin n becomes Fin Z. Require this change before inserting a row.
An unrelated empty type or contradictory context cannot select a witness.
Check the resulting empty case in GHC's usual branch scope. For a written
empty case, establish its specialised scrutinee type with a checked cast.
The pattern checker then sees Fin Z even when the branch has no RHS that
would otherwise keep the transported dictionary alive.
-}

{- Note [Omitted impossible retained equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For index (x : xs) FZ and index (x : xs) (FS i), the missing [] row
has a position of type Fin (sourceLength []). Specialising the retained
list and reducing checked family axioms gives Fin Z. No constructor of
Fin can return that type. Insert the same row as an explicit empty case
on the position. Partial application still binds all source arguments.

Only consider ordinary columns that a written constructor pattern inspects.
In particular, a variable or lazy pattern does not select that column.
If several columns prove the branch empty, use the first in source order.
Check each omitted first constructor independently. Use a nominal branch
equality and the complete constructors of the specialised argument datatype.
An abstract type, a stuck index, or a constructor that can match does not
establish emptiness. The ordinary pattern checker still checks the written rows.
-}

{- Note [Ordinary patterns after a retained constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n).
Both Z and S can occur in two equations, one for [] and one for a cons.
Require a row for every constructor of the retained domain. When the
source distinguishes later ordinary patterns, let the ordinary pattern
checker check their coverage and redundancy, including nested fields.
retained_names still requires a variable or wildcard in a later retained
column unless the dedicated two-index matrix rule applies.

Check each row under its own retained constructor proof. Transport an
ordinary argument to its refined type before matching the pattern. Keep
the argument cast around the checked pattern, so its constructor fields
and any GADT evidence stay in that row. The match compiler already emits
separate alternatives with failure continuations for these proofs.
See Note [Retained constructor equation scope] in GHC.HsToCore.Match.Constructor.
-}

-- | Install the fields used in the proofs for the selected constructor columns.
-- See Note [Parameterized retained constructor patterns] in GHC.Tc.Gen.Pat.
retainedConPats :: [(Int, [Id])] -> [LPat GhcRn] -> [LPat GhcRn]
retainedConPats columns = go 0
  where
    go _ [] = []
    go column (pat : pats)
      | not (isVisArgLPat pat) = pat : go column pats
      | otherwise = case lookup column columns of
          Just fields -> bindRetainedConFields fields pat : go (column+1) pats
          Nothing -> pat : go (column+1) pats

-- | Bind checked nominal constructor evidence on its actual pattern fields.
addRetainedConProof :: EvVar -> TcEvBinds -> Pat GhcTc -> Pat GhcTc
addRetainedConProof cv binds (ParPat x (L loc pat))
  = ParPat x (L loc (addRetainedConProof cv binds pat))
addRetainedConProof cv binds pat@(ConPat { pat_con_ext = ext })
  = assertPpr (null (cpt_dicts ext) && null (cpt_tvs ext) && empty_binds (cpt_binds ext)) (ppr pat) $
    pat { pat_con_ext = ext { cpt_dicts = [cv], cpt_binds = binds } }
  where
    empty_binds ev@EvBinds{} = isEmptyTcEvBinds ev
    empty_binds TcEvBinds{} = False
addRetainedConProof _ _ pat = pprPanic "addRetainedConProof: not a data constructor" (ppr pat)

{- Note [Retained constructor matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Checking a constructor equation substitutes its constructor into both the later
argument types and the result type. Agda's splitCon and checkRHS use this rule.
See docs/dependent-haskell/REFERENCES.md#agda-provenance.

Keep the original argument and result types in MatchGroupTc. Check each branch
against the specialised types. A fresh nominal constructor proof transports each
original argument into its specialised pattern type. The inverse result cast
transports the checked RHS back to the fixed outer result. Put the proof in
ConPatTc.cpt_dicts, so it binds these casts inside the strict Core alternative.
This does not ask the solver to rewrite opaque runtime indices.

Contexts need a different placement from the variable-prefix rule in
Note [Retained argument telescopes]. Keep all value and dictionary lambdas
before the constructor case. tcRetainedConArgs collects contexts at their
actual telescope positions, including a context after the last source value
pattern. Insert shared dictionary VarPats with their original predicate types.
selectMatchVar then chooses these original Ids for the outer lambdas. Do not
put a refined CoPat around a dictionary: its outer lambda would have a fresh
Id, while the evidence binding could refer to an alias bound only later.

Each alternative instead gets fresh Givens at the specialised predicate types.
Check the whole match with these Givens. Transport the original dictionaries
with the constructor proof and bind the fresh Givens in ConPatTc.cpt_binds,
along with the solver evidence. These bindings are inside the proof scope and
cover later patterns, guards, and local bindings. The original dictionary
parameters have the same Unique in every equation. This is deliberate, as for
the hidden index AsPat. Native constructors have no existing dictionaries,
skolems, or evidence bindings; assert this before adding the branch evidence.

Use one hidden unrestricted index Id across the equations. A generated AsPat makes
selectMatchVar choose this Id. tidy1 removes the identical alias. The retained
lambda and all its index occurrences therefore have the same Unique, without
adding a field to MatchGroupTc. Keep exactly one unguarded equation for each
constructor of the index domain. This gives the native proof case complete coverage.
Later arguments accept variables or wildcards and require Many multiplicity.

A later retained binder also needs one shared runtime Id across all equations.
Consume each DepFunTy in telescope order and instantiate its codomain with that
Id before reading later argument types. RetainedConArg distinguishes these
indices from evidence and ordinary payloads. Rename each row's resolved later
index Names to the shared Names before checking it. Update stored free-name sets
and preserve WithUserRdr spelling and source spans. Stop at embedded Core Types.
Check the rewritten row, but use the original source row for error contexts.
Reject quotations at their expression spans before the rewrite.

Keep each shared index VarPat outermost, as for dictionaries. Even a reflexive
CoPat would make selectMatchVar choose a fresh Id and break runtime identity.
Keep its incoming domain even when constructor transport changes that domain.
See Note [Refining later retained domains]. Only dictionary positions get
specialised Givens; ordinary payloads keep their existing CoPats.
All source value and dictionary lambdas still precede constructor discrimination.
Agda lifts its constructor substitution over the later telescope and RHS target.
See docs/dependent-haskell/REFERENCES.md#later-retained-arguments-in-constructor-equations.

An empty domain needs no constructor proof. Bind the chosen index with a
variable pattern and inspect it with an actual empty case. Keep the original
span and origin on the empty case. Skip pattern-match checking for the generated
wrapper clause. This keeps the result's runtime index in the lambda's scope
and preserves empty-case strictness. Preserve any dictionary parameters in the
outer telescope. An empty source case instead uses ordinary case checking in
its original context; there is no branch to refine or context to generalize.

Keep the CoPat even when transport is reflexive. A synonym such as
Ignore b = Int can remove the index during specialisation. selectMatchVar must
still allocate the outer argument from its original Ignore b annotation.
Otherwise the recovered lambda type can lose its retained binder.
-}

{- Note [Refining later retained domains]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An earlier constructor proof can change a later retained argument's domain.
For example, n ~ Z changes [Vec Int n] to [Vec Int Z]. Keep the shared xs Id
and its incoming type [Vec Int n] in every equation. Do not change that Id's
type to the refined domain or add a CoPat around its binding.

The branch checks each use against its expected domain. tcRetainedArgVar in
GHC.Tc.Gen.App retains the nominal conversion as Cast (Var xs) (SubCo delta).
The result coercion from tcRetainedCoercion relates the original xs index to
that forward cast. Thus the branch result and its runtime operand use the
same conversion. See Note [Lifting runtime binder domains] in GHC.Core.Coercion.
No new runtime binding, case or evaluation of xs is required.

Check the branch under the constructor proof as a Given. Ordinary domain
unification can then use that exact equality for an on-use conversion.
In a single-column constructor equation, give the solver the non-reflexive
domain conversions for later retained binders as well. For example, after
matching cut against S k, an input keeps its type Vec a (Add cut m).
Matching VCons against that input provides Add cut m ~ S n. The domain
conversion relates Vec a (Add cut m) to Vec a (Add (S k) m), so the
solver can determine the tail width n ~ Add k m. Its evidence comes from
the same constructor proof and is in scope only inside this branch.
checkConstraints captures the Wanted evidence in a local implication.
Store its evidence bindings in the ConPat that binds the constructor proof.
For a two-index matrix, store the bindings in the last matched ConPat.
All its proofs are in scope there. A fallback row has no constructor Given.
Retained package patterns use the same rule for each inner constructor.
This does not add general rewriting or inversion of runtime indices.

This follows the ordered constructor refinement used for the later telescope
in Agda. The explicit nominal conversions preserve this fork's lazy runtime
identity rule. They do not establish totality or equality of distinct values.
-}


funBindPrecondition :: MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
funBindPrecondition (MG { mg_alts = L _ alts })
  = not (null alts) && all is_fun_rhs alts
  where
    is_fun_rhs (L _ (Match { m_ctxt = FunRhs {} })) = True
    is_fun_rhs _                                    = False

tcLambdaMatches :: HsExpr GhcRn -> HsLamVariant
                -> MatchGroup GhcRn (LHsExpr GhcRn)
                -> [ExpPatType]  -- Already skolemised
                -> ExpSigmaType  -- NB can be a sigma-type
                -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
tcLambdaMatches e lam_variant matches invis_pat_tys res_ty
  =  do { arity <- checkArgCounts matches
            -- Check argument counts since this is also used for \cases

        ; (wrapper, r)
            <- case res_ty of
                 Check ty | hasRetainedFunTy ty
                   -> tcRetainedMatches herald GenSigCtxt ctxt arity invis_pat_tys ty matches
                 _ -> matchExpectedFunTys herald GenSigCtxt arity res_ty $ \ pat_tys rhs_ty ->
                        tcMatches ctxt check_body (invis_pat_tys ++ pat_tys) rhs_ty matches

        ; return (wrapper, r) }
  where
    check_body
      | LamSingle <- lam_variant
      , [L _ Match { m_pats = L _ pats
                   , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }]
          <- unLoc (mg_alts matches)
      = tcScopedRuntimeBody (collectPatsBinders CollNoDictBinders pats)
      | otherwise = tcBody

    ctxt   = LamAlt lam_variant
    herald = ExpectedFunTyLam lam_variant e
             -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify

-- | Infer a result within the value scopes that may occur in its type.
-- See Note [Closing an inferred runtime index scope].
tcScopedRuntimeBody :: [Name] -> LHsExpr GhcRn -> ExpRhoType -> TcM (LHsExpr GhcTc)
tcScopedRuntimeBody bound body@(L loc expr) expected
  | not (null bound)
  , Infer ir <- expected
  = do { local <- newInferExpType (ir_inst ir)
       ; checked <- tcBody body local
       ; actual <- readExpType local
       ; Reduction co reduced <- setSrcSpanA loc $
           tcCloseRuntimeScope bound (exprCtOrigin expr) actual
       ; L loc <$> tcWrapResultMono expr (mkHsWrapCo co (unLoc checked)) reduced expected }
  | otherwise = tcBody body expected

-- | Close the result with nominal family evidence before leaving a runtime scope.
tcCloseRuntimeScope :: [Name] -> CtOrigin -> Type -> TcM Reduction
tcCloseRuntimeScope [] _ ty = return (Reduction (mkNomReflCo ty) ty)
tcCloseRuntimeScope bound origin ty
  = do { actual <- liftZonkM (zonkTcType ty)
       ; if anyFreeVarsOfType mentions_local actual
         then do { loc <- getCtLocM origin (Just TypeLevel)
                 ; reduction@(Reduction _ reduced) <- tcRewriteFamilyAxioms loc actual
                 ; case find mentions_local (tyCoVarsOfTypeList reduced) of
                     Just index -> failWithTc $ TcRnRetainedQuantificationError (RetainedIndexOutOfScope index)
                     Nothing -> return reduction }
         else return (Reduction (mkNomReflCo actual) actual) }
  where
    mentions_local var = isId var && not (isCoVar var) && getName var `elem` bound

{- Note [Closing an inferred runtime index scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An inferred result can contain a local runtime variable even when callers
only need the length of a list containing that variable. Reduce the result
with the checked family axioms while the local variable is still in scope.
Insert the nominal result cast inside the scope. For example, a result
Vec Int (sourceLength '[element]) can leave a let scope as Vec Int (S Z).
A result that still refers to element cannot leave.

Use an inner inference result so the outer hole sees only the closed type.
The conversion does not equate distinct field values or look at a binding's
RHS. A checked outer type already sends its evidence to the scoped body.
Keep the usual checking rule in that case.
-}

{-
@tcCaseMatches@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.
-}

tcCaseMatches :: (AnnoBody body, Outputable (body GhcTc))
              => HsMatchContextRn
              -> TcMatchAltChecker body    -- ^ Typecheck the alternative RHSS
              -> Scaled TcSigmaTypeFRR     -- ^ Type of scrutinee
              -> MatchGroup GhcRn (LocatedA (body GhcRn)) -- ^ The case alternatives
              -> ExpRhoType                               -- ^ Type of the whole case expression
              -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))
                -- Translated alternatives
                -- wrapper goes from MatchGroup's ty to expected ty

tcCaseMatches ctxt tc_body (Scaled scrut_mult scrut_ty) matches res_ty
  = tcMatches ctxt tc_body [ExpFunPatTy (Scaled scrut_mult (mkCheckExpType scrut_ty))] res_ty matches

-- | Check a dependent case on a named opaque GADT scrutinee.
tcIndexedCaseMatches :: Id -> HsMatchContextRn -> Scaled TcSigmaTypeFRR
                     -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
                     -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcIndexedCaseMatches index ctxt (Scaled mult ty) matches res_ty =
  tcMatchesFromWithIndex (Just (index, mkLHsWrapCo . mkSymCo)) (map unLoc) ctxt tcBody
    [ExpFunPatTy (Scaled mult (mkCheckExpType ty))] res_ty matches

-- @tcGRHSsPat@ typechecks @[GRHSs]@ that occur in a @PatMonoBind@.
tcGRHSsPat :: Mult -> GRHSs GhcRn (LHsExpr GhcRn) -> ExpRhoType
           -> TcM (GRHSs GhcTc (LHsExpr GhcTc))
-- Used for pattern bindings
tcGRHSsPat mult grhss res_ty
  = tcScalingUsage mult $ tcGRHSs PatBindRhs tcBody grhss res_ty

{- *********************************************************************
*                                                                      *
                tcMatch
*                                                                      *
********************************************************************* -}

-- | Type checker for a body of a match alternative
type TcMatchAltChecker body   -- c.f. TcStmtChecker, also in this module
  =  LocatedA (body GhcRn)
  -> ExpRhoType
  -> TcM (LocatedA (body GhcTc))

type AnnoBody body
  = ( Outputable (body GhcRn)
    , Anno (Match GhcRn (LocatedA (body GhcRn))) ~ SrcSpanAnnA
    , Anno (Match GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
    , Anno [LocatedA (Match GhcRn (LocatedA (body GhcRn)))] ~ SrcSpanAnnA
    , Anno [LocatedA (Match GhcTc (LocatedA (body GhcTc)))] ~ SrcSpanAnnA
    , Anno (GRHS GhcRn (LocatedA (body GhcRn))) ~ EpAnnCO
    , Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ EpAnnCO
    , Anno (StmtLR GhcRn GhcRn (LocatedA (body GhcRn))) ~ SrcSpanAnnA
    , Anno (StmtLR GhcTc GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
    )

-- | Type-check a MatchGroup.
tcMatches :: (AnnoBody body, Outputable (body GhcTc))
          => HsMatchContextRn
          -> TcMatchAltChecker body
          -> [ExpPatType]             -- ^ Expected pattern types.
          -> ExpRhoType               -- ^ Expected result-type of the Match.
          -> MatchGroup GhcRn (LocatedA (body GhcRn))
          -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))

tcMatches = tcMatchesFrom id

-- | Reuse expression-let checking for a single unguarded equation's where bindings.
-- See Note [Unguarded where aliases].
tcExprMatchesWithWhereFrom
  :: (Match GhcRn (LHsExpr GhcRn) -> Match GhcRn (LHsExpr GhcRn))
  -> HsMatchContextRn -> [ExpPatType] -> ExpRhoType
  -> MatchGroup GhcRn (LHsExpr GhcRn)
  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcExprMatchesWithWhereFrom source ctxt pat_tys exp_ty
                          group@MG { mg_alts = L loc [match] }
  = do { prepared <- prepareUnguardedWhere exp_ty match
       ; tcMatchesFrom (const (source (unLoc match))) ctxt tcBody pat_tys exp_ty
           (group { mg_alts = L loc [prepared] }) }
tcExprMatchesWithWhereFrom source ctxt pat_tys exp_ty group
  = tcMatchesFrom source ctxt tcBody pat_tys exp_ty group

-- | Move one eligible where group into its expression RHS without changing patterns.
-- See Note [Unguarded where aliases].
prepareUnguardedWhere :: ExpRhoType -> LMatch GhcRn (LHsExpr GhcRn)
                     -> TcM (LMatch GhcRn (LHsExpr GhcRn))
prepareUnguardedWhere (Check expected) original@(L match_loc match@Match
  { m_grhss = grhss@GRHSs { grhssGRHSs = L rhs_loc (GRHS ext [] rhs) :| []
                         , grhssLocalBinds = binds@(L binds_loc
                             (HsValBinds _ (XValBindsLR (HsVBG groups sigs)))) } })
  | isTauTy expected
  , not (anyFreeVarsOfType (\v -> isTyVar v && isMetaTyVar v) expected)
  , not (null groups), all nonrecursive_function groups, all isTypeLSig sigs
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; strict <- xoptM LangExt.Strict
       ; if enabled && not strict
         then let body = wrapGenSpan' (combineSrcSpans (getLocA rhs) (locA binds_loc))
                          (HsLet noExtField binds rhs)
                  local = L binds_loc (EmptyLocalBinds noExtField)
              in return (L match_loc (match { m_grhss = grhss
                   { grhssGRHSs = L rhs_loc (GRHS ext [] body) :| []
                   , grhssLocalBinds = local } }))
         else return original }
  where
    nonrecursive_function (NonRecursive, [L _ FunBind{}]) = True
    nonrecursive_function _ = False
prepareUnguardedWhere _ match = return match

{- Note [Unguarded where aliases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One unguarded equation can put its where declarations in a let around its
RHS. The patterns and the local declarations keep their original identities.
Check the let in the pattern scope. Mark only the new let as generated
and use the original equation for errors.
This reuses checked expression aliases without checking any declaration twice.
Keep inferred or mutable result types on the ordinary path. Keep Strict and
extension-off checking unchanged.

Do not move guarded or multiple equations. Their local declarations scope over
guards, and guard failure can continue with another equation. A let in each
RHS would lose that scope and sharing. A multi-way if would change fallthrough.
The generic tcGRHSs path also checks commands and remains unchanged.
-}

-- | Check prepared clauses and report errors against their source equations.
tcMatchesFrom :: (AnnoBody body, Outputable (body GhcTc))
              => (Match GhcRn (LocatedA (body GhcRn)) -> Match GhcRn (LocatedA (body GhcRn)))
              -> HsMatchContextRn -> TcMatchAltChecker body -> [ExpPatType] -> ExpRhoType
              -> MatchGroup GhcRn (LocatedA (body GhcRn))
              -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))
tcMatchesFrom source = tcMatchesFromWithIndex Nothing (map (source . unLoc))

tcMatchesFromWithIndex :: (AnnoBody body, Outputable (body GhcTc))
                       => Maybe (Id, TcCoercion -> LocatedA (body GhcTc) -> LocatedA (body GhcTc))
                       -> ([LMatch GhcRn (LocatedA (body GhcRn))] -> [Match GhcRn (LocatedA (body GhcRn))])
                       -> HsMatchContextRn -> TcMatchAltChecker body -> [ExpPatType] -> ExpRhoType
                       -> MatchGroup GhcRn (LocatedA (body GhcRn))
                       -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))
tcMatchesFromWithIndex index source ctxt tc_body pat_tys exp_ty (MG { mg_alts = L l matches
                                                     , mg_ext = (origin, _) })
  | null matches  -- Deal with case e of {}
    -- Since there are no branches, no one else will fill in exp_ty
    -- when in inference mode, so we must do it ourselves,
    -- here, using expTypeToType
  = do { tcEmitBindingUsage bottomUE
         -- See Note [Pattern types for EmptyCase]
       ; let vis_pat_tys = filter isVisibleExpPatType pat_tys
       ; pat_ty <- case vis_pat_tys of
           [ExpFunPatTy t]      -> scaledExpTypeToType t
           [ExpForAllPatTy ma tvb] -> failWithTc $ TcRnEmptyCase ctxt (EmptyCaseForall (ma,tvb))
           []                   -> panic "tcMatches: no arguments in EmptyCase"
           _t1:(_t2:_ts)        -> panic "tcMatches: multiple arguments in EmptyCase"
       ; rhs_ty <- expTypeToType exp_ty
       ; return (MG { mg_alts = L l []
                    , mg_ext = MatchGroupTc [pat_ty] rhs_ty origin
                    }) }

  | otherwise
  = do { let exp_ty' = adjustExpTypeForCaseBranches exp_ty matches
             tc_match (original, match) = tcCollectingUsage $
                              tcMatchWithIndex index tc_body pat_tys exp_ty' original match
       ; (usages, matches') <- mapAndUnzipM tc_match (zipEqual (source matches) matches)
       ; tcEmitBindingUsage $ supUEs usages
       ; pat_tys  <- mapM readScaledExpType (filter_out_forall_pat_tys pat_tys)
       ; rhs_ty   <- readExpType exp_ty
       ; traceTc "tcMatches" (ppr matches' $$ ppr pat_tys $$ ppr rhs_ty)
       ; return (MG { mg_alts   = L l matches'
                    , mg_ext    = MatchGroupTc pat_tys rhs_ty origin
                    }) }
  where
    -- We filter out foralls because we have no use for them in HsToCore.
    filter_out_forall_pat_tys :: [ExpPatType] -> [Scaled ExpSigmaTypeFRR]
    filter_out_forall_pat_tys = mapMaybe match_fun_pat_ty
      where
        match_fun_pat_ty (ExpFunPatTy t)  = Just t
        match_fun_pat_ty ExpForAllPatTy{} = Nothing

{- Note [Pattern types for EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In tcMatches, we might encounter an empty list of matches if the user wrote
`case x of {}` or `\case {}`.

* First of all, both `case x of {}` and `\case {}` match on exactly one visible
  argument, which follows from

    checkArgCounts :: MatchGroup GhcRn ... -> TcM VisArity
    checkArgCounts (MG { mg_alts = L _ [] })
      = return 1
    ...

  So we expect vis_pat_tys to be a singleton list [pat_ty] and panic otherwise.

  Multi-case `\cases {}` can't violate this assumption in `tcMatches` because it
  must have been rejected earlier in `rnMatchGroup`.

  Other MatchGroup contexts (function equations `f x = ...`, lambdas `\a b -> ...`,
  etc) are not considered here because there is no syntax to construct them with
  an empty list of alternatives.

* With lambda-case, we run the risk of trying to match on a type argument:

    f :: forall (xs :: Type) -> ()
    f = \case {}

  This is not valid and it used to trigger a panic in pmcMatches (#25004).
  We reject it by inspecting the expected pattern type:

    ; pat_ty <- case vis_pat_tys of
        [ExpFunPatTy t]      -> ...    -- value argument, ok
        [ExpForAllPatTy tvb] -> ...    -- type argument, error!

  Test case: typecheck/should_fail/T25004
-}

-------------
-- | Check a rewritten equation with its original source in the error context.
tcMatchWithSource :: (AnnoBody body)
                  => TcMatchAltChecker body
                  -> [ExpPatType]
                  -> ExpRhoType
                  -> Match GhcRn (LocatedA (body GhcRn))
                  -> LMatch GhcRn (LocatedA (body GhcRn))
                  -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))
tcMatchWithSource = tcMatchWithIndex Nothing

tcMatchWithIndex :: AnnoBody body
                 => Maybe (Id, TcCoercion -> LocatedA (body GhcTc) -> LocatedA (body GhcTc))
                 -> TcMatchAltChecker body -> [ExpPatType] -> ExpRhoType
                 -> Match GhcRn (LocatedA (body GhcRn))
                 -> LMatch GhcRn (LocatedA (body GhcRn))
                 -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))
tcMatchWithIndex index tc_body pat_tys rhs_ty source_match match
  = do { (L loc r) <- wrapLocMA (tc_match pat_tys rhs_ty) match
       ; return (L loc r) }
  where
    tc_match pat_tys rhs_ty
             (Match { m_ctxt = ctxt, m_pats = L l pats, m_grhss = grhss })
      = add_match_ctxt $
        do { (pats', grhss') <- case index of
               Nothing -> tcMatchPats ctxt pats pat_tys (tcGRHSs ctxt tc_body grhss rhs_ty)
               Just (scrutinee, wrap_rhs) ->
                 tcMatchPatsWithIndex scrutinee ctxt pats pat_tys (check_grhss scrutinee wrap_rhs)

             -- NB: pats' are just the /value/ patterns
             -- See Note [tcMatchPats] in GHC.Tc.Gen.Pat

           ; return (Match { m_ext   = noExtField
                           , m_ctxt  = ctxt
                           , m_pats  = L l pats'
                           , m_grhss = grhss' }) }
      where
        -- For (\x -> e), tcExpr has already said "In the expression \x->e"
        --     so we don't want to add "In the lambda abstraction \x->e"
        -- But for \cases with many alternatives, it is helpful to say
        --     which particular alternative we are looking at
        check_grhss scrutinee wrap_rhs (Just proof)
          | Check ty <- rhs_ty
          = do { co <- tcRetainedIndexCoercion (TermVarTy scrutinee) (mkCoVarCo proof) ty
               ; let check_rhs body rhs = wrap_rhs co <$> tc_body body rhs
               ; tcGRHSs ctxt check_rhs grhss (mkCheckExpType (pSnd (coercionKind co))) }
        check_grhss _ _ _ = tcGRHSs ctxt tc_body grhss rhs_ty

        add_match_ctxt thing_inside = case ctxt of
            LamAlt LamSingle -> thing_inside
            StmtCtxt (HsDoStmt{}) -> thing_inside -- this is an expanded do stmt
            RecUpd -> thing_inside -- record update is Expanded out so ignore it
            _          -> addErrCtxt (MatchInCtxt source_match) thing_inside

-------------
tcGRHSs :: AnnoBody body
        => HsMatchContextRn
        -> TcMatchAltChecker body
        -> GRHSs GhcRn (LocatedA (body GhcRn))
        -> ExpRhoType
        -> TcM (GRHSs GhcTc (LocatedA (body GhcTc)))
-- Notice that we pass in the full res_ty, so that we get
-- good inference from simple things like
--      f = \(x::forall a.a->a) -> <stuff>
-- We used to force it to be a monotype when there was more than one guard
-- but we don't need to do that any more
tcGRHSs ctxt tc_body (GRHSs _ grhss (L l binds)) res_ty
  = do  { (binds', grhss') <- tcLocalBinds binds $ do
                                       tcGRHSNE ctxt tc_body grhss res_ty
        ; return (GRHSs emptyComments grhss' (L l binds')) }

tcGRHSNE :: forall body. AnnoBody body
           => HsMatchContextRn -> TcMatchAltChecker body
           -> NonEmpty (LGRHS GhcRn (LocatedA (body GhcRn))) -> ExpRhoType
           -> TcM (NonEmpty (LGRHS GhcTc (LocatedA (body GhcTc))))
tcGRHSNE ctxt tc_body grhss res_ty
   = do { (usages, grhss') <- unzip <$> traverse (wrapLocSndMA tc_alt) grhss
        ; tcEmitBindingUsage $ supUEs usages
        ; return grhss' }
   where
     stmt_ctxt = PatGuard ctxt

     tc_alt :: GRHS GhcRn (LocatedA (body GhcRn))
            -> TcM (UsageEnv, GRHS GhcTc (LocatedA (body GhcTc)))
     tc_alt (GRHS _ guards rhs)
       = tcCollectingUsage $
         do  { (guards', rhs')
                   <- tcStmtsAndThen stmt_ctxt tcGuardStmt guards res_ty $
                      tc_body rhs
             ; return (GRHS noAnn guards' rhs') }

{-
************************************************************************
*                                                                      *
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
*                                                                      *
************************************************************************
-}

tcDoStmts :: HsDoFlavour
          -> LocatedA [LStmt GhcRn (LHsExpr GhcRn)]
          -> ExpRhoType
          -> TcM (HsExpr GhcTc)          -- Returns a HsDo
tcDoStmts ListComp (L l stmts) res_ty
  = do  { res_ty <- expTypeToType res_ty
        ; (co, elt_ty) <- matchExpectedListTy res_ty
        ; let list_ty = mkListTy elt_ty
        ; stmts' <- tcStmts (HsDoStmt ListComp) (tcLcStmt listTyCon) stmts
                            (mkCheckExpType elt_ty)
        ; return $ mkHsWrapCo co (HsDo list_ty ListComp (L l stmts')) }

tcDoStmts MonadComp (L l stmts) res_ty
  = do  { stmts' <- tcStmts (HsDoStmt MonadComp) tcMcStmt stmts res_ty
        ; res_ty <- readExpType res_ty
        ; return (HsDo res_ty MonadComp (L l stmts')) }


tcDoStmts doExpr@(DoExpr _) (L l stmts) res_ty
  = do { stmts' <- tcStmts (HsDoStmt doExpr) tcDoStmt stmts res_ty
       ; res_ty <- readExpType res_ty
       ; return (HsDo res_ty doExpr (L l stmts')) }

-- NB: ghcistmts should fail, MDoExpr is handled by expansions
tcDoStmts ctxt _ _ = pprPanic "tcDoStmts" (pprHsDoFlavour ctxt)

tcBody :: LHsExpr GhcRn -> ExpRhoType -> TcM (LHsExpr GhcTc)
tcBody body res_ty
  = do  { traceTc "tcBody" (ppr res_ty)
        ; tcPolyLExpr body res_ty
        }

{-
************************************************************************
*                                                                      *
\subsection{tcStmts}
*                                                                      *
************************************************************************
-}

type TcExprStmtChecker = TcStmtChecker HsExpr ExpRhoType
type TcCmdStmtChecker  = TcStmtChecker HsCmd  TcRhoType

type TcStmtChecker body rho_type
  =  forall thing. HsStmtContextRn
                -> Stmt GhcRn (LocatedA (body GhcRn))
                -> rho_type                 -- Result type for comprehension
                -> (rho_type -> TcM thing)  -- Checker for what follows the stmt
                -> TcM (Stmt GhcTc (LocatedA (body GhcTc)), thing)

tcStmts :: (AnnoBody body) => HsStmtContextRn
        -> TcStmtChecker body rho_type   -- NB: higher-rank type
        -> [LStmt GhcRn (LocatedA (body GhcRn))]
        -> rho_type
        -> TcM [LStmt GhcTc (LocatedA (body GhcTc))]
tcStmts ctxt stmt_chk stmts res_ty
  = do { (stmts', _) <- tcStmtsAndThen ctxt stmt_chk stmts res_ty $
                        const (return ())
       ; return stmts' }

tcStmtsAndThen :: (AnnoBody body) => HsStmtContextRn
               -> TcStmtChecker body rho_type    -- NB: higher-rank type
               -> [LStmt GhcRn (LocatedA (body GhcRn))]
               -> rho_type
               -> (rho_type -> TcM thing)
               -> TcM ([LStmt GhcTc (LocatedA (body GhcTc))], thing)

-- Note the higher-rank type.  stmt_chk is applied at different
-- types in the equations for tcStmts

tcStmtsAndThen _ _ [] res_ty thing_inside
  = do  { thing <- thing_inside res_ty
        ; return ([], thing) }

-- LetStmts are handled uniformly, regardless of context
tcStmtsAndThen ctxt stmt_chk (L loc (LetStmt x (L l binds)) : stmts)
                                                             res_ty thing_inside
  = do  { (binds', (stmts',thing)) <- tcLocalBinds binds $
              tcStmtsAndThen ctxt stmt_chk stmts res_ty thing_inside
        ; return (L loc (LetStmt x (L l binds')) : stmts', thing) }

-- Don't set the error context for an ApplicativeStmt.  It ought to be
-- possible to do this with a popErrCtxt in the tcStmt case for
-- ApplicativeStmt, but it did something strange and broke a test (ado002).
tcStmtsAndThen ctxt stmt_chk (L loc stmt : stmts) res_ty thing_inside
  | XStmtLR ApplicativeStmt{} <- stmt
  = do  { (stmt', (stmts', thing)) <-
             stmt_chk ctxt stmt res_ty $ \ res_ty' ->
               tcStmtsAndThen ctxt stmt_chk stmts res_ty'  $
                 thing_inside
        ; return (L loc stmt' : stmts', thing) }

  -- For the vanilla case, handle the location-setting part
  | otherwise
  = do  { (stmt', (stmts', thing)) <-
                setSrcSpanA loc                             $
                addErrCtxt (StmtErrCtxt ctxt (L loc stmt))  $
                stmt_chk ctxt stmt res_ty                   $ \ res_ty' ->
                popErrCtxt                                  $
                tcStmtsAndThen ctxt stmt_chk stmts res_ty'  $
                thing_inside
        ; return (L loc stmt' : stmts', thing) }

---------------------------------------------------
--              Pattern guards
---------------------------------------------------

tcGuardStmt :: TcExprStmtChecker
tcGuardStmt _ (BodyStmt _ guard _ _) res_ty thing_inside
  = do  { guard' <- tcScalingUsage ManyTy $ tcCheckMonoExpr guard boolTy
          -- Scale the guard to Many (see #19120 and #19193)
        ; thing  <- thing_inside res_ty
        ; return (BodyStmt boolTy guard' noSyntaxExpr noSyntaxExpr, thing) }

tcGuardStmt ctxt (BindStmt _ pat rhs) res_ty thing_inside
  = do  { -- The Many on the next line and the unrestricted on the line after
          -- are linked. These must be the same multiplicity. Consider
          --   x <- rhs -> u
          --
          -- The multiplicity of x in u must be the same as the multiplicity at
          -- which the rhs has been consumed. When solving #18738, we want these
          -- two multiplicity to still be the same.
          (rhs', rhs_ty) <- tcScalingUsage ManyTy $
                            tcInferRhoFRRNC FRRBindStmtGuard rhs
                                   -- Stmt has a context already
        ; (pat', thing)  <- tcCheckPat_O (StmtCtxt ctxt) (lexprCtOrigin rhs)
                                         pat (unrestricted rhs_ty) $
                            thing_inside res_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

tcGuardStmt _ stmt _ _
  = pprPanic "tcGuardStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           List comprehensions
--               (no rebindable syntax)
---------------------------------------------------

-- Dealt with separately, rather than by tcMcStmt, because
--   a) We have special desugaring rules for list comprehensions,
--      which avoid creating intermediate lists.  They in turn
--      assume that the bind/return operations are the regular
--      polymorphic ones, and in particular don't have any
--      coercion matching stuff in them.  It's hard to avoid the
--      potential for non-trivial coercions in tcMcStmt

{-
Note [List comprehension isn't linear]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The usefulness of list comprehension in conjunction with linear types is dubious.
After all, statements are made to be run many times, for instance in

[u | y <- [0,1], stmts]

both u and stmts are going to be run several times.

In principle, though, there are some positions in a monad comprehension
expressions which could be considered linear. We could try and make it so that
these positions are considered linear by the typechecker, but in practice the
desugarer doesn't take enough care to ensure that these are indeed desugared to
linear sites. We tried in the past, and it turned out that we'd miss a
desugaring corner case (#25772).

Until there's a demand for this very specific improvement, let's instead be
conservative, and consider list comprehension to be completely non-linear.

Here are the case where list comprehension could be linear, together with the
known challenges.

(LCL1) [ y | () <- xs, y <- [0,1]] could be linear in `xs`.
But, the way the desugaring works, we get something like

case xs of
  () : xs ' -> letrec next_stmt = … xs' …

In the current typing rules for letrec in Core, next_stmt is necessarily of
multiplicity Many and so is every free variable, including xs'. Which, in turns,
requires xs to be of multiplicity Many.

Rodrigo Mesquita worked out, in his master thesis, how to make letrecs having
non-Many multiplicities. But it's a fair bit of work to implement.

Since nobody actually cares about [ y | () <- xs, y <- [0,1]] being linear, then
we just conservatively make it unrestricted instead.

If we're to change that, we have to be careful that [ y | _ <- xs, y <- [0,1]]
isn't linear in `xs` since the elements of `xs` are ignored. So we'd still have
to call `tcScalingUsage` on `xs` in `tcLcStmt`, we'd just have to create a fresh
multiplicity variable. We'd also use the same multiplicity variable in the call
to `tcCheckPat` instead of `unrestricted`.

(LCL2) [ y | b, y <- [0,1]] could be linear in `b`. This actually works fine
with -O0 (as far as anybody knows), but -O and higher desugar list comprehension
using the `build` combinator (this was the cause of #25772). But `build` isn't
defined to be linear. The consequences of making `build` linear are
unknown. It's not worth trying until a real need arises.

(LCL3) [ x | ] could be linear in x. But it's unclear why anybody would want to
write this instead of [ x ]. Besides, this syntax is currently rejected by the
parser. The `build` obstacle of (LCL2) applies here too.
-}

tcLcStmt :: TyCon       -- The list type constructor ([])
         -> TcExprStmtChecker

tcLcStmt _ _ (LastStmt x body noret _) elt_ty thing_inside
  = do { -- see (LCL3) in Note [List comprehension isn't linear]
         body' <- tcScalingUsage ManyTy $ tcMonoLExprNC body elt_ty
       ; thing <- thing_inside (panic "tcLcStmt: thing_inside")
       ; return (LastStmt x body' noret noSyntaxExpr, thing) }

-- A generator, pat <- rhs
tcLcStmt m_tc ctxt (BindStmt _ pat rhs) elt_ty thing_inside
 = do   { pat_ty <- newFlexiTyVarTy liftedTypeKind
          -- see (LCL1) in Note [List comprehension isn't linear]
        ; rhs'   <- tcScalingUsage ManyTy $ tcCheckMonoExpr rhs (mkTyConApp m_tc [pat_ty])
        ; (pat', thing)  <- tcCheckPat (StmtCtxt ctxt) pat (unrestricted pat_ty) $
                            tcScalingUsage ManyTy $
                            thing_inside elt_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

-- A boolean guard
tcLcStmt _ _ (BodyStmt _ rhs _ _) elt_ty thing_inside
  = do  { -- Regarding the `tcScalingUsage ManyTy` on the `rhs`,
          -- see (LCL2) in Note [List comprehension isn't linear]
          rhs'  <- tcScalingUsage ManyTy $ tcCheckMonoExpr rhs boolTy
        ; thing <- tcScalingUsage ManyTy $ thing_inside elt_ty
        ; return (BodyStmt boolTy rhs' noSyntaxExpr noSyntaxExpr, thing) }

-- ParStmt: See notes with tcMcStmt and Note [Scoping in parallel list comprehensions]
tcLcStmt m_tc ctxt (ParStmt _ bndr_stmts_s _ _) elt_ty thing_inside
  = tcScalingUsage ManyTy $ -- parallel list comprehension never desugars to something linear.
    do  { env <- getLocalRdrEnv
        ; (pairs', thing) <- loop env [] bndr_stmts_s
        ; return (ParStmt unitTy pairs' noExpr noSyntaxExpr, thing) }
  where
    loop
     :: LocalRdrEnv -> [Name] -> NonEmpty (ParStmtBlock GhcRn GhcRn)
     -> TcM (NonEmpty (ParStmtBlock GhcTc GhcTc), _)
    -- Invariant: on entry to `loop`, the LocalRdrEnv is set to
    --            origEnv, the LocalRdrEnv for the entire comprehension
    loop origEnv priorBinds (ParStmtBlock x stmts names _ :| pairs)
      = do { (stmts', (ids, pairs', thing))
                <- tcStmtsAndThen ctxt (tcLcStmt m_tc) stmts elt_ty $ \ _elt_ty' ->
                   do { ids <- tcLookupLocalIds names
                      ; (pairs', thing) <- setLocalRdrEnv origEnv $
                            loop1 origEnv (names ++ priorBinds) pairs
                      ; return (ids, pairs', thing) }
           ; return ( ParStmtBlock x stmts' ids noSyntaxExpr :| pairs', thing ) }

    loop1
     :: LocalRdrEnv -> [Name] -> [ParStmtBlock GhcRn GhcRn]
     -> TcM ([ParStmtBlock GhcTc GhcTc], _)
    -- matching in the branches
    loop1 _ binds [] = [ ([], a) | a <- bindLocalNames binds (thing_inside elt_ty) ]
    loop1 env binds (x:xs) = [ (toList ys, a) | (ys, a) <- loop env binds (x:|xs) ]

tcLcStmt m_tc ctxt (TransStmt { trS_form = form, trS_stmts = stmts
                              , trS_bndrs =  bindersMap
                              , trS_by = by, trS_using = using }) elt_ty thing_inside
  = tcScalingUsage ManyTy $ -- Transform statements are too complex: just make everything multiplicity Many
    do { let (bndr_names, n_bndr_names) = unzip bindersMap
             unused_ty = pprPanic "tcLcStmt: inner ty" (ppr bindersMap)
             -- The inner 'stmts' lack a LastStmt, so the element type
             --  passed in to tcStmtsAndThen is never looked at
       ; (stmts', (bndr_ids, by'))
            <- tcStmtsAndThen (TransStmtCtxt ctxt) (tcLcStmt m_tc) stmts unused_ty $ \_ -> do
               { by' <- traverse tcInferRho by
               ; bndr_ids <- tcLookupLocalIds bndr_names
               ; return (bndr_ids, by') }

       ; let m_app ty = mkTyConApp m_tc [ty]

       --------------- Typecheck the 'using' function -------------
       -- using :: ((a,b,c)->t) -> m (a,b,c) -> m (a,b,c)       (ThenForm)
       --       :: ((a,b,c)->t) -> m (a,b,c) -> m (m (a,b,c)))  (GroupForm)

         -- n_app :: Type -> Type   -- Wraps a 'ty' into '[ty]' for GroupForm
       ; let n_app = case form of
                       ThenForm -> (\ty -> ty)
                       _        -> m_app

             by_arrow :: Type -> Type     -- Wraps 'ty' to '(a->t) -> ty' if the By is present
             by_arrow = case by' of
                          Nothing       -> \ty -> ty
                          Just (_,e_ty) -> \ty -> (alphaTy `mkVisFunTyManyU` e_ty) `mkVisFunTyManyU` ty

             tup_ty        = mkBigCoreVarTupTy bndr_ids
             poly_arg_ty   = m_app alphaTy
             poly_res_ty   = m_app (n_app alphaTy)
             using_poly_ty = mkInfForAllTy unmatchableDataConTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkVisFunTyManyU` poly_res_ty

       ; using' <- tcCheckPolyExpr using using_poly_ty
       ; let final_using = fmap (mkHsWrap (WpTyApp tup_ty)) using'

             -- 'stmts' returns a result of type (m1_ty tuple_ty),
             -- typically something like [(Int,Bool,Int)]
             -- We don't know what tuple_ty is yet, so we use a variable
       ; let mk_n_bndr :: Name -> TcId -> TcId
             mk_n_bndr n_bndr_name bndr_id = mkLocalId n_bndr_name ManyTy (n_app (idType bndr_id))

             -- Ensure that every old binder of type `b` is linked up with its
             -- new binder which should have type `n b`
             -- See Note [TransStmt binder map] in GHC.Hs.Expr
             n_bndr_ids  = zipWith mk_n_bndr n_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` n_bndr_ids

       -- Type check the thing in the environment with
       -- these new binders and return the result
       ; thing <- tcExtendIdEnv n_bndr_ids (thing_inside elt_ty)

       ; return (TransStmt { trS_stmts = stmts', trS_bndrs = bindersMap'
                           , trS_by = fmap fst by', trS_using = final_using
                           , trS_ret = noSyntaxExpr
                           , trS_bind = noSyntaxExpr
                           , trS_fmap = noExpr
                           , trS_ext = unitTy
                           , trS_form = form }, thing) }

tcLcStmt _ _ stmt _ _
  = pprPanic "tcLcStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           Monad comprehensions
--        (supports rebindable syntax)
---------------------------------------------------

tcMcStmt :: TcExprStmtChecker

tcMcStmt _ (LastStmt x body noret return_op) res_ty thing_inside
  = do  { (body', return_op')
            <- tcSyntaxOp MCompOrigin return_op [SynRho] res_ty $
               \ [a_ty] [mult]->
               tcScalingUsage mult $ tcCheckMonoExprNC body a_ty
        ; thing      <- thing_inside (panic "tcMcStmt: thing_inside")
        ; return (LastStmt x body' noret return_op', thing) }

-- Generators for monad comprehensions ( pat <- rhs )
--
--   [ body | q <- gen ]  ->  gen :: m a
--                            q   ::   a
--

tcMcStmt ctxt (BindStmt xbsrn pat rhs) res_ty thing_inside
           -- (>>=) :: rhs_ty -> (pat_ty -> new_res_ty) -> res_ty
  = do  { ((rhs_ty, rhs', pat_mult, pat', thing, new_res_ty), bind_op')
            <- tcSyntaxOp MCompOrigin (xbsrn_bindOp xbsrn)
                          [SynRho, SynFun SynAny SynRho] res_ty $
               \ [rhs_ty, pat_ty, new_res_ty] [rhs_mult, fun_mult, pat_mult] ->
               do { rhs' <- tcScalingUsage rhs_mult $ tcCheckMonoExprNC rhs rhs_ty
                  ; (pat', thing) <- tcScalingUsage fun_mult $ tcCheckPat (StmtCtxt ctxt) pat (Scaled pat_mult pat_ty) $
                                     thing_inside (mkCheckExpType new_res_ty)
                  ; return (rhs_ty, rhs', pat_mult, pat', thing, new_res_ty) }

        ; hasFixedRuntimeRep_syntactic (FRRBindStmt MonadComprehension) rhs_ty

        -- If (but only if) the pattern can fail, typecheck the 'fail' operator
        ; fail_op' <- fmap join . forM (xbsrn_failOp xbsrn) $ \fail ->
            tcMonadFailOp (MCompPatOrigin pat) pat' fail new_res_ty

        ; let xbstc = XBindStmtTc
                { xbstc_bindOp = bind_op'
                , xbstc_boundResultType = new_res_ty
                , xbstc_boundResultMult = pat_mult
                , xbstc_failOp = fail_op'
                }
        ; return (BindStmt xbstc pat' rhs', thing) }

-- Boolean expressions.
--
--   [ body | stmts, expr ]  ->  expr :: m Bool
--
tcMcStmt _ (BodyStmt _ rhs then_op guard_op) res_ty thing_inside
  = do  { -- Deal with rebindable syntax:
          --    guard_op :: test_ty -> rhs_ty
          --    then_op  :: rhs_ty -> new_res_ty -> res_ty
          -- Where test_ty is, for example, Bool
        ; ((thing, rhs', rhs_ty, new_res_ty, test_ty, guard_op'), then_op')
            <- tcSyntaxOp MCompOrigin then_op [SynRho, SynRho] res_ty $
               \ [rhs_ty, new_res_ty] [rhs_mult, fun_mult] ->
               do { ((rhs', test_ty), guard_op')
                      <- tcScalingUsage rhs_mult $
                         tcSyntaxOp MCompOrigin guard_op [SynAny]
                                    (mkCheckExpType rhs_ty) $
                         \ [test_ty] [test_mult] -> do
                           rhs' <- tcScalingUsage test_mult $ tcCheckMonoExpr rhs test_ty
                           return $ (rhs', test_ty)
                  ; thing <- tcScalingUsage fun_mult $ thing_inside (mkCheckExpType new_res_ty)
                  ; return (thing, rhs', rhs_ty, new_res_ty, test_ty, guard_op') }

        ; hasFixedRuntimeRep_syntactic FRRBodyStmtGuard test_ty
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt MonadComprehension 1) rhs_ty
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt MonadComprehension 2) new_res_ty

        ; return (BodyStmt rhs_ty rhs' then_op' guard_op', thing) }

-- Grouping statements
--
--   [ body | stmts, then group by e using f ]
--     ->  e :: t
--         f :: forall a. (a -> t) -> m a -> m (m a)
--   [ body | stmts, then group using f ]
--     ->  f :: forall a. m a -> m (m a)

-- We type [ body | (stmts, group by e using f), ... ]
--     f <optional by> [ (a,b,c) | stmts ] >>= \(a,b,c) -> ...body....
--
-- We type the functions as follows:
--     f <optional by> :: m1 (a,b,c) -> m2 (a,b,c)              (ThenForm)
--                     :: m1 (a,b,c) -> m2 (n (a,b,c))          (GroupForm)
--     (>>=) :: m2 (a,b,c)     -> ((a,b,c)   -> res) -> res     (ThenForm)
--           :: m2 (n (a,b,c)) -> (n (a,b,c) -> res) -> res     (GroupForm)
--
tcMcStmt ctxt (TransStmt { trS_stmts = stmts, trS_bndrs = bindersMap
                         , trS_by = by, trS_using = using, trS_form = form
                         , trS_ret = return_op, trS_bind = bind_op
                         , trS_fmap = fmap_op }) res_ty thing_inside
  = do { m1_ty   <- newFlexiTyVarTy typeToTypeKind
       ; m2_ty   <- newFlexiTyVarTy typeToTypeKind
       ; tup_ty  <- newFlexiTyVarTy liftedTypeKind
       ; by_e_ty <- newFlexiTyVarTy liftedTypeKind  -- The type of the 'by' expression (if any)

         -- n_app :: Type -> Type   -- Wraps a 'ty' into '(n ty)' for GroupForm
       ; n_app <- case form of
                    ThenForm -> return (\ty -> ty)
                    _        -> do { n_ty <- newFlexiTyVarTy typeToTypeKind
                                   ; return (n_ty `mkAppTy`) }
       ; let by_arrow :: Type -> Type
             -- (by_arrow res) produces ((alpha->e_ty) -> res)     ('by' present)
             --                          or res                    ('by' absent)
             by_arrow = case by of
                          Nothing -> \res -> res
                          Just {} -> \res -> (alphaTy `mkVisFunTyManyU` by_e_ty) `mkVisFunTyManyU` res

             poly_arg_ty  = m1_ty `mkAppTy` alphaTy
             using_arg_ty = m1_ty `mkAppTy` tup_ty
             poly_res_ty  = m2_ty `mkAppTy` n_app alphaTy
             using_res_ty = m2_ty `mkAppTy` n_app tup_ty
             using_poly_ty = mkInfForAllTy unmatchableDataConTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkVisFunTyManyU` poly_res_ty

             -- 'stmts' returns a result of type (m1_ty tuple_ty),
             -- typically something like [(Int,Bool,Int)]
             -- We don't know what tuple_ty is yet, so we use a variable
       ; let (bndr_names, n_bndr_names) = unzip bindersMap
       ; (stmts', (bndr_ids, by', return_op')) <-
            tcStmtsAndThen (TransStmtCtxt ctxt) tcMcStmt stmts
                           (mkCheckExpType using_arg_ty) $ \res_ty' -> do
                { by' <- case by of
                           Nothing -> return Nothing
                           Just e  -> do { e' <- tcCheckMonoExpr e by_e_ty
                                         ; return (Just e') }

                -- Find the Ids (and hence types) of all old binders
                ; bndr_ids <- tcLookupLocalIds bndr_names

                -- 'return' is only used for the binders, so we know its type.
                --   return :: (a,b,c,..) -> m (a,b,c,..)
                ; (_, return_op') <- tcSyntaxOp MCompOrigin return_op
                                       [synKnownType (mkBigCoreVarTupTy bndr_ids)]
                                       res_ty' $ \ _ _ -> return ()

                ; return (bndr_ids, by', return_op') }

       --------------- Typecheck the 'bind' function -------------
       -- (>>=) :: m2 (n (a,b,c)) -> ( n (a,b,c) -> new_res_ty ) -> res_ty
       ; new_res_ty <- newFlexiTyVarTy liftedTypeKind
       ; (_, bind_op')  <- tcSyntaxOp MCompOrigin bind_op
                             [ synKnownType using_res_ty
                             , synKnownType (n_app tup_ty `mkVisFunTyManyU` new_res_ty) ]
                             res_ty $ \ _ _ -> return ()

       --------------- Typecheck the 'fmap' function -------------
       ; fmap_op' <- case form of
                       ThenForm -> return noExpr
                       _ -> fmap unLoc . tcCheckPolyExpr (noLocA fmap_op) $
                            mkInfForAllTy unmatchableDataConTy alphaTyVar $
                            mkInfForAllTy unmatchableDataConTy betaTyVar  $
                            (alphaTy `mkVisFunTyManyU` betaTy)
                            `mkVisFunTyManyU` (n_app alphaTy)
                            `mkVisFunTyManyU` (n_app betaTy)

       --------------- Typecheck the 'using' function -------------
       -- using :: ((a,b,c)->t) -> m1 (a,b,c) -> m2 (n (a,b,c))

       ; using' <- tcCheckPolyExpr using using_poly_ty
       ; let final_using = fmap (mkHsWrap (WpTyApp tup_ty)) using'

       --------------- Building the bindersMap ----------------
       ; let mk_n_bndr :: Name -> TcId -> TcId
             mk_n_bndr n_bndr_name bndr_id = mkLocalId n_bndr_name ManyTy (n_app (idType bndr_id))

             -- Ensure that every old binder of type `b` is linked up with its
             -- new binder which should have type `n b`
             -- See Note [TransStmt binder map] in GHC.Hs.Expr
             n_bndr_ids = zipWithEqual mk_n_bndr n_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` n_bndr_ids

       -- Type check the thing in the environment with
       -- these new binders and return the result
       ; thing <- tcExtendIdEnv n_bndr_ids $
                  thing_inside (mkCheckExpType new_res_ty)

       ; return (TransStmt { trS_stmts = stmts', trS_bndrs = bindersMap'
                           , trS_by = by', trS_using = final_using
                           , trS_ret = return_op', trS_bind = bind_op'
                           , trS_ext = n_app tup_ty
                           , trS_fmap = fmap_op', trS_form = form }, thing) }

-- A parallel set of comprehensions
--      [ (g x, h x) | ... ; let g v = ...
--                   | ... ; let h v = ... ]
--
-- It's possible that g,h are overloaded, so we need to feed the LIE from the
-- (g x, h x) up through both lots of bindings (so we get the bindLocalMethods).
-- Similarly if we had an existential pattern match:
--
--      data T = forall a. Show a => C a
--
--      [ (show x, show y) | ... ; C x <- ...
--                         | ... ; C y <- ... ]
--
-- Then we need the LIE from (show x, show y) to be simplified against
-- the bindings for x and y.
--
-- It's difficult to do this in parallel, so we rely on the renamer to
-- ensure that g,h and x,y don't duplicate, and simply grow the environment.
-- So the binders of the first parallel group will be in scope in the second
-- group.  But that's fine; there's no shadowing to worry about.
--
-- Note: The `mzip` function will get typechecked via:
--
--   ParStmt [st1::t1, st2::t2, st3::t3]
--
--   mzip :: m st1
--        -> (m st2 -> m st3 -> m (st2, st3))   -- recursive call
--        -> m (st1, (st2, st3))
--
tcMcStmt ctxt (ParStmt _ bndr_stmts_s mzip_op bind_op) res_ty thing_inside
  = do { m_ty   <- newFlexiTyVarTy typeToTypeKind

       ; let mzip_ty  = mkInfForAllTys [(unmatchableDataConTy,tv) | tv <- [alphaTyVar, betaTyVar]] $
                        (m_ty `mkAppTy` alphaTy)
                        `mkVisFunTyManyU`
                        (m_ty `mkAppTy` betaTy)
                        `mkVisFunTyManyU`
                        (m_ty `mkAppTy` mkBoxedTupleTy [alphaTy, betaTy])
       ; mzip_op' <- unLoc `fmap` tcCheckPolyExpr (noLocA mzip_op) mzip_ty

        -- type dummies since we don't know all binder types yet.
       ; tup_tys_and_bndr_stmts_s <- traverse (\ bndr_stmts@(ParStmtBlock _ _ names _) ->
           [ (tup_tys, bndr_stmts)
           | tup_tys <- mkBigCoreTupTy <$> traverse (newOpenFlexiFRRTyVarTy . FRRBinder) names ]) bndr_stmts_s

       -- Typecheck bind:
       ; let tuple_ty = mk_tuple_ty (NE.map fst tup_tys_and_bndr_stmts_s)

       ; (((blocks', thing), inner_res_ty), bind_op')
           <- tcSyntaxOp MCompOrigin bind_op
                         [ synKnownType (m_ty `mkAppTy` tuple_ty)
                         , SynFun (synKnownType tuple_ty) SynRho ] res_ty $
              \ [inner_res_ty] _ ->
              do { stuff <- loop m_ty (mkCheckExpType inner_res_ty) tup_tys_and_bndr_stmts_s
                 ; return (stuff, inner_res_ty) }

       ; return (ParStmt inner_res_ty blocks' mzip_op' bind_op', thing) }

  where
    mk_tuple_ty tys = foldr1 (\tn tm -> mkBoxedTupleTy [tn, tm]) tys

    loop
     :: Type -> ExpRhoType -> NonEmpty (Type, ParStmtBlock GhcRn GhcRn)
     -> TcM (NonEmpty (ParStmtBlock GhcTc GhcTc), _)
    loop m_ty inner_res_ty ((tup_ty_in, ParStmtBlock x stmts names return_op) :| xs)
      = do { let m_tup_ty = m_ty `mkAppTy` tup_ty_in
           ; (stmts', (ids, return_op', pairs', thing))
                <- tcStmtsAndThen ctxt tcMcStmt stmts (mkCheckExpType m_tup_ty) $
                   \m_tup_ty' ->
                   do { ids <- tcLookupLocalIds names
                      ; let tup_ty = mkBigCoreVarTupTy ids
                      ; (_, return_op') <-
                          tcSyntaxOp MCompOrigin return_op
                                     [synKnownType tup_ty] m_tup_ty' $
                                     \ _ _ -> return ()
                      ; (pairs', thing) <- loop1 m_ty inner_res_ty xs
                      ; return (ids, return_op', pairs', thing) }
           ; return (ParStmtBlock x stmts' ids return_op' :| pairs', thing) }

    loop1
     :: Type -> ExpRhoType -> [(Type, ParStmtBlock GhcRn GhcRn)]
     -> TcM ([ParStmtBlock GhcTc GhcTc], _)
    -- matching in the branches
    loop1 _ r [] = [ ([], a) | a <- thing_inside r ]
    loop1 m r (x:xs) = [ (toList ys, a) | (ys, a) <- loop m r (x:|xs) ]

tcMcStmt _ stmt _ _
  = pprPanic "tcMcStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           Do-notation
--        (supports rebindable syntax)
---------------------------------------------------

tcDoStmt :: TcExprStmtChecker

tcDoStmt _ (LastStmt x body noret _) res_ty thing_inside
  = do { body' <- tcMonoLExprNC body res_ty
       ; thing <- thing_inside (panic "tcDoStmt: thing_inside")
       ; return (LastStmt x body' noret noSyntaxExpr, thing) }
tcDoStmt ctxt (BindStmt xbsrn pat rhs) res_ty thing_inside
  = do  {       -- Deal with rebindable syntax:
                --       (>>=) :: rhs_ty ->_rhs_mult (pat_ty ->_pat_mult new_res_ty) ->_fun_mult res_ty
                -- This level of generality is needed for using do-notation
                -- in full generality; see #1537

          ((rhs_ty, rhs', pat_mult, pat', new_res_ty, thing), bind_op')
            <- tcSyntaxOp DoStmtOrigin (xbsrn_bindOp xbsrn) [SynRho, SynFun SynAny SynRho] res_ty $
                \ [rhs_ty, pat_ty, new_res_ty] [rhs_mult,fun_mult,pat_mult] ->
                do { rhs' <-tcScalingUsage rhs_mult $ tcCheckMonoExprNC rhs rhs_ty
                   ; (pat', thing) <- tcScalingUsage fun_mult $ tcCheckPat (StmtCtxt ctxt) pat (Scaled pat_mult pat_ty) $
                                      thing_inside (mkCheckExpType new_res_ty)
                   ; return (rhs_ty, rhs', pat_mult, pat', new_res_ty, thing) }

        ; hasFixedRuntimeRep_syntactic (FRRBindStmt DoNotation) rhs_ty

        -- If (but only if) the pattern can fail, typecheck the 'fail' operator
        ; fail_op' <- fmap join . forM (xbsrn_failOp xbsrn) $ \fail ->
            tcMonadFailOp (DoPatOrigin pat) pat' fail new_res_ty
        ; let xbstc = XBindStmtTc
                { xbstc_bindOp = bind_op'
                , xbstc_boundResultType = new_res_ty
                , xbstc_boundResultMult = pat_mult
                , xbstc_failOp = fail_op'
                }
        ; return (BindStmt xbstc pat' rhs', thing) }

tcDoStmt _ (BodyStmt _ rhs then_op _) res_ty thing_inside
  = do  {       -- Deal with rebindable syntax;
                --   (>>) :: rhs_ty -> new_res_ty -> res_ty
        ; ((rhs', rhs_ty, new_res_ty, thing), then_op')
            <- tcSyntaxOp DoStmtOrigin then_op [SynRho, SynRho] res_ty $
               \ [rhs_ty, new_res_ty] [rhs_mult,fun_mult] ->
               do { rhs' <- tcScalingUsage rhs_mult $ tcCheckMonoExprNC rhs rhs_ty
                  ; thing <- tcScalingUsage fun_mult $ thing_inside (mkCheckExpType new_res_ty)
                  ; return (rhs', rhs_ty, new_res_ty, thing) }
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt DoNotation 1) rhs_ty
        ; hasFixedRuntimeRep_syntactic (FRRBodyStmt DoNotation 2) new_res_ty
        ; return (BodyStmt rhs_ty rhs' then_op' noSyntaxExpr, thing) }
tcDoStmt ctxt (RecStmt { recS_stmts = L l stmts, recS_later_ids = later_names
                       , recS_rec_ids = rec_names, recS_ret_fn = ret_op
                       , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op })
         res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- mapM (newOpenFlexiFRRTyVarTy . FRRBinder) tup_names
        ; let tup_ids = zipWith (\n t -> mkLocalId n ManyTy t) tup_names tup_elt_tys
                -- Many because it's a recursive definition
              tup_ty  = mkBigCoreTupTy tup_elt_tys

        ; tcExtendIdEnv tup_ids $ do
        { ((stmts', (ret_op', tup_rets)), stmts_ty)
                <- runInferRho $ \ exp_ty ->
                   tcStmtsAndThen ctxt tcDoStmt stmts exp_ty $ \ inner_res_ty ->
                   do { tup_rets <- zipWithM tcCheckId tup_names
                                      (map mkCheckExpType tup_elt_tys)
                             -- Unify the types of the "final" Ids (which may
                             -- be polymorphic) with those of "knot-tied" Ids
                      ; (_, ret_op')
                          <- tcSyntaxOp DoStmtOrigin ret_op [synKnownType tup_ty]
                                        inner_res_ty $ \_ _ -> return ()
                      ; return (ret_op', tup_rets) }

        ; ((_, mfix_op'), mfix_res_ty)
            <- runInferRho $ \ exp_ty ->
               tcSyntaxOp DoStmtOrigin mfix_op
                          [synKnownType (mkVisFunTyManyU tup_ty stmts_ty)] exp_ty $
               \ _ _ -> return ()

        ; ((thing, new_res_ty), bind_op')
            <- tcSyntaxOp DoStmtOrigin bind_op
                          [ synKnownType mfix_res_ty
                          , SynFun (synKnownType tup_ty) SynRho ]
                          res_ty $
               \ [new_res_ty] _ ->
               do { thing <- thing_inside (mkCheckExpType new_res_ty)
                  ; return (thing, new_res_ty) }

        ; let rec_ids = takeList rec_names tup_ids
        ; later_ids <- tcLookupLocalIds later_names
        ; traceTc "tcdo" $ vcat [ppr rec_ids <+> ppr (map idType rec_ids),
                                 ppr later_ids <+> ppr (map idType later_ids)]
        ; return (RecStmt { recS_stmts = L l stmts', recS_later_ids = later_ids
                          , recS_rec_ids = rec_ids, recS_ret_fn = ret_op'
                          , recS_mfix_fn = mfix_op', recS_bind_fn = bind_op'
                          , recS_ext = RecStmtTc
                            { recS_bind_ty = new_res_ty
                            , recS_later_rets = []
                            , recS_rec_rets = tup_rets
                            , recS_ret_ty = stmts_ty} }, thing)
        }}

tcDoStmt ctxt (XStmtLR (ApplicativeStmt _ pairs mb_join)) res_ty thing_inside
  = do  { let tc_app_stmts ty = tcApplicativeStmts ctxt pairs ty $
                                thing_inside . mkCheckExpType
        ; ((pairs', body_ty, thing), mb_join') <- case mb_join of
            Nothing -> (, Nothing) <$> tc_app_stmts res_ty
            Just join_op ->
              second Just <$>
              (tcSyntaxOp DoStmtOrigin join_op [SynRho] res_ty $
               \ [rhs_ty] [rhs_mult] -> tcScalingUsage rhs_mult $ tc_app_stmts (mkCheckExpType rhs_ty))

        ; return (XStmtLR $ ApplicativeStmt body_ty pairs' mb_join', thing) }

tcDoStmt _ stmt _ _
  = pprPanic "tcDoStmt: unexpected Stmt" (ppr stmt)



---------------------------------------------------
-- MonadFail Proposal warnings
---------------------------------------------------

-- The idea behind issuing MonadFail warnings is that we add them whenever a
-- failable pattern is encountered. However, instead of throwing a type error
-- when the constraint cannot be satisfied, we only issue a warning in
-- "GHC.Tc.Errors".

tcMonadFailOp :: CtOrigin
              -> LPat GhcTc
              -> SyntaxExpr GhcRn    -- The fail op
              -> TcType              -- Type of the whole do-expression
              -> TcRn (FailOperator GhcTc)  -- Typechecked fail op
-- Get a 'fail' operator expression, to use if the pattern match fails.
-- This won't be used in cases where we've already determined the pattern
-- match can't fail (so the fail op is Nothing), however, it seems that the
-- isIrrefutableHsPat test is still required here for some reason I haven't
-- yet determined.
tcMonadFailOp orig pat fail_op res_ty = do
    is_strict <- xoptM LangExt.Strict
    comps <- getCompleteMatchesTcM
    if isIrrefutableHsPat is_strict (irrefutableConLikeTc comps) pat
      then return Nothing
      else Just . snd <$> (tcSyntaxOp orig fail_op [synKnownType stringTy]
                            (mkCheckExpType res_ty) $ \_ _ -> return ())

{-
Note [Treat rebindable syntax first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking
        do { bar; ... } :: IO ()
we want to typecheck 'bar' in the knowledge that it should be an IO thing,
pushing info from the context into the RHS.  To do this, we check the
rebindable syntax first, and push that information into (tcLExprNC rhs).
Otherwise the error shows up when checking the rebindable syntax, and
the expected/inferred stuff is back to front (see #3613).

Note [typechecking ApplicativeStmt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
join ((\pat1 ... patn -> body) <$> e1 <*> ... <*> en)

fresh type variables:
   pat_ty_1..pat_ty_n
   exp_ty_1..exp_ty_n
   t_1..t_(n-1)

body  :: body_ty
(\pat1 ... patn -> body) :: pat_ty_1 -> ... -> pat_ty_n -> body_ty
pat_i :: pat_ty_i
e_i   :: exp_ty_i
<$>   :: (pat_ty_1 -> ... -> pat_ty_n -> body_ty) -> exp_ty_1 -> t_1
<*>_i :: t_(i-1) -> exp_ty_i -> t_i
join :: tn -> res_ty

Note [Scoping in parallel list comprehensions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a parallel list comprehension like [ ebody | a <- blah1; e1 | b <- blah2; e2 ]
we want to ensure that in the lexical environment, tcl_rdr :: LocalRdrEnv, we have
  * 'a' in scope in e1, but not 'b'
  * 'b' in scope in e2, but not 'a'
  * Both in scope in ebody
We don't want too /many/ variables in the LocalRdrEnv, else we make stupid
suggestions for an out-of-scope variable (#22940).

To achieve this we:
  * At the start of each branch, reset the LocalRdrEnv to the outer scope.
  * Before typechecking ebody, add to LocalRdrEnv all the variables bound in
    all branches. This step is done with bindLocalNames.
-}

tcApplicativeStmts
  :: HsStmtContextRn
  -> [(SyntaxExpr GhcRn, ApplicativeArg GhcRn)]
  -> ExpRhoType                         -- rhs_ty
  -> (TcRhoType -> TcM t)               -- thing_inside
  -> TcM ([(SyntaxExpr GhcTc, ApplicativeArg GhcTc)], Type, t)

tcApplicativeStmts ctxt pairs rhs_ty thing_inside
 = do { body_ty <- newFlexiTyVarTy liftedTypeKind
      ; let arity = length pairs
      ; ts <- replicateM (arity-1) $ newInferExpType IIF_DeepRho
      ; exp_tys <- replicateM arity $ newFlexiTyVarTy liftedTypeKind
      ; pat_tys <- replicateM arity $ newFlexiTyVarTy liftedTypeKind
      ; let fun_ty = mkVisFunTysManyU pat_tys body_ty

       -- NB. do the <$>,<*> operators first, we don't want type errors here
       --     i.e. goOps before goArgs
       -- See Note [Treat rebindable syntax first]
      ; let (ops, args) = unzip pairs
      ; ops' <- goOps fun_ty (zip3 ops (ts ++ [rhs_ty]) exp_tys)

      -- Typecheck each ApplicativeArg separately
      -- See Note [ApplicativeDo and constraints]
      ; args' <- mapM (goArg body_ty) (zip3 args pat_tys exp_tys)

      -- Bring into scope all the things bound by the args,
      -- and typecheck the thing_inside
      -- See Note [ApplicativeDo and constraints]
      ; res <- tcExtendIdEnv (concatMap get_arg_bndrs args') $
               thing_inside body_ty

      ; return (zip ops' args', body_ty, res) }
  where
    goOps _ [] = return []
    goOps t_left ((op,t_i,exp_ty) : ops)
      = do { (_, op')
               <- tcSyntaxOp DoStmtOrigin op
                             [synKnownType t_left, synKnownType exp_ty] t_i $
                   \ _ _ -> return ()
           ; t_i <- readExpType t_i
           ; ops' <- goOps t_i ops
           ; return (op' : ops') }

    goArg :: Type -> (ApplicativeArg GhcRn, Type, Type)
          -> TcM (ApplicativeArg GhcTc)

    goArg body_ty (ApplicativeArgOne
                    { xarg_app_arg_one = fail_op
                    , app_arg_pattern = pat
                    , arg_expr = rhs
                    , ..
                    }, pat_ty, exp_ty)
      = setSrcSpan (combineSrcSpans (getLocA pat) (getLocA rhs)) $
        addErrCtxt (StmtErrCtxt ctxt (L (getLoc rhs) $ mkRnBindStmt pat rhs))   $
        do { rhs'      <- tcCheckMonoExprNC rhs exp_ty
           ; (pat', _) <- tcCheckPat (StmtCtxt ctxt) pat (unrestricted pat_ty) $
                          return ()
           ; fail_op' <- fmap join . forM fail_op $ \fail ->
               tcMonadFailOp (DoPatOrigin pat) pat' fail body_ty

           ; return (ApplicativeArgOne
                      { xarg_app_arg_one = fail_op'
                      , app_arg_pattern = pat'
                      , arg_expr        = rhs'
                      , .. }
                    ) }

    goArg _body_ty (ApplicativeArgMany x stmts ret pat ctxt, pat_ty, exp_ty)
      = do { (stmts', (ret',pat')) <-
                tcStmtsAndThen (HsDoStmt ctxt) tcDoStmt stmts (mkCheckExpType exp_ty) $
                \res_ty  -> do
                  { ret'      <- tcExpr ret res_ty
                  ; (pat', _) <- tcCheckPat (StmtCtxt (HsDoStmt ctxt)) pat (unrestricted pat_ty) $
                                 return ()
                  ; return (ret', pat')
                  }
           ; return (ApplicativeArgMany x stmts' ret' pat' ctxt) }

    get_arg_bndrs :: ApplicativeArg GhcTc -> [Id]
    get_arg_bndrs (ApplicativeArgOne { app_arg_pattern = pat }) = collectPatBinders CollNoDictBinders pat
    get_arg_bndrs (ApplicativeArgMany { bv_pattern =  pat })    = collectPatBinders CollNoDictBinders pat

{- Note [ApplicativeDo and constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An applicative-do is supposed to take place in parallel, so
constraints bound in one arm can't possibly be available in another
(#13242).  Our current rule is this (more details and discussion
on the ticket). Consider

   ...stmts...
   ApplicativeStmts [arg1, arg2, ... argN]
   ...more stmts...

where argi :: ApplicativeArg. Each 'argi' itself contains one or more Stmts.
Now, we say that:

* Constraints required by the argi can be solved from
  constraint bound by ...stmts...

* Constraints and existentials bound by the argi are not available
  to solve constraints required either by argj (where i /= j),
  or by ...more stmts....

* Within the stmts of each 'argi' individually, however, constraints bound
  by earlier stmts can be used to solve later ones.

To achieve this, we just typecheck each 'argi' separately, bring all
the variables they bind into scope, and typecheck the thing_inside.

************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************
-}

-- | @checkArgCounts@ takes a @[RenamedMatch]@ and decides whether the same
-- number of /required/ (aka visible) args are used in each equation.
-- Returns the arity, the number of required args
-- E.g.  f @a True  y = ...
--       f    False z = ...
--       The MatchGroup for `f` has arity 2, not 3
checkArgCounts :: AnnoBody body
               => MatchGroup GhcRn (LocatedA (body GhcRn))
               -> TcM VisArity
checkArgCounts (MG { mg_alts = L _ [] })
    = return 1 -- See Note [Empty MatchGroups] in GHC.Rename.Bind
               --   case e of {} or \case {}
               -- Both have arity 1

checkArgCounts (MG { mg_alts = L _ (match1:matches) })
    | null matches  -- There was only one match; nothing to check
    = return n_args1

    -- Two or more matches: check that they agree on arity
    | Just bad_matches <- mb_bad_matches
    = failWithTc $ TcRnMatchesHaveDiffNumArgs (m_ctxt (unLoc match1))
                 $ MatchArgMatches match1 bad_matches
    | otherwise
    = return n_args1
  where
    n_args1 = reqd_args_in_match match1
    mb_bad_matches = NE.nonEmpty [m | m <- matches, reqd_args_in_match m /= n_args1]

    reqd_args_in_match :: LocatedA (Match GhcRn body1) -> VisArity
    -- Counts the number of /required/ (aka visible) args in the match
    reqd_args_in_match (L _ (Match { m_pats = L _ pats })) = count isVisArgLPat pats
