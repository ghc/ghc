{-# LANGUAGE RecordWildCards  #-}
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
   , tcCaseMatches
   , tcRetainedCase
   , tcLambdaMatches
   , tcGRHSNE
   , tcGRHSsPat
   , TcStmtChecker
   , TcExprStmtChecker
   , TcCmdStmtChecker
   , tcStmts
   , tcStmtsAndThen
   , tcDoStmts
   , tcBody
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

import GHC.Rename.Utils ( bindLocalNames )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
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
import GHC.Core ( Expr(Var) )
import GHC.Core.TyCo.Rep ( Type(DepFunTy, FunTy, TermVarTy) )
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Type ( coreFullView, isManyTy, splitTyConApp_maybe
                     , anyFreeVarsOfType, typeKind )
import GHC.Core.Coercion ( mkCoVarCo, mkCoCast, mkSymCo, mkSubCo, coercionKind, isReflCo, mkTransCo )
import GHC.Core.DataCon ( DataCon, dataConName, promoteDataCon )
import GHC.Core.ConLike ( ConLike(RealDataCon) )
import GHC.Core.Predicate ( mkNomEqPred )
import GHC.Core.Utils ( applyTypeToArgs )
-- Create chunkified tuple types for monad comprehensions
import GHC.Core.Make.BigTuple

import GHC.Hs

import GHC.Builtin.WiredIn.Types
import GHC.Builtin.WiredIn.Prim

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Name.Set
import GHC.Types.Id
import GHC.Types.Var ( EvVar, varType, isCoVar )
import GHC.Types.Var.Env ( mkInScopeSetList, unitVarEnv )
import GHC.Types.SrcLoc
import GHC.Types.Basic( VisArity, Origin(Generated), GenReason(OtherExpansion), DoPmc(SkipPmc) )
import GHC.Data.Bag ( listToBag, unionBags )
import GHC.Data.FastString ( fsLit )
import GHC.Data.Pair ( pSnd )
import GHC.Data.Maybe ( catMaybes, fromMaybe )

import qualified GHC.Data.List.NonEmpty as NE
import Data.Data ( Data, cast, gmapQ, gmapT )

import Control.Monad
import Control.Arrow ( second )
import Data.Foldable (toList)
import Data.List (zipWith4)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)

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
tcFunBindMatches ctxt fun_name mult matches invis_pat_tys exp_ty
  = assertPpr (funBindPrecondition matches) (pprMatches matches) $
    do  {  -- Check that they all have the same no of arguments
          arity <- checkArgCounts matches
        ; let herald = ExpectedFunTyMatches (NameThing fun_name) matches
              tc_matches pat_tys rhs_ty = tcScalingUsage mult $
                do { traceTc "tcFunBindMatches 2" $
                     vcat [ text "ctxt:" <+> pprUserTypeCtxt ctxt
                          , text "arity:" <+> ppr arity
                          , text "invis_pat_tys:" <+> ppr invis_pat_tys
                          , text "pat_tys:" <+> ppr pat_tys
                          , text "rhs_ty:" <+> ppr rhs_ty ]
                   ; tcMatches mctxt tcBody (invis_pat_tys ++ pat_tys) rhs_ty matches }
        ; traceTc "tcFunBindMatches 1" (ppr fun_name $$ ppr mult $$ ppr exp_ty $$ ppr arity)

        ; (wrap_fun, r)
             <- case exp_ty of
                  Check ty | hasRetainedFunTy ty
                    -> do { result <- tcScalingUsage mult $
                                        tcRetainedMatches herald ctxt mctxt arity invis_pat_tys ty matches
                          ; return (idHsWrapper, result) }
                  _ -> matchExpectedFunTys herald ctxt arity exp_ty tc_matches

        ; return (wrap_fun, r) }
  where
    mctxt  = mkPrefixFunRhs (noLocA fun_name) noAnn

-- | Check source patterns against an expected retained function telescope.
-- See Note [Checked retained lambdas].
tcRetainedMatches :: ExpectedFunTyCtxt -> UserTypeCtxt -> HsMatchContextRn
                  -> VisArity -> [ExpPatType] -> Type
                  -> MatchGroup GhcRn (LHsExpr GhcRn)
                  -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedMatches herald ctxt mctxt arity invis_pat_tys ty matches
  = case unLoc (mg_alts matches) of
      [L _ (Match { m_pats = L _ pats })]
        | not (first_constructor pats) ->
          tcRetainedVarMatches herald ctxt ty (filter isVisArgLPat pats) $ \pat_tys rhs_ty boundary ->
            tcMatches mctxt tcBody (invis_pat_tys ++ pat_tys) rhs_ty
              (case boundary of
                 Nothing -> matches
                 Just prefix -> splitRetainedContext prefix matches)
      _ | DepFunTy binder _ <- coreFullView ty ->
          do { index <- newSysLocalId (fsLit "retained") ManyTy (varType binder)
             ; let body_ty = applyTypeToArgs ty [Var index]
             ; (pat_tys, argument_plan, rhs_ty) <-
                 tcRetainedConArgs herald ctxt (arity - 1) body_ty
             ; tcRetainedConMatches index invis_pat_tys pat_tys argument_plan rhs_ty matches }
        | otherwise -> failWithTc $
            TcRnRetainedQuantificationError RetainedConstructorCoverage
  where
    first_constructor pats = case filter isVisArgLPat pats of
      pat : _ -> constructor_pat pat
      _ -> False
    constructor_pat (L _ (ParPat _ pat)) = constructor_pat pat
    constructor_pat (L _ ConPat{}) = True
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
-}

-- | Find a retained binder after any canonical ordinary arrows.
hasRetainedFunTy :: Type -> Bool
hasRetainedFunTy ty = case coreFullView ty of
  DepFunTy{} -> True
  FunTy _ _ _ result -> hasRetainedFunTy result
  _ -> False

-- | Elaborate a checked dependent case through a retained pattern function.
-- See Note [Source retained case refinement].
tcRetainedCase :: Id -> MatchGroup GhcRn (LHsExpr GhcRn) -> ExpRhoType
               -> TcM (Maybe (HsExpr GhcTc))
tcRetainedCase _ (MG { mg_alts = L _ [] }) _ = return Nothing
tcRetainedCase index matches@(MG { mg_alts = L loc alternatives }) (Check result_ty)
  = do { captures <- fmap catMaybes $ mapM capture (nameSetElemsStable (retainedCaseNames matches))
       ; givens <- filter (depends . varType) . tcl_native_givens . tcl_lcl_ctxt <$> getLclEnv
       ; if null captures && null givens && not (depends result_ty)
         then return Nothing
         else setSrcSpanA loc $
           do { when (retainedCaseHasQuote matches) $
                  failWithTc (TcRnRetainedQuantificationError RetainedCaseQuotation)
              ; unless (isLiftedTypeKind (typeKind result_ty)) $
                  failWithTc (TcRnRetainedQuantificationError (RetainedResultKind result_ty))
              ; constructors <- mapM constructor alternatives
              ; let domain_cons = case splitTyConApp_maybe (coreFullView (varType index)) of
                      Just (tc, []) -> tyConDataCons tc
                      _ -> []
                    cons = map fst constructors
              ; unless (cons `equalLength` domain_cons && all (`elem` cons) domain_cons) case_error
              ; fresh_name <- newSysName (nameOccName (idName index))
              ; let fresh_index = mkLocalId fresh_name ManyTy (varType index)
                    in_scope = mkInScopeSetList
                      (fresh_index : tyCoVarsOfTypesList (result_ty : map varType (index : givens ++ captures)))
                    subst = Subst.mkIdSubst in_scope (unitVarEnv index (Var fresh_index))
                    generalize = Subst.substTy subst
              ; dictionaries <- newEvVars (map (generalize . varType) givens)
              ; names <- mapM (newSysName . nameOccName . idName) captures
              ; let capture_names = zip (map idName captures) names
                    add_arguments (L match_loc match@(Match { m_pats = L pats_loc pats }), (con, fields))
                      = let extra = map (noLocA . VarPat noExtField . noLocA) names
                            rewritten = rewriteRetainedCase (idName index) con fields capture_names match
                        in L match_loc (rewritten { m_ctxt = LamAlt LamSingle
                                                  , m_pats = L pats_loc (pats ++ extra) })
                    matches' = matches { mg_alts = L loc (map add_arguments (zip alternatives constructors)) }
                    dictionary_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) dictionaries
                    capture_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . generalize . varType) captures
                    arg_tys = dictionary_tys ++ capture_tys
                    argument_plan = map RetainedConDict dictionaries
                                    ++ map (const RetainedConValue) captures
              ; checked <- tcRetainedConMatches fresh_index [] arg_tys argument_plan
                             (mkCheckExpType (generalize result_ty)) matches'
              ; mapM_ (tcEmitBindingUsage . singleUsageUE) captures
              ; let fun = HsLam noAnn LamSingle checked
                    index_arg = noLocA (HsVar noExtField (noLocA index))
                    with_evidence = mkHsWrap (mkWpEvVarApps givens)
                                      (HsApp noExtField (noLocA fun) index_arg)
                    arguments = map (noLocA . HsVar noExtField . noLocA) captures
              ; return (Just (foldl' (\f a -> HsApp noExtField (noLocA f) a) with_evidence arguments)) } }
  where
    depends = anyFreeVarsOfType (== index)
    capture name
      = do { thing <- tcLookupLcl_maybe name
           ; case thing of
               Just (ATcId { tct_id = var })
                 | isLocalId var, not (isCoVar var), depends (varType var) ->
                   do { unless (isManyTy (idMult var)) $
                          failWithTc (TcRnRetainedQuantificationError (RetainedPatternType (varType var)))
                      ; return (Just var) }
               _ -> return Nothing }
    constructor (L match_loc (Match { m_pats = L _ [pat]
                                   , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = setSrcSpanA match_loc $
        do { unless (case_pattern pat) case_error
           ; con <- tcRetainedCon (varType index) pat
           ; maybe case_error return con }
    constructor (L match_loc _) = setSrcSpanA match_loc case_error
    case_pattern (L _ (ParPat _ pat)) = case_pattern pat
    case_pattern (L _ ConPat { pat_args = PrefixCon _ fields })
      = all (\(L _ pat) -> case pat of VarPat{} -> True; _ -> False) fields
    case_pattern _ = False
    case_error :: TcM a
    case_error = failWithTc (TcRnRetainedQuantificationError RetainedCasePatterns)
tcRetainedCase _ _ _ = return Nothing

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
actual parameters. Put fresh evidence parameters before the captured values and
pass the original evidence with an evidence wrapper. This includes primitive
equality CoVars; these must not become source HsVar applications. Substitute into
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
Reject Template Haskell quotations before substitution. Quoted source Names
must not become the fresh generalized Names.

Complete unguarded constructor clauses preserve inspection of the scrutinee.
The generated application supplies every generalized argument. Wildcards and
defaults need a separate forcing rule; do not turn a strict case into an unused
function argument. General motive inference and dependent congruence remain
separate work. No equality comes from a local definition or an unfolding.
-}

-- | Introduce runtime index identities before checking the remaining patterns.
tcRetainedVarMatches :: ExpectedFunTyCtxt -> UserTypeCtxt -> Type
                     -> [LPat GhcRn]
                     -> ([ExpPatType] -> ExpRhoType -> Maybe Int -> TcM a) -> TcM a
tcRetainedVarMatches herald ctxt initial_ty initial_pats = go [] initial_ty initial_pats
  where
    go acc ty pats thing_inside
      | (theta, _) <- tcSplitPhiTy ty, not (null theta)
      = do { let prefix = length (filter isVisibleExpPatType acc)
           ; strict <- xoptM LangExt.Strict
           ; unless (null pats || (not strict && all nonforcing_pattern (take prefix initial_pats))) $
               failWithTc (TcRnRetainedQuantificationError RetainedContextPattern)
           ; thing_inside (reverse acc) (mkCheckExpType ty) (Just prefix) }
    go acc ty [] thing_inside = thing_inside (reverse acc) (mkCheckExpType ty) Nothing
    go acc ty (pat : pats) thing_inside
      | DepFunTy binder _ <- coreFullView ty
      = setSrcSpanA (getLoc pat) $
        do { name <- case unLoc pat of
               VarPat _ (L _ name) -> return name
               _ -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
           ; let index = mkLocalId name ManyTy (varType binder)
                 pat_ty = mkCheckExpFunPatTy (Scaled ManyTy (varType binder))
           ; go (pat_ty : acc) (applyTypeToArgs ty [Var index]) pats thing_inside }
      | FunTy{} <- coreFullView ty
      = do { (co, arg_ty, rhs_ty) <- matchActualFunTy herald Nothing (length initial_pats, initial_ty) ty
           ; unless (isIdHsWrapper (mkWpCastN co)) $
               failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
           ; go (mkCheckExpFunPatTy arg_ty : acc) rhs_ty pats thing_inside }
      | otherwise
      = do { (wrap, result) <- matchExpectedFunTys herald ctxt 1 (mkCheckExpType ty) $
               \pat_tys rhs_ty ->
                 do { rhs_ty <- readExpType rhs_ty
                    ; go (reverse pat_tys ++ acc) rhs_ty pats thing_inside }
           ; unless (isIdHsWrapper wrap) $
               failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
           ; return result }

    nonforcing_pattern (L _ pat) = case pat of
      VarPat{} -> True
      WildPat{} -> True
      LazyPat{} -> True
      ParPat _ inner -> nonforcing_pattern inner
      AsPat _ _ inner -> nonforcing_pattern inner
      SigPat _ inner _ -> nonforcing_pattern inner
      _ -> False

-- | Check a constrained suffix as an expression inside its retained prefix.
-- Keep its guards and local bindings inside the evidence lambdas.
splitRetainedContext :: Int -> MatchGroup GhcRn (LHsExpr GhcRn)
                     -> MatchGroup GhcRn (LHsExpr GhcRn)
splitRetainedContext prefix group@(MG { mg_alts = L loc [L match_loc match]
                                     , mg_ext = ext })
  = group { mg_alts = L loc [L match_loc outer] }
  where
    Match { m_pats = L pats_loc pats } = match
    (before, after) = split_prefix prefix pats
    inner = match { m_ctxt = LamAlt LamSingle, m_pats = L pats_loc after }
    lambda = L match_loc (HsLam noAnn LamSingle
               (MG { mg_alts = L loc [L match_loc inner], mg_ext = ext }))
    outer = match { m_pats = L pats_loc before
                  , m_grhss = unguardedGRHSs (locA match_loc) lambda noAnn }

    split_prefix 0 ps = ([], ps)
    split_prefix n (pat : ps)
      = let (before, after) = split_prefix (if isVisArgLPat pat then n-1 else n) ps
        in (pat : before, after)
    split_prefix n [] = pprPanic "splitRetainedContext: missing prefix" (ppr n)
splitRetainedContext _ _ = panic "splitRetainedContext: multiple equations"

-- | Distinguish transported payloads from shared evidence and runtime indices.
data RetainedConArg
  = RetainedConValue
  | RetainedConDict EvVar
  | RetainedConIndex Id

-- | Collect shared binders in telescope order without counting evidence as values.
tcRetainedConArgs :: ExpectedFunTyCtxt -> UserTypeCtxt -> Int -> Type
                  -> TcM ([ExpPatType], [RetainedConArg], ExpRhoType)
tcRetainedConArgs herald ctxt = go [] []
  where
    go acc argument_plan n ty
      | (theta, tau) <- tcSplitPhiTy ty, not (null theta)
      = do { givens <- newEvVars theta
           ; let pat_tys = map (mkCheckExpFunPatTy . Scaled ManyTy . varType) givens
           ; go (reverse pat_tys ++ acc)
                (reverse (map RetainedConDict givens) ++ argument_plan) n tau }
      | n == 0
      = return (reverse acc, reverse argument_plan, mkCheckExpType ty)
      | DepFunTy binder _ <- coreFullView ty
      = do { name <- newSysName (nameOccName (idName binder))
           ; let index = mkLocalId name ManyTy (varType binder)
                 pat_ty = mkCheckExpFunPatTy (Scaled ManyTy (varType binder))
           ; go (pat_ty : acc) (RetainedConIndex index : argument_plan) (n-1)
                (applyTypeToArgs ty [Var index]) }
      | FunTy{} <- coreFullView ty
      = do { (co, arg_ty, rhs_ty) <- matchActualFunTy herald Nothing (n, ty) ty
           ; unless (isIdHsWrapper (mkWpCastN co)) $
               failWithTc (TcRnRetainedQuantificationError RetainedResultWrapper)
           ; go (mkCheckExpFunPatTy arg_ty : acc) (RetainedConValue : argument_plan) (n-1) rhs_ty }
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
Instantiate its codomain before reading later argument types. Check the complete
pattern list only after these identities have been chosen. tcPatBndr and
selectMatchVar preserve their Unique values. Outer erased forall binders keep
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

-- | Select a later retained constructor column without counting dictionaries.
retainedMatrixColumn :: [RetainedConArg]
                     -> MatchGroup GhcRn (LHsExpr GhcRn) -> Maybe (Int, Id)
retainedMatrixColumn plan (MG { mg_alts = L _ rows })
  = case [(column, shared)
         | L _ (Match { m_pats = L _ pats }) <- rows
         , ((column, RetainedConIndex shared), pat) <-
             zip (filter source_argument (zip [0..] plan))
                 (drop 1 (filter isVisArgLPat pats))
         , constructor_pattern pat] of
      first : _ -> Just first
      [] -> Nothing
  where
    source_argument (_, RetainedConDict _) = False
    source_argument _ = True
    constructor_pattern :: LPat GhcRn -> Bool
    constructor_pattern (L _ (ParPat _ pat)) = constructor_pattern pat
    constructor_pattern (L _ ConPat{}) = True
    constructor_pattern _ = False

-- | Check a complete two-index constructor matrix with successive row proofs.
-- See Note [Retained constructor matrices].
tcRetainedMatrixMatches :: Id -> (Int, Id) -> [ExpPatType] -> [ExpPatType]
                       -> [RetainedConArg] -> ExpRhoType
                       -> MatchGroup GhcRn (LHsExpr GhcRn)
                       -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedMatrixMatches first_index (second_column, second_index) invis_pat_tys
                       pat_tys plan rhs_ty
                       (MG { mg_alts = L loc matches, mg_ext = (origin, _) })
  = setSrcSpanA loc $
    do { when (any dictionary plan) $
           failWithTc (TcRnRetainedQuantificationError RetainedMatrixContext)
       ; rows <- mapM constructors matches
       ; _ <- foldM unique_pair [] (zip rows matches)
       ; let pairs = [(fst first, fst second) | (first, second) <- rows]
             expected = [(first, second) | first <- domain_cons first_index
                                         , second <- domain_cons second_index]
       ; unless (pairs `equalLength` expected && all (`elem` pairs) expected) coverage_error
       ; arg_tys <- mapM argument_type pat_tys
       ; result_ty <- readExpType rhs_ty
       ; (usages, checked) <- mapAndUnzipM
           (\(row, match) -> tcCollectingUsage (check_row row arg_tys result_ty match))
           (zip rows matches)
       ; tcEmitBindingUsage (supUEs usages)
       ; return (MG { mg_alts = L loc checked
                    , mg_ext = MatchGroupTc (Scaled ManyTy (varType first_index) : arg_tys)
                                result_ty origin }) }
  where
    dictionary RetainedConDict{} = True
    dictionary _ = False
    domain_cons index = case splitTyConApp_maybe (coreFullView (varType index)) of
      Just (tc, []) -> tyConDataCons tc
      _ -> []
    coverage_error = failWithTc (TcRnRetainedQuantificationError RetainedMatrixCoverage)
    pattern_error = failWithTc (TcRnRetainedQuantificationError RetainedMatrixPattern)
    argument_error = failWithTc (TcRnRetainedQuantificationError RetainedConstructorArguments)

    row_patterns (Match { m_pats = L _ pats })
      = case filter isVisArgLPat pats of
          first : later -> case drop second_column later of
            second : _ -> return (first, second)
            _ -> pattern_error
          _ -> pattern_error

    constructors (L match_loc match@(Match
      { m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = setSrcSpanA match_loc $
        do { (first, second) <- row_patterns match
           ; one <- resolve first_index first
           ; two <- resolve second_index second
           ; return (one, two) }
    constructors (L match_loc _) = setSrcSpanA match_loc coverage_error

    resolve index pat = setSrcSpanA (getLoc pat) $
      do { con <- tcRetainedCon (varType index) pat
         ; maybe pattern_error return con }

    unique_pair seen ((first, second), L match_loc match)
      = setSrcSpanA match_loc $
        do { let pair = (fst first, fst second)
           ; when (pair `elem` seen) $
               do { (_, pat) <- row_patterns match
                  ; setSrcSpanA (getLoc pat) $
                      failWithTc (TcRnRetainedQuantificationError RetainedMatrixDuplicate) }
           ; return (pair : seen) }

    argument_type (ExpFunPatTy ty)
      = do { scaled@(Scaled mult _) <- readScaledExpType ty
           ; unless (isManyTy mult) argument_error
           ; return scaled }
    argument_type _ = argument_error

    check_row ((first_con, first_fields), (second_con, second_fields))
              arg_tys result_ty (L match_loc match@(Match { m_pats = L _ pats }))
      = setSrcSpanA match_loc $
        do { names <- check_later 0 plan (drop 1 (filter isVisArgLPat pats))
           ; unless (null names) $
               case retainedMatchQuotation match of
                 Nothing -> return ()
                 Just span -> setSrcSpan span $
                   failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
           ; first_cv <- newEvVar (mkNomEqPred (TermVarTy first_index)
                           (mkTyConApp (promoteDataCon first_con) (map TermVarTy first_fields)))
           ; second_cv <- newEvVar (mkNomEqPred (TermVarTy second_index)
                            (mkTyConApp (promoteDataCon second_con) (map TermVarTy second_fields)))
           ; let transport ty = do
                   one <- tcRetainedCoercion first_index (mkCoVarCo first_cv) ty
                   two <- tcRetainedCoercion second_index (mkCoVarCo second_cv) (pSnd (coercionKind one))
                   return (mkTransCo one two)
           ; arg_cos <- mapM (transport . scaledThing) arg_tys
           ; result_co <- transport result_ty
           ; sequence_ [unless (isReflCo co) $
                          failWithTc (TcRnRetainedQuantificationError (RetainedPatternType ty))
                       | (RetainedConIndex _, co, Scaled _ ty) <- zip3 plan arg_cos arg_tys]
           ; let refined_args = zipWith3 (\argument (Scaled mult ty) co ->
                                   mkCheckExpFunPatTy (Scaled mult (case argument of
                                     RetainedConValue -> pSnd (coercionKind co)
                                     _ -> ty))) plan arg_tys arg_cos
                 first_arg = mkCheckExpFunPatTy (Scaled ManyTy (varType first_index))
                 renamed = renameRetainedMatch names match
           ; L checked_loc checked <- tcMatchWithSource tcBody
                 (invis_pat_tys ++ first_arg : refined_args)
                 (mkCheckExpType (pSnd (coercionKind result_co))) match (L match_loc renamed)
           ; let Match { m_pats = L pats_loc checked_pats, m_grhss = grhss } = checked
                 wrap_index index cv (L pat_loc pat)
                   = L pat_loc (AsPat noExtField (noLocA index)
                       (L pat_loc (addRetainedConProof cv emptyTcEvBinds pat)))
                 wrap_later column argument co (Scaled _ ty) pat@(L pat_loc inner)
                   | column == second_column = wrap_index second_index second_cv pat
                   | RetainedConValue <- argument = L pat_loc (XPat (CoPat (mkWpCastN co) inner ty))
                   | otherwise = pat
                 checked_pats' = case checked_pats of
                   first : rest -> wrap_index first_index first_cv first :
                     zipWith4 (\(column, argument) co ty pat -> wrap_later column argument co ty pat)
                       (zip [0..] plan) arg_cos arg_tys rest
                   [] -> panic "tcRetainedMatrixMatches: missing first pattern"
                 cast_rhs (L rhs_loc (GRHS x guards rhs))
                   = L rhs_loc (GRHS x guards (mkLHsWrapCo (mkSymCo result_co) rhs))
           ; return (L checked_loc (checked { m_pats = L pats_loc checked_pats'
                       , m_grhss = grhss { grhssGRHSs = fmap cast_rhs (grhssGRHSs grhss) } })) }

    check_later _ [] [] = return []
    check_later column (argument : args) (pat@(L pat_loc inner) : pats)
      | column == second_column = check_later (column+1) args pats
      | RetainedConIndex shared <- argument
      = setSrcSpanA pat_loc $
        do { name <- case inner of
               VarPat _ (L _ name) -> return name
               _ -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
           ; names <- check_later (column+1) args pats
           ; return ((name, idName shared) : names) }
      | otherwise
      = do { case unLoc pat of
               VarPat{} -> return ()
               _ -> setSrcSpanA pat_loc argument_error
           ; check_later (column+1) args pats }
    check_later _ _ _ = argument_error

{- Note [Retained constructor matrices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A complete two-index matrix needs one proof for each matched index in each row.
Keep both shared whole-argument Ids. Resolve their constructor patterns with the
actual source field Ids. Lift the first nominal constructor equality through a
payload or result type, then lift the second equality through that result.
Compose the two coercions. Check the specialised RHS and cast it back to the
original result. No equality follows from an alias or a shared source spelling.

Attach each proof to its own ConPat, inside an outer AsPat for its whole Id.
Keep the original MatchGroupTc types and all source argument lambdas. Ordinary
payload CoPats preserve their outer types. A payload before the second index
cannot mention that binder; its second transport is reflexive. Retained domains
must have reflexive transport. Contexts in this initial matrix telescope reject:
combined dictionary bindings would need the scope of both constructor proofs.

Check unique complete constructor pairs, not the first column alone. Reject
mixed variable/constructor rows and keep bare subfields. Ordinary grouping of
repeated constructors would alias distinct native fields and proofs. Desugar
proof-bearing rows separately as described in GHC.HsToCore.Match.Constructor,
Note [Retained constructor equation scope]. This preserves row failure and
left-before-right demand. Agda supplies the constructor substitution and split
tree precedents in docs/dependent-haskell/REFERENCES.md#two-index-constructor-equations.
-}

-- | Resolve a constructor and its actual closed runtime field identities.

tcRetainedConMatches :: Id -> [ExpPatType] -> [ExpPatType] -> [RetainedConArg] -> ExpRhoType
                    -> MatchGroup GhcRn (LHsExpr GhcRn)
                    -> TcM (MatchGroup GhcTc (LHsExpr GhcTc))
tcRetainedConMatches index invis_pat_tys pat_tys argument_plan rhs_ty group
  | Just second <- retainedMatrixColumn argument_plan group
  = tcRetainedMatrixMatches index second invis_pat_tys pat_tys argument_plan rhs_ty group
tcRetainedConMatches index invis_pat_tys pat_tys argument_plan rhs_ty
                    (MG { mg_alts = L loc matches, mg_ext = (origin, _) })
  = setSrcSpanA loc $
    do { constructors <- mapM constructor matches
       ; let cons = map fst constructors
             domain_cons = case splitTyConApp_maybe (coreFullView (varType index)) of
               Just (tc, []) -> tyConDataCons tc
               _ -> []
       ; unless (cons `equalLength` domain_cons && all (`elem` cons) domain_cons) $
           failWithTc (TcRnRetainedQuantificationError RetainedConstructorCoverage)
       ; arg_tys <- mapM argument_type pat_tys
       ; result_ty <- readExpType rhs_ty
       ; (usages, checked) <- mapAndUnzipM
           (\(con, match) -> tcCollectingUsage (check_match con arg_tys result_ty match))
           (zip constructors matches)
       ; tcEmitBindingUsage (supUEs usages)
       ; if null matches
         then empty_matches arg_tys result_ty
         else return (MG { mg_alts = L loc checked
                         , mg_ext = MatchGroupTc (Scaled ManyTy (varType index) : arg_tys) result_ty origin }) }
  where
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

    constructor (L match_loc (Match { m_pats = L _ pats
                                   , m_grhss = GRHSs { grhssGRHSs = L _ (GRHS _ [] _) :| [] } }))
      = setSrcSpanA match_loc $
        case filter isVisArgLPat pats of
          pat : _ -> do { mb_con <- tcRetainedCon (varType index) pat
                        ; maybe coverage_error return mb_con }
          _ -> coverage_error
    constructor (L match_loc _) = setSrcSpanA match_loc coverage_error

    coverage_error = failWithTc (TcRnRetainedQuantificationError RetainedConstructorCoverage)
    argument_error = failWithTc (TcRnRetainedQuantificationError RetainedConstructorArguments)

    argument_type (ExpFunPatTy ty)
      = do { scaled@(Scaled mult _) <- readScaledExpType ty
           ; unless (isManyTy mult) argument_error
           ; return scaled }
    argument_type _ = argument_error

    check_match (con, fields) arg_tys result_ty (L match_loc match@(Match { m_pats = L _ pats }))
      = setSrcSpanA match_loc $
        do { names <- retained_names argument_plan (drop 1 (filter isVisArgLPat pats))
           ; unless (null names) $
               case retainedMatchQuotation match of
                 Nothing -> return ()
                 Just span -> setSrcSpan span $
                   failWithTc (TcRnRetainedQuantificationError RetainedConstructorQuotation)
           ; mapM_ (\pat -> unless (bare_variable pat) $
                        setSrcSpanA (getLoc pat) argument_error)
               (drop 1 (filter isVisArgLPat pats))
           ; let constructor_index = mkTyConApp (promoteDataCon con) (map TermVarTy fields)
           ; cv <- newEvVar (mkNomEqPred (TermVarTy index) constructor_index)
           ; arg_cos <- mapM (tcRetainedCoercion index (mkCoVarCo cv) . scaledThing) arg_tys
           ; result_co <- tcRetainedCoercion index (mkCoVarCo cv) result_ty
           ; sequence_ [unless (isReflCo co) $
                          failWithTc (TcRnRetainedQuantificationError (RetainedPatternType ty))
                       | (RetainedConIndex _, co, Scaled _ ty) <- zip3 argument_plan arg_cos arg_tys]
           ; let dictionary_cos = [(dict, co) | (RetainedConDict dict, co) <- zip argument_plan arg_cos]
           ; refined_givens <- newEvVars (map (pSnd . coercionKind . snd) dictionary_cos)
           ; let refined_args = zipWith3 (\dictionary (Scaled mult ty) co ->
                                   mkCheckExpFunPatTy (Scaled mult (case dictionary of
                                     RetainedConValue -> pSnd (coercionKind co)
                                     _ -> ty)))
                                 argument_plan arg_tys arg_cos
                 index_arg = mkCheckExpFunPatTy (Scaled ManyTy (varType index))
                 renamed = renameRetainedMatch names match
                 match' = renamed { m_pats = fmap (insert_dictionaries argument_plan) (m_pats renamed) }
                 check = tcMatchWithSource tcBody (invis_pat_tys ++ index_arg : refined_args)
                           (mkCheckExpType (pSnd (coercionKind result_co))) match (L match_loc match')
           ; (ev_binds, L checked_loc checked) <- if null refined_givens
               then do { checked <- check; return (emptyTcEvBinds, checked) }
               else checkConstraints (PatSkol (RealDataCon con) (m_ctxt match)) [] refined_givens check
           ; let given_binds = zipWith (\given (dict, co) ->
                                   mkGivenEvBind given
                                     (if isCoVar dict
                                      then evCoercion (mkCoCast (mkCoVarCo dict) (mkSubCo co))
                                      else evCast (Var dict) (mkSubCo co)))
                                 refined_givens dictionary_cos
           ; branch_binds <- case ev_binds of
               TcEvBinds binds -> do { mapM_ (addTcEvBind binds) given_binds
                                     ; return ev_binds }
               EvBinds binds -> return (EvBinds (listToBag given_binds `unionBags` binds))
           ; let Match { m_pats = L pats_loc checked_pats, m_grhss = grhss } = checked
                 checked_pats' = case checked_pats of
                   L first_loc first : rest ->
                     L first_loc (AsPat noExtField (noLocA index)
                                    (L first_loc (addRetainedConProof cv branch_binds first)))
                     : zipWith4 (\dictionary co (Scaled _ ty) (L pat_loc pat) ->
                                   case dictionary of
                                     RetainedConValue -> L pat_loc (XPat (CoPat (mkWpCastN co) pat ty))
                                     _ -> L pat_loc pat)
                                argument_plan arg_cos arg_tys rest
                   [] -> panic "tcRetainedConMatches: no constructor pattern"
                 cast_rhs (L rhs_loc (GRHS x guards rhs)) =
                   L rhs_loc (GRHS x guards (mkLHsWrapCo (mkSymCo result_co) rhs))
                 grhss' = grhss { grhssGRHSs = fmap cast_rhs (grhssGRHSs grhss) }
           ; return (L checked_loc (checked { m_pats = L pats_loc checked_pats'
                                          , m_grhss = grhss' })) }

    insert_dictionaries ds = before_index
      where
        before_index (pat : pats)
          | isVisArgLPat pat = pat : insert ds pats
          | otherwise = pat : before_index pats
        before_index [] = panic "tcRetainedConMatches: missing source index"

        insert [] pats = pats
        insert (RetainedConDict dict : rest) pats
          = noLocA (VarPat noExtField (noLocA (idName dict))) : insert rest pats
        insert ds@(_ : rest) (pat : pats)
          | isVisArgLPat pat = pat : insert rest pats
          | otherwise = pat : insert ds pats
        insert _ [] = panic "tcRetainedConMatches: missing source argument"

    -- Match shared retained positions to the row's resolved variable Names.
    retained_names :: [RetainedConArg] -> [LPat GhcRn] -> TcM [(Name, Name)]
    retained_names [] [] = return []
    retained_names (RetainedConDict _ : args) pats = retained_names args pats
    retained_names (RetainedConIndex shared : args) (L pat_loc pat : pats)
      = setSrcSpanA pat_loc $
        do { name <- case pat of
               VarPat _ (L _ name) -> return name
               _ -> failWithTc (TcRnRetainedQuantificationError RetainedVariablePattern)
           ; names <- retained_names args pats
           ; return ((name, idName shared) : names) }
    retained_names (RetainedConValue : args) (_ : pats) = retained_names args pats
    retained_names _ _ = argument_error

    bare_variable (L _ VarPat{}) = True
    bare_variable _ = False

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
Later arguments currently require bare variable patterns and Many multiplicity.

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
Its closed domain must have reflexive constructor transport. Only dictionary
positions get specialised Givens; ordinary payloads keep their existing CoPats.
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
                   -> do { result <- tcRetainedMatches herald GenSigCtxt ctxt arity
                                       invis_pat_tys ty matches
                         ; return (idHsWrapper, result) }
                 _ -> matchExpectedFunTys herald GenSigCtxt arity res_ty $ \ pat_tys rhs_ty ->
                        tcMatches ctxt tcBody (invis_pat_tys ++ pat_tys) rhs_ty matches

        ; return (wrapper, r) }
  where
    ctxt   = LamAlt lam_variant
    herald = ExpectedFunTyLam lam_variant e
             -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify

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

tcMatches ctxt tc_body pat_tys exp_ty (MG { mg_alts = L l matches
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
           [ExpForAllPatTy tvb] -> failWithTc $ TcRnEmptyCase ctxt (EmptyCaseForall tvb)
           []                   -> panic "tcMatches: no arguments in EmptyCase"
           _t1:(_t2:_ts)        -> panic "tcMatches: multiple arguments in EmptyCase"
       ; rhs_ty <- expTypeToType exp_ty
       ; return (MG { mg_alts = L l []
                    , mg_ext = MatchGroupTc [pat_ty] rhs_ty origin
                    }) }

  | otherwise
  = do { let exp_ty' = adjustExpTypeForCaseBranches exp_ty matches
             tc_match match = tcCollectingUsage $
                              tcMatch tc_body pat_tys exp_ty' match
       ; (usages, matches') <- mapAndUnzipM tc_match matches
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
tcMatch :: (AnnoBody body)
        => TcMatchAltChecker body
        -> [ExpPatType]          -- Expected pattern types
        -> ExpRhoType            -- Expected result-type of the Match.
        -> LMatch GhcRn (LocatedA (body GhcRn))
        -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))

tcMatch tc_body pat_tys rhs_ty match
  = tcMatchWithSource tc_body pat_tys rhs_ty (unLoc match) match

-- | Check a rewritten equation with its original source in the error context.
tcMatchWithSource :: (AnnoBody body)
                  => TcMatchAltChecker body
                  -> [ExpPatType]
                  -> ExpRhoType
                  -> Match GhcRn (LocatedA (body GhcRn))
                  -> LMatch GhcRn (LocatedA (body GhcRn))
                  -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))
tcMatchWithSource tc_body pat_tys rhs_ty source_match match
  = do { (L loc r) <- wrapLocMA (tc_match pat_tys rhs_ty) match
       ; return (L loc r) }
  where
    tc_match pat_tys rhs_ty
             (Match { m_ctxt = ctxt, m_pats = L l pats, m_grhss = grhss })
      = add_match_ctxt $
        do { (pats', (grhss')) <- tcMatchPats ctxt pats pat_tys $
                                  tcGRHSs ctxt tc_body grhss rhs_ty
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
                          Just (_,e_ty) -> \ty -> (alphaTy `mkVisFunTyMany` e_ty) `mkVisFunTyMany` ty

             tup_ty        = mkBigCoreVarTupTy bndr_ids
             poly_arg_ty   = m_app alphaTy
             poly_res_ty   = m_app (n_app alphaTy)
             using_poly_ty = mkInfForAllTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkVisFunTyMany` poly_res_ty

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
                          Just {} -> \res -> (alphaTy `mkVisFunTyMany` by_e_ty) `mkVisFunTyMany` res

             poly_arg_ty  = m1_ty `mkAppTy` alphaTy
             using_arg_ty = m1_ty `mkAppTy` tup_ty
             poly_res_ty  = m2_ty `mkAppTy` n_app alphaTy
             using_res_ty = m2_ty `mkAppTy` n_app tup_ty
             using_poly_ty = mkInfForAllTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkVisFunTyMany` poly_res_ty

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
                             , synKnownType (n_app tup_ty `mkVisFunTyMany` new_res_ty) ]
                             res_ty $ \ _ _ -> return ()

       --------------- Typecheck the 'fmap' function -------------
       ; fmap_op' <- case form of
                       ThenForm -> return noExpr
                       _ -> fmap unLoc . tcCheckPolyExpr (noLocA fmap_op) $
                            mkInfForAllTy alphaTyVar $
                            mkInfForAllTy betaTyVar  $
                            (alphaTy `mkVisFunTyMany` betaTy)
                            `mkVisFunTyMany` (n_app alphaTy)
                            `mkVisFunTyMany` (n_app betaTy)

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

       ; let mzip_ty  = mkInfForAllTys [alphaTyVar, betaTyVar] $
                        (m_ty `mkAppTy` alphaTy)
                        `mkVisFunTyMany`
                        (m_ty `mkAppTy` betaTy)
                        `mkVisFunTyMany`
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
                          [synKnownType (mkVisFunTyMany tup_ty stmts_ty)] exp_ty $
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
      ; let fun_ty = mkVisFunTysMany pat_tys body_ty

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
