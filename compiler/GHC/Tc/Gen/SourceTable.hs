-- | Closed type families for checked functions that match one constructor.
module GHC.Tc.Gen.SourceTable (sourceTableTyCons) where

import GHC.Prelude

import GHC.Builtin.WiredIn.Types (listTyCon, mkListTy, unmatchableDataConTy)
import GHC.Core.Coercion.Axiom (ClosedTyFam(..), CoAxBranch)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.Bag (isEmptyBag)
import GHC.Hs
import qualified GHC.LanguageExtensions as LangExt
import GHC.Tc.TyCl.Build (newImplicitBinder)
import GHC.Tc.Types.Evidence (HsWrapper(..), TcEvBinds(..), isEmptyEvBindsMap)
import GHC.Tc.Utils.Monad (TcM, getTcEvBindsMap, liftZonkM, newSysName, xoptM)
import GHC.Tc.Utils.TcType (isTyFamFree)
import GHC.Tc.Zonk.TcType (zonkTcType, zonkTcTypes)
import GHC.Types.Basic (Origin(..))
import GHC.Types.Name (Name, isExternalName, mkSourceTableOcc, mkInstTyCoOcc,
                       mkTyVarOcc, nameOccName, setOccNameSpace, tvName)
import GHC.Types.SrcLoc
import GHC.Types.Unique.Set (mkUniqSet)
import GHC.Types.Var (Id, VarBndr(..), mkTyVar, varName)
import GHC.Utils.Monad (allM)

import Control.Monad (guard, zipWithM)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, isNothing, mapMaybe)

{- Note [Closed source tables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a definition such as

  flipBit :: Bool -> Bool
  flipBit False = True
  flipBit True  = False

make one closed family with the corresponding two equations. The left
and right sides refer to the promoted DataCons used in the checked
clauses. As in singletons-th, a family axiom justifies each reduction.
An application to an unknown input stays stuck. The term binding keeps
its ordinary Haskell pattern matching.

Each row matches one outer constructor of an ordinary lifted datatype.
For example, the clause previous (S n) = n contributes the equation
T:previous ('S n) = n. As in singletons-th, give each pattern field a
fresh type variable in that equation. Use the checked Id to find the
variable for a returned field. A wildcard gets a fresh variable too.
Matching a lazy field with a variable or wildcard leaves it unevaluated.
A strict field or a nested pattern would require another demand check.

Check the domain for complete, disjoint coverage before creating the
axiom. A row can return one of its fields or apply a result constructor.
It can also call the same function on one field of the input type. For
example, listCount (_ : xs) = S (listCount xs) checks because xs is a
proper subterm of the matched cons. Use the checked Ids for both the
function and the field. A call on a rebuilt constructor does not pass
this test. This is the direct structural decrease used in Agda.

Instantiate the input constructor at the parameters of the checked
domain. For listCount :: [Bool] -> Nat, the promoted cons takes Bool as
its first argument. Its field variables have kinds Bool and [Bool].
These constructor arguments occur in the family pattern. For example,
the promoted nil at [F ()] contains F () even though it has no fields.
Apply the same family-free check as Core Lint to the completed pattern.
The kind of a field variable may mention a family.
Check each RHS before tying the family to its axiom. A successful branch
builder only supplies the family TyCon at verified recursive calls.
Each branch keeps the source span of its equation.

For sourceLength :: forall a. [a] -> Nat, tcPolyCheck binds the body
skolem in a WpTyLam on the FunBind. The variable in the public type
can have a different Unique. Check the public list type with its own
binder. Check the constructor instantiations and the recursive type argument
with the body skolem. An inferred application carries a WpTyApp. A written
application carries the checked type in HsAppType. Give the family and each
branch fresh Core binders for the element type. In the branch LHS and the
recursive application, this element type precedes the list argument.

A declaration splice can introduce a type that uses a function from an
earlier group. At this boundary, simplifyTop has solved the constraints
for the group. Read the evidence bindings and follow the solved type of
each candidate. The final module zonker can run typed splices and default
metavariables, so do not run it on the group's terms here.

Note [The second argument of a source equation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The equations for plus can inspect their first argument and carry a
second argument unchanged:

  plus Z     m = m
  plus (S n) m = S (plus n m)

As in Agda's definition of addition, only the first argument decreases.
Compare the second operand of a recursive call with the row's second
pattern Id. For the first operand, look up the Id among the fields of
the matched constructor. When both arguments have type Nat, a call
such as plus m m would otherwise pass the type check without decreasing.

Represent the second variable by a fresh nominal binder in each axiom
branch. The equation can return this variable or put it in a constructor.
With Strict, desugaring adds a bang to the second variable pattern after
typechecking. Such a function must force its second argument even to
return an outer successor. Skip binary equations in that case.
-}

-- | Derive closed families after solving the group's top-level constraints.
sourceTableTyCons :: LHsBinds GhcTc -> TcM [TyCon]
sourceTableTyCons binds = do
    strict <- xoptM LangExt.Strict
    catMaybes <$> mapM (certify strict) (mapMaybe (sourceRows . unLoc) binds)
  where
    certify strict (fun, body_tv, evidence, rows) = do
      clean <- allM emptyEvidence evidence
      if clean then do
        ty <- liftZonkM (zonkTcType (varType fun))
        case checkedDomains strict body_tv ty rows of
          Nothing -> pure Nothing
          Just (input, carried, result) -> do
            branches <- traverse (sourceBranch input carried result) rows
            traverse (makeFamily (varName fun) input carried result) (sequence branches)
      else pure Nothing

-- | The first constructor pattern, any carried variable, and the checked RHS.
data SourceRow = SourceRow SrcSpan DataCon [Type] [Maybe Id] [Id] SourceRhs

-- | The expression forms allowed in one source equation.
data SourceRhs = Field Id | Construct DataCon [SourceRhs] | Recur [Type] Id

-- | The input type of a closed table, or the body skolem of a polymorphic list table.
data SourceInput = ClosedInput Type [Type] | PolyList TyVar

-- | Collect the constructors and evidence for a candidate source function.
sourceRows :: HsBind GhcTc -> Maybe (Id, Maybe TyVar, [TcEvBinds], [SourceRow])
sourceRows binding = do
    (fun, mono, body_tv, MG { mg_ext = MatchGroupTc { mg_origin = FromSource },
               mg_alts = L _ matches }, evidence) <- case binding of
      XHsBindsLR (AbsBinds { abs_tvs = [], abs_ev_vars = [], abs_ev_binds = evs
                          , abs_exports = [ABE { abe_poly = fun, abe_mono = mono
                                              , abe_wrap = WpHole }]
                          , abs_binds = [L _ body], abs_sig = has_sig }) -> do
        (bound, body_tv, matches) <- funBind body
        guard (bound == mono)
        guard (isNothing body_tv || has_sig)
        pure (fun, mono, body_tv, matches, evs)
      _ -> do
        (fun, Nothing, matches) <- funBind binding
        pure (fun, fun, Nothing, matches, [])
    guard (isExternalName (varName fun))
    let self v = v == fun || v == mono
    (row_evidence, rows) <- unzip <$> traverse (sourceRow self) matches
    pure (fun, body_tv, evidence ++ row_evidence, rows)
  where
    funBind :: HsBind GhcTc -> Maybe (Id, Maybe TyVar, MatchGroup GhcTc (LHsExpr GhcTc))
    funBind (FunBind { fun_ext = (wrap, []), fun_id = L _ f, fun_matches = mg })
      = case wrap of
          WpHole -> Just (f, Nothing, mg)
          WpTyLam tv | isTyVar tv, tcIsLiftedTypeKind (tyVarKind tv) -> Just (f, Just tv, mg)
          _ -> Nothing
    funBind _ = Nothing

-- | Check that the rows cover the domain of the solved function type.
checkedDomains :: Bool -> Maybe TyVar -> Type -> [SourceRow] -> Maybe (SourceInput, [Type], TyCon)
checkedDomains strict body_tv ty rows = do
    (element, tau) <- case body_tv of
      Nothing -> pure (Nothing, ty)
      Just sk -> do
        (ma, Bndr tv (Invisible SpecifiedSpec), tau) <- splitForAllForAllTyBinder_maybe ty
        guard (ma `eqType` unmatchableDataConTy && isTyVar tv && tcIsLiftedTypeKind (tyVarKind tv))
        pure (Just (tv, sk), tau)
    (FTF_T_T, ma, mult, arg_ty, res_ty) <- splitFunTy_maybe tau
    guard (ma `eqType` unmatchableDataConTy && isManyTy mult)
    (domain, input) <- case element of
      Nothing -> do
        (tc, params) <- ordinaryDataType arg_ty
        pure (tc, ClosedInput arg_ty params)
      Just (tv, sk) -> do
        guard (arg_ty `eqType` mkListTy (mkTyVarTy tv))
        pure (listTyCon, PolyList sk)
    (carried, final_ty) <- case splitFunTy_maybe res_ty of
      Nothing -> pure ([], res_ty)
      Just (FTF_T_T, second_ma, second_mult, second_ty, final_ty)
        | ClosedInput{} <- input
        , not strict, second_ma `eqType` unmatchableDataConTy, isManyTy second_mult -> do
            _ <- ordinaryDataType second_ty
            pure ([second_ty], final_ty)
      _ -> Nothing
    (result, []) <- ordinaryDataType final_ty
    let expected = tyConDataCons domain
        actual   = [con | SourceRow _ con _ _ _ _ <- rows]
    guard (length actual == length expected && mkUniqSet actual == mkUniqSet expected)
    pure (input, carried, result)

-- | Recognise a closed instance of a datatype with ordinary lifted parameters.
ordinaryDataType :: Type -> Maybe (TyCon, [Type])
ordinaryDataType ty
  | Just (tc, args) <- splitTyConApp_maybe ty
  , isVanillaAlgTyCon tc, isBoxedDataTyCon tc, isLiftedAlgTyCon tc
  , length args == tyConArity tc, null (tyConStupidTheta tc)
  , all (tcIsLiftedTypeKind . tyVarKind) (tyConTyVars tc), all ordinaryField args
  , let cons = tyConDataCons tc
  , (null args && isEnumerationTyCon tc) ||
    (not (isGadtSyntaxTyCon tc) && not (null cons) && all (ordinaryCon args) cons)
  = Just (tc, args)
  | otherwise = Nothing
  where
    ordinaryCon args con =
      null (dataConRepArgBinders con) && isVanillaDataCon con &&
      length args == length (dataConUnivTyVars con) &&
      dataConUserTyVars con == dataConUnivTyVars con &&
      isNothing (dataConWrapId_maybe con) &&
      not (any isBanged (dataConImplBangs con)) &&
      all (ordinaryField . scaledThing) (dataConInstOrigArgTys con args)

    ordinaryField field =
      tcIsLiftedTypeKind (typeKind field) && noFreeVarsOfType field && isTauTy field

-- | Read one unguarded source equation with no local bindings.
sourceRow :: (Id -> Bool) -> LMatch GhcTc (LHsExpr GhcTc) -> Maybe (TcEvBinds, SourceRow)
sourceRow self (L loc (Match { m_pats = L _ (pat : rest), m_grhss = rhss }))
  | ConPat (ConPatTc params [] [] evs WpHole) (L _ (RealDataCon input)) details <- unParen pat
  , GRHSs _ (L _ (GRHS _ [] (L _ rhs)) :| []) (L _ (EmptyLocalBinds _)) <- rhss
  = do
      fields <- case details of
        PrefixCon _ ps   -> Just ps
        InfixCon _ p1 p2 -> Just [p1,p2]
        _                -> Nothing
      binders <- traverse fieldBinder fields
      carried <- case rest of
        [] -> Just []
        [p] | VarPat _ (L _ v) <- unParen p -> Just [v]
        _ -> Nothing
      output <- sourceRhs self carried rhs
      pure (evs, SourceRow (locA loc) input params binders carried output)
  where
    unParen :: LPat GhcTc -> Pat GhcTc
    unParen (L _ (ParPat _ p)) = unParen p
    unParen (L _ p) = p

    fieldBinder p = case unParen p of
      VarPat _ (L _ v) -> Just (Just v)
      WildPat _        -> Just Nothing
      _                -> Nothing
sourceRow _ _ = Nothing

-- | Recognise constructors and self calls that preserve the carried argument.
sourceRhs :: (Id -> Bool) -> [Id] -> HsExpr GhcTc -> Maybe SourceRhs
sourceRhs self carried = go []
  where
    go :: [HsExpr GhcTc] -> HsExpr GhcTc -> Maybe SourceRhs
    go args (HsPar _ e)   = go args (unLoc e)
    go args (HsApp _ f x) = go (unLoc x : args) (unLoc f)
    go args (XExpr (ConLikeTc (RealDataCon con))) = Construct con <$> traverse (go []) args
    go [] (HsVar _ (L _ v)) = Just (Field v)
    go args (HsVar _ (L _ f)) | self f = call [] args
    go args (XExpr (WrapExpr (WpTyApp ty) (HsVar _ (L _ f)))) | self f = call [ty] args
    go args (HsAppType ty fun _)
      | HsVar _ (L _ f) <- unLoc (stripParensLHsExpr fun), self f = call [ty] args
    go _ _ = Nothing

    call tys (arg : rest) = do
      variables <- traverse variable rest
      guard (variables == carried)
      Recur tys <$> variable arg
    call _ [] = Nothing

    variable e = do { Field v <- go [] e; pure v }

-- | Check both the mutable evidence after solving and the final zonked evidence.
emptyEvidence :: TcEvBinds -> TcM Bool
emptyEvidence (EvBinds binds) = pure (isEmptyBag binds)
emptyEvidence (TcEvBinds var) = isEmptyEvBindsMap <$> getTcEvBindsMap var

-- | Instantiate the input with fresh Core binders for one family header or equation.
freshInput :: SourceInput -> TcM ([TyVar], Type, [Type])
freshInput (ClosedInput domain params) = pure ([], domain, params)
freshInput (PolyList sk) = do
    name <- newSysName (nameOccName (varName sk))
    let a = mkTyVar name liftedTypeKind
        ty = mkTyVarTy a
    pure ([a], mkListTy ty, [ty])

-- | Check a branch before supplying the family at its recursive calls.
sourceBranch :: SourceInput -> [Type] -> TyCon -> SourceRow -> TcM (Maybe (TyCon -> CoAxBranch))
sourceBranch input carried_types result (SourceRow loc con pat_args fields carried output) = do
    args <- liftZonkM (zonkTcTypes pat_args)
    carried_tys <- liftZonkM (zonkTcTypes (map varType carried))
    same_calls <- checkCalls output
    if same_calls && eqTypes args pattern_args && eqTypes carried_tys carried_types
       && length fields == dataConSourceArity con then do
      (quantified, domain, params) <- freshInput input
      let field_types = map scaledThing (dataConInstOrigArgTys con params)
          family_args = mkTyVarTys quantified
      tvs <- zipWithM fieldVar fields field_types
      carried_tvs <- zipWithM (fieldVar . Just) carried carried_types
      pure $ do
        let lhs = mkTyConApp (promoteDataCon con) (params ++ mkTyVarTys tvs)
            patterns = family_args ++ [lhs] ++ mkTyVarTys carried_tvs
            binders = quantified ++ tvs ++ carried_tvs
            carried_scope = zip (map Just carried) carried_tvs
        guard (all isTyFamFree patterns)
        rhs <- check_rhs domain family_args (zip fields tvs) carried_scope (mkTyConTy result) output
        pure (\fam -> mkCoAxBranch binders [] [] patterns (rhs fam)
                        (map (const Nominal) binders) loc)
    else pure Nothing
  where
    (call_args, pattern_args) = case input of
      ClosedInput _ params -> ([], params)
      PolyList sk -> ([mkTyVarTy sk], [mkTyVarTy sk])

    checkCalls (Field _) = pure True
    checkCalls (Construct _ args) = allM checkCalls args
    checkCalls (Recur tys _) = eqTypes call_args <$> liftZonkM (zonkTcTypes tys)

    fieldVar field ty = do
      let occ = maybe (mkTyVarOcc "_") (setOccNameSpace tvName . nameOccName . varName) field
      name <- newSysName occ
      pure (mkTyVar name ty)

    check_rhs domain family_args fields_scope carried_scope expected rhs = case rhs of
      Field v -> do
        tv <- lookup (Just v) (fields_scope ++ carried_scope)
        guard (tyVarKind tv `eqType` expected)
        pure (const (mkTyVarTy tv))
      Recur _ v -> do
        -- See Note [The second argument of a source equation].
        tv <- lookup (Just v) fields_scope
        guard (tyVarKind tv `eqType` domain && mkTyConTy result `eqType` expected)
        let operands = mkTyVarTys (tv : map snd carried_scope)
        pure (\fam -> mkTyConApp fam (family_args ++ operands))
      Construct dc args -> do
        (tc, []) <- ordinaryDataType expected
        guard (dataConTyCon dc == tc && length args == dataConSourceArity dc)
        let arg_tys = map scaledThing (dataConOrigArgTys dc)
        children <- zipWithM (check_rhs domain family_args fields_scope carried_scope) arg_tys args
        pure (\fam -> mkTyConApp (promoteDataCon dc) [child fam | child <- children])

-- | Give the table the representation of a closed, non-injective family.
makeFamily :: Name -> SourceInput -> [Type] -> TyCon -> [TyCon -> CoAxBranch] -> TcM TyCon
makeFamily fun input carried_types result branches = do
    name <- newImplicitBinder fun mkSourceTableOcc
    ax_name <- newImplicitBinder name mkInstTyCoOcc
    (quantified, domain, _) <- freshInput input
    args <- zipWithM argVar ["x", "y"] (domain : carried_types)
    let binders = mkNamedTyConBinders (Invisible SpecifiedSpec) quantified ++
                  mkAnonTyConBinders args
        res_kind = mkTyConTy result
        kind = mkTyConKindWithMatchability unmatchableDataConTy binders res_kind
        fam_tc = mkFamilyTyCon name kind binders 0 res_kind Nothing
                   (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal ax_name fam_tc (map ($ fam_tc) branches)
    pure fam_tc
  where
    argVar occ ty = do { n <- newSysName (mkTyVarOcc occ); pure (mkTyVar n ty) }
