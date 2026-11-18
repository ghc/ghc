-- | Closed type families for checked functions that match one constructor.
module GHC.Tc.Gen.SourceTable (sourceTableTyCons) where

import GHC.Prelude

import GHC.Builtin.WiredIn.Types (unmatchableDataConTy)
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
import GHC.Tc.TyCl.Build (newImplicitBinder)
import GHC.Tc.Types.Evidence (HsWrapper(..), TcEvBinds(..), isEmptyEvBindsMap)
import GHC.Tc.Utils.Monad (TcM, getTcEvBindsMap, liftZonkM, newSysName)
import GHC.Tc.Utils.TcType (isTyFamFree)
import GHC.Tc.Zonk.TcType (zonkTcType, zonkTcTypes)
import GHC.Types.Basic (Origin(..))
import GHC.Types.Name (Name, isExternalName, mkSourceTableOcc, mkInstTyCoOcc,
                       mkTyVarOcc, nameOccName, setOccNameSpace, tvName)
import GHC.Types.SrcLoc
import GHC.Types.Unique.Set (mkUniqSet)
import GHC.Types.Var (Id, mkTyVar, varName)
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

A declaration splice can introduce a type that uses a function from an
earlier group. At this boundary, simplifyTop has solved the constraints
for the group. Read the evidence bindings and follow the solved type of
each candidate. The final module zonker can run typed splices and default
metavariables, so do not run it on the group's terms here.
-}

-- | Derive closed families after solving the group's top-level constraints.
sourceTableTyCons :: LHsBinds GhcTc -> TcM [TyCon]
sourceTableTyCons binds = catMaybes <$> mapM certify (mapMaybe (sourceRows . unLoc) binds)
  where
    certify (fun, evidence, rows) = do
      clean <- allM emptyEvidence evidence
      if clean then do
        ty <- liftZonkM (zonkTcType (varType fun))
        case checkedDomains ty rows of
          Nothing -> pure Nothing
          Just (domain, params, result) -> do
            branches <- traverse (sourceBranch domain params result) rows
            traverse (makeFamily (varName fun) domain result) (sequence branches)
      else pure Nothing

-- | A constructor pattern, including its type arguments, and its checked RHS.
data SourceRow = SourceRow SrcSpan DataCon [Type] [Maybe Id] SourceRhs

-- | The expression forms allowed in one source equation.
data SourceRhs = Field Id | Construct DataCon [SourceRhs] | Recur Id

-- | Collect the constructors and evidence for a candidate source function.
sourceRows :: HsBind GhcTc -> Maybe (Id, [TcEvBinds], [SourceRow])
sourceRows binding = do
    (fun, mono, MG { mg_ext = MatchGroupTc { mg_origin = FromSource },
               mg_alts = L _ matches }, evidence) <- case binding of
      XHsBindsLR (AbsBinds { abs_tvs = [], abs_ev_vars = [], abs_ev_binds = evs
                          , abs_exports = [ABE { abe_poly = fun, abe_mono = mono
                                              , abe_wrap = WpHole }]
                          , abs_binds = [L _ body] }) -> do
        (bound, matches) <- funBind body
        guard (bound == mono)
        pure (fun, mono, matches, evs)
      _ -> do
        (fun, matches) <- funBind binding
        pure (fun, fun, matches, [])
    guard (isExternalName (varName fun))
    let self v = v == fun || v == mono
    (row_evidence, rows) <- unzip <$> traverse (sourceRow self) matches
    pure (fun, evidence ++ row_evidence, rows)
  where
    funBind :: HsBind GhcTc -> Maybe (Id, MatchGroup GhcTc (LHsExpr GhcTc))
    funBind (FunBind { fun_ext = (WpHole, []), fun_id = L _ f, fun_matches = mg })
      = Just (f, mg)
    funBind _ = Nothing

-- | Check that the rows cover the domain of the solved function type.
checkedDomains :: Type -> [SourceRow] -> Maybe (Type, [Type], TyCon)
checkedDomains ty rows = do
    (FTF_T_T, ma, mult, arg_ty, res_ty) <- splitFunTy_maybe ty
    guard (ma `eqType` unmatchableDataConTy && isManyTy mult)
    (domain, params) <- ordinaryDataType arg_ty
    (result, []) <- ordinaryDataType res_ty
    let expected = tyConDataCons domain
        actual   = [con | SourceRow _ con _ _ _ <- rows]
    guard (length actual == length expected && mkUniqSet actual == mkUniqSet expected)
    pure (arg_ty, params, result)

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
sourceRow self (L loc (Match { m_pats = L _ [pat], m_grhss = rhss }))
  | ConPat (ConPatTc params [] [] evs WpHole) (L _ (RealDataCon input)) details <- unParen pat
  , GRHSs _ (L _ (GRHS _ [] (L _ rhs)) :| []) (L _ (EmptyLocalBinds _)) <- rhss
  = do
      fields <- case details of
        PrefixCon _ ps   -> Just ps
        InfixCon _ p1 p2 -> Just [p1,p2]
        _                -> Nothing
      binders <- traverse fieldBinder fields
      output <- sourceRhs self rhs
      pure (evs, SourceRow (locA loc) input params binders output)
  where
    unParen :: LPat GhcTc -> Pat GhcTc
    unParen (L _ (ParPat _ p)) = unParen p
    unParen (L _ p) = p

    fieldBinder p = case unParen p of
      VarPat _ (L _ v) -> Just (Just v)
      WildPat _        -> Just Nothing
      _                -> Nothing
sourceRow _ _ = Nothing

-- | Recognise constructor applications and a self call on a variable.
sourceRhs :: (Id -> Bool) -> HsExpr GhcTc -> Maybe SourceRhs
sourceRhs self = go []
  where
    go :: [HsExpr GhcTc] -> HsExpr GhcTc -> Maybe SourceRhs
    go args (HsPar _ e)   = go args (unLoc e)
    go args (HsApp _ f x) = go (unLoc x : args) (unLoc f)
    go args (XExpr (ConLikeTc (RealDataCon con))) = Construct con <$> traverse (go []) args
    go [] (HsVar _ (L _ v)) = Just (Field v)
    go [arg] (HsVar _ (L _ f))
      | self f = do { Field v <- go [] arg; pure (Recur v) }
    go _ _ = Nothing

-- | Check both the mutable evidence after solving and the final zonked evidence.
emptyEvidence :: TcEvBinds -> TcM Bool
emptyEvidence (EvBinds binds) = pure (isEmptyBag binds)
emptyEvidence (TcEvBinds var) = isEmptyEvBindsMap <$> getTcEvBindsMap var

-- | Check a branch before supplying the family at its recursive calls.
sourceBranch :: Type -> [Type] -> TyCon -> SourceRow -> TcM (Maybe (TyCon -> CoAxBranch))
sourceBranch domain params result (SourceRow loc con pat_args fields output) = do
    args <- liftZonkM (zonkTcTypes pat_args)
    if eqTypes args params && length fields == dataConSourceArity con then do
      let field_types = map scaledThing (dataConInstOrigArgTys con params)
      tvs <- zipWithM fieldVar fields field_types
      pure $ do
        let lhs = mkTyConApp (promoteDataCon con) (params ++ mkTyVarTys tvs)
        guard (isTyFamFree lhs)
        rhs <- check_rhs (zip fields tvs) (mkTyConTy result) output
        pure (\fam -> mkCoAxBranch tvs [] [] [lhs] (rhs fam) (map (const Nominal) tvs) loc)
    else pure Nothing
  where
    fieldVar field ty = do
      let occ = maybe (mkTyVarOcc "_") (setOccNameSpace tvName . nameOccName . varName) field
      name <- newSysName occ
      pure (mkTyVar name ty)

    check_rhs scope expected rhs = case rhs of
      Field v -> do
        tv <- lookup (Just v) scope
        guard (tyVarKind tv `eqType` expected)
        pure (const (mkTyVarTy tv))
      Recur v -> do
        tv <- lookup (Just v) scope
        guard (tyVarKind tv `eqType` domain && mkTyConTy result `eqType` expected)
        pure (\fam -> mkTyConApp fam [mkTyVarTy tv])
      Construct dc args -> do
        (tc, []) <- ordinaryDataType expected
        guard (dataConTyCon dc == tc && length args == dataConSourceArity dc)
        let arg_tys = map scaledThing (dataConOrigArgTys dc)
        children <- zipWithM (check_rhs scope) arg_tys args
        pure (\fam -> mkTyConApp (promoteDataCon dc) [child fam | child <- children])

-- | Give the table the representation of a closed, non-injective family.
makeFamily :: Name -> Type -> TyCon -> [TyCon -> CoAxBranch] -> TcM TyCon
makeFamily fun domain result branches = do
    name <- newImplicitBinder fun mkSourceTableOcc
    ax_name <- newImplicitBinder name mkInstTyCoOcc
    arg_name <- newSysName (mkTyVarOcc "x")
    let binders = [mkAnonTyConBinder (mkTyVar arg_name domain)]
        res_kind = mkTyConTy result
        kind = mkTyConKindWithMatchability unmatchableDataConTy binders res_kind
        fam_tc = mkFamilyTyCon name kind binders 0 res_kind Nothing
                   (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal ax_name fam_tc (map ($ fam_tc) branches)
    pure fam_tc
