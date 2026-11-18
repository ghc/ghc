-- | Closed type families for checked functions that match one constructor.
module GHC.Tc.Gen.SourceTable (sourceTableTyCons) where

import GHC.Prelude

import GHC.Builtin.WiredIn.Types (unmatchableDataConTy)
import GHC.Core.Coercion.Axiom (ClosedTyFam(..), CoAxBranch)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.Bag (isEmptyBag)
import GHC.Hs
import GHC.Tc.TyCl.Build (newImplicitBinder)
import GHC.Tc.Types.Evidence (HsWrapper(..), TcEvBinds(..), isEmptyEvBindsMap)
import GHC.Tc.Utils.Monad (TcM, getTcEvBindsMap, liftZonkM, newSysName)
import GHC.Tc.Zonk.TcType (zonkTcType)
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
axiom. A row may return a nullary constructor or one of its own fields.
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
          Just (domain, result) -> do
            branches <- traverse (sourceBranch result) rows
            traverse (makeFamily (varName fun) domain result) (sequence branches)
      else pure Nothing

-- | A checked constructor pattern and the constructor or field returned by the row.
data SourceRow = SourceRow SrcSpan DataCon [Maybe Id] (Either DataCon Id)

-- | Collect the constructors and evidence for a candidate source function.
sourceRows :: HsBind GhcTc -> Maybe (Id, [TcEvBinds], [SourceRow])
sourceRows binding = do
    (fun, MG { mg_ext = MatchGroupTc { mg_origin = FromSource },
               mg_alts = L _ matches }, evidence) <- case binding of
      XHsBindsLR (AbsBinds { abs_tvs = [], abs_ev_vars = [], abs_ev_binds = evs
                          , abs_exports = [ABE { abe_poly = fun, abe_mono = mono
                                              , abe_wrap = WpHole }]
                          , abs_binds = [L _ body] }) -> do
        (bound, matches) <- funBind body
        guard (bound == mono)
        pure (fun, matches, evs)
      _ -> do
        (fun, matches) <- funBind binding
        pure (fun, matches, [])
    guard (isExternalName (varName fun))
    (row_evidence, rows) <- unzip <$> traverse sourceRow matches
    pure (fun, evidence ++ row_evidence, rows)
  where
    funBind :: HsBind GhcTc -> Maybe (Id, MatchGroup GhcTc (LHsExpr GhcTc))
    funBind (FunBind { fun_ext = (WpHole, []), fun_id = L _ f, fun_matches = mg })
      = Just (f, mg)
    funBind _ = Nothing

-- | Check that the rows cover the domain of the solved function type.
checkedDomains :: Type -> [SourceRow] -> Maybe (TyCon, TyCon)
checkedDomains ty rows = do
    (FTF_T_T, ma, mult, arg_ty, res_ty) <- splitFunTy_maybe ty
    guard (ma `eqType` unmatchableDataConTy && isManyTy mult)
    domain <- ordinaryDataType arg_ty
    result <- ordinaryDataType res_ty
    let expected = tyConDataCons domain
        actual   = [con | SourceRow _ con _ _ <- rows]
    guard (length actual == length expected && mkUniqSet actual == mkUniqSet expected)
    pure (domain, result)
  where
    ordinaryDataType ty
      | Just (tc, []) <- splitTyConApp_maybe ty
      , isVanillaAlgTyCon tc, isBoxedDataTyCon tc, isLiftedAlgTyCon tc
      , tyConArity tc == 0, null (tyConStupidTheta tc)
      , let cons = tyConDataCons tc
      , isEnumerationTyCon tc ||
        (not (isGadtSyntaxTyCon tc) && not (null cons) && all ordinaryCon cons)
      = Just tc
    ordinaryDataType _ = Nothing

    ordinaryCon con =
      null (dataConRepArgBinders con) && isVanillaDataCon con &&
      isNothing (dataConWrapId_maybe con) &&
      not (any isBanged (dataConImplBangs con)) &&
      all (ordinaryField . scaledThing) (dataConOrigArgTys con)

    ordinaryField field =
      isLiftedTypeKind (typeKind field) && noFreeVarsOfType field && isTauTy field

-- | Read one unguarded source equation with no local bindings.
sourceRow :: LMatch GhcTc (LHsExpr GhcTc) -> Maybe (TcEvBinds, SourceRow)
sourceRow (L loc (Match { m_pats = L _ [pat], m_grhss = rhss }))
  | ConPat (ConPatTc [] [] [] evs WpHole) (L _ (RealDataCon input))
           (PrefixCon _ fields) <- unParen pat
  , GRHSs _ (L _ (GRHS _ [] (L _ rhs)) :| []) (L _ (EmptyLocalBinds _)) <- rhss
  = do
      binders <- traverse fieldBinder fields
      output <- case rhs of
        XExpr (ConLikeTc (RealDataCon con)) | isNullarySrcDataCon con -> Just (Left con)
        HsVar _ (L _ v) -> Just (Right v)
        _ -> Nothing
      pure (evs, SourceRow (locA loc) input binders output)
  where
    unParen :: LPat GhcTc -> Pat GhcTc
    unParen (L _ (ParPat _ p)) = unParen p
    unParen (L _ p) = p

    fieldBinder p = case unParen p of
      VarPat _ (L _ v) -> Just (Just v)
      WildPat _        -> Just Nothing
      _                -> Nothing
sourceRow _ = Nothing

-- | Check both the mutable evidence after solving and the final zonked evidence.
emptyEvidence :: TcEvBinds -> TcM Bool
emptyEvidence (EvBinds binds) = pure (isEmptyBag binds)
emptyEvidence (TcEvBinds var) = isEmptyEvBindsMap <$> getTcEvBindsMap var

-- | Quantify the constructor fields in one equation and check the result kind.
sourceBranch :: TyCon -> SourceRow -> TcM (Maybe CoAxBranch)
sourceBranch result (SourceRow loc con fields output)
  | length fields == length field_types = do
      tvs <- zipWithM fieldVar fields field_types
      pure $ do
        rhs <- case output of
          Left dc -> do
            guard (dataConTyCon dc == result)
            pure (mkTyConTy (promoteDataCon dc))
          Right v -> do
            tv <- lookup (Just v) (zip fields tvs)
            guard (tyVarKind tv `eqType` mkTyConTy result)
            pure (mkTyVarTy tv)
        let lhs = mkTyConApp (promoteDataCon con) (mkTyVarTys tvs)
        pure (mkCoAxBranch tvs [] [] [lhs] rhs (map (const Nominal) tvs) loc)
  | otherwise = pure Nothing
  where
    field_types = map scaledThing (dataConOrigArgTys con)
    fieldVar field ty = do
      let occ = maybe (mkTyVarOcc "_") (setOccNameSpace tvName . nameOccName . varName) field
      name <- newSysName occ
      pure (mkTyVar name ty)

-- | Give the table the representation of a closed, non-injective family.
makeFamily :: Name -> TyCon -> TyCon -> [CoAxBranch] -> TcM TyCon
makeFamily fun domain result branches = do
    name <- newImplicitBinder fun mkSourceTableOcc
    ax_name <- newImplicitBinder name mkInstTyCoOcc
    arg_name <- newSysName (mkTyVarOcc "x")
    let binders = [mkAnonTyConBinder (mkTyVar arg_name (mkTyConTy domain))]
        res_kind = mkTyConTy result
        kind = mkTyConKindWithMatchability unmatchableDataConTy binders res_kind
        fam_tc = mkFamilyTyCon name kind binders 0 res_kind Nothing
                   (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal ax_name fam_tc branches
    pure fam_tc
