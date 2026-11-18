-- | Closed type families for checked functions on finite datatypes.
module GHC.Tc.Gen.SourceTable (sourceTableTyCons) where

import GHC.Prelude

import GHC.Builtin.WiredIn.Types (unmatchableDataConTy)
import GHC.Core.Coercion.Axiom (ClosedTyFam(..))
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
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
import GHC.Types.Name (Name, isExternalName, mkSourceTableOcc, mkInstTyCoOcc, mkTyVarOcc)
import GHC.Types.SrcLoc
import GHC.Types.Unique.Set (mkUniqSet)
import GHC.Types.Var (Id, mkTyVar, varName)
import GHC.Utils.Monad (allM)

import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, mapMaybe)

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

Only use complete, disjoint tables on lifted enumeration types. In
particular, an existential, a constructor field, a guard, or a recursive
call would require an additional rule. Check the whole set of rows
before creating any axiom. Each branch keeps the source span of its equation.

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
        traverse makeFamily (checkedTable (varName fun) ty rows)
      else pure Nothing

-- | The checked domain, result, and rows of one top-level function.
data SourceTable = SourceTable Name TyCon TyCon [SourceRow]

-- | Source location, matched constructor, and returned constructor.
type SourceRow = (SrcSpan, DataCon, DataCon)

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

-- | Validate a source table against the solved type of its function.
checkedTable :: Name -> Type -> [SourceRow] -> Maybe SourceTable
checkedTable name ty rows = do
    (FTF_T_T, ma, mult, arg_ty, res_ty) <- splitFunTy_maybe ty
    guard (ma `eqType` unmatchableDataConTy && isManyTy mult)
    domain <- enumeration arg_ty
    result <- enumeration res_ty
    let expected = tyConDataCons domain
        actual   = [con | (_, con, _) <- rows]
    guard (length actual == length expected && mkUniqSet actual == mkUniqSet expected)
    guard (all (\(_, _, con) -> dataConTyCon con == result) rows)
    pure (SourceTable name domain result rows)
  where
    enumeration ty
      | Just (tc, []) <- splitTyConApp_maybe ty
      , isVanillaAlgTyCon tc, isBoxedDataTyCon tc, isLiftedAlgTyCon tc
      , isEnumerationTyCon tc, tyConArity tc == 0, null (tyConStupidTheta tc)
      = Just tc
      | otherwise = Nothing

-- | Read one unguarded source equation with no local bindings.
sourceRow :: LMatch GhcTc (LHsExpr GhcTc) -> Maybe (TcEvBinds, SourceRow)
sourceRow (L loc (Match { m_pats = L _ [L _ pat], m_grhss = rhss }))
  | ConPat (ConPatTc [] [] [] evs WpHole) (L _ (RealDataCon input)) (PrefixCon _ []) <- pat
  , GRHSs _ (L _ (GRHS _ [] (L _ rhs)) :| []) (L _ (EmptyLocalBinds _)) <- rhss
  , XExpr (ConLikeTc (RealDataCon output)) <- rhs
  = Just (evs, (locA loc, input, output))
sourceRow _ = Nothing

-- | Check both the mutable evidence after solving and the final zonked evidence.
emptyEvidence :: TcEvBinds -> TcM Bool
emptyEvidence (EvBinds binds) = pure (isEmptyBag binds)
emptyEvidence (TcEvBinds var) = isEmptyEvBindsMap <$> getTcEvBindsMap var

-- | Give the table the representation of a closed, non-injective family.
makeFamily :: SourceTable -> TcM TyCon
makeFamily (SourceTable fun domain result rows) = do
    name <- newImplicitBinder fun mkSourceTableOcc
    ax_name <- newImplicitBinder name mkInstTyCoOcc
    arg_name <- newSysName (mkTyVarOcc "x")
    let binders = [mkAnonTyConBinder (mkTyVar arg_name (mkTyConTy domain))]
        res_kind = mkTyConTy result
        kind = mkTyConKindWithMatchability unmatchableDataConTy binders res_kind
        fam_tc = mkFamilyTyCon name kind binders 0 res_kind Nothing
                   (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal ax_name fam_tc (map branch rows)
        branch (loc, input, output) =
          mkCoAxBranch [] [] [] [promoted input] (promoted output) [] loc
        promoted = mkTyConTy . promoteDataCon
    pure fam_tc
