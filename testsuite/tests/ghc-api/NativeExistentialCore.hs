{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Control.Exception (IOException, SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Array as A
import qualified Data.IntMap.Strict as IM
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import GHC hiding (typeKind)
import GHC.Builtin.WiredIn.Types
    (boolTy, constraintKind, eqTyCon, liftedTypeKind, manyDataConTy, maybeTyCon, mkBoxedTupleTy, promotedFalseDataCon,
     unmatchableDataConTy)
import GHC.Core (Expr (Coercion, Lam, Var))
import GHC.Core.Coercion (coercionLKind, coercionRKind, coercionRole, existsCoRule, liftCoSubst, mkCoVarCo, mkLiftingContext, mkNomReflCo)
import GHC.Core.Coercion.Axiom (Role(..))
import GHC.Core.Lint (lintExpr)
import GHC.Core.Predicate (EqRel (NomEq, ReprEq), mkNomEqPred, mkReprEqPred)
import GHC.Core.TyCo.Compare (eqType, nonDetCmpType)
import GHC.Core.TyCo.Rep (Coercion(AxiomCo), Type (..))
import GHC.Core.Type (isForAllTy, mkTyConApp, substTyWith, typeKind, tyCoVarsOfType)
import GHC.Core.Unify (tcMatchTy, tcUnifyTy, typesCantMatch)
import GHC.CoreToIface (toIfaceType)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.Env.Types (HscEnv (hsc_NC, hsc_dflags))
import GHC.Driver.Session (targetProfile)
import GHC.Iface.Binary
import GHC.Iface.Type (IfaceType (..), getIfaceType, mkIfLclName)
import GHC.Iface.Ext.Binary (hie_file_result, hie_file_result_version, readHieFile, writeHieFile)
import GHC.Iface.Ext.Types (HieASTs (HieASTs), HieFile (..), hieImplicitExistsVersion)
import GHC.Iface.Ext.Utils
    (freshIndex, getTypeIndex, hieTypeToIface, htyTable, initialHTS, recoverFullType)
import GHC.Iface.Syntax (IfaceDecl (IfaceId), IfaceIdDetails (IfVanillaId), ifName, ifType)
import GHC.IfaceToCore (typecheckIface)
import GHC.Tc.Utils.TcType (anyRewritableTyVar)
import GHC.Tc.Utils.Monad (initIfaceLoad)
import GHC.Types.Id (mkSysLocal)
import GHC.Types.Name (mkExternalName, mkSysTvName, mkSystemName)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.TypeEnv (lookupTypeEnv)
import GHC.Types.Unique.Supply (mkSplitUniqSupplyGrimily, uniqsFromSupply)
import GHC.Types.Var (ExistsTyFlag (ExplicitExists, ImplicitExists), TyVar, mkCoVar, mkTyVar, varType, VarBndr (Bndr), data Specified)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.Unit.Module (mkModule)
import GHC.Unit.Module.ModDetails (md_types)
import GHC.Unit.Module.ModIface (emptyFullModIface, mi_decls, set_mi_decls)
import GHC.Unit.Types (mainUnit)
import GHC.Utils.Binary (openBinMem, putByte, readBinMem, writeBinMem)
import GHC.Utils.Fingerprint (fingerprint0)
import GHC.Utils.Monad.StrictState (runState)
import GHC.Utils.Outputable (showSDocUnsafe, vcat)
import System.Environment (getArgs)

fixture :: TyVar -> Type -> Type
fixture = ExistsTy ExplicitExists

universal :: TyVar -> Type -> Type
universal a = ForAllTy unmatchableDataConTy (Bndr a Specified)

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
        getSessionDynFlags >>= void . setSessionDynFlags
        getSession >>= liftIO . verify "ExistentialSignature.hi"

verify :: FilePath -> HscEnv -> IO ()
verify interfacePath env = do
    us <- uniqsFromSupply <$> mkSplitUniqSupplyGrimily 'p'
    let fresh u n = mkTyVar (mkSysTvName u (fsLit n))
        ua : ub : uc : bad : dict : v : top : uWitness : uProof : _ = us
        a = fresh ua "a" liftedTypeKind
        b = fresh ub "b" liftedTypeKind
        c = fresh uc "c" liftedTypeKind
        badBinder = fresh bad "bad" (mkTyConApp promotedFalseDataCon [])
        dictBinder = fresh dict "dictionary" constraintKind
        dictPayload = mkTyConApp eqTyCon [liftedTypeKind, boolTy, boolTy]
        free = mkTyConApp maybeTyCon [TyVarTy c]
        abstract = mkTyConApp maybeTyCon [TyVarTy a]
        closed = fixture a abstract
        renamed = fixture b (mkTyConApp maybeTyCon [TyVarTy b])
        open = fixture a free
        captured = substTyWith [c] [TyVarTy a] open
        assert label yes = unless yes (fail label)
        lints ty = isNothing (lintExpr (initLintConfig (hsc_dflags env) []) (Lam binder (Var binder)))
          where
            binder = mkSysLocal (fsLit "arg") v manyDataConTy ty
    assert "lifted kind" (typeKind closed `eqType` liftedTypeKind)
    assert "alpha equivalence" (closed `eqType` renamed && nonDetCmpType closed renamed == EQ)
    assert "bound variable is not free" (isEmptyVarSet (tyCoVarsOfType closed))
    assert "substitution protects the bound variable" $
        substTyWith [a] [TyVarTy c] closed `eqType` closed
    assert "substitution avoids capture" $
        a `elemVarSet` tyCoVarsOfType captured &&
        captured `eqType` fixture b (mkTyConApp maybeTyCon [TyVarTy a])
    assert "valid signature fails Core Lint" (lints closed)
    assert "Core Lint accepted an out-of-scope type variable" (not (lints open))
    assert "existential is not a forall" $
        not (isForAllTy closed) && not (closed `eqType` universal a abstract)
    assert "closed alpha-equivalent existential failed to unify" $
        isJust (tcUnifyTy closed renamed) && isJust (tcMatchTy closed renamed)
    assert "whole existential failed as a variable instantiation" $
        isJust (tcUnifyTy (TyVarTy c) closed) && isJust (tcMatchTy (TyVarTy c) closed)
    assert "occurs check accepted a recursive existential instantiation" $
        isNothing (tcUnifyTy (TyVarTy c) open)
    forM_ [NomEq, ReprEq] $ \role -> do
        let rewritable = anyRewritableTyVar role (\_ _ tv -> tv == c)
        assert "the forall scan missed a free variable" (rewritable (universal a free))
        assert "the bar scan entered an opaque package" (not (rewritable open))
        assert "the dot scan entered an opaque package" (not (rewritable (ExistsTy ImplicitExists a free)))
    let known = fixture a (mkTyConApp maybeTyCon [boolTy])
    assert "opaque existential matching solved through the body" $
        isNothing (tcMatchTy open known) && not (typesCantMatch [(open, known)])
    assert "Core Lint rejected an erased witness of kind Constraint" $
        lints (fixture dictBinder (mkTyConApp maybeTyCon [boolTy]))
    assert "Core Lint accepted a Constraint payload" $
        not (lints (fixture a dictPayload))
    assert "Core Lint accepted the wrong existential binder kind" $
        not (lints (fixture badBinder abstract))
    assert "Core Lint accepted the wrong payload kind" $
        not (lints (fixture a (mkTyConApp promotedFalseDataCon [])))

    let hidden = fresh uWitness "hidden" liftedTypeKind
        packed x = fixture hidden (mkBoxedTupleTy [TyVarTy x, TyVarTy hidden])
        lintCo scope co = lintExpr (initLintConfig (hsc_dflags env) scope) (Coercion co)
        wellScoped scope = isNothing . lintCo scope
    forM_ [(Nominal, mkNomEqPred), (Representational, mkReprEqPred)] $ \(role, predicate) -> do
        let cv = mkCoVar (mkSystemName uProof (mkVarOcc "given")) (predicate (TyVarTy a) (TyVarTy b))
            co = liftCoSubst role (mkLiftingContext [(c, mkCoVarCo cv)]) (packed c)
            scope = [a, b, cv]
        assert "existential lifting changed the role or endpoints"
            (coercionRole co == role && coercionLKind co `eqType` packed a && coercionRKind co `eqType` packed b)
        assert "Core Lint rejected the lifted existential evidence" (wellScoped scope co)
        when (role == Representational) $ case co of
            AxiomCo _ [premise] ->
                case lintCo scope (AxiomCo (snd (existsCoRule ExplicitExists Nominal)) [premise]) of
                    Just errors -> do
                        let diagnostic = showSDocUnsafe (vcat (bagToList errors))
                        assert diagnostic $ all (`isInfixOf` diagnostic)
                            [ "Argument roles mismatch", "NominalExistsCo"
                            , "Expected: nominal", "Found: representational" ]
                    Nothing -> fail "representational evidence yielded nominal existential equality"
            _ -> fail "expected a representational existential coercion with one premise"
    assert "a non-universal premise yielded existential equality"
        (not (wellScoped [] (AxiomCo (snd (existsCoRule ExplicitExists Nominal)) [mkNomReflCo boolTy])))

    let mdl = mkModule mainUnit (mkModuleName "ExistentialSignature")
        nm = mkExternalName top mdl (mkVarOcc "value") noSrcSpan
        decl = IfaceId nm (toIfaceType closed) IfVanillaId []
        iface = set_mi_decls [(fingerprint0, decl)] (emptyFullModIface mdl)
        prof = targetProfile (hsc_dflags env)
    writeBinIface prof QuietBinIFace SafeExtraCompression interfacePath iface
    decoded <- readBinIface prof (hsc_NC env) CheckHiWay QuietBinIFace interfacePath
    case mi_decls decoded of
      [(_, entry@IfaceId{})] -> do
        assert "wrong interface type constructor" $
          case ifType entry of
            IfaceExistsTy {} -> True
            _ -> False
        details <- initIfaceLoad env (typecheckIface decoded)
        case lookupTypeEnv (md_types details) (ifName entry) of
            Just (AnId value) -> assert "changed signature after interface roundtrip" (varType value `eqType` closed)
            _ -> fail "missing value in decoded module"
      _ -> fail "missing signature in interface"
    let dot = ExistsTy ImplicitExists a abstract
        types = (,,,) <$> getTypeIndex closed
                      <*> getTypeIndex renamed
                      <*> getTypeIndex (universal a abstract)
                      <*> getTypeIndex dot
        ((index, renamedIndex, forallIndex, dotIndex), table) = runState types initialHTS
        hie = HieFile "ExistentialSignature.hs" mdl
                  (A.array (0, freshIndex table - 1) (IM.toList (htyTable table)))
                  (HieASTs mempty) [] mempty mempty
        hiePath = interfacePath ++ ".hie"
    assert "HIE did not deduplicate alpha-equivalent binders" (index == renamedIndex)
    assert "HIE merged an existential with a forall" (index /= forallIndex)
    assert "HIE merged dot and bar existentials" (index /= dotIndex)
    writeHieFile hiePath hie
    decodedHie <- readHieFile (hsc_NC env) hiePath
    assert "dot existential did not select its HIE version" $
        hie_file_result_version decodedHie == hieImplicitExistsVersion
    let recover i = hieTypeToIface (recoverFullType i (hie_types (hie_file_result decodedHie)))
    assert "wrong separator after HIE roundtrip" $
        case (recover index, recover dotIndex) of
          (IfaceExistsTy ExplicitExists _ _, IfaceExistsTy ImplicitExists _ _) -> True
          _ -> False
    let reject suffix ty = do
            let path = interfacePath ++ suffix
                bad = set_mi_decls [(fingerprint0, IfaceId nm ty IfVanillaId [])] iface
            writeBinIface prof QuietBinIFace NormalCompression path bad
            encoded <- readBinIface prof (hsc_NC env) CheckHiWay QuietBinIFace path
            [(_, entry)] <- pure (mi_decls encoded)
            outcome <- try $ do
              details <- initIfaceLoad env (typecheckIface encoded)
              case lookupTypeEnv (md_types details) (ifName entry) of
                Just (AnId value) -> evaluate (varType value)
                _ -> fail "missing malformed signature in interface"
            case (outcome :: Either SomeException Type) of
              Left _ -> putStrLn ("rejected malformed interface " ++ suffix)
              Right _ -> fail ("accepted malformed interface " ++ suffix)
    reject ".binder" (toIfaceType (fixture badBinder (mkTyConApp maybeTyCon [boolTy])))
    reject ".payload" (toIfaceType (fixture a dictPayload))
    let scopeName = mkIfLclName (fsLit "selfKind")
        external = mkIfLclName (fsLit "missingPayloadBinder")
    reject ".self-scope" (IfaceExistsTy ExplicitExists (scopeName, IfaceTyVar scopeName) (IfaceTyVar scopeName))
    reject ".payload-scope" (IfaceExistsTy ExplicitExists (scopeName, toIfaceType liftedTypeKind) (IfaceTyVar external))
    let unknown = interfacePath ++ ".unknown-type"
    writer <- openBinMem 32
    putByte writer 254
    writeBinMem writer unknown
    badTag <- try (readBinMem unknown >>= getIfaceType)
    assert "unexpected handling of an unknown interface type tag" $
      case badTag of
        Left err -> "getIfaceType: invalid type tag 254" `isInfixOf`
                    displayException (err :: IOException)
        Right _ -> False
    putStrLn "checked type binder scope, substitution, alpha equality, Core Lint and binary signature roundtrip"
