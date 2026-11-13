{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (IOException, SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find, foldl', isInfixOf, sort)
import GHC (DynFlags, getSession, getSessionDynFlags, runGhc)
import GHC.Builtin.WiredIn.Prim (intPrimTy)
import GHC.Builtin.WiredIn.Types
import GHC.Core
import GHC.Core.Coercion (coercionLKind, coercionRKind, coercionRole, eqCoercion, isReflCo, mkAxInstCo, mkCoVarCo, mkDepFunCo, mkFunCoNoFTF, mkNomReflCo, mkPiCo, mkReflCo, mkSubCo, mkSymCo, mkTransCo, mkTyConAppCo)
import GHC.Core.Coercion.Axiom (Branched, ClosedTyFam (..), CoAxiom, CoAxiomRule (..), Role (..))
import GHC.Core.DataCon (DataCon, DataConRep (NoDataConRep), HsImplBang (HsLazy), HsSrcBang (HsSrcBang), SrcStrictness (NoSrcStrict), SrcUnpackedness (NoSrcUnpack), StrictnessMark (NotMarkedStrict), dataConWorkId, mkDataCon, promoteDataCon)
import GHC.Core.Dependent
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.FamInstEnv (emptyFamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch, normaliseType)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Map.Type (LooseTypeMap, TypeMap, emptyTypeMap)
import GHC.Core.Predicate (mkNomEqPred, mkReprEqPred)
import GHC.Core.Reduction (Reduction (..))
import GHC.Core.SimpleOpt (simpleOptNativeExpr)
import qualified GHC.Core.Subst as CoreSubst
import GHC.Core.TyCo.Compare (eqType, nonDetCmpType)
import GHC.Core.TyCo.FVs (tyCoVarsOfCo)
import GHC.Core.TyCo.Rep (Coercion (..), Scaled (..), TyLit (..), Type (..))
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.TyCo.Tidy (tidyTopType)
import GHC.Core.TyCon (AlgTyConFlav (VanillaAlgTyCon), FamTyConFlav (..), Injectivity (..), PromDataConInfo (NoPromInfo), TyCon, mkAlgTyCon, mkAnonTyConBinder, mkDataTyConRhs, mkFamilyTyCon, mkSynonymTyCon, mkTyConKind)
import GHC.Core.Type
import GHC.Core.Unfold.Make (mkCoreUnfolding)
import GHC.Core.Unify (BindFlag (..), MaybeApartReason (..), UnifyResultM (..), alwaysBindFam, alwaysBindTv, tcMatchRetainedTyX_BM, tcMatchTy, tcMatchTyX_BM, tcMatchTys, tcUnifyTysFG)
import GHC.Core.Utils (applyTypeToArgs, exprType)
import GHC.CoreToIface (toIfaceAlt, toIfaceCoercion, toIfaceExpr, toIfaceType, toIfaceTypeX)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Data.IOEnv (IOEnvFailure)
import GHC.Data.StringBuffer (stringToStringBuffer)
import qualified GHC.Data.TrieMap as Trie
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Flags (GeneralFlag (Opt_DoCoreLinting))
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (gopt_unset, parseDynamicFlagsCmdLine)
import GHC.Hs (HsType (..))
import GHC.Iface.Binary (CompressionIFace (..), TraceBinIFace (..), getWithUserData, putWithUserData)
import GHC.Iface.Env (extendIfaceIdEnv)
import GHC.Iface.Syntax (IfGuidance (IfWhen), IfaceAlt (..), IfaceConAlt (..), IfaceDecl (IfaceId), IfaceExpr (..), IfaceIdDetails (IfDFunId, IfVanillaId), IfaceInfoItem (HsUnfold), IfaceRule (IfaceRule), IfaceUnfolding (IfCoreUnfold, IfDFunUnfold))
import GHC.Iface.Type (IfaceBndr (IfaceIdBndr), IfaceCoercion (IfaceCoVarCo), IfaceType (..), getIfaceType, ifaceBndrName, mkIfLclName)
import GHC.IfaceToCore (tcIfaceDecl, tcIfaceExpr, tcIfaceRules)
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Lexer as Lexer
import GHC.Tc.Utils.Monad (initIfaceLcl, initIfaceLoad)
import GHC.Tc.Utils.TcType (noConcreteTyVars)
import GHC.Types.Basic (OccInfo (..), TailCallInfo (..))
import GHC.Types.Id
import GHC.Types.Id.Info (IdDetails (VanillaId), RuleInfo (..))
import GHC.Types.Id.Make (mkDataConWorkId)
import GHC.Types.InlinePragma (ActivationX (AlwaysActive))
import GHC.Types.Literal (Literal (LitChar))
import GHC.Types.Name (getOccName, mkExternalName, mkSystemName, nameModule)
import GHC.Types.Name.Occurrence (mkDataConWorkerOcc, mkDataOcc, mkTcOcc, mkTyVarOcc, mkVarOcc, occNameFS)
import GHC.Types.SourceText (SourceText (NoSourceText))
import GHC.Types.SrcLoc (mkRealSrcLoc, noLoc, noSrcSpan, unLoc)
import GHC.Types.TyThing (TyThing (AnId))
import GHC.Types.Unique (Unique, mkUniqueGrimilyWithTag)
import GHC.Types.Unique.FM (sizeUFM)
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Unit.Types (IsBootInterface (NotBoot), mainUnit, mkModule, mkModuleName)
import qualified GHC.Utils.Binary as Binary
import GHC.Utils.Logger (initLogger, pushLogHook)
import GHC.Utils.Outputable (ppr, text, vcat)
import GHC.Utils.Panic (GhcException (..))
import System.Environment (getArgs)
import System.Timeout (timeout)

-- | Run the native Core API checks against the supplied GHC library directory.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [libdir] -> do
            dflags <- runGhc (Just libdir) getSessionDynFlags
            runSyntaxChecks dflags
            runChecks dflags
            runMatchingChecks dflags
            runTypeMapChecks
            runCaseChecks dflags
            runNatChecks dflags
            runDepFunCoChecks dflags
            runIfaceChecks libdir
            putStrLn "native-core-api-ok"
        _ -> fail "Usage: NativeDependentCore GHC_LIBDIR"

-- | Create a stable test unique without a global unique supply.
unique :: Int -> Unique
unique = mkUniqueGrimilyWithTag 'N' . fromIntegral

-- | Create a local unrestricted identifier for a test.
local :: Int -> String -> Type -> Id
local n label = mkSysLocal (fsLit label) (unique n) ManyTy

-- | Build a lifted result that retains the Boolean index in a dictionary type.
indexed :: Type -> Type
indexed index = mkFunctionType ManyTy (mkTyConApp eqTyCon [boolTy, index, index]) intTy

-- | Build an identity function at a supplied type.
identity :: Int -> Type -> CoreExpr
identity n ty = let x = local n "x" ty in Lam x (Var x)

-- | Build the native retained keep function.
keep :: Id -> Int -> CoreExpr
keep b n = Lam b (identity n (indexed (TermVarTy b)))

-- | Build the expected ordinary result of applying keep to an index.
keepResult :: Type -> Type
keepResult index = mkFunctionType ManyTy (indexed index) (indexed index)

-- | Check native type keys against equality and exercise all public trie operations.
runTypeMapChecks :: IO ()
runTypeMapChecks = do
    let b = local 700 "b" boolTy
        c = local 701 "b" boolTy
        d = local 702 "b" boolTy
        e = local 703 "b" boolTy
        free = local 704 "free" boolTy
        otherFree = local 705 "free" boolTy
        (nat, _, _) = peanoNat
        natTy = mkTyConTy nat
        n = local 706 "n" natTy
        tv = mkTyVar (mkSystemName (unique 700) (mkTyVarOcc "b")) boolTy
        at v = mkFunctionType ManyTy (mkTyConApp eqTyCon [varType v, TermVarTy v, TermVarTy v]) intTy
        atB = at b
        body x y = mkFunctionType ManyTy x y
        piB = mkDepFunTy b (body atB atB)
        piC = mkDepFunTy c (body (at c) (at c))
        boundFree = mkDepFunTy c (body (at c) atB)
        piN = mkDepFunTy n (body (at n) (at n))
        nested x y = mkDepFunTy x (mkDepFunTy y (body (at x) (at y)))
        nestedFree x y z = mkDepFunTy x (mkDepFunTy y (body (at x) (body (at y) (at z))))
        predicate = mkTyConApp eqTyCon [boolTy, TermVarTy b, TermVarTy b]
        entries :: [(Type, Int)]
        entries =
            [ (TermVarTy b, 1)
            , (TermVarTy c, 2)
            , (piB, 3)
            , (piN, 4)
            , (nested b c, 5)
            , (nestedFree b c free, 6)
            , (TyVarTy tv, 7)
            , (predicate, 8)
            , (intTy, 9)
            , (boolTy, 10)
            ]
        probes =
            zipWith (\i (ty, _) -> ("inserted key " ++ show i, ty)) [1 :: Int ..] entries
                ++ [ ("alpha-equivalent binder", piC)
                   , ("shared bound/free subtree", boundFree)
                   , ("nested alpha-equivalent binders", nested d e)
                   , ("nested binder order", mkDepFunTy d (mkDepFunTy e (body (at e) (at d))))
                   , ("nested shared free identifier", nestedFree d e free)
                   , ("nested distinct free identifier", nestedFree d e otherFree)
                   , ("absent free identifier", TermVarTy free)
                   ]
        typed = foldl' (\m (ty, value) -> Trie.insertTM ty value m) emptyTypeMap entries
        loose = foldl' (\m (ty, value) -> Trie.insertTM ty value m) (Trie.emptyTM :: LooseTypeMap Int) entries
        updated = [(ty, if eqType ty piB then value + 100 else value) | (ty, value) <- entries]
        deleted = [(ty, value) | (ty, value) <- entries, not (eqType ty (TermVarTy b) || eqType ty piN)]
        mapped = [(ty, value * 10) | (ty, value) <- entries]
        filtered = [(ty, value) | (ty, value) <- entries, odd value]
        partial = [(ty, value + 1000) | (ty, value) <- entries, even value]
        keepEven value = if even value then Just (value + 1000) else Nothing
        typedStates :: [(String, [(Type, Int)], TypeMap Int)]
        typedStates =
            [ ("multi map", entries, typed)
            , ("alpha update", updated, Trie.alterTM piC (fmap (+ 100)) typed)
            , ("delete native keys", deleted, Trie.deleteTM piN (Trie.deleteTM (TermVarTy b) typed))
            , ("fmap", mapped, fmap (* 10) typed)
            , ("filter", filtered, Trie.filterTM odd typed)
            , ("mapMaybe", partial, Trie.mapMaybeTM keepEven typed)
            , ("empty filter", [], Trie.filterTM (const False) typed)
            , ("delete all", [], foldl' (flip Trie.deleteTM) typed (map fst entries))
            ]
        looseStates :: [(String, [(Type, Int)], LooseTypeMap Int)]
        looseStates =
            [ ("multi map", entries, loose)
            , ("alpha update", updated, Trie.alterTM piC (fmap (+ 100)) loose)
            , ("delete native keys", deleted, Trie.deleteTM piN (Trie.deleteTM (TermVarTy b) loose))
            , ("fmap", mapped, fmap (* 10) loose)
            , ("filter", filtered, Trie.filterTM odd loose)
            , ("mapMaybe", partial, Trie.mapMaybeTM keepEven loose)
            , ("empty filter", [], Trie.filterTM (const False) loose)
            , ("delete all", [], foldl' (flip Trie.deleteTM) loose (map fst entries))
            ]
        states =
            [("TypeMap " ++ label, expected, \ty -> Trie.lookupTM ty m, Trie.foldTM (:) m []) | (label, expected, m) <- typedStates]
                ++ [("LooseTypeMap " ++ label, expected, \ty -> Trie.lookupTM ty m, Trie.foldTM (:) m []) | (label, expected, m) <- looseStates]
    check "type map native alpha equality" (eqType piB piC && eqType (nested b c) (nested d e))
    check "type map bound/free equality differs" (not (eqType piB boundFree))
    check "type map runtime/type variable namespace differs" (not (eqType (TermVarTy b) (TyVarTy tv)))
    forM_ states $ \(label, expected, lookupValue, values) -> do
        forM_ probes $ \(probe, ty) ->
            check (label ++ ": " ++ probe) (lookupValue ty == fmap snd (find (\(key, _) -> eqType key ty) expected))
        check (label ++ ": fold values") (sort values == sort (map snd expected))
    let singletonTyped :: TypeMap Int
        singletonTyped = Trie.insertTM piB 3 emptyTypeMap
        singletonLoose = Trie.insertTM piB 3 (Trie.emptyTM :: LooseTypeMap Int)
    check "TypeMap singleton alpha equality" (Trie.lookupTM piC singletonTyped == Just 3)
    check "LooseTypeMap singleton alpha equality" (Trie.lookupTM piC singletonLoose == Just 3)
    check "TypeMap singleton shared bound/free mismatch" (Trie.lookupTM boundFree singletonTyped == Nothing)
    check "LooseTypeMap singleton shared bound/free mismatch" (Trie.lookupTM boundFree singletonLoose == Nothing)
    let termSingleton = Trie.insertTM (TermVarTy b) 1 (Trie.emptyTM :: LooseTypeMap Int)
    check "native singleton distinguishes free identifiers" (Trie.lookupTM (TermVarTy c) termSingleton == Nothing)
    check "native singleton distinguishes variable namespaces" (Trie.lookupTM (TyVarTy tv) termSingleton == Nothing)
    let otherKind = TermVarTy (setIdType b natTy)
        kindMap :: TypeMap Int
        kindMap = Trie.insertTM otherKind 2 (Trie.insertTM (TermVarTy b) 1 emptyTypeMap)
    check "TypeMap native keys have distinct kinds" (not (eqType (typeKind (TermVarTy b)) (typeKind otherKind)))
    check "TypeMap preserves native Bool kind key" (Trie.lookupTM (TermVarTy b) kindMap == Just 1)
    check "TypeMap preserves native Nat kind key" (Trie.lookupTM otherKind kindMap == Just 2)
    check "TypeMap folds both native kind entries" (sort (Trie.foldTM (:) kindMap []) == [1, 2])

-- | Construct an ordinary recursive datatype without wired-in constructor Names.
peanoNat :: (TyCon, DataCon, DataCon)
peanoNat = (nat, zero, successor)
  where
    modu = mkModule mainUnit (mkModuleName "NativeIndexAPI")
    name n occurrence = mkExternalName (unique n) modu occurrence noSrcSpan
    nat =
        mkAlgTyCon
            (name 500 (mkTcOcc "Nat"))
            liftedTypeKind
            []
            0
            liftedTypeKind
            []
            Nothing
            []
            (mkDataTyConRhs [zero, successor])
            (VanillaAlgTyCon (name 501 (mkVarOcc "nat_rep")))
            False
    zero = constructor 510 "Z" 1 []
    successor = constructor 520 "S" 2 [mkTyConTy nat]
    constructor n label tag fields = con
      where
        occurrence = mkDataOcc label
        con =
            mkDataCon
                (name n occurrence)
                False
                (name (n + 2) (mkVarOcc (label ++ "_rep")))
                (map (const (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)) fields)
                (map (const HsLazy) fields)
                (map (const NotMarkedStrict) fields)
                []
                []
                []
                noConcreteTyVars
                []
                []
                []
                (map (Scaled OneTy) fields)
                (mkTyConTy nat)
                NoPromInfo
                nat
                tag
                []
                (mkDataConWorkId (name (n + 1) (mkDataConWorkerOcc occurrence)) con)
                NoDataConRep

-- | Check recursive constructor indices, substitutions, and field-scoped proofs.
runNatChecks :: DynFlags -> IO ()
runNatChecks dflags = do
    let (nat, zero, successor) = peanoNat
        natTy = mkTyConTy nat
        zeroTy = mkTyConTy (promoteDataCon zero)
        succTy index = mkTyConApp (promoteDataCon successor) [index]
        zeroExpr = Var (dataConWorkId zero)
        succExpr value = App (Var (dataConWorkId successor)) value
        n = local 530 "n" natTy
        m = local 531 "m" natTy
        field = local 532 "predecessor" natTy
        payload = local 533 "payload" intTy
        caseBinder = local 534 "nat_case" natTy
        index = TermVarTy n
        indexedNat ty = mkFunctionType ManyTy (mkTyConApp eqTyCon [natTy, ty, ty]) intTy
        keepNat = Lam n (identity 535 (indexedNat index))
        keepNatResult ty = mkFunctionType ManyTy (indexedNat ty) (indexedNat ty)
        zeroProof = proof 536 index zeroTy
        succProof = proof 537 index (succTy (TermVarTy field))
        branch number concrete cv = Cast body (mkSymCo transport)
          where
            dictionary = local number "nat_dictionary" (mkTyConApp eqTyCon [natTy, concrete, concrete])
            body = Lam dictionary (Var payload)
            co = mkCoVarCo cv
            predicateCo = mkTyConAppCo Nominal eqTyCon [mkNomReflCo natTy, co, co]
            transport = mkFunCoNoFTF Representational (mkNomReflCo ManyTy) (mkSubCo predicateCo) (mkReflCo Representational intTy)
        zeroBranch = branch 538 zeroTy zeroProof
        succBranch = branch 539 (succTy (TermVarTy field)) succProof
        alternatives =
            [ Alt (DataAlt zero) [zeroProof] zeroBranch
            , Alt (DataAlt successor) [field, succProof] succBranch
            ]
        natCase = Case (Var n) caseBinder (indexedNat index) alternatives
    domain <- timeout 2000000 (evaluate (isNativeIndexType natTy))
    check "recursive Nat index domain terminates" (domain == Just True)
    check "Nat runtime Id is an index" (isNativeIndexId n)
    check "linear Nat Id is not an index" (not (isNativeIndexId (setIdMult n OneTy)))
    checkType dflags "runtime Nat index has kind Nat" natTy (typeKind index)
    forM_
        [ (Var n, index)
        , (zeroExpr, zeroTy)
        , (succExpr (Var n), succTy index)
        , (succExpr (succExpr (Var n)), succTy (succTy index))
        ]
        $ \(expression, expected) -> do
            lintAccepts dflags "ordinary Peano constructor application" [n] expression
            case exprToNativeIndex expression of
                Just actual -> do
                    checkType dflags "constructor index retains its runtime fields" expected actual
                    checkType dflags "constructor index has kind Nat" natTy (typeKind actual)
                Nothing -> fail "FAIL: Peano constructor application was not an index"
    let opaque = setIdUnfolding n (unfolding (error "forced a Nat index unfolding"))
    case exprToNativeIndex (succExpr (Var opaque)) of
        Just actual -> checkType dflags "successor index does not inspect a lazy field unfolding" (succTy index) actual
        Nothing -> fail "FAIL: successor lost its opaque runtime field"
    forM_
        [ Var (dataConWorkId successor)
        , App (succExpr zeroExpr) zeroExpr
        , succExpr (Var (dataConWorkId trueDataCon))
        ]
        $ \expression -> check "index conversion rejects malformed constructor applications" $ case exprToNativeIndex expression of
            Nothing -> True
            Just _ -> False
    let replace = indexSubst [m] n (succExpr (Var m))
        range = Subst.getSubstRangeTyCoFVs replace
        nested = succTy (succTy index)
    check "constructor substitution range includes its field Id" (m `elemVarSet` range)
    checkType dflags "index substitution descends through constructors" (succTy (succTy (succTy (TermVarTy m)))) (Subst.substTy replace nested)
    case exprToNativeIndex (mapNativeIndex (Subst.substTy replace) (succExpr (Var n))) of
        Just actual -> checkType dflags "index mapping reconstructs constructor fields" (succTy (succTy (TermVarTy m))) actual
        Nothing -> fail "FAIL: mapped constructor expression lost its index"
    let replaceField = indexSubst [m] field (succExpr (Var m))
        replaceN = indexSubst [field] n (succExpr (Var field))
        composed = Subst.composeTCvSubst replaceField replaceN
    checkType dflags "index composition traverses successor fields" (succTy (succTy (TermVarTy m))) (Subst.substTy composed index)
    let template = mkTyVar (mkSystemName (unique 542) (mkTyVarOcc "length")) natTy
    case tcMatchTy (succTy (mkTyVarTy template)) (succTy (TermVarTy field)) of
        Just matched -> case Subst.lookupTyVar matched template of
            Just actual -> checkType dflags "constructor matching binds an erased parameter to a runtime field" (TermVarTy field) actual
            Nothing -> fail "FAIL: constructor matching lost its length parameter"
        Nothing -> fail "FAIL: successor template did not match a runtime predecessor"
    check "opaque Nat cannot exclude the zero family equation" $ case tcUnifyTysFG alwaysBindFam alwaysBindTv [index] [zeroTy] of
        MaybeApart _ _ -> True
        _ -> False
    check "known Nat constructor heads remain apart" $ case tcUnifyTysFG alwaysBindFam alwaysBindTv [zeroTy] [succTy index] of
        SurelyApart -> True
        _ -> False
    check "opaque Nat may be its own successor" $ case tcUnifyTysFG alwaysBindFam alwaysBindTv [index] [succTy index] of
        MaybeApart _ _ -> True
        _ -> False
    let left = mkTyVar (mkSystemName (unique 550) (mkTyVarOcc "left")) natTy
        right = mkTyVar (mkSystemName (unique 551) (mkTyVarOcc "right")) natTy
        binders = map mkAnonTyConBinder [left, right]
        addition =
            mkFamilyTyCon
                (mkSystemName (unique 552) (mkTcOcc "Add"))
                (mkTyConKind binders natTy)
                binders
                0
                natTy
                Nothing
                (ClosedTypeFamilyTyCon (CTF (Just additionAxiom)))
                Nothing
                NotInjective
        add a b = mkTyConApp addition [a, b]
        additionAxiom =
            mkBranchedCoAxiom
                Nominal
                (mkSystemName (unique 553) (mkTcOcc "AddAx"))
                addition
                [ mkCoAxBranch [right] [] [] [zeroTy, mkTyVarTy right] (mkTyVarTy right) [Nominal] noSrcSpan
                , mkCoAxBranch
                    [left, right]
                    []
                    []
                    [succTy (mkTyVarTy left), mkTyVarTy right]
                    (succTy (add (mkTyVarTy left) (mkTyVarTy right)))
                    [Nominal, Nominal]
                    noSrcSpan
                ]
    forM_
        [ ("zero addition", add zeroTy (TermVarTy m), TermVarTy m)
        , ("successor addition", add (succTy index) (TermVarTy m), succTy (add index (TermVarTy m)))
        , ("opaque addition stays stuck", add index (TermVarTy m), add index (TermVarTy m))
        , ("known unfolding stays opaque", add (TermVarTy (setIdUnfolding n (unfolding zeroExpr))) (TermVarTy m), add index (TermVarTy m))
        ]
        $ \(label, original, expected) -> do
            let Reduction co reduced = normaliseType emptyFamInstEnvs Nominal original
            checkType dflags label expected reduced
            checkType dflags (label ++ " coercion source") original (coercionLKind co)
            checkType dflags (label ++ " coercion target") expected (coercionRKind co)
            lintAccepts dflags (label ++ " coercion") [n, m] (Coercion co)
    let checkNormalization role label scope original expected = do
            let context = label ++ " at " ++ showSDoc dflags (ppr role)
                Reduction co reduced = normaliseType emptyFamInstEnvs role original
                Reduction again twice = normaliseType emptyFamInstEnvs role reduced
                actualFree = filterVarSet isNonCoVarId (tyCoVarsOfCo co)
                expectedFree = filterVarSet isNonCoVarId (tyCoVarsOfType original)
            checkType dflags context expected reduced
            checkType dflags (context ++ " source") original (coercionLKind co)
            checkType dflags (context ++ " target") expected (coercionRKind co)
            check (context ++ " role") (coercionRole co == role)
            check
                (context ++ " free runtime indices")
                (sizeVarSet actualFree == sizeVarSet expectedFree && actualFree `subVarSet` expectedFree)
            lintAccepts dflags (context ++ " coercion") scope (Coercion co)
            checkType dflags (context ++ " idempotence") reduced twice
            check (context ++ " idempotent evidence") (isReflCo again)
            lintAccepts dflags (context ++ " idempotent coercion") scope (Coercion again)
            pure reduced
        nestedPi body = mkDepFunTy n (mkDepFunTy m (indexedNat body))
        opaqueFree = setIdUnfolding m (unfolding (error "normalization forced a runtime index unfolding"))
    -- Existing family reduction policy preserves applications at Phantom.
    -- Ordinary forall kinds still normalize at Nominal.
    forM_ [Nominal, Representational, Phantom] $ \role ->
        forM_
            [ ("retained zero addition", [m], mkDepFunTy n (indexedNat (add zeroTy index)), mkDepFunTy n (indexedNat index))
            , ("retained successor addition", [m], mkDepFunTy n (indexedNat (add (succTy index) (TermVarTy m))), mkDepFunTy n (indexedNat (succTy (add index (TermVarTy m)))))
            , ("retained opaque addition stays stuck", [m], mkDepFunTy n (indexedNat (add index (TermVarTy m))), mkDepFunTy n (indexedNat (add index (TermVarTy m))))
            , ("nested retained addition", [], nestedPi (add (succTy index) (TermVarTy m)), nestedPi (succTy (add index (TermVarTy m))))
            , ("free index unfolding stays opaque under a retained binder", [m], mkDepFunTy n (indexedNat (add (TermVarTy opaqueFree) index)), mkDepFunTy n (indexedNat (add (TermVarTy m) index)))
            ]
            $ \(label, scope, original, expected) ->
                checkNormalization role label scope original (if role == Phantom then original else expected)
    let forgetParameter = mkTyVar (mkSystemName (unique 1700) (mkTyVarOcc "ignored")) natTy
        forgetBinders = [mkAnonTyConBinder forgetParameter]
        forgetFamily =
            mkFamilyTyCon
                (mkSystemName (unique 1701) (mkTcOcc "ForgetNat"))
                (mkTyConKind forgetBinders liftedTypeKind)
                forgetBinders
                0
                liftedTypeKind
                Nothing
                (ClosedTypeFamilyTyCon (CTF (Just forgetAxiom)))
                Nothing
                NotInjective
        forgetAxiom =
            mkBranchedCoAxiom
                Nominal
                (mkSystemName (unique 1702) (mkTcOcc "ForgetNatAx"))
                forgetFamily
                [mkCoAxBranch [forgetParameter] [] [] [mkTyVarTy forgetParameter] intTy [Nominal] noSrcSpan]
        forgotten = mkTyConApp forgetFamily [index]
        erased = mkTyVar (mkSystemName (unique 1703) (mkTyVarOcc "kind_parameter")) (selected (constructorIndex trueDataCon))
        erasedNormalized = setTyVarKind erased intTy
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        _ <-
            checkNormalization
                role
                "normalization removes retained dependence"
                []
                (mkDepFunTy n forgotten)
                (if role == Phantom then mkDepFunTy n forgotten else mkFunctionType ManyTy natTy intTy)
        checkNormalization
            role
            "ordinary forall kind normalizes above retained body"
            []
            (mkForAllTy (Bndr erased Specified) (mkDepFunTy n (indexedNat (add zeroTy index))))
            (mkForAllTy (Bndr erasedNormalized Specified) (mkDepFunTy n (indexedNat (if role == Phantom then add zeroTy index else index))))
    let originalShadow = mkFunctionType ManyTy (indexedNat index) (mkDepFunTy n (indexedNat (add zeroTy index)))
        expectedShadow = mkFunctionType ManyTy (indexedNat index) (mkDepFunTy n (indexedNat index))
    freshened <- checkNormalization Nominal "retained normalization avoids a free outer Id" [n] originalShadow expectedShadow
    case freshened of
        FunTy{ft_res = DepFunTy fresh body} -> do
            check "retained normalization freshens a shadowing Id" (fresh /= n)
            checkType dflags "retained normalization uses the fresh Id in its body" (indexedNat (TermVarTy fresh)) body
            check "retained normalization preserves its free outer Id" (n `elemVarSet` tyCoVarsOfType freshened)
            check "retained normalization binds the fresh Id" (not (fresh `elemVarSet` tyCoVarsOfType freshened))
        _ -> fail "FAIL: retained normalization lost its nested binder"
    forM_
        [ ("type variable", mkTyVar (mkSystemName (unique 1704) (mkTyVarOcc "bad_index")) natTy)
        , ("coercion variable", proof 1705 zeroTy zeroTy)
        , ("linear Id", setIdMult n OneTy)
        , ("unsupported domain", local 1706 "bad_index" intTy)
        , ("unlifted domain", local 1707 "bad_index" intPrimTy)
        ]
        $ \(label, bad) ->
            rejectsWith ("normalization rejects a retained " ++ label) "normalise_type: invalid retained function" $
                evaluate (normaliseType emptyFamInstEnvs Nominal (DepFunTy bad intTy))
    forM_ [("unlifted", intPrimTy), ("constraint", mkNomEqPred index index)] $ \(label, body) ->
        rejectsWith ("normalization rejects a retained " ++ label ++ " result") "normalise_type: invalid retained function" $
            evaluate (normaliseType emptyFamInstEnvs Nominal (DepFunTy n body))
    let captured = mkDepFunTy m (mkFunctionType ManyTy (indexedNat (TermVarTy m)) (indexedNat index))
    case Subst.substTy replace captured of
        DepFunTy fresh body -> do
            check "Nat substitution avoids capturing a field Id" (fresh /= m)
            checkType
                dflags
                "Nat substitution preserves both binder identities"
                (mkFunctionType ManyTy (indexedNat (TermVarTy fresh)) (indexedNat (succTy (TermVarTy m))))
                body
        _ -> fail "FAIL: Nat substitution lost its retained binder"
    lintAccepts dflags "retained Nat identity" [] keepNat
    checkType dflags "retained Nat identity type" (mkDepFunTy n (keepNatResult index)) (exprType keepNat)
    forM_ [zeroExpr, succExpr (Var m)] $ \argument -> do
        lintAccepts dflags "retained application accepts Peano constructors" [m] (App keepNat argument)
        case exprToNativeIndex argument of
            Just actual -> checkType dflags "retained application substitutes constructor fields" (keepNatResult actual) (exprType (App keepNat argument))
            Nothing -> fail "FAIL: retained Peano argument lost its index"
    lintAccepts dflags "successor field is in scope in its virtual proof" [n, payload] natCase
    lintAccepts dflags "Nat case substitution preserves successor evidence" [m, payload] $
        CoreSubst.substExpr (indexSubst [m, payload] n (Var m)) natCase
    lintAccepts dflags "Nat case substitution admits a constructor index" [m, payload] $
        CoreSubst.substExpr (indexSubst [m, payload] n (succExpr (Var m))) natCase
    lintAccepts dflags "Nat field substitution avoids alternative capture" [field, payload] $
        CoreSubst.substExpr (indexSubst [field, payload] n (Var field)) natCase
    lintAccepts dflags "closed retained Nat constructor case" [] (Lam n (Lam payload natCase))
    checkType dflags "Nat case retains its outer index" (indexedNat index) (exprType natCase)
    lintAccepts dflags "ordinary successor fields remain ordinary payload" [n, payload] $
        Case
            (Var n)
            caseBinder
            intTy
            [ Alt (DataAlt zero) [] (Var payload)
            , Alt (DataAlt successor) [field] (Var payload)
            ]
    lintRejects dflags "native successor proof cannot precede its field" [n, payload] $
        Case
            (Var n)
            caseBinder
            (indexedNat index)
            [Alt (DataAlt zero) [zeroProof] zeroBranch, Alt (DataAlt successor) [succProof, field] succBranch]
    lintRejects dflags "native successor requires its real runtime field" [n, payload] $
        Case
            (Var n)
            caseBinder
            (indexedNat index)
            [Alt (DataAlt zero) [zeroProof] zeroBranch, Alt (DataAlt successor) [succProof] succBranch]
    let wrongProof = proof 540 index zeroTy
    lintRejects dflags "successor evidence must name its actual constructor and field" [n, payload] $
        Case
            (Var n)
            caseBinder
            (indexedNat index)
            [Alt (DataAlt zero) [zeroProof] zeroBranch, Alt (DataAlt successor) [field, wrongProof] (branch 543 zeroTy wrongProof)]
    let wrongField = local 544 "wrong_field" boolTy
    lintRejects dflags "successor field must have its declared Nat type" [n, payload] $
        Case
            (Var n)
            caseBinder
            (indexedNat index)
            [Alt (DataAlt zero) [zeroProof] zeroBranch, Alt (DataAlt successor) [wrongField, succProof] succBranch]
    let shadowProof = proof 545 index (succTy index)
    lintRejects dflags "successor field cannot capture its scrutinee index" [n, payload] $
        Case
            (Var n)
            caseBinder
            (indexedNat index)
            [Alt (DataAlt zero) [zeroProof] zeroBranch, Alt (DataAlt successor) [n, shadowProof] (branch 546 (succTy index) shadowProof)]
    lintRejects dflags "constructor field cannot escape into the case result" [n, payload] $
        Case (Var n) caseBinder (indexedNat (TermVarTy field)) alternatives
    lintRejects dflags "native Nat proof alternatives must cover the domain" [n, payload] $
        Case (Var n) caseBinder (indexedNat index) [Alt (DataAlt successor) [field, succProof] succBranch]
    let literalProof = proof 541 zeroTy zeroTy
        literalCase = Case zeroExpr caseBinder intTy [Alt DEFAULT [] (Var payload), Alt (DataAlt zero) [literalProof] (Var payload)]
    lintAccepts dflags "literal Nat evidence with DEFAULT remains checked" [payload] literalCase
    check "scanner detects literal Nat evidence" (hasNativeDependentExpr literalCase)
    let alias = local 560 "named_successor" natTy
        aliasIndex = TermVarTy alias
        successorIndex = succTy index
        aliasProof = proof 561 aliasIndex successorIndex
        aliasDictionary = local 562 "alias_dictionary" (mkTyConApp eqTyCon [natTy, aliasIndex, aliasIndex])
        predicateCo = mkTyConAppCo Nominal eqTyCon [mkNomReflCo natTy, mkCoVarCo aliasProof, mkCoVarCo aliasProof]
        transport = mkFunCoNoFTF Representational (mkNomReflCo ManyTy) (mkSubCo predicateCo) (mkReflCo Representational intTy)
        aliasBody = Cast (Lam aliasDictionary (Var payload)) transport
        aliasCase = Case (succExpr (Var n)) alias (indexedNat successorIndex) [Alt DEFAULT [aliasProof] aliasBody]
        literalAliasProof = proof 563 aliasIndex zeroTy
        literalAlias = Case zeroExpr alias intTy [Alt DEFAULT [literalAliasProof] (Var payload)]
        wrongAliasProof = proof 564 aliasIndex (succTy (TermVarTy m))
        reversedAliasProof = proof 565 successorIndex aliasIndex
        reprAliasProof = mkCoVar (mkSystemName (unique 566) (mkVarOcc "repr_alias")) (mkReprEqPred aliasIndex successorIndex)
        extraAliasProof = proof 567 aliasIndex successorIndex
        notProof = local 568 "not_alias_evidence" intTy
        plainAliasCase cv = Case (succExpr (Var n)) alias intTy [Alt DEFAULT [cv] (Var payload)]
        outerAliasProof = proof 561 index index
    lintAccepts dflags "DEFAULT alias evidence transports an indexed result" [n, payload] aliasCase
    lintAccepts dflags "DEFAULT alias evidence accepts a closed constructor" [payload] literalAlias
    lintAccepts dflags "DEFAULT alias evidence survives outer index substitution" [m, payload] $
        CoreSubst.substExpr (indexSubst [m, payload] n (succExpr (Var m))) aliasCase
    lintAccepts dflags "DEFAULT alias substitution avoids capturing a free index" [alias, payload] $
        CoreSubst.substExpr (indexSubst [alias, payload] n (Var alias)) aliasCase
    lintAccepts dflags "DEFAULT alias evidence also names an opaque variable" [n, payload] $
        Case (Var n) alias intTy [Alt DEFAULT [proof 571 aliasIndex index] (Var payload)]
    lintAccepts dflags "DEFAULT alias evidence can coexist with an ordinary constructor alternative" [n, payload] $
        Case (succExpr (Var n)) alias intTy [Alt DEFAULT [aliasProof] (Var payload), Alt (DataAlt zero) [] (Var payload)]
    check "scanner detects DEFAULT alias evidence with ordinary result" (hasNativeDependentExpr literalAlias)
    lintRejects dflags "DEFAULT alias evidence must name the actual constructor fields" [n, m, payload] (plainAliasCase wrongAliasProof)
    lintRejects dflags "DEFAULT alias evidence cannot reverse endpoints" [n, payload] (plainAliasCase reversedAliasProof)
    lintRejects dflags "DEFAULT alias evidence must be nominal" [n, payload] (plainAliasCase reprAliasProof)
    lintRejects dflags "DEFAULT alias evidence requires exactly one proof" [n, payload] $
        Case (succExpr (Var n)) alias intTy [Alt DEFAULT [aliasProof, extraAliasProof] (Var payload)]
    lintRejects dflags "DEFAULT alias evidence cannot be an ordinary Id" [n, payload] (plainAliasCase notProof)
    lintRejects dflags "DEFAULT alias evidence must be fresh" [n, payload, outerAliasProof] (plainAliasCase aliasProof)
    lintRejects dflags "DEFAULT alias binder must be unrestricted" [n, payload] $
        Case (succExpr (Var n)) (setIdMult alias OneTy) intTy [Alt DEFAULT [aliasProof] (Var payload)]
    lintRejects dflags "DEFAULT alias binder cannot escape into the result annotation" [n, payload] $
        Case (succExpr (Var n)) alias (indexedNat aliasIndex) [Alt DEFAULT [aliasProof] (Lam aliasDictionary (Var payload))]
    lintRejects dflags "DEFAULT alias evidence cannot describe an arbitrary producer" [n, payload] $
        Case (App (identity 569 natTy) (Var n)) alias intTy [Alt DEFAULT [proof 570 aliasIndex index] (Var payload)]

-- | Construct an index substitution with an explicit scope.
indexSubst :: [Var] -> Id -> CoreExpr -> Subst.Subst
indexSubst scope b rhs =
    Subst.Subst (mkInScopeSet (mkVarSet scope)) (unitVarEnv b rhs) emptyVarEnv emptyVarEnv

-- | Assert a semantic property and identify its failure precisely.
check :: String -> Bool -> IO ()
check label condition = unless condition (fail ("FAIL: " ++ label))

-- | Check retained types and closed expressions through binary interface tables.
runIfaceChecks :: FilePath -> IO ()
runIfaceChecks libdir = runGhc (Just libdir) $ do
    env <- getSession
    dflags <- getSessionDynFlags
    liftIO $ do
        let b = local 300 "b" boolTy
            original = tidyTopType (exprType (keep b 301))
            iface = toIfaceType original
            ifaceModule = nameModule (idName (dataConWorkId trueDataCon))
            restoreExpr :: [Id] -> IfaceExpr -> IO CoreExpr
            restoreExpr scope input =
                initIfaceLoad env
                    $ initIfaceLcl ifaceModule (text "native interface test") NotBoot
                    $ extendIfaceIdEnv scope
                    $ tcIfaceExpr input
            restore :: [Id] -> IfaceType -> IO Type
            restore scope input = do
                expression <- restoreExpr scope (IfaceType input)
                case expression of
                    Type ty -> pure ty
                    _ -> fail "FAIL: interface type reconstruction returned a term"
            roundtrip :: (Binary.Binary a) => CompressionIFace -> a -> IO a
            roundtrip compression input = do
                writer <- Binary.openBinMem 1024
                putWithUserData QuietBinIFace compression writer input
                reader <- Binary.shrinkBinBuffer writer
                getWithUserData (hsc_NC env) reader
            rejectsIface :: String -> String -> [Id] -> IfaceType -> IO ()
            rejectsIface label fragment scope input = rejectsIfaceExpr label fragment scope (IfaceType input)
            rejectsIfaceExpr :: String -> String -> [Id] -> IfaceExpr -> IO ()
            rejectsIfaceExpr label fragment scope input = do
                messages <- newIORef []
                let logger = pushLogHook (\_ _ _ _ message -> modifyIORef' messages (showSDoc dflags message :)) (hsc_logger env)
                    quietEnv = env{hsc_logger = logger}
                outcome <-
                    try @IOEnvFailure
                        $ initIfaceLoad quietEnv
                        $ initIfaceLcl ifaceModule (text "native interface test") NotBoot
                        $ extendIfaceIdEnv scope
                        $ tcIfaceExpr input
                case outcome of
                    Left _ -> do
                        diagnostics <- unlines <$> readIORef messages
                        unless (fragment `isInfixOf` diagnostics) $
                            fail ("FAIL: " ++ label ++ "; unexpected interface diagnostic:\n" ++ diagnostics)
                    Right _ -> fail ("FAIL: " ++ label ++ "; malformed interface succeeded")
            cache = UnfoldingCache False False False False
            unlintedEnv = hscUpdateFlags (`gopt_unset` Opt_DoCoreLinting) env
            importTemplate :: HscEnv -> IfaceType -> IfaceExpr -> IO CoreExpr
            importTemplate importEnv declared body = do
                let name = mkExternalName (unique 330) ifaceModule (mkVarOcc "native_unfolding") noSrcSpan
                    declaration = IfaceId name declared IfVanillaId [HsUnfold False (IfCoreUnfold VanillaSrc cache (IfWhen 0 False False) body)]
                thing <-
                    initIfaceLoad importEnv
                        $ initIfaceLcl ifaceModule (text "native unfolding test") NotBoot
                        $ tcIfaceDecl False declaration
                case thing of
                    AnId imported -> case realIdUnfolding imported of
                        unfoldingInfo@CoreUnfolding{uf_tmpl = template} -> do
                            check "imported unfolding preserves its wire cache" (uf_cache unfoldingInfo == cache)
                            evaluate template
                        _ -> fail "FAIL: imported native unfolding was lost"
                    _ -> fail "FAIL: interface declaration did not reconstruct an Id"
            rejectsImportedUnfolding :: String -> String -> IfaceType -> IfaceExpr -> IO ()
            rejectsImportedUnfolding label fragment declared body =
                rejectsDeferredIface label fragment $ \importEnv -> do
                    _ <- importTemplate importEnv declared body
                    pure ()
            rejectsDeferredIface :: String -> String -> (HscEnv -> IO ()) -> IO ()
            rejectsDeferredIface label fragment action = do
                messages <- newIORef []
                let logger = pushLogHook (\_ _ _ _ message -> modifyIORef' messages (showSDoc dflags message :)) (hsc_logger unlintedEnv)
                outcome <- try @GhcException (action unlintedEnv{hsc_logger = logger})
                case outcome of
                    Left (ProgramError message) -> do
                        check (label ++ " fork failure") (message == "Cannot continue after interface file error")
                        diagnostics <- unlines <$> readIORef messages
                        check (label ++ "; diagnostic: " ++ diagnostics) (fragment `isInfixOf` diagnostics)
                    Left exception -> fail ("FAIL: " ++ label ++ "; unexpected exception: " ++ displayException exception)
                    Right _ -> fail ("FAIL: " ++ label ++ "; unsupported interface payload was accepted")
            rejectsIfaceRule :: String -> [IfaceBndr] -> IfaceExpr -> IO ()
            rejectsIfaceRule label binders body =
                rejectsDeferredIface label "Native dependent interface rules are not supported" $ \importEnv -> do
                    let rule = IfaceRule (fsLit "native_rule") AlwaysActive binders (idName (dataConWorkId trueDataCon)) [] body False IsOrphan
                    rules <-
                        initIfaceLoad importEnv
                            $ initIfaceLcl ifaceModule (text "native rule test") NotBoot
                            $ tcIfaceRules False [rule]
                    case rules of
                        [rule'] -> evaluate (ru_rhs rule') >> pure ()
                        _ -> fail "FAIL: interface rule was lost"
        let payload = local 309 "payload" intTy
            caseBinder = local 310 "case_b" boolTy
            index = TermVarTy b
            trueExpr = Var (dataConWorkId trueDataCon)
            falseProof = proof 311 index (constructorIndex falseDataCon)
            trueProof = proof 312 index (constructorIndex trueDataCon)
            literalFalseProof = proof 313 (constructorIndex trueDataCon) (constructorIndex falseDataCon)
            literalTrueProof = proof 314 (constructorIndex trueDataCon) (constructorIndex trueDataCon)
            oldProofs = [falseProof, trueProof, literalFalseProof, literalTrueProof]
            close expression = Lam b (Lam payload expression)
            variableCase =
                close $ Case (Var b) caseBinder (indexed index) (dictionaryAlts payload falseProof trueProof)
            literalCase =
                close $
                    Case
                        trueExpr
                        caseBinder
                        (indexed (constructorIndex trueDataCon))
                        (dictionaryAlts payload literalFalseProof literalTrueProof)
            ordinaryCase =
                close $
                    Case
                        (Var b)
                        caseBinder
                        intTy
                        [ Alt (DataAlt falseDataCon) [] (Var payload)
                        , Alt (DataAlt trueDataCon) [] (Var payload)
                        ]
            defaultCase =
                close $
                    Case
                        (Var b)
                        caseBinder
                        intTy
                        [ Alt DEFAULT [] (Var payload)
                        , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
                        ]
            expressionCases =
                [ ("variable proof case", variableCase, 2)
                , ("literal proof case", literalCase, 2)
                , ("ordinary Boolean case", ordinaryCase, 0)
                , ("DEFAULT and proof case", defaultCase, 1)
                ]
            lazyUnfolding = mkCoreUnfolding VanillaSrc True (error "forced a deferred unfolding RHS") Nothing UnfNever
            keptIndex = local 331 "index_only" boolTy
            consumer = local 332 "consumer" (indexed (TermVarTy keptIndex))
            dictionary = local 333 "dictionary" (mkTyConApp eqTyCon [boolTy, TermVarTy keptIndex, TermVarTy keptIndex])
            indexOnlyBody =
                Let
                    (NonRec keptIndex trueExpr)
                    (App (Lam consumer (Var payload)) (Lam dictionary (Var payload)))
            indexOnlyBinding = Lam payload indexOnlyBody
            stableTemplate =
                close $
                    Case
                        (Var b)
                        caseBinder
                        intTy
                        [ Alt (DataAlt falseDataCon) [] (Var payload)
                        , Alt (DataAlt trueDataCon) [trueProof] indexOnlyBody
                        ]
            nestedStable =
                setIdUnfolding
                    (local 334 "nested_stable" (exprType stableTemplate))
                    (mkCoreUnfolding StableUserSrc False stableTemplate (Just cache) UnfNever)
            ordinaryParent = Let (NonRec nestedStable ordinaryCase) (Var nestedStable)
        check "unfolding construction leaves its RHS deferred" $ case uf_src lazyUnfolding of
            VanillaSrc -> True
            _ -> False
        forM_ [Nothing, Just cache] $ \precomputed -> do
            let constructed = mkCoreUnfolding VanillaSrc True indexOnlyBinding precomputed UnfNever
                template = uf_tmpl constructed
            lintAccepts dflags "constructed unfolding preserves an index-only binding" [] template
            case template of
                Lam _ (Let (NonRec surviving _) _) -> check "unfolding retains index binder identity" (surviving == keptIndex)
                _ -> fail "FAIL: unfolding construction removed an index-only binding"
            case precomputed of
                Just supplied -> check "unfolding preserves its supplied cache" (uf_cache constructed == supplied)
                Nothing -> pure ()
        check "parent RHS has no native syntax" (not (hasNativeDependentExprRhs ordinaryParent))
        let parentTemplate = uf_tmpl (mkCoreUnfolding VanillaSrc False ordinaryParent (Just cache) UnfNever)
        case parentTemplate of
            Let (NonRec surviving _) _ -> do
                let nestedTemplate = uf_tmpl (realIdUnfolding surviving)
                lintAccepts dflags "parent preserves its stable native unfolding" [] nestedTemplate
                case nestedTemplate of
                    Lam _ (Lam _ (Case _ _ _ [_, Alt (DataAlt constructor) [cv] (Let (NonRec retained _) _)])) -> do
                        check "stable nested unfolding retains its proof" (constructor == trueDataCon && cv == trueProof)
                        check "stable nested unfolding retains its index-only binding" (retained == keptIndex)
                    _ -> fail "FAIL: parent construction changed its stable native unfolding"
            _ -> fail "FAIL: parent construction changed its binding"
        let nativeRuleHelper = local 336 "native_rule_helper" original
            nativeRuleRhs = Let (NonRec nativeRuleHelper (keep b 301)) trueExpr
            nativeRuleBinder = IfaceIdBndr (toIfaceType ManyTy, mkIfLclName (fsLit "native_rule_binder"), iface)
        rejectsIfaceRule "native interface rule RHS" [] (toIfaceExpr nativeRuleRhs)
        rejectsIfaceRule "native interface rule binder" [nativeRuleBinder] (toIfaceExpr trueExpr)
        rejectsDeferredIface "native interface dictionary operators" "Native dependent dictionary unfoldings are not supported" $ \importEnv -> do
            let name = mkExternalName (unique 337) ifaceModule (mkVarOcc "native_dictionary") noSrcSpan
                dfunType = mkTyConApp eqTyCon [liftedTypeKind, original, original]
                operators = map toIfaceExpr [Type liftedTypeKind, Type original, Type original, Coercion (mkNomReflCo original)]
                declaration = IfaceId name (toIfaceType dfunType) IfDFunId [HsUnfold False (IfDFunUnfold [] operators)]
            thing <-
                initIfaceLoad importEnv
                    $ initIfaceLcl ifaceModule (text "native dictionary test") NotBoot
                    $ tcIfaceDecl False declaration
            case thing of
                AnId imported -> case realIdUnfolding imported of
                    DFunUnfolding{df_args = arguments} -> evaluate arguments >> pure ()
                    _ -> fail "FAIL: interface dictionary unfolding was lost"
                _ -> fail "FAIL: interface dictionary did not reconstruct an Id"
        forM_ [NormalCompression, SafeExtraCompression, MaximumCompression] $ \compression -> do
            decoded <- roundtrip compression iface
            first <- restore [] decoded
            second <- restore [] decoded
            let label = "retained interface " ++ show compression
            checkType dflags (label ++ " (alpha equality)") original first
            checkType dflags (label ++ " (second reconstruction)") first second
            check (label ++ " (closed scope)") (isEmptyVarSet (tyCoVarsOfType first))
            case (original, first, second) of
                (DepFunTy old _, DepFunTy fresh body, DepFunTy next _) -> do
                    check (label ++ " (fresh binder)") (fresh /= old && next /= old && fresh /= next)
                    check (label ++ " (local runtime binder)") (isLocalId fresh && not (isCoVar fresh))
                    checkType dflags (label ++ " (Bool domain)") boolTy (varType fresh)
                    checkType dflags (label ++ " (Many multiplicity)") ManyTy (idMult fresh)
                    checkType dflags (label ++ " (shared occurrences)") (keepResult (TermVarTy fresh)) body
                    forM_ [falseDataCon, trueDataCon] $ \constructor ->
                        checkType
                            dflags
                            (label ++ " (constructor application)")
                            (keepResult (constructorIndex constructor))
                            (applyTypeToArgs first [Var (dataConWorkId constructor)])
                    let caller = local 302 "caller" boolTy
                    checkType
                        dflags
                        (label ++ " (caller index identity)")
                        (keepResult (TermVarTy caller))
                        (applyTypeToArgs first [Var caller])
                _ -> fail ("FAIL: " ++ label ++ "; retained binder was lost")
            let outer = local 303 "same" boolTy
                inner = local 304 "same" boolTy
                nested =
                    tidyTopType
                        $ DepFunTy outer
                        $ DepFunTy inner
                        $ mkFunctionType ManyTy (indexed (TermVarTy outer)) (indexed (TermVarTy inner))
            nestedIface <- roundtrip compression (toIfaceType nested)
            nestedResult <- restore [] nestedIface
            checkType dflags (label ++ " (nested alpha equality)") nested nestedResult
            case nestedResult of
                DepFunTy outer' (DepFunTy inner' body) -> do
                    check (label ++ " (nested distinct identities)") (outer' /= inner')
                    check (label ++ " (nested fresh identities)") (outer' /= outer && inner' /= inner)
                    checkType
                        dflags
                        (label ++ " (nested lexical references)")
                        (mkFunctionType ManyTy (indexed (TermVarTy outer')) (indexed (TermVarTy inner')))
                        body
                _ -> fail ("FAIL: " ++ label ++ "; nested retained binders were lost")
            let literal = LitTy (NumTyLit 7)
            literalIface <- roundtrip compression (toIfaceType literal)
            literalResult <- restore [] literalIface
            checkType dflags (label ++ " (ordinary literal tag)") literal literalResult
            let worker = keep b 301
            lintAccepts dflags (label ++ " (keep input)") [] worker
            workerIface <- roundtrip compression (toIfaceExpr worker)
            workerResult <- restoreExpr [] workerIface
            importedKeep <- importTemplate unlintedEnv iface workerIface
            lintAccepts dflags (label ++ " imported keep unfolding") [] importedKeep
            checkType dflags (label ++ " imported keep type") original (exprType importedKeep)
            indexOnlyIface <- roundtrip compression (toIfaceExpr indexOnlyBinding)
            importedIndexOnly <- importTemplate unlintedEnv (toIfaceType (exprType indexOnlyBinding)) indexOnlyIface
            lintAccepts dflags (label ++ " imported index-only binding") [] importedIndexOnly
            case importedIndexOnly of
                Lam _ (Let (NonRec _ _) _) -> pure ()
                _ -> fail ("FAIL: " ++ label ++ "; imported unfolding removed an index-only binding")
            lintAccepts dflags (label ++ " (keep reconstruction)") [] workerResult
            checkType dflags (label ++ " (keep expression type)") (exprType worker) (exprType workerResult)
            case workerResult of
                Lam fresh (Lam value (Var occurrence)) -> do
                    check (label ++ " (keep fresh binder)") (fresh /= b)
                    check (label ++ " (keep payload identity)") (value == occurrence)
                    checkType dflags (label ++ " (keep annotation identity)") (indexed (TermVarTy fresh)) (varType value)
                _ -> fail ("FAIL: " ++ label ++ "; keep expression shape changed")
            forM_ expressionCases $ \(description, expression, expectedProofs) -> do
                let expressionLabel = label ++ " (" ++ description ++ ")"
                lintAccepts dflags (expressionLabel ++ " input") [] expression
                let constructed = mkCoreUnfolding VanillaSrc True expression (Just cache) UnfNever
                lintAccepts dflags (expressionLabel ++ " constructed unfolding") [] (uf_tmpl constructed)
                check (expressionLabel ++ " supplied unfolding cache") (uf_cache constructed == cache)
                expressionIface <- roundtrip compression (toIfaceExpr expression)
                restored <- restoreExpr [] expressionIface
                restoredAgain <- restoreExpr [] expressionIface
                imported <- importTemplate unlintedEnv (toIfaceType (tidyTopType (exprType expression))) expressionIface
                lintAccepts dflags (expressionLabel ++ " imported unfolding") [] imported
                checkType dflags (expressionLabel ++ " imported unfolding type") (exprType expression) (exprType imported)
                lintAccepts dflags expressionLabel [] restored
                lintAccepts dflags (expressionLabel ++ " second reconstruction") [] restoredAgain
                checkType dflags (expressionLabel ++ " type") (exprType expression) (exprType restored)
                checkType dflags (expressionLabel ++ " second type") (exprType restored) (exprType restoredAgain)
                check (expressionLabel ++ " closed scope") (isEmptyVarSet (exprFreeVars restored))
                case (restored, restoredAgain) of
                    (Lam fresh (Lam freshPayload (Case scrut freshCase _ alternatives)), Lam next _) -> do
                        check (expressionLabel ++ " fresh runtime binder") (fresh /= b && fresh /= next)
                        check (expressionLabel ++ " fresh case binder") (freshCase /= caseBinder && freshCase /= fresh)
                        let proofs = [(con, cv, rhs) | Alt (DataAlt con) [cv] rhs <- alternatives]
                        check (expressionLabel ++ " proof count") (length proofs == expectedProofs)
                        case exprToBoolIndex scrut of
                            Nothing -> fail ("FAIL: " ++ expressionLabel ++ "; Boolean index was lost")
                            Just scrutIndex -> forM_ proofs $ \(constructor, cv, rhs) -> do
                                check (expressionLabel ++ " fresh local CoVar") (isCoVar cv && isLocalId cv && cv `notElem` oldProofs)
                                let co = mkCoVarCo cv
                                check (expressionLabel ++ " nominal proof") (coercionRole co == Nominal)
                                checkType dflags (expressionLabel ++ " proof index identity") scrutIndex (coercionLKind co)
                                checkType dflags (expressionLabel ++ " proof constructor") (constructorIndex constructor) (coercionRKind co)
                                case rhs of
                                    Cast (Lam _ (Var value)) transport -> do
                                        check (expressionLabel ++ " payload reference") (value == freshPayload)
                                        check (expressionLabel ++ " proof reference") (cv `elemVarSet` tyCoVarsOfCo transport)
                                    Var value -> check (expressionLabel ++ " payload reference") (value == freshPayload)
                                    _ -> fail ("FAIL: " ++ expressionLabel ++ "; branch shape changed")
                    _ -> fail ("FAIL: " ++ expressionLabel ++ "; case expression shape changed")
        case toIfaceExpr variableCase of
            IfaceLam retained (IfaceLam value (IfaceCase scrut caseName [falseAlt, IfaceAlt trueCon [proofName] rhs])) -> do
                let wrap scrutinee alternatives = IfaceLam retained (IfaceLam value (IfaceCase scrutinee caseName alternatives))
                    reject label fragment expression = do
                        decoded <- roundtrip NormalCompression expression
                        rejectsIfaceExpr label fragment [] decoded
                reject
                    "extra interface proof names"
                    "Native Boolean interface alternative must bind exactly one proof name"
                    (wrap scrut [falseAlt, IfaceAlt trueCon [proofName, proofName] rhs])
                reject
                    "unsupported interface proof scrutinee"
                    "Unsupported scrutinee for native Boolean interface evidence"
                    (wrap (IfaceApp (IfaceLam retained scrut) scrut) [falseAlt, IfaceAlt trueCon [proofName] rhs])
                reject
                    "incomplete interface proof case"
                    "Native Boolean interface proof alternatives must be exhaustive"
                    (wrap scrut [falseAlt])
                reject
                    "interface DEFAULT cannot bind proof names"
                    "Native index alias evidence has no interface codec"
                    (wrap scrut [IfaceAlt IfaceDefaultAlt [proofName] rhs, falseAlt, IfaceAlt trueCon [proofName] rhs])
                reject
                    "interface literal cannot bind proof names"
                    "Native Boolean interface literal alternative cannot bind names"
                    (wrap scrut [falseAlt, IfaceAlt trueCon [proofName] rhs, IfaceAlt (IfaceLitAlt (LitChar 'x')) [proofName] rhs])
                reject
                    "interface proof cannot escape another alternative"
                    "Iface id out of scope"
                    (wrap scrut [falseAlt, IfaceAlt trueCon [mkIfLclName (fsLit "differentProof")] rhs])
                reject
                    "missing interface proof occurrence"
                    "Iface id out of scope"
                    (wrap scrut [falseAlt, IfaceAlt trueCon [proofName] (IfaceCo (IfaceCoVarCo (mkIfLclName (fsLit "missingProof"))))])
                let payloadName = ifaceBndrName (fst value)
                    invalid = wrap scrut [falseAlt, IfaceAlt trueCon [proofName] (IfaceLcl payloadName)]
                    escapeBinder = IfaceIdBndr (toIfaceType ManyTy, mkIfLclName (fsLit "escape"), toIfaceType (indexed (TermVarTy caseBinder)))
                    escaping = wrap scrut [falseAlt, IfaceAlt trueCon [proofName] (IfaceLam (escapeBinder, snd value) (IfaceLcl payloadName))]
                reject
                    "interface result cannot mention the case binder"
                    "Escaping binder in native Boolean interface result"
                    escaping
                rejectsImportedUnfolding
                    "native import checks branch types without -dcore-lint"
                    "Invalid native unfolding in interface"
                    (toIfaceType (tidyTopType (exprType variableCase)))
                    invalid
                rejectsImportedUnfolding
                    "native import checks its declared type without -dcore-lint"
                    "Native interface unfolding does not match its declared type"
                    (toIfaceType intTy)
                    (toIfaceExpr variableCase)
                rejectsImportedUnfolding
                    "native signature checks an ordinary RHS without -dcore-lint"
                    "Native interface unfolding does not match its declared type"
                    iface
                    (toIfaceExpr (identity 335 intTy))
            _ -> fail "FAIL: native proof case interface shape changed"
        case iface of
            IfaceDepFunTy (multiplicity, name, domain) body -> do
                missing <- roundtrip NormalCompression (IfaceTermVar (mkIfLclName (fsLit "missing")))
                rejectsIface "missing interface index" "Iface id out of scope" [] missing
                let malformed label fragment binder = do
                        decoded <- roundtrip NormalCompression (IfaceDepFunTy binder body)
                        rejectsIface label fragment [] decoded
                malformed "non-Bool interface binder" "Invalid retained binder" (multiplicity, name, toIfaceType intTy)
                malformed "linear interface binder" "Invalid retained binder" (toIfaceType OneTy, name, domain)
                malformed "interface binder outside its own domain scope" "Iface id out of scope" (multiplicity, name, IfaceTermVar name)
            _ -> fail "FAIL: interface conversion did not retain the Boolean binder"
        let constraintResult = DepFunTy b (mkTyConApp eqTyCon [boolTy, TermVarTy b, TermVarTy b])
        constraintIface <- roundtrip NormalCompression (toIfaceType (tidyTopType constraintResult))
        rejectsIface "Constraint interface result" "Invalid retained function result" [] constraintIface
        let badType = local 305 "badType" intTy
            badMultiplicity = mkSysLocal (fsLit "badMultiplicity") (unique 306) OneTy boolTy
        forM_ [badType, badMultiplicity] $ \bad -> do
            decoded <- roundtrip NormalCompression (IfaceTermVar (mkIfLclName (occNameFS (getOccName bad))))
            rejectsIface "invalid local interface index" "Invalid retained index" [bad] decoded
        let diagnostic = toIfaceTypeX (unitVarSet b) (TermVarTy b)
        check "free interface index retains diagnostic identity" $ case diagnostic of
            IfaceFreeTermVar free -> free == b
            _ -> False
        rejectsWith "free interface index cannot be serialized" "Can't serialise IfaceFreeTermVar" (roundtrip NormalCompression diagnostic)
        rejectsIface "free interface index cannot be imported" "diagnostic-only retained index" [] diagnostic
        unknownWriter <- Binary.openBinMem 16
        Binary.putByte unknownWriter 255
        unknownReader <- Binary.shrinkBinBuffer unknownWriter
        unknown <- try @IOException (getIfaceType unknownReader)
        case unknown of
            Left exception -> check "unknown interface type tag diagnostic" ("invalid type tag 255" `isInfixOf` displayException exception)
            Right _ -> fail "FAIL: unknown interface type tag was accepted"

-- | Check rigid index matching and conservative constructor apartness.
runMatchingChecks :: DynFlags -> IO ()
runMatchingChecks dflags = do
    let b = local 200 "b" boolTy
        other = local 201 "other" boolTy
        index = TermVarTy b
        otherIndex = TermVarTy other
        template = mkTyVar (mkSystemName (unique 202) (mkTyVarOcc "template")) boolTy
        templateTy = mkTyVarTy template
        trueExpr = Var (dataConWorkId trueDataCon)
        trueIndex = constructorIndex trueDataCon
        falseIndex = constructorIndex falseDataCon
        knownTrue = setIdUnfolding b ((unfolding trueExpr){uf_cache = UnfoldingCache True True True True})
        fine lhs rhs = tcUnifyTysFG alwaysBindFam alwaysBindTv lhs rhs
        matches lhs rhs = case tcMatchTy lhs rhs of Just _ -> True; Nothing -> False
        maybeApart lhs rhs = case fine lhs rhs of MaybeApart{} -> True; _ -> False
    check "the same runtime index matches" (matches index index)
    check "distinct runtime indices do not match" (not (matches index otherIndex))
    check "index matching ignores Id unfolding metadata" (matches index (TermVarTy knownTrue))
    check "constructor templates cannot match an opaque runtime index" (not (matches trueIndex index))
    check "a known-True unfolding cannot match the True template" (not (matches trueIndex (TermVarTy knownTrue)))
    check "runtime index versus True is only maybe apart" (maybeApart [index] [trueIndex])
    check "True versus runtime index is only maybe apart" (maybeApart [trueIndex] [index])
    check "False versus runtime index is only maybe apart" (maybeApart [falseIndex] [index])
    check "a known-True unfolding remains only maybe apart from True" (maybeApart [trueIndex] [TermVarTy knownTrue])
    case tcMatchTy templateTy index >>= (`Subst.lookupTyVar` template) of
        Just actual -> checkType dflags "a Bool template variable can bind a runtime index" index actual
        Nothing -> fail "FAIL: a Bool template variable did not bind the runtime index"
    check
        "a repeated template cannot match distinct runtime indices"
        (case tcMatchTys [templateTy, templateTy] [index, otherIndex] of Nothing -> True; Just _ -> False)
    check
        "distinct targets of a repeated template are only maybe apart"
        (maybeApart [templateTy, templateTy] [index, otherIndex])
    check
        "a repeated template matches one shared runtime index"
        (case tcMatchTys [templateTy, templateTy] [index, index] of Just _ -> True; Nothing -> False)

    let whole = mkTyVar (mkSystemName (unique 203) (mkTyVarOcc "whole")) liftedTypeKind
        wholeTy = mkTyVarTy whole
        closedPi = mkDepFunTy b (indexed index)
        alphaPi = mkDepFunTy other (indexed otherIndex)
        differentPi = mkDepFunTy other (mkFunctionType ManyTy (indexed otherIndex) boolTy)
        occursPi = mkDepFunTy b (mkFunctionType ManyTy (indexed index) wholeTy)
        retainedApart lhs rhs = case fine lhs rhs of
            MaybeApart MARRetainedFunction _ -> True
            _ -> False
    case tcMatchTy wholeTy closedPi >>= (`Subst.lookupTyVar` whole) of
        Just actual -> checkType dflags "a whole-type template matches a closed retained function" closedPi actual
        Nothing -> fail "FAIL: a whole-type template did not match a closed retained function"
    case fine [wholeTy] [closedPi] of
        Unifiable subst -> case Subst.lookupTyVar subst whole of
            Just actual -> checkType dflags "fine unification binds a whole-type template to a retained function" closedPi actual
            Nothing -> fail "FAIL: fine unification omitted the retained function substitution"
        _ -> fail "FAIL: fine unification did not bind a whole-type template to a retained function"
    check "an occurs check traverses a retained function body" $ case fine [wholeTy] [occursPi] of
        MaybeApart MARInfinite _ -> True
        _ -> False
    check
        "a repeated whole-type template matches alpha-equivalent retained functions"
        (case tcMatchTys [wholeTy, wholeTy] [closedPi, alphaPi] of Just _ -> True; Nothing -> False)
    check
        "fine repeated-template unification accepts alpha-equivalent retained functions"
        (case fine [wholeTy, wholeTy] [closedPi, alphaPi] of Unifiable _ -> True; _ -> False)
    check
        "a repeated whole-type template cannot match different retained functions"
        (case tcMatchTys [wholeTy, wholeTy] [closedPi, differentPi] of Nothing -> True; Just _ -> False)
    check
        "different retained-function targets of a repeated template are only maybe apart"
        (retainedApart [wholeTy, wholeTy] [closedPi, differentPi])
    check "a constructor type versus a retained function is only maybe apart" (retainedApart [intTy] [closedPi])
    check "a retained function versus a constructor type is only maybe apart" (retainedApart [closedPi] [intTy])

    let outer = mkTyVar (mkSystemName (unique 204) (mkTyVarOcc "outer")) liftedTypeKind
        inner = mkTyVar (mkSystemName (unique 205) (mkTyVarOcc "inner")) liftedTypeKind
        captureTemplate = mkForAllTy (Bndr outer Specified) wholeTy
        captureTarget =
            mkForAllTy (Bndr inner Specified) $
                mkDepFunTy b (mkFunctionType ManyTy (mkTyVarTy inner) (indexed index))
    check
        "a whole-type template cannot capture a forall variable through a retained function"
        (not (matches captureTemplate captureTarget))

    let payloadTemplate = mkTyVar (mkSystemName (unique 206) (mkTyVarOcc "payload")) liftedTypeKind
        rigidPayload = mkTyVar (mkSystemName (unique 207) (mkTyVarOcc "rigid")) liftedTypeKind
        payloadTy = mkTyVarTy payloadTemplate
        rigidTy = mkTyVarTy rigidPayload
        allowed = unitVarSet payloadTemplate
        (nat, _, _) = peanoNat
        n = local 208 "n" (mkTyConTy nat)
        m = local 209 "m" (mkTyConTy nat)
        at payload var =
            mkFunctionType ManyTy (mkTyConApp eqTyCon [varType var, TermVarTy var, TermVarTy var]) payload
        arrow = mkFunctionType ManyTy
        telescope var payload = mkDepFunTy var (arrow (at payload var) (at payload var))
        openPi = telescope b payloadTy
        intPi = telescope other intTy
        nested var inner payload = mkDepFunTy var (mkDepFunTy inner (arrow (at payload var) (arrow (at payload inner) payload)))
        openNested = nested b n payloadTy
        intNested = nested other m intTy
        bindAllowed var _ = if var `elemVarSet` allowed then BindMe else DontBindMe
        initial lhs rhs = Subst.mkEmptySubst (mkInScopeSet (tyCoVarsOfType lhs `unionVarSet` tyCoVarsOfType rhs))
        scoped lhs rhs = tcMatchRetainedTyX_BM bindAllowed (initial lhs rhs) lhs rhs
        defaultMatch lhs rhs = tcMatchTyX_BM bindAllowed (initial lhs rhs) lhs rhs
        rejectsScoped label lhs rhs = check label $ case scoped lhs rhs of Nothing -> True; Just _ -> False
        expectPayload label lhs rhs = case scoped lhs rhs of
            Nothing -> fail ("FAIL: " ++ label ++ "; scoped retained matching failed")
            Just subst -> do
                case Subst.lookupTyVar subst payloadTemplate of
                    Nothing -> fail ("FAIL: " ++ label ++ "; payload substitution missing")
                    Just actual -> checkType dflags (label ++ " (payload)") intTy actual
                check (label ++ " (only the allowed template binds)") $
                    sizeUFM (Subst.getTvSubstEnv subst) == 1
                        && isEmptyVarEnv (Subst.getIdSubstEnv subst)
                        && isEmptyVarEnv (Subst.getCvSubstEnv subst)
                checkType dflags (label ++ " (instantiated telescope)") rhs (Subst.substTy subst lhs)
                check (label ++ " (no bound index escapes)") (isEmptyVarSet (Subst.getSubstRangeTyCoFVs subst))
    expectPayload "scoped matching infers an erased payload under alpha-renamed native binders" openPi intPi
    expectPayload "scoped matching infers one payload under nested Bool and Nat binders" openNested intNested
    let rigidTemplate = mkDepFunTy b (arrow (at payloadTy b) (at rigidTy b))
        rigidTarget = mkDepFunTy other (arrow (at intTy other) (at rigidTy other))
        changedRigid = mkDepFunTy other (arrow (at intTy other) (at boolTy other))
        disagreeingPayload = mkDepFunTy other (arrow (at intTy other) (at boolTy other))
        boundFreeTarget = mkDepFunTy other (arrow (at intTy other) (at intTy b))
        capturingTemplate = mkDepFunTy b (arrow (at payloadTy b) payloadTy)
        localPayload = at intTy other
        capturingTarget = mkDepFunTy other (arrow (at localPayload other) localPayload)
    expectPayload "scoped matching preserves an unbindable outer payload variable" rigidTemplate rigidTarget
    rejectsScoped "scoped matching cannot bind a rigid outer template variable" rigidTemplate changedRigid
    rejectsScoped "scoped matching rejects repeated payload disagreement" openPi disagreeingPayload
    rejectsScoped "scoped matching rejects different runtime domains" openPi (telescope m intTy)
    rejectsScoped "scoped matching distinguishes a bound index from the same free Id" openPi boundFreeTarget
    rejectsScoped "an outer payload cannot capture an inner runtime index" capturingTemplate capturingTarget
    let freeShared = mkDepFunTy b (arrow (indexed index) (indexed otherIndex))
        boundShared = mkDepFunTy other (arrow (indexed otherIndex) (indexed otherIndex))
    check "empty-scope retained matching freshens a target binder shared with a free template Id" $
        case tcMatchRetainedTyX_BM bindAllowed Subst.emptySubst freeShared boundShared of
            Nothing -> True
            Just _ -> False
    check "default explicit-template matching still rejects an open retained telescope" $ case defaultMatch openPi intPi of
        Nothing -> True
        Just _ -> False
    check "fine unification remains conservative for an open retained telescope" (retainedApart [openPi] [intPi])
    forM_ [("default", defaultMatch), ("scoped", scoped)] $ \(label, match) ->
        check (label ++ " matching preserves closed retained alpha reflexivity") $ case match closedPi alphaPi of
            Just subst -> Subst.isEmptySubst subst
            Nothing -> False

-- | Check retained syntax, printing, and compatibility with ordinary identifiers.
runSyntaxChecks :: DynFlags -> IO ()
runSyntaxChecks dflags = do
    logger <- initLogger
    (nativeFlags, leftovers, _) <-
        parseDynamicFlagsCmdLine logger dflags [noLoc "-XExperimentalDependentTypes"]
    check "retained extension flag" (null leftovers)
    let source = "foreach (b :: Bool) -> Select b -> Select b"
        parseWith flags input =
            case Lexer.unP Parser.parseType (Lexer.initParserState (initParserOpts flags) (stringToStringBuffer input) (mkRealSrcLoc (fsLit "retained-syntax") 1 1)) of
                Lexer.POk _ ty -> pure ty
                Lexer.PFailed _ -> fail ("FAIL: cannot parse type: " ++ input)
        isRetained ty = case unLoc ty of
            HsForEachTy{hst_bndrs = [_]} -> True
            _ -> False
    retained <- parseWith nativeFlags source
    check "retained quantification has a distinct AST node" (isRetained retained)
    reparsed <- parseWith nativeFlags (showSDoc nativeFlags (ppr retained))
    check "retained quantification printing round trip" (isRetained reparsed)
    check "retained binders reject inferred braces during parsing" $
        case Lexer.unP Parser.parseType (Lexer.initParserState (initParserOpts nativeFlags) (stringToStringBuffer "foreach {b} -> Bool") (mkRealSrcLoc (fsLit "retained-syntax") 1 1)) of
            Lexer.PFailed _ -> True
            Lexer.POk{} -> False
    ordinary <- parseWith dflags source
    check "foreach remains an identifier when disabled" $ case unLoc ordinary of
        HsFunTy{} -> True
        _ -> False
    visible <- parseWith nativeFlags "forall a -> a -> a"
    check "visible forall remains erased quantification" $ case unLoc visible of
        HsForAllTy{} -> True
        _ -> False

-- | Check retained congruence with a family equation and scoped nominal evidence.
runDepFunCoChecks :: DynFlags -> IO ()
runDepFunCoChecks dflags = do
    let b = local 700 "co_b" boolTy
        free = local 701 "co_free" boolTy
        other = local 702 "co_other" boolTy
        equation = mkAxInstCo Nominal (BranchedAxiom (snd selectFamily) 0) [] []
        concrete = selected (constructorIndex trueDataCon)
        cv = proof 703 concrete intTy
        argument = mkFunctionType ManyTy (indexed (TermVarTy b)) (indexed (TermVarTy free))
        body = mkFunCoNoFTF Nominal (mkNomReflCo ManyTy) (mkNomReflCo argument) (mkCoVarCo cv)
        raw = DepFunCo b body
        lifted = mkDepFunCo b body
        left = mkDepFunTy b (mkFunctionType ManyTy argument concrete)
        right = mkDepFunTy b (mkFunctionType ManyTy argument intTy)
    check "retained coercion inherits nominal role" (coercionRole lifted == Nominal)
    checkType dflags "retained coercion left endpoint" left (coercionLKind lifted)
    checkType dflags "retained coercion right endpoint" right (coercionRKind lifted)
    lintAccepts dflags "raw retained coercion scopes its index" [free, cv] (Coercion raw)
    lintAccepts dflags "smart retained coercion scopes its index" [free, cv] (Coercion lifted)
    check "retained coercion removes its bound index from free variables" (not (b `elemVarSet` tyCoVarsOfCo raw))
    check "retained coercion preserves a free runtime index" (free `elemVarSet` tyCoVarsOfCo raw)
    check "retained coercion preserves nominal evidence dependencies" (cv `elemVarSet` tyCoVarsOfCo raw)
    check "scanner detects retained coercion syntax" (hasNativeDependentExpr (Coercion raw))
    let representational = mkDepFunCo b (mkSubCo body)
        phantom = DepFunCo b (mkReflCo Phantom (coercionLKind body))
    check "retained coercion inherits representational role" (coercionRole representational == Representational)
    checkType dflags "representational retained coercion source" left (coercionLKind representational)
    checkType dflags "representational retained coercion target" right (coercionRKind representational)
    lintAccepts dflags "representational retained congruence" [free, cv] (Coercion representational)
    check "retained coercion inherits phantom role" (coercionRole phantom == Phantom)
    lintAccepts dflags "phantom retained reflexivity" [free] (Coercion phantom)
    let renamedBody = Subst.substCo (indexSubst [other, free, cv] b (Var other)) body
        renamed = DepFunCo other renamedBody
    check "retained coercion alpha equality" (eqCoercion raw renamed)
    lintAccepts dflags "alpha-renamed retained coercion" [free, cv] (Coercion renamed)
    check
        "index substitution respects a retained coercion binder"
        (eqCoercion raw (Subst.substCo (indexSubst [free, cv] b (Var (dataConWorkId trueDataCon))) raw))
    let captured = Subst.substCo (indexSubst [b, cv] free (Var b)) raw
    case captured of
        DepFunCo fresh _ -> do
            check "coercion substitution freshens a capturing runtime binder" (fresh /= b)
            let expectedArgument = mkFunctionType ManyTy (indexed (TermVarTy fresh)) (indexed (TermVarTy b))
            checkType
                dflags
                "coercion substitution keeps free and bound indices distinct"
                (mkDepFunTy fresh (mkFunctionType ManyTy expectedArgument concrete))
                (coercionLKind captured)
            check "coercion substitution retains the free replacement" (b `elemVarSet` tyCoVarsOfCo captured)
            check "coercion substitution removes the renamed binder" (not (fresh `elemVarSet` tyCoVarsOfCo captured))
            lintAccepts dflags "capture-avoiding retained coercion substitution" [b, cv] (Coercion captured)
        _ -> fail "FAIL: retained coercion substitution lost its binder"
    let parameter = mkTyVar (mkSystemName (unique 704) (mkTyVarOcc "ignored")) boolTy
        binders = [mkAnonTyConBinder parameter]
        forgetful =
            mkSynonymTyCon
                (mkSystemName (unique 705) (mkTcOcc "ForgetIndex"))
                (mkTyConKind binders liftedTypeKind)
                binders
                liftedTypeKind
                [Phantom]
                concrete
                True
                False
                True
                False
        forgotten = mkTyConApp forgetful [TermVarTy b]
        forgetting = TransCo (Refl forgotten) equation
        disappearing = mkDepFunCo b forgetting
    checkType dflags "one-sided retained endpoint preserves syntactic dependence" (DepFunTy b forgotten) (coercionLKind disappearing)
    checkType dflags "one-sided retained endpoint canonicalizes the ordinary arrow" (mkFunctionType ManyTy boolTy intTy) (coercionRKind disappearing)
    lintAccepts dflags "a family equation removes one endpoint's retained dependence" [] (Coercion disappearing)
    let proofOnly = mkDepFunCo b (TransCo (SymCo forgetting) forgetting)
        ordinaryEndpoint = mkFunctionType ManyTy boolTy intTy
    check "proof-only dependence keeps the retained coercion binder" $ case proofOnly of
        DepFunCo{} -> True
        _ -> False
    checkType dflags "proof-only retained coercion left endpoint" ordinaryEndpoint (coercionLKind proofOnly)
    checkType dflags "proof-only retained coercion right endpoint" ordinaryEndpoint (coercionRKind proofOnly)
    check "proof-only retained coercion removes its bound index from free variables" (not (b `elemVarSet` tyCoVarsOfCo proofOnly))
    lintAccepts dflags "retained binder scopes an intermediate proof-only dependency" [] (Coercion proofOnly)
    lintAccepts dflags "Pi construction scopes an intermediate proof-only dependency" [] $
        Coercion (mkPiCo Nominal b (TransCo (SymCo forgetting) forgetting))
    check "scanner detects a coercion with ordinary endpoint types" (hasNativeDependentExpr (Coercion proofOnly))
    check "retained coercion diagnostics print its runtime binder" ("foreach" `isInfixOf` showSDoc dflags (ppr (toIfaceCoercion lifted)))
    writer <- Binary.openBinMem 16
    rejectsWith
        "retained coercion diagnostic form cannot enter interface files"
        "Native dependent coercion interfaces are not implemented"
        (Binary.put_ writer (toIfaceCoercion lifted))
    case mkDepFunCo b (mkNomReflCo (indexed (TermVarTy b))) of
        Refl ty -> checkType dflags "reflexive retained coercion simplifies" (mkDepFunTy b (indexed (TermVarTy b))) ty
        _ -> fail "FAIL: reflexive retained coercion did not simplify"
    let nat = local 706 "co_nat" (mkTyConTy (let (tc, _, _) = peanoNat in tc))
        erased = mkTyVar (mkSystemName (unique 707) (mkTyVarOcc "co_erased")) boolTy
        wrongDomain = local 708 "co_int" intTy
        linear = setIdMult b OneTy
    lintAccepts dflags "retained Nat coercion binder" [] (Coercion (DepFunCo nat (mkNomReflCo intTy)))
    forM_ [("erased variable", erased), ("coercion variable", cv), ("unsupported domain", wrongDomain), ("linear runtime binder", linear)] $ \(label, binder) ->
        lintRejects dflags ("retained coercion rejects " ++ label) [] (Coercion (DepFunCo binder (mkNomReflCo intTy)))
    lintRejects dflags "retained coercion rejects unlifted results" [] (Coercion (DepFunCo b (mkNomReflCo intPrimTy)))
    forM_ [("left", proof 709 intPrimTy intTy), ("right", proof 710 intTy intPrimTy)] $ \(side, evidence) -> do
        lintAccepts dflags ("ordinary heterogeneous " ++ side ++ " endpoint evidence") [evidence] (Coercion (mkCoVarCo evidence))
        lintRejects dflags ("retained coercion rejects an unlifted " ++ side ++ " result") [evidence] (Coercion (DepFunCo b (mkCoVarCo evidence)))
    lintRejects dflags "retained coercion rejects an out-of-scope runtime dependency" [cv] (Coercion raw)
    lintRejects dflags "valid raw retained coercion cannot escape a let-bound index" [cv] $
        Let (NonRec free (Var (dataConWorkId trueDataCon))) (Coercion raw)

-- | Compare recovered types and print both sides on failure.
checkType :: DynFlags -> String -> Type -> Type -> IO ()
checkType dflags label expected actual = do
    equal <- evaluate (eqType expected actual)
    unless equal $
        fail
            ( "FAIL: "
                ++ label
                ++ "\nexpected: "
                ++ showSDoc dflags (ppr expected)
                ++ "\nactual: "
                ++ showSDoc dflags (ppr actual)
            )

-- | Require Core Lint to accept an expression without an exception.
lintAccepts :: DynFlags -> String -> [Var] -> CoreExpr -> IO ()
lintAccepts dflags label scope expr = do
    result <- try @SomeException (evaluate (lintExpr (initLintConfig dflags scope) expr))
    case result of
        Left err -> fail ("FAIL: " ++ label ++ "; Lint threw: " ++ displayException err)
        Right Nothing -> pure ()
        Right (Just errors) ->
            fail
                ( "FAIL: "
                    ++ label
                    ++ "; Lint rejected:\n"
                    ++ showSDoc dflags (vcat (bagToList errors))
                )

-- | Require a Lint error, rather than a panic, for malformed Core.
lintRejects :: DynFlags -> String -> [Var] -> CoreExpr -> IO ()
lintRejects dflags label scope expr = do
    result <- try @SomeException (evaluate (lintExpr (initLintConfig dflags scope) expr))
    case result of
        Left err -> fail ("FAIL: " ++ label ++ "; Lint panicked instead of rejecting: " ++ displayException err)
        Right Nothing -> fail ("FAIL: " ++ label ++ "; malformed Core passed Lint")
        Right (Just _) -> pure ()

-- | Require an explicit rejection with the expected diagnostic fragment.
rejectsWith :: String -> String -> IO a -> IO ()
rejectsWith label fragment action = do
    result <- try @SomeException (action >> pure ())
    case result of
        Left err -> check (label ++ " (diagnostic)") (fragment `isInfixOf` displayException err)
        Right () -> fail ("FAIL: " ++ label ++ "; unsupported operation succeeded")

-- | Require an unsupported-operation diagnostic with its compiler phase.
rejectsUnsupported :: String -> String -> IO () -> IO ()
rejectsUnsupported label context action = do
    result <- try @GhcException action
    case result of
        Left (Sorry message) -> do
            check (label ++ " (phase)") (context `isInfixOf` message)
            check (label ++ " (restriction)") ("This operation does not support" `isInfixOf` message)
        Left exception -> fail ("FAIL: " ++ label ++ "; wrong exception: " ++ displayException exception)
        Right () -> fail ("FAIL: " ++ label ++ "; unsupported operation succeeded")

-- | Check simplification against the original type and both Lint boundaries.
simplifyAccepts :: DynFlags -> String -> [Var] -> CoreExpr -> IO CoreExpr
simplifyAccepts dflags label scope expression = do
    lintAccepts dflags (label ++ " (before)") scope expression
    let simplified = simpleOptNativeExpr expression
    lintAccepts dflags (label ++ " (after)") scope simplified
    checkType dflags label (exprType expression) (exprType simplified)
    pure simplified

-- | Make an unfolding without invoking the unsupported optimizer.
unfolding :: CoreExpr -> Unfolding
unfolding body =
    CoreUnfolding
        { uf_tmpl = body
        , uf_src = VanillaSrc
        , uf_is_top = False
        , uf_cache = UnfoldingCache False False False False
        , uf_guidance = UnfNever
        }

-- | Check the first native Core increment without executing native code.
runChecks :: DynFlags -> IO ()
runChecks dflags = do
    let b = local 1 "b" boolTy
        c = local 2 "c" boolTy
        x = local 3 "x" (indexed (TermVarTy b))
        n = local 4 "n" intTy
        worker = keep b 5
        workerTy = mkDepFunTy b (keepResult (TermVarTy b))
        trueExpr = Var (dataConWorkId trueDataCon)
        falseExpr = Var (dataConWorkId falseDataCon)
        trueIndex = mkTyConTy (promoteDataCon trueDataCon)
        falseIndex = mkTyConTy (promoteDataCon falseDataCon)
        substituteTrue = indexSubst [] b trueExpr
        substituteFalse = indexSubst [] b falseExpr

    lintAccepts dflags "retained keep lambda" [] worker
    checkType dflags "keep lambda type" workerTy (exprType worker)
    lintAccepts dflags "keep True" [] (App worker trueExpr)
    lintAccepts dflags "keep False" [] (App worker falseExpr)
    checkType dflags "True application substitutes the constructor" (keepResult trueIndex) (exprType (App worker trueExpr))
    checkType dflags "False application substitutes the constructor" (keepResult falseIndex) (exprType (App worker falseExpr))
    optimizedTrue <- simplifyAccepts dflags "keep True beta reduction" [] (App worker trueExpr)
    optimizedFalse <- simplifyAccepts dflags "keep False beta reduction" [] (App worker falseExpr)
    optimizedVariable <- simplifyAccepts dflags "keep variable beta reduction" [c] (App worker (Var c))
    optimizedShadow <- simplifyAccepts dflags "keep beta reduction avoids capture" [b] (App worker (Var b))
    let isIdentity (Lam arg (Var result)) = arg == result
        isIdentity _ = False
    check "keep True removes the retained beta redex" (isIdentity optimizedTrue)
    check "keep False removes the retained beta redex" (isIdentity optimizedFalse)
    check "keep variable removes the retained beta redex" (isIdentity optimizedVariable)
    check "keep shadowing removes the retained beta redex" (isIdentity optimizedShadow)
    let g = local 80 "g" workerTy
        f = setIdOccInfo (local 81 "f" workerTy) (ManyOccs (AlwaysTailCalled 1))
        boundWorker = Let (NonRec g worker) (Let (NonRec f (Var g)) (App (Var f) trueExpr))
    optimizedBound <- simplifyAccepts dflags "native simplification preserves ordinary bindings" [] boundWorker
    let noTemplate binder = case realIdUnfolding binder of
            CoreUnfolding{} -> False
            DFunUnfolding{} -> False
            _ -> null (idCoreRules binder)
    check
        "native simplification creates neither join points nor unfolding templates"
        ( case optimizedBound of
            Let (NonRec g' _) (Let (NonRec f' _) _) ->
                not (isJoinId g' || isJoinId f') && noTemplate g' && noTemplate f'
            _ -> False
        )
    checkType dflags "True Id-only type substitution" (indexed trueIndex) (Subst.substTy substituteTrue (indexed (TermVarTy b)))
    checkType dflags "False Id-only type substitution" (indexed falseIndex) (Subst.substTy substituteFalse (indexed (TermVarTy b)))
    let toC = indexSubst [c] b (Var c)
        cToTrue = indexSubst [] c trueExpr
        composed = Subst.composeTCvSubst cToTrue toC
    checkType dflags "composition substitutes a Boolean variable range" trueIndex (Subst.substTy composed (TermVarTy b))
    checkType
        dflags
        "composition preserves an opaque variable range"
        (TermVarTy c)
        (Subst.substTy (Subst.composeTCvSubst Subst.emptySubst toC) (TermVarTy b))
    checkType
        dflags
        "composition preserves a constructor range"
        falseIndex
        (Subst.substTy (Subst.composeTCvSubst cToTrue substituteFalse) (TermVarTy b))
    let both = Subst.Subst (mkInScopeSet (mkVarSet [c])) (mkVarEnv [(b, falseExpr), (c, trueExpr)]) emptyVarEnv emptyVarEnv
    checkType
        dflags
        "composition uses the second environment on overlapping domains"
        trueIndex
        (Subst.substTy (Subst.composeTCvSubst both toC) (TermVarTy b))
    checkType
        dflags
        "composition keeps replacements apply-once"
        (TermVarTy c)
        (Subst.substTy (Subst.composeTCvSubst toC cToTrue) (TermVarTy b))
    let ordinaryExprSubst = indexSubst [] b (Let (NonRec c trueExpr) (Var c))
    checkType
        dflags
        "ordinary expression environments remain valid in type composition"
        intTy
        (Subst.substTy (Subst.composeTCvSubst Subst.emptySubst ordinaryExprSubst) intTy)

    let sharedX = local 6 "sharedX" (indexed (TermVarTy c))
        sharedCall value = App (App worker (Var c)) value
        sharedWorker = Lam c (Lam sharedX (sharedCall (sharedCall (Var sharedX))))
    lintAccepts dflags "two dependent calls share an unknown runtime index" [] sharedWorker
    checkType dflags "shared runtime variable instantiation" (keepResult (TermVarTy c)) (exprType (App worker (Var c)))
    lintAccepts dflags "unknown runtime variable application" [c] (App worker (Var c))
    check "alpha-equivalent retained functions" (eqType workerTy (exprType (keep c 7)))

    let sharedBody = mkFunctionType ManyTy (indexed (TermVarTy b)) (indexed (TermVarTy c))
        bindsB = DepFunTy b sharedBody
        bindsC = DepFunTy c sharedBody
    check "shared body pointer does not equate bound and free indices" (not (eqType bindsB bindsC))
    check "type ordering distinguishes bound and free indices" (nonDetCmpType bindsB bindsC /= EQ)

    let lambdaX = Lam x (Var x)
        rewrittenLambda = CoreSubst.substExpr substituteTrue lambdaX
    lintAccepts dflags "Id-only substitution updates lambda annotations" [] rewrittenLambda
    checkType dflags "Id-only substituted lambda type" (keepResult trueIndex) (exprType rewrittenLambda)
    let (_, clonedX) = CoreSubst.cloneBndr substituteTrue (unique 8) x
    check "cloneBndr gives a fresh identity" (clonedX /= x)
    checkType dflags "cloneBndr substitutes the binder type" (indexed trueIndex) (varType clonedX)
    let (cloningSubst, clonedB) = CoreSubst.cloneBndr (Subst.mkEmptySubst (mkInScopeSet (mkVarSet [b]))) (unique 9) b
        clonedLambda = CoreSubst.substExpr cloningSubst lambdaX
    lintAccepts dflags "cloning a runtime binder updates dependent annotations" [clonedB] clonedLambda
    checkType dflags "runtime clone retains its own index" (keepResult (TermVarTy clonedB)) (exprType clonedLambda)

    let captureSubst = indexSubst [c] b (Var c)
        captured = Subst.substTy captureSubst bindsC
    case captured of
        DepFunTy renamed body -> do
            check "substitution renames a capturing binder" (renamed /= c)
            checkType
                dflags
                "substitution preserves free and bound index identities"
                (mkFunctionType ManyTy (indexed (TermVarTy c)) (indexed (TermVarTy renamed)))
                body
            check "capture avoidance retains the free replacement" (c `elemVarSet` tyCoVarsOfType captured)
            check "capture avoidance removes the renamed bound index" (not (renamed `elemVarSet` tyCoVarsOfType captured))
        _ -> fail "FAIL: capture avoidance lost the retained binder"
    let missingRange = indexSubst [] b (Var c)
    check "missing substitution range fails the scope invariant" (not (Subst.isValidTCvSubst missingRange))
    rejectsWith "unsupported expression substitution" "unsupported runtime index" $
        evaluate (Subst.substTy (indexSubst [] b (Let (NonRec c trueExpr) (Var c))) (TermVarTy b))
    rejectsWith "composition rejects an unsupported index range" "unsupported runtime index" $
        evaluate
            ( Subst.substTy
                (Subst.composeTCvSubst (indexSubst [] c (Let (NonRec b trueExpr) (Var b))) toC)
                (TermVarTy b)
            )

    checkType
        dflags
        "vacuous retained binder canonicalizes to an arrow"
        (mkFunctionType ManyTy boolTy intTy)
        (mkDepFunTy b intTy)
    lintRejects dflags "raw vacuous retained type is rejected" [] (identity 10 (DepFunTy b intTy))
    lintRejects dflags "wrong retained argument type is rejected" [n] (App worker (Var n))
    lintRejects dflags "non-Boolean index is rejected" [n] (identity 11 (indexed (TermVarTy n)))
    lintRejects dflags "out-of-scope index is rejected" [] lambdaX
    lintRejects dflags "non-Boolean retained binder is rejected" [] (identity 12 (DepFunTy n (indexed (TermVarTy n))))
    lintRejects dflags "non-Type dependent codomain is rejected" [] (identity 13 (DepFunTy b (TermVarTy b)))
    lintRejects dflags "Constraint dependent codomain is rejected" [] (identity 307 (DepFunTy b (mkTyConApp eqTyCon [boolTy, TermVarTy b, TermVarTy b])))
    lintRejects dflags "primitive equality dependent codomain is rejected" [] (identity 308 (DepFunTy b (mkNomEqPred (TermVarTy b) (TermVarTy b))))
    let erased = mkTyVar (mkSystemName (unique 14) (mkTyVarOcc "erased")) boolTy
        forged = local 14 "forged" boolTy
    lintRejects dflags "type-variable binding cannot supply a runtime index" [erased] (identity 15 (indexed (TermVarTy forged)))
    let linear = setIdMult (local 16 "linear" boolTy) OneTy
        forgedMany = setIdMult linear ManyTy
    lintRejects dflags "linear binding site cannot supply a Many index" [linear] (identity 17 (indexed (TermVarTy forgedMany)))
    let unsupportedArg = Let (NonRec c trueExpr) (Var c)
    lintRejects dflags "unsupported Boolean producer is rejected by Lint" [] (App worker unsupportedArg)

    let dict = local 18 "dict" (mkTyConApp eqTyCon [boolTy, TermVarTy b, TermVarTy b])
        escaping = Let (NonRec b trueExpr) (Let (NonRec x (Lam dict (Var n))) (Var x))
    lintRejects dflags "let-bound runtime index cannot escape" [n] escaping
    rejectsWith "type recovery rejects let-index escape" "let-bound runtime index escapes" (evaluate (exprType escaping))
    checkType
        dflags
        "reflexive dependent coercion lifting retains the binder"
        (mkDepFunTy b (indexed (TermVarTy b)))
        (coercionLKind (mkPiCo Nominal b (mkNomReflCo (indexed (TermVarTy b)))))

    check "scanner detects a closed retained function" (hasNativeDependentExpr worker)
    check "scanner detects an isolated native type" (hasNativeDependentExpr (Type workerTy))
    check "scanner detects a coercion annotation" (hasNativeDependentExpr (Coercion (mkNomReflCo workerTy)))
    check "scanner accepts ordinary Core" (not (hasNativeDependentExpr (Lam b (Var b))))
    let hiddenSynonym =
            mkSynonymTyCon
                (mkSystemName (unique 70) (mkTcOcc "HiddenKeep"))
                liftedTypeKind
                []
                liftedTypeKind
                []
                workerTy
                True
                False
                False
                False
        hiddenType = mkTyConTy hiddenSynonym
        unusedParameter = mkTyVar (mkSystemName (unique 71) (mkTyVarOcc "unused")) liftedTypeKind
        unusedBinders = [mkAnonTyConBinder unusedParameter]
        unusedSynonym =
            mkSynonymTyCon
                (mkSystemName (unique 72) (mkTcOcc "Ignore"))
                (mkTyConKind unusedBinders liftedTypeKind)
                unusedBinders
                liftedTypeKind
                [Phantom]
                intTy
                True
                True
                True
                True
        ignoredType = mkTyConApp unusedSynonym [workerTy]
        ignoredHiddenType = mkTyConApp unusedSynonym [hiddenType]
        identityParameter = mkTyVar (mkSystemName (unique 73) (mkTyVarOcc "identity")) liftedTypeKind
        identityBinders = [mkAnonTyConBinder identityParameter]
        identitySynonym =
            mkSynonymTyCon
                (mkSystemName (unique 74) (mkTcOcc "Identity"))
                (mkTyConKind identityBinders liftedTypeKind)
                identityBinders
                liftedTypeKind
                [Representational]
                (mkTyVarTy identityParameter)
                True
                True
                False
                False
        nestedOrdinary = iterate (mkTyConApp identitySynonym . (: [])) boolTy !! 12
    check "scanner expands a hidden retained function synonym" (hasNativeDependentExpr (Type hiddenType))
    check "scanner expands synonyms inside ordinary types" (hasNativeDependentExpr (Type (mkVisFunTyMany intTy hiddenType)))
    check "scanner expands synonyms inside coercions" (hasNativeDependentExpr (Coercion (mkNomReflCo hiddenType)))
    check "scanner preserves unused native synonym arguments" (hasNativeDependentExpr (Type ignoredType))
    check "scanner expands synonyms in unused synonym arguments" (hasNativeDependentExpr (Type ignoredHiddenType))
    check "scanner accepts an ordinary synonym" (not (hasNativeDependentExpr (Type (mkTyConApp unusedSynonym [boolTy]))))
    nestedResult <- timeout 2000000 (evaluate (hasNativeDependentExpr (Type nestedOrdinary)))
    check "scanner terminates on nested ordinary identity synonyms" (nestedResult == Just False)
    rejectsWith "hidden synonym expression boundary gate" "Native dependent Core cannot enter" $
        rejectNativeDependentExpr "synonym smoke" (Type hiddenType)
    let host = local 19 "host" boolTy
        nativeUnfoldingHost = setIdUnfolding host (unfolding (Type workerTy))
    check
        "scanner inspects bound-Id unfolding metadata"
        (hasNativeDependentProgram [NonRec nativeUnfoldingHost trueExpr] [])
    check
        "scanner does not chase occurrence unfoldings"
        (not (hasNativeDependentExpr (Var nativeUnfoldingHost)))
    let rule =
            Rule
                { ru_name = fsLit "native-smoke"
                , ru_act = AlwaysActive
                , ru_fn = varName host
                , ru_rough = []
                , ru_bndrs = []
                , ru_args = [Type workerTy]
                , ru_rhs = trueExpr
                , ru_auto = False
                , ru_origin = nameModule eqTyConName
                , ru_orphan = IsOrphan
                , ru_local = True
                }
        nativeRuleHost = setIdSpecialisation host (RuleInfo [rule] emptyDVarSet)
    check "scanner inspects explicit rule metadata" (hasNativeDependentProgram [] [rule])
    check "scanner inspects bound-Id rule metadata" (hasNativeDependentProgram [NonRec nativeRuleHost trueExpr] [])
    let cyclic = setIdUnfolding host (unfolding (Let (NonRec cyclic trueExpr) (Var cyclic)))
    cycleResult <- timeout 2000000 (evaluate (hasNativeDependentProgram [NonRec cyclic trueExpr] []))
    check "scanner terminates on cyclic bound metadata" (cycleResult == Just False)
    let poisonedUnfolding = setIdUnfolding host (unfolding (error "forced nested unfolding metadata"))
        poisonedRules = setIdSpecialisation host (RuleInfo (error "forced nested rule metadata") emptyDVarSet)
    forM_ [poisonedUnfolding, poisonedRules] $ \poisoned -> do
        check
            "own-RHS scan leaves nested metadata deferred"
            (not (hasNativeDependentExprRhs (Let (NonRec poisoned trueExpr) (Var poisoned))))
        check
            "own-RHS scan still detects native annotations"
            (hasNativeDependentExprRhs (Let (NonRec poisoned trueExpr) worker))
    let ordinaryPoisonedRhs = Let (NonRec poisonedUnfolding trueExpr) (Var poisonedUnfolding)
        ordinaryPoisonedTemplate = uf_tmpl (mkCoreUnfolding VanillaSrc False ordinaryPoisonedRhs (Just (UnfoldingCache False False False False)) UnfNever)
    check "unfolding construction skips unstable nested metadata" (not (hasNativeDependentExprRhs ordinaryPoisonedTemplate))
    rejectNativeDependentExpr "ordinary smoke" (Lam b (Var b))
    rejectsUnsupported "native expression boundary gate" "native smoke" $
        rejectNativeDependentExpr "native smoke" worker
    rejectsUnsupported "native program boundary gate" "native smoke" $
        rejectNativeDependentProgram "native smoke" [NonRec (local 20 "keep" workerTy) worker] []
    rejectsUnsupported "native JavaScript boundary diagnostic" "JavaScript code generation" $
        rejectNativeDependentExpr "JavaScript code generation" worker

-- | Build a nominal proof identifier with the specified endpoints.
proof :: Int -> Type -> Type -> CoVar
proof n lhs rhs = mkCoVar (mkSystemName (unique n) (mkVarOcc "cv")) (mkNomEqPred lhs rhs)

-- | Map a Boolean constructor to its promoted index.
constructorIndex :: DataCon -> Type
constructorIndex = mkTyConTy . promoteDataCon

-- | Build a function at a constructor index and transport it through a proof.
dictionaryBranch :: Int -> Id -> DataCon -> CoVar -> CoreExpr
dictionaryBranch n payload con cv = Cast body (mkSymCo funCo)
  where
    concrete = constructorIndex con
    dict = local n "dict" (mkTyConApp eqTyCon [boolTy, concrete, concrete])
    body = Lam dict (Var payload)
    indexCo = mkCoVarCo cv
    predCo = mkTyConAppCo Nominal eqTyCon [mkNomReflCo boolTy, indexCo, indexCo]
    funCo = mkFunCoNoFTF Representational (mkNomReflCo ManyTy) (mkSubCo predCo) (mkReflCo Representational intTy)

-- | Build exhaustive alternatives in the wired Boolean constructor tag order.
dictionaryAlts :: Id -> CoVar -> CoVar -> [CoreAlt]
dictionaryAlts payload falseProof trueProof =
    [ Alt (DataAlt falseDataCon) [falseProof] (dictionaryBranch 20 payload falseDataCon falseProof)
    , Alt (DataAlt trueDataCon) [trueProof] (dictionaryBranch 21 payload trueDataCon trueProof)
    ]

-- | Construct a small closed family and its two ordinary family equations.
selectFamily :: (TyCon, CoAxiom Branched)
selectFamily = (family, axiom)
  where
    parameter = mkTyVar (mkSystemName (unique 100) (mkTyVarOcc "b")) boolTy
    binders = [mkAnonTyConBinder parameter]
    family =
        mkFamilyTyCon
            (mkSystemName (unique 101) (mkTcOcc "Select"))
            (mkTyConKind binders liftedTypeKind)
            binders
            0
            liftedTypeKind
            Nothing
            (ClosedTypeFamilyTyCon (CTF (Just axiom)))
            Nothing
            NotInjective
    axiom =
        mkBranchedCoAxiom
            Nominal
            (mkSystemName (unique 102) (mkTcOcc "SelectAx"))
            family
            [ mkCoAxBranch [] [] [] [constructorIndex trueDataCon] intTy [] noSrcSpan
            , mkCoAxBranch [] [] [] [constructorIndex falseDataCon] boolTy [] noSrcSpan
            ]

-- | Apply the test family to one Boolean index.
selected :: Type -> Type
selected index = mkTyConApp (fst selectFamily) [index]

-- | Compose constructor refinement with an existing closed-family equation.
selectionBranch :: Int -> CoVar -> CoreExpr -> CoreExpr
selectionBranch branch cv payload = Cast payload (mkSubCo (mkSymCo toConcrete))
  where
    refinement = mkTyConAppCo Nominal (fst selectFamily) [mkCoVarCo cv]
    equation = mkAxInstCo Nominal (BranchedAxiom (snd selectFamily) branch) [] []
    toConcrete = mkTransCo refinement equation

-- | Check branch evidence without evaluating native dependent code.
runCaseChecks :: DynFlags -> IO ()
runCaseChecks dflags = do
    let b = local 1 "b" boolTy
        other = local 2 "other" boolTy
        caseBinder = local 3 "case_b" boolTy
        payload = local 4 "n" intTy
        index = TermVarTy b
        falseIndex = constructorIndex falseDataCon
        trueIndex = constructorIndex trueDataCon
        falseExpr = Var (dataConWorkId falseDataCon)
        trueExpr = Var (dataConWorkId trueDataCon)
        falseProof = proof 5 index falseIndex
        trueProof = proof 6 index trueIndex
        result = indexed index
        alts = dictionaryAlts payload falseProof trueProof
        nativeCase = Case (Var b) caseBinder result alts
        whole = Lam b nativeCase
        plainCase = Case (Var b) caseBinder intTy [Alt (DataAlt falseDataCon) [] (Var payload), Alt (DataAlt trueDataCon) [] (Var payload)]
    lintAccepts dflags "nominal branch evidence transports dictionary functions" [b, payload] nativeCase
    lintAccepts dflags "retained Boolean lambda contains a refined case" [payload] whole
    checkType dflags "refined case result keeps the outer index" result (exprType nativeCase)
    checkType dflags "refined lambda retains its Boolean argument" (mkDepFunTy b result) (exprType whole)
    lintAccepts dflags "ordinary Boolean alternatives remain unchanged" [b, payload] plainCase
    lintAccepts dflags "DEFAULT plus one constructor proof covers Bool" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt DEFAULT [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
            ]

    let selectAlts =
            [ Alt (DataAlt falseDataCon) [falseProof] (selectionBranch 1 falseProof falseExpr)
            , Alt (DataAlt trueDataCon) [trueProof] (selectionBranch 0 trueProof (Var payload))
            ]
        selectCase = Case (Var b) caseBinder (selected index) selectAlts
        choose = Lam b selectCase
        reduce branch = mkAxInstCo Representational (BranchedAxiom (snd selectFamily) branch) [] []
    lintAccepts dflags "Select True is Int and Select False is Bool" [payload] choose
    lintRejects dflags "Select False branch rejects an Int payload" [payload] $
        Lam
            b
            ( Case
                (Var b)
                caseBinder
                (selected index)
                [ Alt (DataAlt falseDataCon) [falseProof] (selectionBranch 1 falseProof (Var payload))
                , Alt (DataAlt trueDataCon) [trueProof] (selectionBranch 0 trueProof (Var payload))
                ]
            )
    checkType dflags "choose has a retained Select result" (mkDepFunTy b (selected index)) (exprType choose)
    lintAccepts dflags "choose True followed by its family equation" [payload] (Cast (App choose trueExpr) (reduce 0))
    lintAccepts dflags "choose False followed by its family equation" [payload] (Cast (App choose falseExpr) (reduce 1))
    checkType dflags "True application selects Int" intTy (exprType (Cast (App choose trueExpr) (reduce 0)))
    checkType dflags "False application selects Bool" boolTy (exprType (Cast (App choose falseExpr) (reduce 1)))
    optimizedChooseTrue <- simplifyAccepts dflags "choose True reduces with checked reflexivity" [payload] (App choose trueExpr)
    optimizedChooseFalse <- simplifyAccepts dflags "choose False reduces with checked reflexivity" [payload] (App choose falseExpr)
    check "choose True removes virtual branch proofs" (not (hasNativeDependentExpr optimizedChooseTrue))
    check "choose False removes virtual branch proofs" (not (hasNativeDependentExpr optimizedChooseFalse))
    optimizedChoose <- simplifyAccepts dflags "choose retains a strict variable case" [payload] choose
    check
        "choose keeps constructor discrimination on an opaque index"
        (case optimizedChoose of Lam _ (Case (Var _) _ _ _) -> True; _ -> False)
    let knownUnfolding = (unfolding trueExpr){uf_cache = UnfoldingCache True True True True}
        knownTrue = setIdUnfolding b knownUnfolding
        knownCase =
            CoreSubst.substExpr (indexSubst [knownTrue, payload] b (Var knownTrue)) $
                Case (Var b) (setIdOccInfo caseBinder IAmDead) (selected index) selectAlts
    optimizedKnown <- simplifyAccepts dflags "constructor unfolding preserves opaque proof" [knownTrue, payload] knownCase
    check
        "an unfolding cannot turn an opaque index proof into reflexivity"
        (case optimizedKnown of Case (Var _) _ _ _ -> True; _ -> False)
    let producer = local 82 "producer" (mkFunctionType ManyTy boolTy boolTy)
        producerArgument = App (Var producer) trueExpr
        closedResult =
            Lam b $
                Case
                    (Var b)
                    caseBinder
                    intTy
                    [ Alt (DataAlt falseDataCon) [falseProof] (Var payload)
                    , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
                    ]
    optimizedProducer <- simplifyAccepts dflags "unsupported Boolean beta producer stays lazy" [producer, payload] (App closedResult producerArgument)
    check
        "an unsupported producer keeps its application redex"
        (case optimizedProducer of App (Lam _ _) (App (Var _) _) -> True; _ -> False)

    let keepBool = local 30 "keep_b" boolTy
        keepIndex = TermVarTy keepBool
        keepValue = local 31 "value" (selected keepIndex)
        keepSelect = Lam keepBool (Lam keepValue (Var keepValue))
        again = Lam other (App (App keepSelect (Var other)) (App choose (Var other)))
        mixed = App (App keepSelect (Var b)) (App choose (Var other))
        falsePayload = Cast falseExpr (mkSymCo (reduce 1))
    lintAccepts dflags "keepSelect retains a Select value at its runtime index" [] keepSelect
    checkType
        dflags
        "keepSelect has a retained dependent function type"
        (mkDepFunTy keepBool (mkFunctionType ManyTy (selected keepIndex) (selected keepIndex)))
        (exprType keepSelect)
    lintAccepts dflags "choose and keepSelect compose at one shared runtime index" [payload] again
    checkType
        dflags
        "shared choose and keepSelect composition retains its index"
        (mkDepFunTy other (selected (TermVarTy other)))
        (exprType again)
    lintRejects dflags "independent runtime Boolean indices do not unify" [b, other, payload] mixed
    lintRejects dflags "keepSelect True rejects an explicit Select False payload" [] $
        App (App keepSelect trueExpr) falsePayload

    let substituteTrue = indexSubst [payload] b trueExpr
        substituteOther = indexSubst [other, payload] b (Var other)
        trueCase = CoreSubst.substExpr substituteTrue nativeCase
        otherCase = CoreSubst.substExpr substituteOther nativeCase
        trueSelect = CoreSubst.substExpr substituteTrue selectCase
        otherSelect = CoreSubst.substExpr substituteOther selectCase
        boundChoose = CoreSubst.substExpr substituteTrue choose
    lintAccepts dflags "constructor substitution updates every refined-case proof" [payload] trueCase
    checkType dflags "constructor-substituted case recovers its instantiated type" (indexed trueIndex) (exprType trueCase)
    lintAccepts dflags "variable substitution updates every refined-case proof" [other, payload] otherCase
    checkType dflags "variable-substituted case retains the replacement identity" (indexed (TermVarTy other)) (exprType otherCase)
    lintAccepts dflags "constructor substitution preserves Select branch equations" [payload] trueSelect
    checkType dflags "constructor-substituted Select case recovers its instantiated type" (selected trueIndex) (exprType trueSelect)
    lintAccepts dflags "substituted Select case reduces through its declared equation" [payload] (Cast trueSelect (reduce 0))
    checkType dflags "substituted Select True reduces to Int" intTy (exprType (Cast trueSelect (reduce 0)))
    lintAccepts dflags "variable substitution preserves Select branch equations" [other, payload] otherSelect
    checkType dflags "variable-substituted Select case retains the replacement identity" (selected (TermVarTy other)) (exprType otherSelect)
    lintAccepts dflags "substitution respects the retained binder of choose" [payload] boundChoose
    checkType dflags "bound choose keeps its original dependent type" (mkDepFunTy b (selected index)) (exprType boundChoose)

    lintRejects dflags "proof cannot refine an unrelated scrutinee" [b, other, payload] (Case (Var other) caseBinder result alts)
    lintRejects dflags "opposite constructor proof is rejected" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [trueProof] (Var payload)
            , Alt (DataAlt trueDataCon) [] (Var payload)
            ]
    let reverseProof = proof 7 trueIndex index
    lintRejects dflags "reversed proof endpoints are rejected" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [reverseProof] (Var payload)
            ]
    let representational = mkCoVar (mkSystemName (unique 8) (mkVarOcc "repr")) (mkReprEqPred index trueIndex)
    lintRejects dflags "representational proof cannot refine a nominal index" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [representational] (Var payload)
            ]
    lintRejects dflags "extra proof binder is rejected" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof, proof 9 index trueIndex] (Var payload)
            ]
    let ordinary = setIdDetails (proof 10 index trueIndex) VanillaId
    lintRejects dflags "an ordinary Id cannot replace a proof CoVar" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [ordinary] (Var payload)
            ]
    let erased = mkTyVar (mkSystemName (unique 11) (mkTyVarOcc "erased")) boolTy
    lintRejects dflags "a type variable cannot replace a proof CoVar" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [erased] (Var payload)
            ]
    lintRejects dflags "DEFAULT cannot bind constructor evidence" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt DEFAULT [trueProof] (Var payload)
            , Alt (DataAlt trueDataCon) [] (Var payload)
            ]
    lintRejects dflags "constructor proof cannot shadow an in-scope proof identity" [b, payload, trueProof] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
            ]
    lintRejects dflags "missing coverage cannot justify a contradictory proof" [payload] $
        Case
            trueExpr
            caseBinder
            intTy
            [Alt (DataAlt falseDataCon) [proof 18 trueIndex falseIndex] (Var payload)]
    lintRejects dflags "proof cannot escape into another branch" [b, payload] $
        Case
            (Var b)
            caseBinder
            result
            [ Alt (DataAlt falseDataCon) [falseProof] (dictionaryBranch 22 payload trueDataCon trueProof)
            , Alt (DataAlt trueDataCon) [trueProof] (dictionaryBranch 23 payload trueDataCon trueProof)
            ]
    lintRejects dflags "case binder cannot occur in the outer result annotation" [b, payload] $
        Case (Var b) caseBinder (indexed (TermVarTy caseBinder)) alts
    lintRejects dflags "arbitrary producers cannot supply an index" [payload] $
        Case
            (Let (NonRec b trueExpr) (Var b))
            caseBinder
            (indexed trueIndex)
            (dictionaryAlts payload (proof 12 trueIndex falseIndex) (proof 13 trueIndex trueIndex))
    let notBool = local 14 "not_bool" intTy
    lintRejects dflags "non-Boolean scrutinee cannot justify Boolean evidence" [notBool, payload] $
        Case
            (Var notBool)
            (local 15 "case_int" intTy)
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
            ]

    let literalCase =
            Case
                trueExpr
                caseBinder
                (indexed trueIndex)
                (dictionaryAlts payload (proof 16 trueIndex falseIndex) (proof 17 trueIndex trueIndex))
    lintAccepts dflags "literal scrutinee supplies branch evidence" [payload] literalCase
    check "scanner detects literal-only virtual proof binders" (hasNativeDependentExpr literalCase)
    check "scanner still detects runtime-index case proofs" (hasNativeDependentExpr nativeCase)
    let literalProofAlt = Alt (DataAlt trueDataCon) [proof 24 trueIndex trueIndex] (Var payload)
    check "interfaces encode one Boolean proof name" $ case toIfaceAlt literalProofAlt of
        IfaceAlt (IfaceDataAlt _) [_] _ -> True
        _ -> False
    rejectsWith "interface writer rejects extra Boolean proof binders" "toIfaceAlt: invalid native Boolean proof binder" $
        evaluate (toIfaceAlt (Alt (DataAlt trueDataCon) [trueProof, falseProof] (Var payload)))
    rejectsWith "interface writer rejects a Boolean payload binder" "toIfaceAlt: invalid native Boolean proof binder" $
        evaluate (toIfaceAlt (Alt (DataAlt trueDataCon) [payload] (Var payload)))
