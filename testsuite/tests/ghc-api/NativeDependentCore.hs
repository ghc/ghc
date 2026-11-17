{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (IOException, SomeException, bracket, displayException, evaluate, try)
import Control.Monad (forM, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, foldl', isInfixOf, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC (DynFlags, InteractiveImport (IIDecl), getSession, getSessionDynFlags, lookupName, parseImportDecl, parseName, runGhc, setContext, setSessionDynFlags)
import GHC.Builtin.WiredIn.Prim (fUNTyCon, intPrimTy, realWorldStatePrimTy)
import GHC.Builtin.WiredIn.Types
import GHC.Core
import GHC.Core.Coercion (MCoercion (..), abstractNativeIndices, coercionLKind, coercionRKind, coercionRole, emptyLiftingContext, eqCoercion, extendLiftingContext, isReflCo, liftCoSubst, mkAppCo, mkAxInstCo, mkCoercionType, mkCoVarCo, mkDepFunCo, mkDepFunCoWithDomain, mkDepFunCoWithVis, mkForAllCo, mkFunCoNoFTF, mkKindCo, mkInstCo, mkLiftingContext, mkNomReflCo, mkPhantomCo, mkPiCo, mkReflCo, mkSelCo, mkSubCo, mkSymCo, mkTermAppCo, mkTransCo, mkTyConAppCo, prepareNativeLiftingContext, setNominalRole_maybe, splitAppCo_maybe)
import GHC.Core.Coercion.Opt (OptCoercionOpts (..), optCoercion)
import GHC.Core.Coercion.Axiom (Branched, ClosedTyFam (..), CoAxiom, CoAxiomRule (..), Role (..))
import GHC.Core.ConLike (ConLike (RealDataCon))
import GHC.Core.DataCon (DataCon, DataConRep (NoDataConRep), HsImplBang (HsLazy), HsSrcBang (HsSrcBang), SrcStrictness (NoSrcStrict, SrcStrict), SrcUnpackedness (NoSrcUnpack), StrictnessMark (NotMarkedStrict), dataConInstArgTys, dataConInstOrigArgTys, dataConInstSig, dataConName, dataConOrigArgTys, dataConRepArgBinders, dataConRepArity, dataConWorkId, dataConWrapperType, mkDataCon, mkDataConWithRetainedArgs, promoteDataCon)
import GHC.Core.Dependent
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.FamInstEnv (emptyFamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch, normaliseType)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Make (mkIntExprInt)
import GHC.Core.Map.Type (LooseTypeMap, TypeMap, emptyTypeMap)
import GHC.Core.Opt.Arity (etaExpand, pushCoArg, pushCoTyArg)
import GHC.Core.Predicate (mkNomEqPred, mkReprEqPred, scopedSortWith)
import GHC.Core.Reduction (Reduction (..))
import GHC.Core.SimpleOpt (simpleOptNativeExpr)
import qualified GHC.Core.Subst as CoreSubst
import GHC.Core.TyCo.Compare (eqType, nonDetCmpType)
import GHC.Core.TyCo.FVs (isInjectiveInType, tyCoVarsOfCo, tyCoVarsOfType)
import GHC.Core.TyCo.Rep (Coercion (..), CoSel (..), ForAllSel (..), FunSel (..), Scaled (..), TyLit (..), Type (..))
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.TyCo.Tidy (tidyTopType)
import GHC.Core.TyCon (AlgTyConFlav (VanillaAlgTyCon), FamTyConFlav (..), Injectivity (..), PromDataConInfo (NoPromInfo), NativeDomainShape (NativeInvalidDomain), TyCon, mkAlgTyCon, mkAnonTyConBinder, mkNamedTyConBinder, mkDataTyConRhs, mkFamilyTyCon, mkSynonymTyCon, mkTyConKind, mkTyConKindWithMatchability, setTyConNativeDomainShape, tyConDataCons, tyConNativeDomainShape, tyConSingleDataCon)
import GHC.Core.Type
import GHC.Core.Unfold.Make (mkCoreUnfolding)
import GHC.Core.Unify (BindFlag (..), MaybeApartReason (..), UnifyResultM (..), alwaysBindFam, alwaysBindTv, tcMatchRetainedTyX_BM, tcMatchTy, tcMatchTyKi, tcMatchTyX_BM, tcMatchTys, tcUnifyTysFG)
import GHC.Core.Utils (applyTypeToArgs, cheapEqExpr, dataConRepFSInstPat, dataConRepInstPat, exprType)
import GHC.CoreToIface (toIfaceAlt, toIfaceCoercion, toIfaceExpr, toIfaceType, toIfaceTypeX)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Data.IOEnv (IOEnvFailure)
import GHC.Data.StringBuffer (stringToStringBuffer)
import qualified GHC.Data.TrieMap as Trie
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.DynFlags (targetProfile)
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Env.KnotVars (KnotVars (KnotVars))
import GHC.Driver.Flags (GeneralFlag (Opt_DoCoreLinting))
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (gopt_unset, parseDynamicFlagsCmdLine, targetPlatform)
import GHC.Hs (HsType (..), hsForEachVisibility)
import GHC.Iface.Binary (CheckHiWay (CheckHiWay), CompressionIFace (..), TraceBinIFace (..), getWithUserData, putWithUserData, readBinIface, writeBinIface)
import GHC.Iface.Decl (tyThingToIfaceDecl)
import GHC.Iface.Env (extendIfaceIdEnv)
import GHC.Iface.Syntax (IfGuidance (IfWhen), IfaceAlt (..), IfaceBang (IfNoBang, IfStrict), IfaceConAlt (..), IfaceConDecl (..), IfaceConDecls (IfDataTyCon), IfaceDecl (IfaceData, IfaceId, IfaceSynonym, ifCons, ifKind, ifName, ifParent, ifResKind), IfaceExpr (..), IfaceIdDetails (IfDFunId, IfVanillaId), IfaceInfoItem (HsUnfold), IfaceRule (IfaceRule), IfaceSrcBang (IfSrcBang), IfaceTyConParent (IfDataInstance), IfaceUnfolding (IfCoreUnfold, IfDFunUnfold))
import GHC.Iface.Type (IfaceAppArgs (IA_Nil, IA_Arg), IfaceBndr (IfaceIdBndr, IfaceTvBndr), IfaceCoercion (IfaceCoVarCo, IfaceForAllCo, IfaceTermAppCo), IfaceForAllTyFlag (IfaceForAllTyFlag), IfaceTyCon (IfaceTyCon), IfaceTyConSort (IfaceNormalTyCon), IfaceType (..), getIfaceType, putIfaceType, ifaceBndrName, ifLclNameFS, mkIfLclName, mkIfaceForAllTvBndr, mkIfaceTyConInfo)
import GHC.IfaceToCore (tcIfaceDecl, tcIfaceExpr, tcIfaceRules)
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Tc.Solver.Monad as TcS
import GHC.Tc.Solver.Solve (solveWanteds)
import GHC.Tc.Types.Constraint (CtEvidence (CtWanted), emptyCoHoleSet, isSolvedWC, mkSimpleWC, wantedCtEvCoercion)
import GHC.Tc.Types.Origin (CtOrigin (TypeEqOrigin), SkolemInfoAnon (UnifyForAllSkol), mkSkolemInfo)
import GHC.Tc.Utils.Monad (initIfaceCheck, initIfaceLcl, initIfaceLoad)
import qualified GHC.Tc.Utils.Monad as TcM
import GHC.Tc.Utils.TcMType (isFilledMetaTyVar_maybe, newFlexiTyVar, newSkolemTyVar)
import GHC.Tc.Utils.TcType (noConcreteTyVars)
import qualified GHC.Tc.Zonk.TcType as TcZonk
import qualified GHC.Tc.Zonk.Type as FinalZonk
import GHC.Types.Basic (TypeOrKind (TypeLevel))
import GHC.Types.Basic (LeftOrRight (..), OccInfo (..), PromotionFlag (IsPromoted, NotPromoted), TailCallInfo (..))
import GHC.Types.Id
import GHC.Types.Id.Info (IdDetails (VanillaId), RuleInfo (..))
import GHC.Types.Id.Make (mkDataConWorkId)
import GHC.Types.RepType (countConRepArgs, countFunRepArgs)
import GHC.Types.InlinePragma (ActivationX (AlwaysActive), neverInlinePragma)
import GHC.Types.Literal (Literal (LitChar))
import GHC.Types.Name (getOccName, mkExternalName, mkSystemName, nameModule)
import GHC.Types.Name.Occurrence (mkDataConWorkerOcc, mkDataOcc, mkTcOcc, mkTyVarOcc, mkVarOcc, occNameFS)
import GHC.Types.SourceText (SourceText (NoSourceText))
import GHC.Types.SrcLoc (mkRealSrcLoc, noLoc, noSrcSpan, unLoc)
import GHC.Types.TyThing (TyThing (AConLike, ATyCon, AnId))
import GHC.Types.TypeEnv (mkTypeEnv)
import GHC.Types.Unique (Unique, mkUniqueGrimilyWithTag)
import GHC.Types.Unique.FM (sizeUFM)
import GHC.Types.Var hiding (setIdExported)
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Unit.Module.ModIface (emptyFullModIface, mi_decls, set_mi_decls)
import GHC.Unit.Types (IsBootInterface (NotBoot), mainUnit, mkModule, mkModuleName)
import qualified GHC.Utils.Binary as Binary
import GHC.Utils.Fingerprint (fingerprint0)
import GHC.Utils.Logger (initLogger, pushLogHook)
import GHC.Utils.Outputable (ppr, text, vcat)
import GHC.Utils.Panic (GhcException (..))
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.IO (hClose, openBinaryTempFile)
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
            runOccurrenceChecks dflags
            runAliasChecks dflags
            runOpaqueCallChecks dflags
            runFunctionBinderChecks dflags
            runFunctionFieldChecks dflags
            runArrowMatchabilityChecks dflags
            runForAllSelectorChecks dflags
            runForAllMatchabilityChecks dflags
            runForAllSolverChecks libdir
            runMatchingChecks dflags
            runTypeMapChecks
            runCaseChecks dflags
            runNatChecks dflags
            runParameterizedDomainChecks dflags
            runNativeMetadataChecks libdir dflags
            runNativeZonkMetadataChecks libdir
            runRetainedFieldChecks dflags
            runDepFunCoChecks dflags
            runDepFunVisibilityChecks dflags
            runIfaceChecks libdir
            putStrLn "native-core-api-ok"
        _ -> fail "Usage: NativeDependentCore GHC_LIBDIR"

-- | Create a stable test unique without a global unique supply.
unique :: Int -> Unique
unique = mkUniqueGrimilyWithTag 'N' . fromIntegral

-- | Create a local unrestricted identifier for a test.
local :: Int -> String -> Type -> Id
local n label = mkSysLocal (fsLit label) (unique n) ManyTy

{- Note [Native API arrow classification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These fixtures construct runtime arrows with U. Their function coercions carry
reflexive U evidence before multiplicity evidence. Family and synonym makers
preserve supplied kinds, so their declared argument prefixes also need U.
Algebraic constructor kind prefixes keep M. Keep each result kind unchanged.
-}

-- | Build a lifted result that retains the Boolean index in a dictionary type.
indexed :: Type -> Type
indexed index = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [boolTy, index, index]) intTy

-- | Build an identity function at a supplied type.
identity :: Int -> Type -> CoreExpr
identity n ty = let x = local n "x" ty in Lam x (Var x)

-- | Build the native retained keep function.
keep :: Id -> Int -> CoreExpr
keep b n = Lam b (identity n (indexed (TermVarTy b)))

-- | Build the expected ordinary result of applying keep to an index.
keepResult :: Type -> Type
keepResult index = mkFunctionType unmatchableDataConTy ManyTy (indexed index) (indexed index)

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
        at v = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [varType v, TermVarTy v, TermVarTy v]) intTy
        atB = at b
        body x y = mkFunctionType unmatchableDataConTy ManyTy x y
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
            , (mkDepFunTyWithVis DepFunInvisible b (body atB atB), 11)
            , (mkDepFunTyWithVis DepFunInvisible b intTy, 12)
            , (mkDepFunTy b intTy, 13)
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
                   , ("alpha-equivalent hidden binder", mkDepFunTyWithVis DepFunInvisible c (body (at c) (at c)))
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

{- | Check a native constructor field telescope with a lifted indexed function.
The payload is an equality-dictionary function, not an executed vector.
-}
runRetainedFieldChecks :: DynFlags -> IO ()
runRetainedFieldChecks dflags = do
    let (nat, zero, _) = peanoNat
        natTy = mkTyConTy nat
        template = local 2400 "index_template" natTy
        n = local 2401 "n" natTy
        m = local 2402 "m" natTy
        payloadTy ix = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [natTy, ix, ix]) intTy
        originalArgs = [Scaled ManyTy natTy, Scaled ManyTy (payloadTy (TermVarTy template))]
        modu = mkModule mainUnit (mkModuleName "NativeRetainedFieldAPI")
        name i occurrence = mkExternalName (unique i) modu occurrence noSrcSpan
        package =
            mkAlgTyCon
                (name 2403 (mkTcOcc "NativePackage"))
                liftedTypeKind
                []
                0
                liftedTypeKind
                []
                Nothing
                []
                (mkDataTyConRhs [constructor])
                (VanillaAlgTyCon (name 2404 (mkVarOcc "package_rep")))
                False
        packageTy = mkTyConTy package
        constructor = build [Just template, Nothing] originalArgs packageTy
        build = buildFor package 2405
        buildFor parent base binders args result = con
          where
            occurrence = mkDataOcc "NativePackage"
            con =
                mkDataConWithRetainedArgs
                    binders
                    (name base occurrence)
                    False
                    (name (base + 1) (mkVarOcc "constructor_rep"))
                    (map (const (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)) args)
                    (map (const HsLazy) args)
                    (map (const NotMarkedStrict) args)
                    []
                    []
                    []
                    noConcreteTyVars
                    []
                    []
                    []
                    args
                    result
                    NoPromInfo
                    parent
                    1
                    []
                    (mkDataConWorkId (name (base + 2) (mkDataConWorkerOcc occurrence)) con)
                    NoDataConRep
        expected = mkDepFunTy template (mkFunctionType unmatchableDataConTy ManyTy (payloadTy (TermVarTy template)) packageTy)
        payload = local 2408 "payload" (payloadTy (TermVarTy n))
        otherPayload = local 2409 "other_payload" (payloadTy (TermVarTy m))
        answer = local 2410 "answer" intTy
        consumer = local 2413 "consumer" (mkDepFunTy m (mkFunctionType unmatchableDataConTy ManyTy (payloadTy (TermVarTy m)) intTy))
        scrutinee = local 2411 "package" packageTy
        caseBinder = local 2412 "package_case" packageTy
        construction = mkApps (Var (dataConWorkId constructor)) [Var n, Var payload]
        (existentials, fields) = dataConRepInstPat (map unique [2420 .. 2429]) ManyTy constructor []
        (otherExistentials, otherFields) =
            dataConRepFSInstPat (repeat (fsLit "fresh_field")) (map unique [2430 .. 2439]) ManyTy constructor []
        eliminate args result rhs = Case (Var scrutinee) caseBinder result [Alt (DataAlt constructor) args rhs]
        nestedBinder = local 2450 "nested_index" natTy
        nestedPayload =
            mkDepFunTy nestedBinder $
                mkFunctionType unmatchableDataConTy ManyTy (payloadTy (TermVarTy template)) (payloadTy (TermVarTy nestedBinder))
        nestedPackage =
            mkAlgTyCon
                (name 2460 (mkTcOcc "NestedNativePackage"))
                liftedTypeKind
                []
                0
                liftedTypeKind
                []
                Nothing
                []
                (mkDataTyConRhs [nestedConstructor])
                (VanillaAlgTyCon (name 2461 (mkVarOcc "nested_package_rep")))
                False
        nestedConstructor =
            buildFor
                nestedPackage
                2462
                [Just template, Nothing]
                [Scaled ManyTy natTy, Scaled ManyTy nestedPayload]
                (mkTyConTy nestedPackage)
        (_, nestedFields) = dataConRepInstPat (map unique [2450 .. 2459]) ManyTy nestedConstructor []
        vacuousPackage =
            mkAlgTyCon
                (name 2480 (mkTcOcc "VacuousNativePackage"))
                liftedTypeKind
                []
                0
                liftedTypeKind
                []
                Nothing
                []
                (mkDataTyConRhs [vacuousConstructor])
                (VanillaAlgTyCon (name 2481 (mkVarOcc "vacuous_package_rep")))
                False
        vacuousTy = mkTyConTy vacuousPackage
        vacuousConstructor =
            buildFor
                vacuousPackage
                2482
                [Just template, Nothing]
                [Scaled ManyTy natTy, Scaled ManyTy natTy]
                vacuousTy
        rejectsMetadata label binders args result =
            rejectsWith label "mkDataConWithRetainedArgs: invalid telescope" $
                evaluate (dataConWrapperType (build binders args result))
    case nestedFields of
        [field, dependent] -> case idType dependent of
            DepFunTy DepFunVisible inner body -> do
                check "nested Pi fixture collides with the actual earlier field Unique" (field == nestedBinder)
                check "field substitution freshens a colliding inner Pi binder" (inner /= field)
                checkType
                    dflags
                    "nested Pi preserves its outer field dependency and inner identity"
                    (mkFunctionType unmatchableDataConTy ManyTy (payloadTy (TermVarTy field)) (payloadTy (TermVarTy inner)))
                    body
                check "nested Pi substitution removes the retained template identity" $
                    not (anyFreeVarsOfType (== template) (idType dependent))
            _ -> fail "FAIL: retained field substitution lost the nested Pi"
        _ -> fail "FAIL: nested retained field instantiation returned the wrong arity"
    check "vacuous retained metadata remains explicit" $
        case dataConRepArgBinders vacuousConstructor of
            [Just b, Nothing] -> b == template
            _ -> False
    check "vacuous retained constructor is not a native index domain" (not (isNativeIndexType vacuousTy))
    lintAccepts dflags "vacuous retained constructor has an ordinary runtime payload" [n, m] $
        mkApps (Var (dataConWorkId vacuousConstructor)) [Var n, Var m]
    check "ordinary constructor has no retained-field metadata" (null (dataConRepArgBinders zero))
    check "a retained package is not a native index domain" (not (isNativeIndexType packageTy))
    check "retained constructor records its actual template binder" $
        case dataConRepArgBinders constructor of
            [Just b, Nothing] -> b == template
            _ -> False
    checkType dflags "retained constructor worker telescope" expected (idType (dataConWorkId constructor))
    checkType dflags "retained constructor wrapper telescope" expected (dataConWrapperType constructor)
    check "retained constructor has no erased existentials" (null existentials && null otherExistentials)
    case (fields, otherFields) of
        ([field, dependent], [otherField, otherDependent]) -> do
            check "retained field instantiation freshens both invocations" $
                field /= template && otherField /= template && field /= otherField
            checkType dflags "first dependent field names its actual preceding field" (payloadTy (TermVarTy field)) (idType dependent)
            checkType dflags "second dependent field names its actual preceding field" (payloadTy (TermVarTy otherField)) (idType otherDependent)
            check "dependent field does not retain the template identity" $
                not (anyFreeVarsOfType (== template) (idType dependent))
            lintAccepts dflags "native package elimination" [scrutinee, answer] $
                eliminate fields intTy (Var answer)
            lintAccepts dflags "native package scoped consumer" [scrutinee, consumer] $
                eliminate fields intTy (mkApps (Var consumer) (map Var fields))
            lintAccepts dflags "native package repackaging closes its field scope" [scrutinee] $
                eliminate fields packageTy (mkApps (Var (dataConWorkId constructor)) (map Var fields))
            lintRejects dflags "native package fields cannot be reversed" [scrutinee, answer] $
                eliminate [dependent, field] intTy (Var answer)
            lintRejects dflags "native package index field has the declared domain" [scrutinee, answer] $
                eliminate [setIdType field boolTy, dependent] intTy (Var answer)
            lintRejects dflags "native package payload cannot name a stale template" [scrutinee, answer, template] $
                eliminate [field, setIdType dependent (payloadTy (TermVarTy template))] intTy (Var answer)
            lintRejects dflags "native package index binder cannot capture an outer Id" [scrutinee, answer, field] $
                eliminate fields intTy (Var answer)
            lintRejects dflags "native package field cannot escape into case result" [scrutinee] $
                eliminate fields (idType dependent) (Var dependent)
        _ -> fail "FAIL: retained field instantiation returned the wrong arity"
    lintAccepts dflags "native package construction" [n, payload] construction
    checkType dflags "native package construction result" packageTy (exprType construction)
    lintRejects dflags "native package construction checks dependent payload identity" [n, m, otherPayload] $
        mkApps (Var (dataConWorkId constructor)) [Var n, Var otherPayload]
    rejectsMetadata "retained metadata must align with fields" [Just template] originalArgs packageTy
    rejectsMetadata "retained metadata must use the declared field domain" [Just (setIdType template boolTy), Nothing] originalArgs packageTy
    rejectsMetadata "retained metadata must use unrestricted binders" [Just (setIdMult template OneTy), Nothing] originalArgs packageTy
    let functionField = local 6299 "function_field" (mkFunctionType unmatchableDataConTy ManyTy natTy natTy)
        functionConstructor = build [Just functionField] [Scaled ManyTy (idType functionField)] packageTy
        functionWorkerType = idType (dataConWorkId functionConstructor)
        innerLinearFunction = setIdType functionField (mkFunctionType unmatchableDataConTy OneTy natTy natTy)
    check "vacuous function field preserves its retained metadata" $
        case dataConRepArgBinders functionConstructor of
            [Just field] -> field == functionField && isNativeIndexFunctionId field
            _ -> False
    check "vacuous function field worker uses an ordinary arrow" $ case functionWorkerType of
        FunTy{} -> True
        _ -> False
    checkType dflags "vacuous function field worker retains its exact domain"
        (mkFunctionType unmatchableDataConTy ManyTy (idType functionField) packageTy) functionWorkerType
    check "vacuous function field occupies one runtime slot" $
        dataConRepArity functionConstructor == 1
            && countFunRepArgs 1 functionWorkerType == 1
            && countConRepArgs functionConstructor == 1
    rejectsMetadata "retained function field rejects an inner linear arrow"
        [Just innerLinearFunction] [Scaled ManyTy (idType innerLinearFunction)] packageTy
    rejectsMetadata "retained metadata cannot reference an unbound runtime Id" [Nothing, Nothing] originalArgs packageTy
    rejectsMetadata "retained field cannot escape into constructor result" [Just template, Nothing] originalArgs (payloadTy (TermVarTy template))
    let recursivePackage base constructors =
            mkAlgTyCon
                (name base (mkTcOcc ("RecursiveNativePackage" ++ show base)))
                liftedTypeKind
                []
                0
                liftedTypeKind
                []
                Nothing
                []
                (mkDataTyConRhs constructors)
                (VanillaAlgTyCon (name (base + 1) (mkVarOcc ("recursive_package_rep_" ++ show base))))
                False
        recursiveConstructor parent base domain =
            let field = local (base + 3) "recursive_index" (mkTyConTy domain)
             in buildFor parent base [Just field] [Scaled ManyTy (mkTyConTy domain)] (mkTyConTy parent)
        selfPackage = recursivePackage 2800 [selfConstructor]
        selfConstructor = recursiveConstructor selfPackage 2802 selfPackage
        mutualLeft = recursivePackage 2810 [mutualLeftConstructor]
        mutualRight = recursivePackage 2820 [mutualRightConstructor]
        mutualLeftConstructor = recursiveConstructor mutualLeft 2812 mutualRight
        mutualRightConstructor = recursiveConstructor mutualRight 2822 mutualLeft
    forM_
        [("self-recursive", selfConstructor), ("mutually recursive", mutualLeftConstructor)]
        $ \(label, con) -> do
            completed <- timeout 2000000 $ try @GhcException (evaluate (dataConWrapperType con))
            case completed of
                Nothing -> fail ("FAIL: " ++ label ++ " retained domain validation did not terminate")
                Just (Left exception) ->
                    check (label ++ " retained domain is rejected") $
                        "mkDataConWithRetainedArgs: invalid telescope" `isInfixOf` displayException exception
                Just (Right _) -> fail ("FAIL: " ++ label ++ " retained domain was accepted")
    rejectsWith "retained constructor cannot be promoted" "promoteDataCon: retained field telescope" $
        evaluate (promoteDataCon constructor)
    rejectsWith "flat signature cannot discard retained field scope" "dataConInstSig: retained constructor field telescope" $
        evaluate (dataConInstSig constructor [])
    rejectsWith "flat representation fields cannot discard retained scope" "dataConInstArgTys: retained constructor field telescope" $
        evaluate (dataConInstArgTys constructor [])
    rejectsWith "flat original fields cannot discard retained scope" "dataConInstOrigArgTys: retained constructor field telescope" $
        evaluate (dataConInstOrigArgTys constructor [])

{- | Build an interface package with a Boolean index and a lifted payload.
The dependent payload is a dictionary function. It is not an executed vector.
-}
retainedIfacePackage :: Int -> Bool -> TyCon
retainedIfacePackage base vacuous = package
  where
    modu = mkModule mainUnit (mkModuleName "NativeRetainedIfaceAPI")
    name n occurrence = mkExternalName (unique n) modu occurrence noSrcSpan
    label = if vacuous then "IfaceVacuousPackage" else "IfaceNativePackage"
    field = local base "package_index" boolTy
    payloadTy = if vacuous then boolTy else indexed (TermVarTy field)
    args = [Scaled ManyTy boolTy, Scaled ManyTy payloadTy]
    package =
        mkAlgTyCon
            (name (base + 1) (mkTcOcc label))
            liftedTypeKind
            []
            0
            liftedTypeKind
            []
            Nothing
            []
            (mkDataTyConRhs [constructor])
            (VanillaAlgTyCon (name (base + 2) (mkVarOcc ("package_rep_" ++ show base))))
            False
    occurrence = mkDataOcc ("Mk" ++ label)
    constructor =
        mkDataConWithRetainedArgs
            [Just field, Nothing]
            (name (base + 3) occurrence)
            False
            (name (base + 4) (mkVarOcc ("constructor_rep_" ++ show base)))
            (map (const (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)) args)
            (map (const HsLazy) args)
            (map (const NotMarkedStrict) args)
            []
            []
            []
            noConcreteTyVars
            []
            []
            []
            args
            (mkTyConTy package)
            NoPromInfo
            package
            1
            []
            (mkDataConWorkId (name (base + 5) (mkDataConWorkerOcc occurrence)) constructor)
            NoDataConRep

-- | Build a polymorphic package with two same-spelling retained fields.
retainedIfaceParameterizedPackage :: Int -> TyCon
retainedIfaceParameterizedPackage base = package
  where
    modu = mkModule mainUnit (mkModuleName "NativeRetainedIfaceAPI")
    name n occurrence = mkExternalName (unique n) modu occurrence noSrcSpan
    label = "IfaceParameterizedPackage"
    parameter = mkTyVar (mkSystemName (unique (base + 11)) (mkTyVarOcc "a")) liftedTypeKind
    binders = [mkAnonTyConBinder parameter]
    otherField = local (base + 10) "package_index" boolTy
    field = local base "package_index" boolTy
    payloadTy =
        mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [boolTy, TermVarTy field, TermVarTy field]) $
            mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [boolTy, TermVarTy otherField, TermVarTy otherField]) (mkTyVarTy parameter)
    args = [Scaled ManyTy boolTy, Scaled ManyTy boolTy, Scaled ManyTy payloadTy]
    package =
        mkAlgTyCon
            (name (base + 1) (mkTcOcc label))
            (mkTyConKind binders liftedTypeKind)
            binders
            0
            liftedTypeKind
            [Representational]
            Nothing
            []
            (mkDataTyConRhs [constructor])
            (VanillaAlgTyCon (name (base + 2) (mkVarOcc ("package_rep_" ++ show base))))
            False
    occurrence = mkDataOcc ("Mk" ++ label)
    constructor =
        mkDataConWithRetainedArgs
            [Just field, Just otherField, Nothing]
            (name (base + 3) occurrence)
            False
            (name (base + 4) (mkVarOcc ("constructor_rep_" ++ show base)))
            (map (const (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)) args)
            (map (const HsLazy) args)
            (map (const NotMarkedStrict) args)
            []
            [parameter]
            []
            noConcreteTyVars
            [Bndr parameter Specified]
            []
            []
            args
            (mkTyConApp package [mkTyVarTy parameter])
            NoPromInfo
            package
            1
            []
            (mkDataConWorkId (name (base + 5) (mkDataConWorkerOcc occurrence)) constructor)
            NoDataConRep

{- Note [Parameterized domain API controls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The domain query checks each recursive path at its full static arguments.
The sibling control uses two distinct Maybe instances on separate paths.
The growing and permuting controls change the arguments on one active path.
A closed function type is permitted as a phantom parameter. A function field
is not a scalar index. These controls test that distinction separately.
-}

-- | Build an ordinary parameterized domain with independently specified fields.
parameterizedDomainFixture :: Int -> [Kind] -> ([TyVar] -> TyCon -> [[Type]]) -> TyCon
parameterizedDomainFixture base kinds fieldTypes = owner
  where
    modu = mkModule mainUnit (mkModuleName "ParameterizedIndexAPI")
    name n occ = mkExternalName (unique n) modu occ noSrcSpan
    parameters = zipWith (\n kind -> mkTyVar (mkSystemName (unique n) (mkTyVarOcc ("a" ++ show n))) kind) [base + 10 ..] kinds
    binders = map mkAnonTyConBinder parameters
    owner =
        mkAlgTyCon
            (name base (mkTcOcc ("Domain" ++ show base)))
            (mkTyConKind binders liftedTypeKind)
            binders
            0
            liftedTypeKind
            (map (const Nominal) parameters)
            Nothing
            []
            (mkDataTyConRhs constructors)
            (VanillaAlgTyCon (name (base + 1) (mkVarOcc ("domain_rep" ++ show base))))
            False
    constructors = zipWith constructor [1 ..] (fieldTypes parameters owner)
    constructor tag fields = con
      where
        n = base + 100 + 10 * tag
        label = "C" ++ show n
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
                parameters
                []
                noConcreteTyVars
                (map (`Bndr` Specified) parameters)
                []
                []
                (map (Scaled OneTy) fields)
                (mkTyConApp owner (map mkTyVarTy parameters))
                NoPromInfo
                owner
                tag
                []
                (mkDataConWorkId (name (n + 1) (mkDataConWorkerOcc occurrence)) con)
                NoDataConRep

{- | Check ground domain instantiation, constructor indices and nominal branches.
These quiet controls preserve the existing one-line golden output.
-}
runParameterizedDomainChecks :: DynFlags -> IO ()
runParameterizedDomainChecks dflags = do
    let (nat, _, _) = peanoNat
        natTy = mkTyConTy nat
        listBool = mkListTy boolTy
        listNat = mkListTy natTy
        maybeNat = mkTyConApp maybeTyCon [natTy]
        xs = local 12000 "list_index" listBool
        ys = local 12001 "other_list_index" listBool
        b = local 12002 "list_head" boolTy
        tailId = local 12003 "list_tail" listBool
        natTail = local 12004 "nat_tail" listNat
        binder = local 12005 "list_beta" listBool
        nilExpr = mkApps (Var (dataConWorkId nilDataCon)) [Type boolTy]
        nilIndex = mkTyConApp (promoteDataCon nilDataCon) [boolTy]
        consExpr = mkApps (Var (dataConWorkId consDataCon)) [Type boolTy, Var b, Var tailId]
        consIndex = mkTyConApp (promoteDataCon consDataCon) [boolTy, TermVarTy b, TermVarTy tailId]
        scope = [xs, ys, b, tailId]
    forM_ [("ground List Bool", listBool), ("ground Maybe Nat", maybeNat)] $ \(label, domain) ->
        check label (isNativeIndexType domain)
    check "ground List instances retain their full static domain" (not (eqType listBool listNat))
    forM_ [("empty list", nilExpr, nilIndex), ("cons list", consExpr, consIndex), ("polymorphic nullary beta", App (Lam binder (Var binder)) nilExpr, nilIndex)] $ \(label, expr, index) -> do
        lintAccepts dflags label scope expr
        checkType dflags (label ++ " runtime type") listBool (exprType expr)
        check (label ++ " index conversion") (maybe False (eqType index) (exprToNativeIndex expr))
        checkType dflags (label ++ " index kind") listBool (typeKind index)
    let rebuilt = mapNativeIndex id consExpr
    case collectArgs rebuilt of
        (Var worker, [Type argument, Var headId, Var restId]) -> do
            check "constructor reconstruction preserves worker and actual fields" (worker == dataConWorkId consDataCon && headId == b && restId == tailId)
            checkType dflags "constructor reconstruction preserves erased Bool argument" boolTy argument
        _ -> fail "FAIL: parameterized constructor reconstruction lost its erased argument"
    let listPayload = local 12015 "list_payload" (mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [listBool, TermVarTy xs, TermVarTy xs]) intTy)
        listKeep = Lam xs (Lam listPayload (Var listPayload))
    lintAccepts dflags "retained list-domain worker" [] listKeep
    lintRejects dflags "retained application rejects List Nat at List Bool" [natTail] (App listKeep (Var natTail))
    let wrongCons = mkApps (Var (dataConWorkId consDataCon)) [Type natTy, Var b, Var natTail]
    lintRejects dflags "constructor static argument checks its instantiated fields" [b, natTail] wrongCons
    check "index conversion rejects a mismatched instantiated field" (case exprToNativeIndex wrongCons of Nothing -> True; _ -> False)
    check "constructor lookup rejects the wrong parent" (case nativeIndexDataConTyArgs maybeNat consDataCon of Nothing -> True; _ -> False)
    let caseBinder = local 12006 "list_case" listBool
        headId = local 12007 "pattern_head" boolTy
        restId = local 12008 "pattern_tail" listBool
        nilProof = proof 12009 (TermVarTy xs) nilIndex
        consPattern = mkTyConApp (promoteDataCon consDataCon) [boolTy, TermVarTy headId, TermVarTy restId]
        consProof = proof 12010 (TermVarTy xs) consPattern
        listCase cv =
            Case
                (Var xs)
                caseBinder
                boolTy
                [ Alt (DataAlt nilDataCon) [nilProof] (Var (dataConWorkId falseDataCon))
                , Alt (DataAlt consDataCon) [headId, restId, cv] (Var headId)
                ]
        wrongProof = proof 12011 (TermVarTy xs) (mkTyConApp (promoteDataCon consDataCon) [natTy, TermVarTy headId, TermVarTy restId])
    lintAccepts dflags "list constructor branch evidence has erased arguments and actual fields" [xs] (listCase consProof)
    lintRejects dflags "list branch evidence cannot change the static parameter" [xs] (listCase wrongProof)
    let f = local 12012 "list_function" (mkFunctionType unmatchableDataConTy ManyTy listBool maybeNat)
        g = local 12013 "list_function" (idType f)
        call = TermAppTy f (TermVarTy xs)
        subst = CoreSubst.extendIdSubst (CoreSubst.mkEmptySubst (mkInScopeSet (mkVarSet [f, g, xs]))) f (Var g)
        cv = proof 12014 (TermVarTy xs) (TermVarTy ys)
        co = mkTermAppCo f (mkCoVarCo cv)
    check "opaque head accepts applied scalar input and output" (isNativeIndexFunctionId f)
    checkType dflags "function head substitution preserves the full applied domain" (TermAppTy g (TermVarTy xs)) (CoreSubst.substTy subst call)
    lintAccepts dflags "applied-domain forward nominal call congruence" [f, xs, ys, cv] (Coercion co)
    checkType dflags "applied-domain call result kind" maybeNat (coercionLKind (mkKindCo co))
    let phantom = parameterizedDomainFixture 13000 [liftedTypeKind] (\_ _ -> [[]])
        actualFunction = parameterizedDomainFixture 14000 [liftedTypeKind] (\[a] _ -> [[mkFunctionType unmatchableDataConTy ManyTy (mkTyVarTy a) (mkTyVarTy a)]])
        tree = parameterizedDomainFixture 15000 [liftedTypeKind] (\[a] self -> [[mkTyVarTy a], [mkTyConApp self [mkTyVarTy a], mkTyConApp self [mkTyVarTy a]]])
        growing = parameterizedDomainFixture 16000 [liftedTypeKind] (\[a] self -> [[mkTyConApp self [mkListTy (mkTyVarTy a)]]])
        permuting = parameterizedDomainFixture 17000 [liftedTypeKind, liftedTypeKind] (\[a, c] self -> [[mkTyConApp self [mkTyVarTy c, mkTyVarTy a]]])
        siblings = parameterizedDomainFixture 18000 [liftedTypeKind, liftedTypeKind] (\[a, c] _ -> [[mkTyConApp maybeTyCon [mkTyVarTy a], mkTyConApp maybeTyCon [mkTyVarTy c]]])
        empty = parameterizedDomainFixture 19000 [liftedTypeKind] (\_ _ -> [])
        wrongKind = parameterizedDomainFixture 20000 [boolTy] (\_ _ -> [])
        fnTy = mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy
        domains =
            [ ("phantom closed function parameter", mkTyConApp phantom [fnTy], True)
            , ("actual closed function field", mkTyConApp actualFunction [boolTy], True)
            , ("uniform recursive Tree Bool", mkTyConApp tree [boolTy], True)
            , ("growing recursive arguments", mkTyConApp growing [boolTy], False)
            , ("permuting recursive arguments", mkTyConApp permuting [boolTy, natTy], False)
            , ("independent sibling specializations", mkTyConApp siblings [boolTy, natTy], True)
            , ("empty applied data domain", mkTyConApp empty [boolTy], True)
            , ("unsaturated empty domain", mkTyConTy empty, False)
            , ("empty non-Type parameter kind", mkTyConApp wrongKind [constructorIndex trueDataCon], False)
            ]
    forM_ domains $ \(label, domain, expected) -> do
        outcome <- timeout 2000000 (evaluate (isNativeIndexType domain))
        check (label ++ "; bounded domain query") (outcome == Just expected)

{- Note [Native generic metadata controls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These controls inspect runtime Id metadata. They do not establish source
admission or dependent-domain transport on their own.
Check actual Id metadata: eqType alone compares the runtime Id's identity.
Only closed List Bool specializations below are submitted to Core Lint.
The indexed element control checks metadata, not vector operations or transport.
-}

-- | Check source binder metadata, target ranges and deep kind substitution.
runNativeMetadataChecks :: FilePath -> DynFlags -> IO ()
runNativeMetadataChecks libdir dflags = do
    ioTyCon <- runGhc (Just libdir) $ do
        initialFlags <- getSessionDynFlags
        _ <- setSessionDynFlags initialFlags
        declaration <- parseImportDecl "import Prelude"
        setContext [IIDecl declaration]
        names <- parseName "IO"
        case names of
            name :| [] -> do
                thing <- lookupName name
                case thing of
                    Just (ATyCon tc) -> pure tc
                    _ -> liftIO $ fail "FAIL: IO metadata control did not resolve a type constructor"
            _ -> liftIO $ fail "FAIL: IO metadata control did not resolve one name"
    let a = mkTyVar (mkSystemName (unique 81000) (mkTyVarOcc "metadata_a")) liftedTypeKind
        aTy = mkTyVarTy a
        xs = local 81001 "metadata_xs" (mkListTy aTy)
        arrow = mkFunctionType unmatchableDataConTy ManyTy
        payload v = arrow (mkTyConApp eqTyCon [varType v, TermVarTy v, TermVarTy v]) intTy
        telescope v = DepFunTy DepFunVisible v (arrow (payload v) (payload v))
        raw = telescope xs
        boolSubst = Subst.mkTvSubstPrs [(a, boolTy)]
        specialized = Subst.substTy boolSubst raw
        inspect label expected result = case result of
            DepFunTy DepFunVisible binder FunTy {ft_arg = FunTy {ft_arg = TyConApp _ [domain, TermVarTy left, TermVarTy right]}} -> do
                checkType dflags (label ++ " binder domain") expected (varType binder)
                checkType dflags (label ++ " proof domain") expected domain
                forM_ [left, right] $ \occurrence -> do
                    check (label ++ " actual binding identity") (occurrence == binder)
                    checkType dflags (label ++ " occurrence domain") expected (varType occurrence)
                    checkType dflags (label ++ " occurrence multiplicity") ManyTy (idMult occurrence)
                checkType dflags (label ++ " binder multiplicity") ManyTy (idMult binder)
                pure binder
            _ -> fail ("FAIL: " ++ label ++ " lost its raw dependent telescope")
    binder <- inspect "erased substitution" (mkListTy boolTy) specialized
    check "metadata-only update preserves the binder Unique" (binder == xs)
    check "erased substitution removes the old deep type dependency" $
        not (a `elemVarSet` tyCoVarsOfType specialized)
    lintAccepts dflags "closed specialization has consistent binder and occurrence types" [] (identity 81002 specialized)
    forM_ [("Int", intTy), ("function", arrow boolTy boolTy), ("IO", mkTyConApp ioTyCon [intTy])] $ \(label, actual) -> do
        let result = Subst.substTy (Subst.mkTvSubstPrs [(a, actual)]) raw
        _ <- inspect (label ++ " element metadata") (mkListTy actual) result
        check (label ++ " specialization removes the erased variable") $
            not (a `elemVarSet` tyCoVarsOfType result)
    let recursiveRange = mkTyConApp maybeTyCon [aTy]
        recursiveSubst = Subst.mkTvSubstPrs [(a, recursiveRange)]
        rangeResult = Subst.substTy recursiveSubst raw
        out = setIdType xs (mkListTy recursiveRange)
        targetSubst = Subst.extendSubstInScope recursiveSubst out
    _ <- inspect "apply-once binder metadata" (mkListTy recursiveRange) rangeResult
    case Subst.substTy targetSubst (TermVarTy xs) of
        TermVarTy actual -> checkType dflags "target metadata is not substituted again" (varType out) (varType actual)
        _ -> fail "FAIL: target representative lost its runtime identity"
    let collision = Subst.extendSubstInScope boolSubst xs
    clone <- inspect "cloned binder metadata" (mkListTy boolTy) (Subst.substTy collision raw)
    check "capture avoidance freshens the binder" (clone /= xs)
    let other = local 81003 "metadata_outer" (mkListTy boolTy)
        outer = CoreSubst.extendIdSubst (Subst.extendSubstInScope boolSubst other) xs (Var other)
    masked <- inspect "bound Id masks an outer range" (mkListTy boolTy) (Subst.substTy outer raw)
    check "the outer range does not replace the bound value" (masked == xs && masked /= other)
    let mult = mkTyVar (mkSystemName (unique 81004) (mkTyVarOcc "metadata_mult")) multiplicityTy
        scaled = setIdMult xs (mkTyVarTy mult)
        scaleSubst = Subst.mkTvSubstPrs [(a, boolTy), (mult, ManyTy)]
    _ <- inspect "binder multiplicity substitution" (mkListTy boolTy) (Subst.substTy scaleSubst (telescope scaled))
    let (nat, _, _) = peanoNat
        natTy = mkTyConTy nat
        n = local 81005 "metadata_n" natTy
        element = parameterizedDomainFixture 82000 [liftedTypeKind, natTy] (\_ _ -> [[]])
        elementTy = mkTyConApp element [intTy, TermVarTy n]
        elementSubst = Subst.extendSubstInScope (Subst.mkTvSubstPrs [(a, elementTy)]) n
        indexedResult = Subst.substTy elementSubst raw
    _ <- inspect "runtime-index-bearing element" (mkListTy elementTy) indexedResult
    check "runtime index remains a deep dependency" (n `elemVarSet` tyCoVarsOfType indexedResult)
    check "runtime-bearing specialization removes the erased variable" $
        not (a `elemVarSet` tyCoVarsOfType indexedResult)
    let k = mkTyVar (mkSystemName (unique 81006) (mkTyVarOcc "metadata_k")) liftedTypeKind
        abstract = mkTyVar (mkSystemName (unique 81007) (mkTyVarOcc "metadata_abstract")) (mkTyVarTy k)
        familyKind = mkTyVar (mkSystemName (unique 81008) (mkTyVarOcc "family_k")) liftedTypeKind
        familyArg = mkTyVar (mkSystemName (unique 81009) (mkTyVarOcc "family_a")) (mkTyVarTy familyKind)
        familyBinders = [mkNamedTyConBinder Inferred familyKind, mkAnonTyConBinder familyArg]
        family = mkFamilyTyCon (mkSystemName (unique 81010) (mkTcOcc "MetadataFamily"))
            (mkTyConKindWithMatchability unmatchableDataConTy familyBinders liftedTypeKind)
            familyBinders 0 liftedTypeKind Nothing OpenTypeFamilyTyCon Nothing NotInjective
        abstractId = local 81011 "metadata_abstract_index" (mkTyConApp family [mkTyVarTy k, mkTyVarTy abstract])
        kindSubst = Subst.extendSubstInScope (Subst.mkTvSubstPrs [(k, boolTy)]) abstract
        deepResult = Subst.deepSubstTy kindSubst (telescope abstractId)
    check "deep substitution removes the old kind from all native metadata" $
        not (k `elemVarSet` tyCoVarsOfType deepResult)
    case deepResult of
        DepFunTy DepFunVisible deepBinder _ -> case varType deepBinder of
            TyConApp _ [kind, TyVarTy actual] -> do
                checkType dflags "deep metadata substitutes the explicit kind" boolTy kind
                checkType dflags "deep metadata updates an unmapped variable kind" boolTy (tyVarKind actual)
                _ <- inspect "deep binder and occurrence metadata" (varType deepBinder) deepResult
                pure ()
            _ -> fail "FAIL: deep metadata lost the applied family domain"
        _ -> fail "FAIL: deep metadata lost the raw binder"

    let sigma = mkForAllTy unmatchableDataConTy (Bndr a Specified) raw
        z = mkTyVar (mkSystemName (unique 81030) (mkTyVarOcc "metadata_result")) liftedTypeKind
        reveal = mkForAllTy unmatchableDataConTy (Bndr z Specified) (mkTyVarTy z)
        completeResults =
            [ ("single forall result", piResultTy sigma boolTy)
            , ("batched forall result", piResultTys sigma [boolTy])
            , ("revealed forall restart", piResultTys reveal [sigma, boolTy])
            ]
    forM_ completeResults $ \(label, result) -> do
        _ <- inspect label (mkListTy boolTy) result
        checkType dflags (label ++ " result kind") liftedTypeKind (typeKind result)
        check (label ++ " removes the consumed type variable") $
            not (a `elemVarSet` tyCoVarsOfType result)
        lintAccepts dflags label [] (identity 81033 result)
    forM_
        [ ("single apply-once forall", piResultTy sigma recursiveRange)
        , ("batched apply-once forall", piResultTys sigma [recursiveRange])
        , ("apply-once forall restart", piResultTys reveal [sigma, recursiveRange])
        ]
        $ \(label, result) -> do
            _ <- inspect label (mkListTy recursiveRange) result
            checkType dflags (label ++ " result kind") liftedTypeKind (typeKind result)
    forM_
        [ ("single runtime-bearing forall", piResultTy sigma elementTy)
        , ("batched runtime-bearing forall", piResultTys sigma [elementTy])
        , ("runtime-bearing forall restart", piResultTys reveal [sigma, elementTy])
        ]
        $ \(label, result) -> do
            actual <- inspect label (mkListTy elementTy) result
            checkType dflags (label ++ " result kind") liftedTypeKind (typeKind result)
            check (label ++ " retains the earlier runtime index") $
                n `elemVarSet` tyCoVarsOfType result
            check (label ++ " removes the consumed type variable") $
                not (a `elemVarSet` tyCoVarsOfType result)
            case varType actual of
                TyConApp _ [TyConApp _ [_, TermVarTy actualN]] -> do
                    check (label ++ " keeps the actual earlier Id") (actualN == n)
                    checkType dflags (label ++ " earlier Id type") natTy (varType actualN)
                    checkType dflags (label ++ " earlier Id multiplicity") ManyTy (idMult actualN)
                _ -> fail ("FAIL: " ++ label ++ " lost the runtime-bearing element")

    let checkIndex label expected result = case result of
            TermVarTy actual -> do
                check (label ++ " actual Id") (actual == expected)
                checkType dflags (label ++ " actual type") (varType expected) (varType actual)
                checkType dflags (label ++ " actual multiplicity") ManyTy (idMult actual)
                check (label ++ " deep metadata dependencies") $
                    tyCoVarsOfType (varType actual) == tyCoVarsOfType (varType expected)
            _ -> fail ("FAIL: " ++ label ++ " lost its runtime index")
        sourceNames = tyCoVarsOfType (TermVarTy xs)
        merged = Subst.extendSubstInScopeSetFromSource targetSubst sourceNames
    checkIndex "source names preserve the target representative" out $
        Subst.substTy merged (TermVarTy xs)
    checkIndex "source-aware substitution preserves target metadata" out $
        Subst.substTyAddInScope targetSubst (TermVarTy xs)

    let later = local 81031 "metadata_later" (payload xs)
        alias = local 81032 "metadata_alias" (varType xs)
        (firstSubst, actualFirst) = Subst.substNativeIndexBndr recursiveSubst xs
        (prefixSubst, actualLater) = Subst.substNativeIndexBndr firstSubst later
        intermediateScope = mkInScopeSet (tyCoVarsOfTypes [TermVarTy xs, TermVarTy later])
        aliasSubst = CoreSubst.extendIdSubst (Subst.mkEmptySubst intermediateScope) alias (Var xs)
        composed = Subst.composeTCvSubst prefixSubst aliasSubst
        sequential = Subst.substTy prefixSubst (Subst.substTy aliasSubst (TermVarTy alias))
    check "ordered prefix keeps the first binder Unique" (actualFirst == xs)
    check "ordered prefix keeps the later binder Unique" (actualLater == later)
    checkType dflags "ordered prefix substitutes the first domain once" (mkListTy recursiveRange) (varType actualFirst)
    checkType dflags "ordered prefix updates the later metadata" (payload actualFirst) (varType actualLater)
    checkIndex "sequential prefix alias substitution" actualFirst sequential
    checkIndex "composed prefix alias range stays apply-once" actualFirst $
        Subst.substTy composed (TermVarTy alias)
    checkIndex "composition preserves the final later representative" actualLater $
        Subst.substTy composed (TermVarTy later)
    case Subst.substTy composed (TermVarTy later) of
        TermVarTy actual -> case varType actual of
            FunTy {ft_arg = TyConApp _ [domain, TermVarTy left, TermVarTy right]} -> do
                checkType dflags "later metadata has the updated index domain" (varType actualFirst) domain
                forM_ [("left", left), ("right", right)] $ \(side, nested) ->
                    checkIndex ("later metadata " ++ side ++ " index") actualFirst (TermVarTy nested)
            _ -> fail "FAIL: later metadata lost its indexed payload"
        _ -> fail "FAIL: composed prefix lost its later runtime index"

    -- Check changed domains independently of source inference. The free head
    -- keeps its runtime identity and receives an explicit nominal conversion.
    let (_, _, successor) = peanoNat
        k = local 81040 "transport_k" natTy
        next = mkTyConApp (promoteDataCon successor) [TermVarTy k]
        given = proof 81041 (TermVarTy n) next
        template = mkTyVar (mkSystemName (unique 81042) (mkTyVarOcc "transport_n")) natTy
        headValue = local 81043 "transport_head" elementTy
        inputList = setIdType xs (mkListTy elementTy)
        input = mkDepFunTy inputList (arrow (payload inputList) (payload headValue))
        rightElement = mkTyConApp element [intTy, next]
    abstracted <- case abstractNativeIndices [(TermVarTy n, template)] input of
        Just result -> pure result
        Nothing -> fail "FAIL: generic domain transport abstraction failed"
    let lifting = prepareNativeLiftingContext (mkLiftingContext [(template, mkCoVarCo given)]) abstracted
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        let evidence = liftCoSubst role lifting abstracted
        check "generic domain transport preserves the requested role" (coercionRole evidence == role)
        checkType dflags "generic domain transport source" input (coercionLKind evidence)
        lintAccepts dflags "generic domain transport evidence" [n, k, headValue, given] (Coercion evidence)
        case coercionRKind evidence of
            DepFunTy DepFunVisible binder FunTy{ft_res = FunTy{ft_arg = TyConApp _ [domain, left, right]}} -> do
                checkType dflags "transported list binder domain" (mkListTy rightElement) (varType binder)
                checkType dflags "transported captured value domain" rightElement domain
                check "transport binds the later list" (not (binder `elemVarSet` tyCoVarsOfType (coercionRKind evidence)))
                forM_ [left, right] $ \index -> do
                    checkType dflags "transported captured index kind" rightElement (typeKind index)
                    case index of
                        CastTy (TermVarTy actual) delta -> do
                            check "transport preserves the captured runtime identity" (actual == headValue)
                            checkType dflags "captured runtime value keeps its source type" elementTy (varType actual)
                            checkType dflags "captured domain proof source" elementTy (coercionLKind delta)
                            checkType dflags "captured domain proof target" rightElement (coercionRKind delta)
                            check "captured domain proof stays nominal" (coercionRole delta == Nominal)
                        _ -> fail "FAIL: captured runtime value lost its explicit domain conversion"
            _ -> fail "FAIL: generic domain transport lost its dependent telescope"

-- | Check both zonkers on original native Id prototypes with filled metadata.
runNativeZonkMetadataChecks :: FilePath -> IO ()
runNativeZonkMetadataChecks libdir = runGhc (Just libdir) $ do
    initialFlags <- getSessionDynFlags
    _ <- setSessionDynFlags initialFlags
    env <- getSession
    dflags <- getSessionDynFlags
    liftIO $ do
        (_, outcome) <- TcM.initTcInteractive TcM.NoTcMPlugins env $ do
            alpha <- newFlexiTyVar liftedTypeKind
            mu <- newFlexiTyVar multiplicityTy
            ma <- newFlexiTyVar matchabilityTy
            let xs = setIdMult (local 81020 "zonk_metadata_xs" (mkListTy (mkTyVarTy alpha))) (mkTyVarTy mu)
                same = mkTyConApp eqTyCon [varType xs, TermVarTy xs, TermVarTy xs]
                raw = DepFunTy DepFunVisible xs (mkFunctionType unmatchableDataConTy ManyTy same intTy)
                f = setIdMult (local 81021 "zonk_metadata_head" (mkFunctionType (mkTyVarTy ma) (mkTyVarTy mu) boolTy boolTy)) (mkTyVarTy mu)
                call = TermAppTy f (constructorIndex trueDataCon)
                callCo = TermAppCo f (mkNomReflCo (constructorIndex trueDataCon))
            TcM.liftZonkM $ do
                TcZonk.writeMetaTyVar alpha intTy
                TcZonk.writeMetaTyVar mu ManyTy
                TcZonk.writeMetaTyVar ma unmatchableDataConTy
            within <- TcM.liftZonkM (TcZonk.zonkTcType raw)
            free <- TcM.liftZonkM (TcZonk.zonkTyCoVar xs)
            final <- FinalZonk.zonkTcTypeToType raw
            ambient <- FinalZonk.initZonkEnv FinalZonk.DefaultFlexi $ FinalZonk.ZonkT $ \ze ->
                FinalZonk.runZonkT (FinalZonk.zonkTcTypeToTypeX (TermVarTy xs))
                    ze {FinalZonk.ze_id_env = error "native metadata inspected the recursive Id environment"}
            withinCall <- TcM.liftZonkM (TcZonk.zonkTcType call)
            finalCall <- FinalZonk.zonkTcTypeToType call
            withinCo <- TcM.liftZonkM (TcZonk.zonkCo callCo)
            finalCo <- FinalZonk.initZonkEnv FinalZonk.DefaultFlexi (FinalZonk.zonkCoToCo callCo)
            pure (xs, [alpha, mu], [within, final], [free, ambient], f,
                  [withinCall, finalCall, coercionLKind withinCo, coercionLKind finalCo])
        case outcome of
            Nothing -> fail "FAIL: native metadata zonking controls could not initialize TcM"
            Just (xs, metas, telescopes, freeIndices, f, calls) -> do
                let checkId label expected original actual = do
                        check (label ++ " retains the binding identity") (actual == original)
                        checkType dflags (label ++ " stored domain") expected (varType actual)
                        checkType dflags (label ++ " stored multiplicity") ManyTy (idMult actual)
                forM_ telescopes $ \result -> do
                    case result of
                        DepFunTy DepFunVisible binder FunTy {ft_arg = TyConApp _ [domain, TermVarTy left, TermVarTy right]} -> do
                            checkType dflags "zonked proof domain" (mkListTy intTy) domain
                            forM_ [binder, left, right] (checkId "zonked native Id" (mkListTy intTy) xs)
                        _ -> fail "FAIL: zonking lost the raw dependent telescope"
                    check "zonking removes filled metadata metas from deep dependencies" $
                        all (\meta -> not (meta `elemVarSet` tyCoVarsOfType result)) metas
                forM_ freeIndices $ \result -> case result of
                    TermVarTy actual -> checkId "zonked ambient index" (mkListTy intTy) xs actual
                    _ -> fail "FAIL: zonking lost the ambient runtime identity"
                forM_ calls $ \result -> case result of
                    TermAppTy actual _ -> checkId "zonked call head" (mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy) f actual
                    _ -> fail "FAIL: zonking lost the actual call head"

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
        indexedNat ty = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [natTy, ty, ty]) intTy
        keepNat = Lam n (identity 535 (indexedNat index))
        keepNatResult ty = mkFunctionType unmatchableDataConTy ManyTy (indexedNat ty) (indexedNat ty)
        zeroProof = proof 536 index zeroTy
        succProof = proof 537 index (succTy (TermVarTy field))
        branch number concrete cv = Cast body (mkSymCo transport)
          where
            dictionary = local number "nat_dictionary" (mkTyConApp eqTyCon [natTy, concrete, concrete])
            body = Lam dictionary (Var payload)
            co = mkCoVarCo cv
            predicateCo = mkTyConAppCo Nominal eqTyCon [mkNomReflCo natTy, co, co]
            transport = mkFunCoNoFTF Representational (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) (mkSubCo predicateCo) (mkReflCo Representational intTy)
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
    let beta = local 3100 "beta_index" natTy
        innerBeta = local 3101 "beta_index" natTy
        boolBeta = local 3102 "beta_bool" boolTy
        boolInput = local 3103 "beta_bool_input" boolTy
        betaOpaque =
            setIdUnfolding
                (local 3105 "opaque_beta_index" natTy)
                (unfolding (error "forced a beta index unfolding"))
        opaqueProducer =
            setIdUnfolding
                (local 3104 "opaque_beta_producer" (mkFunctionType unmatchableDataConTy ManyTy natTy natTy))
                (unfolding (error "forced a beta producer unfolding"))
        betaIdentity argument = App (Lam beta (Var beta)) argument
        betaSuccessor argument = App (Lam beta (succExpr (Var beta))) argument
        betaConstant argument = App (Lam beta zeroExpr) argument
        betaCases =
            [ ("identity variable", betaIdentity (Var m), Var m, TermVarTy m)
            , ("identity constructor", betaIdentity zeroExpr, zeroExpr, zeroTy)
            , ("reflexive nominally recoverable cast", betaIdentity (Cast (Var m) (mkReflCo Representational natTy)), Cast (Var m) (mkReflCo Representational natTy), TermVarTy m)
            , ("stored range avoids reused-Unique capture", App (Lam beta (App (Lam m (Var beta)) zeroExpr)) (Var m), Var m, TermVarTy m)
            , ("successor variable", betaSuccessor (Var m), succExpr (Var m), succTy (TermVarTy m))
            , ("constant ignores opaque index", betaConstant (Var betaOpaque), zeroExpr, zeroTy)
            , ("nested atomic argument", betaSuccessor (App (Lam innerBeta (Var innerBeta)) (Var m)), succExpr (Var m), succTy (TermVarTy m))
            , ("nested body", App (Lam beta (succExpr (App (Lam innerBeta (Var innerBeta)) (Var beta)))) (Var m), succExpr (Var m), succTy (TermVarTy m))
            ]
    forM_ betaCases $ \(label, expression, expectedValue, expectedIndex) -> do
        case normaliseNativeIndexExpr expression of
            Just (normalised, actualIndex) -> do
                check ("checked beta runtime form: " ++ label) (cheapEqExpr expectedValue normalised)
                checkType dflags ("checked beta index: " ++ label) expectedIndex actualIndex
                checkType dflags ("checked beta domain: " ++ label) natTy (typeKind actualIndex)
                lintAccepts dflags ("normalised beta value: " ++ label) [m, betaOpaque] normalised
            Nothing -> fail ("FAIL: checked beta was not an index: " ++ label)
        lintAccepts dflags ("retained beta application: " ++ label) [m, betaOpaque] (App keepNat expression)
        checkType dflags ("retained beta exact result: " ++ label) (keepNatResult expectedIndex) (exprType (App keepNat expression))
    case normaliseNativeIndexExpr (betaIdentity (Var m)) of
        Just (Var actual, TermVarTy actualIndex) ->
            check "beta preserves the original shared runtime Id" (actual == m && actualIndex == m)
        _ -> fail "FAIL: beta identity changed runtime variable sharing"
    forM_
        [ (App (Lam boolBeta (Var boolBeta)) (Var boolInput), Var boolInput, TermVarTy boolInput)
        , (App (Lam boolBeta (Var (dataConWorkId trueDataCon))) (Var boolInput), Var (dataConWorkId trueDataCon), mkTyConTy (promoteDataCon trueDataCon))
        , (App (Lam beta (Var (dataConWorkId trueDataCon))) (Var m), Var (dataConWorkId trueDataCon), mkTyConTy (promoteDataCon trueDataCon))
        ]
        $ \(expression, expectedValue, expectedIndex) -> case normaliseNativeIndexExpr expression of
            Just (normalised, actualIndex) -> do
                check "Bool beta retains its runtime form" (cheapEqExpr expectedValue normalised)
                checkType dflags "Bool beta retains its exact index" expectedIndex actualIndex
                checkType dflags "Bool beta index kind" boolTy (typeKind actualIndex)
                lintAccepts dflags "Bool beta ordinary typing" [boolInput, m] expression
            Nothing -> fail "FAIL: checked Bool beta was rejected"
    forM_
        [ ("linear binder", App (Lam (setIdMult beta OneTy) (Var beta)) (Var m))
        , ("linear argument", betaConstant (Var (setIdMult m OneTy)))
        , ("wrong argument domain", betaIdentity (Var (dataConWorkId trueDataCon)))
        , ("ignored wrong argument domain", betaConstant (Var (dataConWorkId trueDataCon)))
        , ("wrong Bool argument domain", App (Lam boolBeta (Var boolBeta)) zeroExpr)
        , ("malformed constructor body", App (Lam beta (succExpr (Var (dataConWorkId trueDataCon)))) (Var m))
        , ("non-atomic successor argument", betaIdentity (succExpr (Var m)))
        , ("opaque producer argument", betaIdentity (App (Var opaqueProducer) (Var m)))
        , ("opaque producer body", App (Lam beta (App (Var opaqueProducer) (Var beta))) (Var m))
        , ("let argument", betaIdentity (Let (NonRec innerBeta (Var m)) (Var innerBeta)))
        ]
        $ \(label, expression) -> check ("checked beta rejects " ++ label) $ case normaliseNativeIndexExpr expression of
            Nothing -> True
            Just _ -> False
    lintRejects dflags "beta cannot admit an out-of-scope runtime argument" [] (App keepNat (betaIdentity (Var m)))
    lintRejects dflags "beta cannot erase an ignored out-of-scope argument" [] (App keepNat (betaConstant (Var m)))
    let representationalProof = mkCoVar (mkSystemName (unique 3106) (mkVarOcc "beta_repr")) (mkReprEqPred natTy natTy)
        representationalBeta = betaIdentity (Cast (Var m) (mkCoVarCo representationalProof))
    lintAccepts dflags "ordinary beta permits a scoped representational cast" [m, representationalProof] representationalBeta
    lintRejects dflags "beta index conversion requires nominal evidence" [m, representationalProof] $
        App keepNat representationalBeta
    lintRejects dflags "beta does not unfold an opaque producer" [m, opaqueProducer] $
        App keepNat (betaIdentity (App (Var opaqueProducer) (Var m)))
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
                (mkTyConKindWithMatchability unmatchableDataConTy binders natTy)
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
                (mkTyConKindWithMatchability unmatchableDataConTy forgetBinders liftedTypeKind)
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
                (if role == Phantom then mkDepFunTy n forgotten else mkFunctionType unmatchableDataConTy ManyTy natTy intTy)
        checkNormalization
            role
            "ordinary forall kind normalizes above retained body"
            []
            (mkForAllTy unmatchableDataConTy (Bndr erased Specified) (mkDepFunTy n (indexedNat (add zeroTy index))))
            (mkForAllTy unmatchableDataConTy (Bndr erasedNormalized Specified) (mkDepFunTy n (indexedNat (if role == Phantom then add zeroTy index else index))))
    let originalShadow = mkFunctionType unmatchableDataConTy ManyTy (indexedNat index) (mkDepFunTy n (indexedNat (add zeroTy index)))
        expectedShadow = mkFunctionType unmatchableDataConTy ManyTy (indexedNat index) (mkDepFunTy n (indexedNat index))
    freshened <- checkNormalization Nominal "retained normalization avoids a free outer Id" [n] originalShadow expectedShadow
    case freshened of
        FunTy{ft_res = DepFunTy DepFunVisible fresh body} -> do
            check "retained normalization freshens a shadowing Id" (fresh /= n)
            checkType dflags "retained normalization uses the fresh Id in its body" (indexedNat (TermVarTy fresh)) body
            check "retained normalization preserves its free outer Id" (n `elemVarSet` tyCoVarsOfType freshened)
            check "retained normalization binds the fresh Id" (not (fresh `elemVarSet` tyCoVarsOfType freshened))
        _ -> fail "FAIL: retained normalization lost its nested binder"
    let opaqueInt = local 81034 "normalised_int_index" intTy
        intPayload = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [intTy, TermVarTy opaqueInt, TermVarTy opaqueInt]) intTy
        intTelescope = DepFunTy DepFunVisible opaqueInt intPayload
    _ <- checkNormalization Nominal "opaque Int retained domain" [] intTelescope intTelescope
    forM_
        [ ("type variable", mkTyVar (mkSystemName (unique 1704) (mkTyVarOcc "bad_index")) natTy)
        , ("coercion variable", proof 1705 zeroTy zeroTy)
        , ("linear Id", setIdMult n OneTy)
        , ("join Id", asJoinId (local 1706 "bad_index" (mkVisFunTyManyU boolTy boolTy)) 1)
        , ("unlifted domain", local 1707 "bad_index" intPrimTy)
        ]
        $ \(label, bad) ->
            rejectsWith ("normalization rejects a retained " ++ label) "normalise_type: invalid retained function" $
                evaluate (normaliseType emptyFamInstEnvs Nominal (DepFunTy DepFunVisible bad intTy))
    forM_ [("unlifted", intPrimTy), ("constraint", mkNomEqPred index index)] $ \(label, body) ->
        rejectsWith ("normalization rejects a retained " ++ label ++ " result") "normalise_type: invalid retained function" $
            evaluate (normaliseType emptyFamInstEnvs Nominal (DepFunTy DepFunVisible n body))
    let captured = mkDepFunTy m (mkFunctionType unmatchableDataConTy ManyTy (indexedNat (TermVarTy m)) (indexedNat index))
    case Subst.substTy replace captured of
        DepFunTy DepFunVisible fresh body -> do
            check "Nat substitution avoids capturing a field Id" (fresh /= m)
            checkType
                dflags
                "Nat substitution preserves both binder identities"
                (mkFunctionType unmatchableDataConTy ManyTy (indexedNat (TermVarTy fresh)) (indexedNat (succTy (TermVarTy m))))
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
        transport = mkFunCoNoFTF Representational (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) (mkSubCo predicateCo) (mkReflCo Representational intTy)
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
    lintAccepts dflags "DEFAULT alias evidence accepts a checked identity beta index" [n, payload] $
        Case (App (identity 569 natTy) (Var n)) alias intTy [Alt DEFAULT [proof 570 aliasIndex index] (Var payload)]
    lintRejects dflags "DEFAULT alias evidence cannot describe an opaque producer" [n, payload, opaqueProducer] $
        Case (App (Var opaqueProducer) (Var n)) alias intTy [Alt DEFAULT [proof 570 aliasIndex index] (Var payload)]

-- | Construct an index substitution with an explicit scope.
indexSubst :: [Var] -> Id -> CoreExpr -> Subst.Subst
indexSubst scope b rhs =
    Subst.Subst (mkInScopeSet (mkVarSet scope)) (unitVarEnv b rhs) emptyVarEnv emptyVarEnv

-- | Assert a semantic property and identify its failure precisely.
check :: String -> Bool -> IO ()
check label condition = unless condition (fail ("FAIL: " ++ label))


{- Note [Function field constructor interface checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Import each declaration twice to distinguish fresh field binders from repeated
references to one field. The payload uses both the actual function head and
its following scalar argument. Instantiating a pattern must substitute these
fields in order. The lifted payload is an equality-dictionary function. It is
not an executed vector; the separate source module checks a generic Nat/Vec
package through its compiled object and interface.

Call congruence uses the ordinary AppCo encoding. Raw diagnostic call-coercion
imports remain rejected. The constructor-field checks use the
prefix decoder and its scoped call-type codec. The generic Pi checks demand
ordinary field types after all imported parent knots exist. Unsupported raw
domains must fail before typed field reconstruction recurses. Normalization
remains subject to typed checks when the raw view is inconclusive. Finite
first-order recursive declarations and nullary synonyms remain valid.
A timeout or an unexpected exception fails. These checks do not prove
termination for arbitrary interface data.
-}

-- | Check retained types and closed expressions through binary interface tables.
runIfaceChecks :: FilePath -> IO ()
runIfaceChecks libdir = runGhc (Just libdir) $ do
    initialFlags <- getSessionDynFlags
    _ <- setSessionDynFlags initialFlags
    declaration <- parseImportDecl "import Prelude"
    setContext [IIDecl declaration]
    names <- parseName "IO"
    ioTyCon <- case names of
        name :| [] -> do
            thing <- lookupName name
            case thing of
                Just (ATyCon tc) -> pure tc
                _ -> liftIO $ fail "FAIL: IO interface control did not resolve a type constructor"
        _ -> liftIO $ fail "FAIL: IO interface control did not resolve one name"
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
            -- | Resolve an opaque element TyCon without loading a home module.
            restoreWithElement :: TyCon -> IfaceType -> IO Type
            restoreWithElement element input = do
                types <- newIORef (mkTypeEnv [ATyCon element])
                let modu = case toIfaceType (mkTyConTy element) of
                        IfaceTyConApp (IfaceTyCon name _) _ -> nameModule name
                        _ -> error "FAIL: opaque element does not have a TyCon name"
                    elementEnv = env{hsc_type_env_vars = KnotVars [modu] (\candidate -> if candidate == modu then Just types else Nothing)}
                expression <- initIfaceCheck (text "opaque element interface test") elementEnv $
                    initIfaceLcl ifaceModule (text "opaque element interface test") NotBoot $
                        tcIfaceExpr (IfaceType input)
                case expression of
                    Type ty -> pure ty
                    _ -> fail "FAIL: opaque element reconstruction returned a term"
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
            restorePackage :: HscEnv -> IfaceDecl -> IO TyCon
            restorePackage = restorePackageWithScope []
            restorePackageWithScope :: [Id] -> HscEnv -> IfaceDecl -> IO TyCon
            restorePackageWithScope scope importEnv declaration = do
                thing <-
                    initIfaceLoad importEnv
                        $ initIfaceLcl (nameModule (ifName declaration)) (text "retained constructor interface test") NotBoot
                        $ extendIfaceIdEnv scope
                        $ tcIfaceDecl False declaration
                case thing of
                    ATyCon package -> pure package
                    _ -> fail "FAIL: constructor declaration did not reconstruct a TyCon"
            forcePackage :: TyCon -> IO ()
            forcePackage package = forM_ (tyConDataCons package) $ \constructor -> do
                _ <- evaluate (dataConWrapperType constructor)
                forM_ (dataConOrigArgTys constructor) $ \(Scaled mult ty) -> do
                    _ <- evaluate (sizeVarSet (tyCoVarsOfType mult))
                    _ <- evaluate (sizeVarSet (tyCoVarsOfType ty))
                    pure ()
            changePackage :: (IfaceConDecl -> IfaceConDecl) -> IfaceDecl -> IfaceDecl
            changePackage change declaration@IfaceData{ifCons = IfDataTyCon typeData [constructor]} =
                declaration{ifCons = IfDataTyCon typeData [change constructor]}
            changePackage _ _ = error "FAIL: constructor interface fixture has the wrong shape"
            rejectsPackage :: String -> String -> IfaceDecl -> IO ()
            rejectsPackage = rejectsPackageWithScope []
            rejectsPackageWithScope :: [Id] -> String -> String -> IfaceDecl -> IO ()
            rejectsPackageWithScope scope label fragment declaration = do
                messages <- newIORef []
                let logger = pushLogHook (\_ _ _ _ message -> modifyIORef' messages (showSDoc dflags message :)) (hsc_logger env)
                    quietEnv = env{hsc_logger = logger}
                decoded <- roundtrip NormalCompression declaration
                outcome <- try @IOEnvFailure $ try @GhcException $ do
                    completed <- timeout 2000000 (restorePackageWithScope scope quietEnv decoded >>= forcePackage)
                    case completed of
                        Just () -> pure ()
                        Nothing -> fail ("FAIL: " ++ label ++ "; constructor reconstruction did not terminate")
                case outcome of
                    Left _ -> pure ()
                    Right (Left (ProgramError message)) ->
                        check (label ++ " fork failure") (message == "Cannot continue after interface file error")
                    Right (Left exception) -> fail ("FAIL: " ++ label ++ "; unexpected exception: " ++ displayException exception)
                    Right (Right ()) -> fail ("FAIL: " ++ label ++ "; malformed constructor interface succeeded")
                diagnostics <- unlines <$> readIORef messages
                check (label ++ "; diagnostic: " ++ diagnostics) (fragment `isInfixOf` diagnostics)
            -- | Reconstruct all declaration knots before demanding field types.
            restoreImportedKnots :: CompressionIFace -> HscEnv -> [IfaceDecl] -> IO [TyCon]
            restoreImportedKnots compression importEnv declarations = do
                temporaryTypes <- newIORef (mkTypeEnv [])
                decoded <- mapM (roundtrip compression) declarations
                let modu = case decoded of
                        first : _ -> nameModule (ifName first)
                        [] -> error "FAIL: recursive interface fixture is empty"
                    knotEnv = importEnv{hsc_type_env_vars = KnotVars [modu] (\candidate -> if candidate == modu then Just temporaryTypes else Nothing)}
                imported <-
                    initIfaceCheck (text "recursive retained interface test") knotEnv $
                        mapM (\decl -> initIfaceLcl modu (text "recursive retained interface test") NotBoot (tcIfaceDecl False decl)) decoded
                writeIORef temporaryTypes (mkTypeEnv imported)
                forM imported $ \thing -> case thing of
                    ATyCon package -> pure package
                    _ -> fail "FAIL: recursive interface did not reconstruct a TyCon"
            -- | Require a normal interface error with every expected diagnostic fragment.
            rejectsImportedKnots :: String -> [String] -> [IfaceDecl] -> IO ()
            rejectsImportedKnots label fragments declarations =
                forM_ [NormalCompression, MaximumCompression] $ \compression -> do
                    messages <- newIORef []
                    let quietEnv = env{hsc_logger = pushLogHook (\_ _ _ _ message -> modifyIORef' messages (showSDoc dflags message :)) (hsc_logger env)}
                    outcome <- try @IOEnvFailure $ try @GhcException $ do
                        completed <- timeout 2000000 $ do
                            imported <- restoreImportedKnots compression quietEnv declarations
                            forM_ imported $ \package -> do
                                forcePackage package
                                _ <- evaluate (isNativeIndexType (mkTyConTy package))
                                pure ()
                        case completed of
                            Just () -> pure ()
                            Nothing -> fail ("FAIL: " ++ label ++ "; imported domain validation did not terminate")
                    case outcome of
                        Left _ -> pure ()
                        Right (Left (ProgramError message)) ->
                            check (label ++ " fork failure") (message == "Cannot continue after interface file error")
                        Right (Left exception) -> fail ("FAIL: " ++ label ++ "; unexpected exception: " ++ displayException exception)
                        Right (Right ()) -> fail ("FAIL: " ++ label ++ "; recursive retained domain was accepted")
                    diagnostics <- unlines <$> readIORef messages
                    forM_ fragments $ \fragment ->
                        check (label ++ "; diagnostic: " ++ diagnostics) (fragment `isInfixOf` diagnostics)
                    check (label ++ "; no recursive-information fallback") $
                        not ("recursive type information" `isInfixOf` diagnostics)
            -- | Force valid knots in the supplied order and check scalar-domain eligibility.
            acceptsImportedKnots :: String -> [(IfaceDecl, Bool)] -> IO ()
            acceptsImportedKnots label declarations =
                forM_ [NormalCompression, MaximumCompression] $ \compression -> do
                    completed <- timeout 2000000 $ do
                        imported <- restoreImportedKnots compression env (map fst declarations)
                        forM_ (zip imported declarations) $ \(package, (_, eligible)) -> do
                            forcePackage package
                            check (label ++ "; scalar domain eligibility") (isNativeIndexType (mkTyConTy package) == eligible)
                    check (label ++ "; imported declarations terminate") (completed == Just ())
        -- Parameter fields remain opaque after both erased and runtime-bearing
        -- instantiation. Check actual imported Id metadata, not only eqType.
        -- See Note [Opaque runtime index values] in GHC.Core.Type.
        let elementVar = mkTyVar (mkSystemName (unique 110000) (mkTyVarOcc "wire_element")) liftedTypeKind
            listTelescope base element =
                let headId = local base "wire_head" element
                    tailId = local (base + 1) "wire_tail" (mkListTy element)
                    listId = local (base + 2) "wire_list" (mkListTy element)
                    consIndex = mkTyConApp (promoteDataCon consDataCon) [element, TermVarTy headId, TermVarTy tailId]
                    equality = mkTyConApp eqTyCon [mkListTy element, TermVarTy listId, consIndex]
                 in mkDepFunTy headId $ mkDepFunTy tailId $ mkDepFunTy listId $
                        mkFunctionType unmatchableDataConTy ManyTy equality intTy
            genericList = tidyTopType $ mkForAllTy unmatchableDataConTy (Bndr elementVar Specified) $
                listTelescope 110001 (mkTyVarTy elementVar)
            checkListMetadata label element result = case result of
                DepFunTy DepFunVisible headId (DepFunTy DepFunVisible tailId (DepFunTy DepFunVisible listId FunTy{ft_arg = TyConApp _ [domain, TermVarTy left, TyConApp _ [actualElement, TermVarTy headIndex, TermVarTy tailIndex]]})) -> do
                    checkType dflags (label ++ " head type") element (varType headId)
                    forM_ [tailId, listId] $ \index ->
                        checkType dflags (label ++ " list type") (mkListTy element) (varType index)
                    checkType dflags (label ++ " proof domain") (mkListTy element) domain
                    checkType dflags (label ++ " constructor parameter") element actualElement
                    check (label ++ " actual field identities") (left == listId && headIndex == headId && tailIndex == tailId)
                    check (label ++ " distinct field identities") (headId /= tailId && headId /= listId && tailId /= listId)
                    forM_ [(left, mkListTy element), (headIndex, element), (tailIndex, mkListTy element)] $ \(occurrence, expected) -> do
                        checkType dflags (label ++ " occurrence type") expected (varType occurrence)
                        checkType dflags (label ++ " occurrence multiplicity") ManyTy (idMult occurrence)
                    forM_ [headId, tailId, listId] $ \index ->
                        checkType dflags (label ++ " field multiplicity") ManyTy (idMult index)
                _ -> fail ("FAIL: " ++ label ++ "; imported list telescope changed shape")
            earlier = local 110010 "wire_earlier" boolTy
            elementPrototype = parameterizedDomainFixture 110020 [liftedTypeKind, boolTy] (\_ _ -> [[]])
        importedElements <- restoreImportedKnots NormalCompression env [tyThingToIfaceDecl (ATyCon elementPrototype)]
        elementOwner <- case importedElements of
            [element] -> pure element
            _ -> fail "FAIL: opaque element declaration did not reconstruct one TyCon"
        let runtimeElement = mkTyConApp elementOwner [intTy, TermVarTy earlier]
            runtimeList = tidyTopType $ mkDepFunTy earlier (listTelescope 110200 runtimeElement)
        forM_ [NormalCompression, MaximumCompression] $ \compression -> do
            restored <- restore [] =<< roundtrip compression (toIfaceType genericList)
            checkType dflags "generic list interface alpha equality" genericList restored
            check "generic list interface has no escaped dependencies" (isEmptyVarSet (tyCoVarsOfType restored))
            case restored of
                ForAllTy ma (Bndr actual _) body -> do
                    checkType dflags "generic list forall metadata" unmatchableDataConTy ma
                    check "generic list type binder is fresh" (actual /= elementVar)
                    checkListMetadata "generic list interface" (mkTyVarTy actual) body
                _ -> fail "FAIL: generic list interface lost its erased parameter"
            forM_ [boolTy, intTy, mkTyConApp ioTyCon [intTy], mkVisFunTyManyU boolTy boolTy, mkFunctionType unmatchableDataConTy ManyTy boolTy intTy, mkFunctionType unmatchableDataConTy OneTy boolTy boolTy] $ \element -> do
                let specialized = tidyTopType (listTelescope 110300 element)
                actual <- restore [] =<< roundtrip compression (toIfaceType specialized)
                checkType dflags "opaque list specialization alpha equality" specialized actual
                checkListMetadata "opaque list specialization" element actual
            runtime <- restoreWithElement elementOwner =<< roundtrip compression (toIfaceType runtimeList)
            checkType dflags "runtime-bearing list interface alpha equality" runtimeList runtime
            check "runtime-bearing list has no escaped dependencies" (isEmptyVarSet (tyCoVarsOfType runtime))
            case runtime of
                DepFunTy DepFunVisible index body -> do
                    let element = mkTyConApp elementOwner [intTy, TermVarTy index]
                    check "runtime-bearing list prefix is fresh" (index /= earlier)
                    checkType dflags "runtime-bearing list prefix type" boolTy (varType index)
                    checkListMetadata "runtime-bearing list interface" element body
                _ -> fail "FAIL: runtime-bearing list interface lost its earlier index"
            let wrongHead = local 110310 "wire_wrong_head" boolTy
                intTail = local 110311 "wire_int_tail" (mkListTy intTy)
                malformed = toIfaceType $ mkTyConApp (promoteDataCon consDataCon) [intTy, TermVarTy wrongHead, TermVarTy intTail]
            rejectsIface "Int cons cannot contain a Bool head" "Invalid retained constructor fields in interface" [wrongHead, intTail] =<< roundtrip compression malformed
        let package = retainedIfacePackage 2500 False
            vacuousPackage = retainedIfacePackage 2520 True
            declaration = tyThingToIfaceDecl (ATyCon package)
            vacuousDeclaration = tyThingToIfaceDecl (ATyCon vacuousPackage)
            originalField = case dataConRepArgBinders (tyConSingleDataCon package) of
                [Just field, Nothing] -> field
                _ -> error "FAIL: retained package fixture has the wrong metadata"
        forM_ (zip [0 :: Int ..] [NormalCompression, MaximumCompression]) $ \(iteration, compression) -> do
            wire <- roundtrip compression declaration
            first <- restorePackage env wire
            secondWire <- roundtrip compression declaration
            second <- restorePackage env secondWire
            forcePackage first
            forcePackage second
            let firstConstructor = tyConSingleDataCon first
                secondConstructor = tyConSingleDataCon second
                packageTy = mkTyConTy first
                base = 2600 + iteration * 100
                argument = local base "imported_index" boolTy
                payload = local (base + 1) "imported_payload" (indexed (TermVarTy argument))
                scrutinee = local (base + 2) "imported_package" packageTy
                caseBinder = local (base + 3) "imported_package_case" packageTy
                answer = local (base + 4) "imported_answer" intTy
                (existentials, fields) = dataConRepInstPat (map unique [base + 10 .. base + 20]) ManyTy firstConstructor []
            case (dataConRepArgBinders firstConstructor, dataConRepArgBinders secondConstructor) of
                ([Just field, Nothing], [Just otherField, Nothing]) -> do
                    check "constructor interface imports allocate fresh retained Ids" $
                        field /= originalField && otherField /= originalField && field /= otherField
                    check "constructor interface retained binder remains a valid local index" (isNativeIndexId field && isNativeIndexId otherField)
                    case dataConOrigArgTys firstConstructor of
                        [Scaled _ domain, Scaled _ payloadTy] -> do
                            checkType dflags "constructor interface retained domain" boolTy domain
                            checkType dflags "constructor interface payload shares its preceding Id" (indexed (TermVarTy field)) payloadTy
                        _ -> fail "FAIL: imported retained constructor has the wrong field count"
                    checkType dflags "constructor interfaces preserve alpha-equivalent worker types" (idType (dataConWorkId firstConstructor)) (idType (dataConWorkId secondConstructor))
                _ -> fail "FAIL: constructor interface lost retained-field metadata"
            check "constructor interface does not add erased existentials" (null existentials)
            lintAccepts dflags "imported native package construction" [argument, payload] $
                mkApps (Var (dataConWorkId firstConstructor)) [Var argument, Var payload]
            lintAccepts dflags "imported native package elimination" [scrutinee, answer] $
                Case (Var scrutinee) caseBinder intTy [Alt (DataAlt firstConstructor) fields (Var answer)]
            lintAccepts dflags "imported native package repackaging preserves field scope" [scrutinee] $
                Case (Var scrutinee) caseBinder packageTy [Alt (DataAlt firstConstructor) fields (mkApps (Var (dataConWorkId firstConstructor)) (map Var fields))]
            vacuous <- restorePackage env =<< roundtrip compression vacuousDeclaration
            forcePackage vacuous
            let vacuousConstructor = tyConSingleDataCon vacuous
            check "constructor interface preserves vacuous retained metadata" $ case dataConRepArgBinders vacuousConstructor of
                [Just field, Nothing] -> isNativeIndexId field
                _ -> False
            check "vacuous constructor worker still has an ordinary arrow" $ case idType (dataConWorkId vacuousConstructor) of
                FunTy{} -> True
                _ -> False
        let constructor = case declaration of
                IfaceData{ifCons = IfDataTyCon _ [con]} -> con
                _ -> error "FAIL: retained constructor interface has the wrong shape"
            firstName = case ifConArgBinders constructor of
                [Just name, Nothing] -> name
                _ -> error "FAIL: retained constructor interface has the wrong binder count"
            dependent = ifConArgTys constructor !! 1
            typeName = mkIfLclName (fsLit "package_parameter")
            typeBinder = (typeName, toIfaceType liftedTypeKind)
            boolCon = case toIfaceType boolTy of
                IfaceTyConApp con _ -> con
                _ -> error "FAIL: Boolean interface domain has the wrong shape"
            many = toIfaceType ManyTy
            bool = toIfaceType boolTy
            invalid = "Invalid retained constructor"
        -- See Note [Function field constructor interface checks] below.
        let functionTy = mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy
            scalarName = mkIfLclName (fsLit "function_package_scalar")
            functionTemplate = mkSysLocal (ifLclNameFS firstName) (unique 9500) ManyTy functionTy
            scalarTemplate = mkSysLocal (ifLclNameFS scalarName) (unique 9501) ManyTy boolTy
            functionPayload = toIfaceType (indexed (TermAppTy functionTemplate (TermVarTy scalarTemplate)))
            functionConstructor = constructor
                { ifConArgBinders = [Just firstName, Just scalarName, Nothing]
                , ifConArgTys = [(many, toIfaceType functionTy), (many, bool), (many, functionPayload)]
                , ifConStricts = replicate 3 IfNoBang
                , ifConSrcStricts = replicate 3 (IfSrcBang NoSrcUnpack NoSrcStrict)
                }
            functionDeclaration = changePackage (const functionConstructor) declaration
        forM_ (zip [0 :: Int ..] [NormalCompression, MaximumCompression]) $ \(iteration, compression) -> do
            first <- restorePackage env =<< roundtrip compression functionDeclaration
            second <- restorePackage env =<< roundtrip compression functionDeclaration
            forcePackage first
            forcePackage second
            let con = tyConSingleDataCon first
                otherCon = tyConSingleDataCon second
                base = 9520 + iteration * 30
                actualF = local base "actual_function" functionTy
                actualN = local (base + 1) "actual_scalar" boolTy
                actualPayload = local (base + 2) "actual_function_payload" (indexed (TermAppTy actualF (TermVarTy actualN)))
                packageTy = mkTyConTy first
                scrutinee = local (base + 3) "actual_function_package" packageTy
                caseBinder = local (base + 4) "actual_function_case" packageTy
                (existentials, fields) = dataConRepInstPat (map unique [base + 10 .. base + 19]) ManyTy con []
                (_, otherFields) = dataConRepInstPat (map unique [base + 20 .. base + 29]) ManyTy con []
            case (dataConRepArgBinders con, dataConRepArgBinders otherCon) of
                ([Just f, Just n, Nothing], [Just g, Just m, Nothing]) -> do
                    check "function field imports allocate distinct actual fields" (f /= g && n /= m && f /= functionTemplate && n /= scalarTemplate)
                    check "function field imports preserve head and scalar policies" (isNativeIndexFunctionId f && isNativeIndexId n)
                    case dataConOrigArgTys con of
                        [Scaled _ fTy, Scaled _ nTy, Scaled _ payloadTy] -> do
                            checkType dflags "function field imported domain" functionTy fTy
                            checkType dflags "function field imported scalar domain" boolTy nTy
                            checkType dflags "function field payload uses both imported fields" (indexed (TermAppTy f (TermVarTy n))) payloadTy
                            case payloadTy of
                                FunTy _ _ _ (TyConApp _ [_, TermAppTy left (TermVarTy ln), TermAppTy right (TermVarTy rn)]) _ ->
                                    check "constructor call endpoints share actual field identities" (left == f && right == f && ln == n && rn == n)
                                _ -> fail "FAIL: constructor call payload lost repeated endpoints"
                        _ -> fail "FAIL: function-field constructor changed source arity"
                _ -> fail "FAIL: function-field constructor lost metadata"
            checkType dflags "function field imported worker alpha equivalence" (idType (dataConWorkId con)) (idType (dataConWorkId otherCon))
            checkType dflags "function field imported wrapper alpha equivalence" (dataConWrapperType con) (dataConWrapperType otherCon)
            check "function fields occupy exactly three runtime slots" (dataConRepArity con == 3 && countConRepArgs con == 3 && countFunRepArgs 3 (idType (dataConWorkId con)) == 3)
            check "function fields add no erased existentials" (null existentials)
            case (fields, otherFields) of
                ([f, n, payload], [g, m, otherPayload]) -> do
                    check "function pattern fields freshen per instantiation" (f /= g && n /= m && payload /= otherPayload)
                    checkType dflags "function pattern payload substitutes sequential actual fields" (indexed (TermAppTy f (TermVarTy n))) (idType payload)
                    checkType dflags "second function pattern payload has its own fields" (indexed (TermAppTy g (TermVarTy m))) (idType otherPayload)
                _ -> fail "FAIL: imported function package pattern changed runtime arity"
            lintAccepts dflags "function field imported construction" [actualF, actualN, actualPayload] $
                mkApps (Var (dataConWorkId con)) [Var actualF, Var actualN, Var actualPayload]
            lintAccepts dflags "function field imported elimination and repackaging" [scrutinee] $
                Case (Var scrutinee) caseBinder packageTy [Alt (DataAlt con) fields (mkApps (Var (dataConWorkId con)) (map Var fields))]
            vacuous <- restorePackage env =<< roundtrip compression (changePackage (\_ -> functionConstructor{ifConArgTys = take 2 (ifConArgTys functionConstructor) ++ [(many, bool)]}) declaration)
            forcePackage vacuous
            let vacuousCon = tyConSingleDataCon vacuous
            check "vacuous function codec preserves named field metadata" $ case dataConRepArgBinders vacuousCon of
                [Just f, Just n, Nothing] -> isNativeIndexFunctionId f && isNativeIndexId n
                _ -> False
            check "vacuous function codec worker uses ordinary arrows" $ case idType (dataConWorkId vacuousCon) of
                FunTy{} -> True
                _ -> False
        let badFunctionDomain = toIfaceType (mkFunctionType unmatchableDataConTy ManyTy boolTy intTy)
            innerLinearFunction = toIfaceType (mkFunctionType unmatchableDataConTy OneTy boolTy boolTy)
        forM_
            [ ("function field metadata cannot duplicate names", invalid, functionConstructor{ifConArgBinders = [Just firstName, Just firstName, Nothing]})
            , ("function field payload cannot precede its head", "Iface id out of scope", functionConstructor{ifConArgBinders = [Nothing, Just firstName, Just scalarName], ifConArgTys = [(many, functionPayload), (many, toIfaceType functionTy), (many, bool)]})
            , ("function field cannot reference itself in its domain", "Iface id out of scope", functionConstructor{ifConArgTys = (many, functionPayload) : drop 1 (ifConArgTys functionConstructor)})
            , ("function field rejects unsupported output domain", invalid, functionConstructor{ifConArgTys = (many, badFunctionDomain) : drop 1 (ifConArgTys functionConstructor)})
            , ("function field rejects linear inner arrow", invalid, functionConstructor{ifConArgTys = (many, innerLinearFunction) : drop 1 (ifConArgTys functionConstructor)})
            , ("function field rejects linear outer multiplicity", invalid, functionConstructor{ifConArgTys = (toIfaceType OneTy, toIfaceType functionTy) : drop 1 (ifConArgTys functionConstructor)})
            ] $ \(label, fragment, malformed) -> rejectsPackage label fragment (changePackage (const malformed) declaration)
        let outerHead = local 9590 "outer_function" functionTy
            outerScalar = local 9591 "outer_function_scalar" boolTy
            outerPayload = toIfaceType (indexed (TermAppTy outerHead (TermVarTy outerScalar)))
            siblingName = mkIfLclName (fsLit "sibling_function")
            sibling = functionConstructor
                { ifConName = mkExternalName (unique 9592) (nameModule (ifName declaration)) (mkDataOcc "MkFunctionSibling") noSrcSpan
                , ifConArgBinders = [Just siblingName, Just scalarName, Nothing]
                }
        rejectsPackageWithScope [outerHead, outerScalar] "function payload cannot inherit outer runtime fields" invalid $
            changePackage (const functionConstructor{ifConArgTys = take 2 (ifConArgTys functionConstructor) ++ [(many, outerPayload)]}) declaration
        rejectsPackage "function payload cannot inherit sibling head scope" "Iface id out of scope" $
            declaration{ifCons = IfDataTyCon False [functionConstructor, sibling]}

        forM_
            [ ("constructor interface rejects misaligned binder metadata", invalid, constructor{ifConArgBinders = [Just firstName]})
            , ("constructor interface rejects duplicate field names", invalid, constructor{ifConArgBinders = [Just firstName, Just firstName]})
            , ("constructor interface rejects a forward field reference", "Iface id out of scope", constructor{ifConArgBinders = [Nothing, Just firstName], ifConArgTys = [dependent, (many, bool)]})
            , ("constructor interface rejects a self field reference", "Iface id out of scope", constructor{ifConArgTys = [dependent, (many, bool)]})
            , ("constructor interface rejects a linear index field", invalid, constructor{ifConArgTys = (toIfaceType OneTy, bool) : drop 1 (ifConArgTys constructor)})
            , ("constructor interface rejects an unsupported index domain", invalid, constructor{ifConArgTys = (many, toIfaceType intTy) : drop 1 (ifConArgTys constructor)})
            , ("constructor interface rejects an unlifted field", invalid, constructor{ifConArgTys = take 1 (ifConArgTys constructor) ++ [(many, toIfaceType intPrimTy)]})
            , ("constructor interface rejects missing user binders", invalid, constructor{ifConUnivTvs = [IfaceTvBndr typeBinder]})
            , ("constructor interface rejects inconsistent user kinds", invalid, constructor{ifConUnivTvs = [IfaceTvBndr typeBinder], ifConUserTvBinders = [mkIfaceForAllTvBndr Specified (typeName, bool)]})
            , ("constructor interface rejects implementation strictness", invalid, constructor{ifConStricts = [IfStrict, IfNoBang]})
            , ("constructor interface rejects source strictness", invalid, constructor{ifConSrcStricts = [IfSrcBang NoSrcUnpack SrcStrict, IfSrcBang NoSrcUnpack NoSrcStrict]})
            , ("constructor interface cannot discard payload scope", "Iface id out of scope", constructor{ifConArgBinders = []})
            ]
            $ \(label, fragment, malformed) -> rejectsPackage label fragment (changePackage (const malformed) declaration)
        rejectsPackage "constructor interface rejects a data-family parent" invalid $
            declaration{ifParent = IfDataInstance (ifName declaration) boolCon IA_Nil}
        rejectsPackage "constructor interface rejects an unlifted parent" invalid $
            declaration{ifResKind = toIfaceType (typeKind intPrimTy)}
        rejectsPackage "constructor interface rejects an inconsistent cached kind" invalid $
            declaration{ifKind = toIfaceType boolTy}
        let inherited = local 2850 "inherited_index" boolTy
            inheritedPayload = (many, toIfaceType (indexed (TermVarTy inherited)))
        rejectsPackageWithScope [inherited] "constructor fields cannot inherit an enclosing index" invalid $
            changePackage (\con -> con{ifConArgTys = take 1 (ifConArgTys con) ++ [inheritedPayload]}) declaration
        rejectsPackage "constructor fields cannot inherit sibling field scope" "Iface id out of scope" $
            declaration{ifCons = IfDataTyCon False [constructor, constructor{ifConName = mkExternalName (unique 2851) (nameModule (ifName declaration)) (mkDataOcc "MkIfaceSibling") noSrcSpan, ifConArgBinders = []}]}
        let parameterized = tyThingToIfaceDecl (ATyCon (retainedIfaceParameterizedPackage 2860))
        case parameterized of
            IfaceData{ifCons = IfDataTyCon _ [con]} -> case ifConArgBinders con of
                [Just leftName, Just rightName, Nothing] ->
                    check "tidying distinguishes same-spelling retained fields" (leftName /= rightName)
                _ -> fail "FAIL: parameterized interface lost retained fields"
            _ -> fail "FAIL: parameterized interface has the wrong shape"
        firstParameterized <- restorePackage env =<< roundtrip MaximumCompression parameterized
        secondParameterized <- restorePackage env =<< roundtrip MaximumCompression parameterized
        forcePackage firstParameterized
        forcePackage secondParameterized
        let firstParameterizedCon = tyConSingleDataCon firstParameterized
            secondParameterizedCon = tyConSingleDataCon secondParameterized
        checkType
            dflags
            "parameterized constructor imports are alpha equivalent"
            (dataConWrapperType firstParameterizedCon)
            (dataConWrapperType secondParameterizedCon)
        case (dataConRepArgBinders firstParameterizedCon, dataConRepArgBinders secondParameterizedCon) of
            ([Just left, Just right, Nothing], [Just otherLeft, Just otherRight, Nothing]) -> do
                check "parameterized field imports are distinct and fresh" $
                    left /= right && otherLeft /= otherRight && left /= otherLeft && right /= otherRight
                case dataConOrigArgTys firstParameterizedCon of
                    [_, _, Scaled _ payloadTy] ->
                        check "parameterized payload retains both field identities" $
                            left `elemVarSet` tyCoVarsOfType payloadTy && right `elemVarSet` tyCoVarsOfType payloadTy
                    _ -> fail "FAIL: parameterized constructor has the wrong field count"
            _ -> fail "FAIL: parameterized import lost retained fields"
        let leftIndex = local 2880 "left_index" boolTy
            rightIndex = local 2881 "right_index" boolTy
            pairPayloadTy =
                mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [boolTy, TermVarTy leftIndex, TermVarTy leftIndex]) $
                    mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [boolTy, TermVarTy rightIndex, TermVarTy rightIndex]) intTy
            pairPayload = local 2882 "pair_payload" pairPayloadTy
        lintAccepts dflags "imported parameterized package construction" [leftIndex, rightIndex, pairPayload] $
            mkApps (Var (dataConWorkId firstParameterizedCon)) [Type intTy, Var leftIndex, Var rightIndex, Var pairPayload]
        let recursiveDeclaration base label =
                let modu = nameModule (ifName declaration)
                    renamed = declaration{ifName = mkExternalName (unique base) modu (mkTcOcc label) noSrcSpan}
                 in changePackage (\con -> con{ifConName = mkExternalName (unique (base + 1)) modu (mkDataOcc ("Mk" ++ label)) noSrcSpan}) renamed
            withDomain domain = changePackage $ \con ->
                con
                    { ifConArgTys = [(many, IfaceTyConApp (IfaceTyCon (ifName domain) (mkIfaceTyConInfo NotPromoted IfaceNormalTyCon)) IA_Nil), (many, bool)]
                    }
            -- | Require structural admission through a source retained field.
            -- An ordinary opaque Pi can name these same nonstructural types.
            structuralUse declarations = case declarations of
                domain : _ -> withDomain domain (recursiveDeclaration 110400 "IfaceStructuralUse") : declarations
                [] -> error "FAIL: structural interface control has no domain"
            selfDeclaration = withDomain selfDeclaration (recursiveDeclaration 2900 "IfaceSelfDomain")
            leftDeclaration = withDomain rightDeclaration (recursiveDeclaration 2910 "IfaceLeftDomain")
            rightDeclaration = withDomain leftDeclaration (recursiveDeclaration 2920 "IfaceRightDomain")
        rejectsImportedKnots "self-recursive imported retained domain" ["Invalid retained constructor fields in interface:"] [selfDeclaration]
        rejectsImportedKnots "mutually recursive imported retained domains" ["Invalid retained constructor fields in interface:"] [leftDeclaration, rightDeclaration]
        -- Check the domain graph only after all imported parent knots exist.
        let withFunctionDomain domain inputSide = changePackage $ \con ->
                let reference = IfaceTyConApp (IfaceTyCon (ifName domain) (mkIfaceTyConInfo NotPromoted IfaceNormalTyCon)) IA_Nil
                    arrow = case toIfaceType (mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy) of
                        IfaceFunTy flag ma mult _ _ ->
                            if inputSide then IfaceFunTy flag ma mult reference bool else IfaceFunTy flag ma mult bool reference
                        _ -> error "FAIL: ordinary interface function changed shape"
                 in con{ifConArgTys = [(many, arrow), (many, bool)]}
            selfInputFunction = withFunctionDomain selfInputFunction True (recursiveDeclaration 9800 "IfaceFunctionSelfInput")
            selfOutputFunction = withFunctionDomain selfOutputFunction False (recursiveDeclaration 9810 "IfaceFunctionSelfOutput")
            leftInputFunction = withFunctionDomain rightInputFunction True (recursiveDeclaration 9820 "IfaceFunctionLeftInput")
            rightInputFunction = withFunctionDomain leftInputFunction True (recursiveDeclaration 9830 "IfaceFunctionRightInput")
            leftOutputFunction = withFunctionDomain rightOutputFunction False (recursiveDeclaration 9840 "IfaceFunctionLeftOutput")
            rightOutputFunction = withFunctionDomain leftOutputFunction False (recursiveDeclaration 9850 "IfaceFunctionRightOutput")
        rejectsImportedKnots "function field rejects a self-referencing input domain" ["Invalid retained constructor fields in interface:"] [selfInputFunction]
        rejectsImportedKnots "function field rejects a self-referencing output domain" ["Invalid retained constructor fields in interface:"] [selfOutputFunction]
        rejectsImportedKnots "function field rejects mutually recursive input domains" ["Invalid retained constructor fields in interface:"] [leftInputFunction, rightInputFunction]
        rejectsImportedKnots "function field rejects mutually recursive output domains" ["Invalid retained constructor fields in interface:"] [leftOutputFunction, rightOutputFunction]
        let -- | Build an ordinary field without retained telescope metadata.
            ordinaryField field = changePackage $ \con ->
                con{ifConArgBinders = [], ifConArgTys = [(many, field)], ifConStricts = [IfNoBang], ifConSrcStricts = [IfSrcBang NoSrcUnpack NoSrcStrict]}
            -- | Refer to a nullary declaration by its actual Name.
            reference domain = IfaceTyConApp (IfaceTyCon (ifName domain) (mkIfaceTyConInfo NotPromoted IfaceNormalTyCon)) IA_Nil
            -- | Build a vacuous scalar binder for domain validation.
            scalarPi domain = IfaceDepFunTy DepFunVisible (many, firstName, reference domain) (toIfaceType intTy)
            -- | Place an input or output domain in a retained function field.
            genericFunctionDomain domain inputSide =
                let arrow = case toIfaceType (mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy) of
                        IfaceFunTy flag ma mult _ _ ->
                            if inputSide then IfaceFunTy flag ma mult (reference domain) bool else IfaceFunTy flag ma mult bool (reference domain)
                        _ -> error "FAIL: ordinary interface function changed shape"
                 in ordinaryField (IfaceDepFunTy DepFunVisible (many, firstName, arrow) (toIfaceType intTy))
            -- | Put the scalar binder in an ordinary constructor field.
            genericScalarDomain domain = ordinaryField (scalarPi domain)
            genericSelfInput = genericFunctionDomain genericSelfInput True (recursiveDeclaration 9900 "IfaceGenericSelfInput")
            genericSelfOutput = genericFunctionDomain genericSelfOutput False (recursiveDeclaration 9910 "IfaceGenericSelfOutput")
            genericLeft = genericFunctionDomain genericRight True (recursiveDeclaration 9920 "IfaceGenericLeft")
            genericRight = genericFunctionDomain genericLeft True (recursiveDeclaration 9930 "IfaceGenericRight")
            scalarSelf = genericScalarDomain scalarSelf (recursiveDeclaration 9940 "IfaceGenericScalarSelf")
            scalarLeft = genericScalarDomain scalarRight (recursiveDeclaration 9950 "IfaceGenericScalarLeft")
            scalarRight = genericScalarDomain scalarLeft (recursiveDeclaration 9960 "IfaceGenericScalarRight")
            -- | Build a nullary synonym with the supplied interface RHS.
            synonym base label rhs =
                IfaceSynonym
                    (mkExternalName (unique base) (nameModule (ifName declaration)) (mkTcOcc label) noSrcSpan)
                    (toIfaceType liftedTypeKind)
                    []
                    []
                    (toIfaceType liftedTypeKind)
                    rhs
            selfAlias = synonym 9970 "IfaceGenericSelfAlias" (reference aliasSelf)
            aliasSelf = genericScalarDomain selfAlias (recursiveDeclaration 9980 "IfaceGenericAliasSelf")
            fieldAlias = synonym 10100 "IfaceGenericFieldAlias" (scalarPi hiddenSelf)
            hiddenSelf = ordinaryField (reference fieldAlias) (recursiveDeclaration 10110 "IfaceGenericHiddenSelf")
            -- | Keep the retained scalar in both equality operands.
            nonvacuousBody domain = case snd dependent of
                IfaceFunTy flag ma mult (IfaceTyConApp eqCon (IA_Arg _ visibility rest)) result ->
                    IfaceFunTy flag ma mult (IfaceTyConApp eqCon (IA_Arg (reference domain) visibility rest)) result
                _ -> error "FAIL: interface equality payload changed shape"
            nonvacuousSelf =
                ordinaryField
                    (IfaceDepFunTy DepFunVisible (many, firstName, reference nonvacuousSelf) (nonvacuousBody nonvacuousSelf))
                    (recursiveDeclaration 9990 "IfaceGenericNonvacuousSelf")
            shapeDiagnostic = ["Unsupported retained domain shape."]
        forM_
            [ ("generic Pi ordinary self input", [genericSelfInput])
            , ("generic Pi ordinary self output", [genericSelfOutput])
            , ("generic Pi ordinary mutual input", [genericLeft, genericRight])
            , ("generic Pi ordinary scalar self", [scalarSelf])
            , ("generic Pi ordinary scalar mutual left first", [scalarLeft, scalarRight])
            , ("generic Pi ordinary scalar mutual right first", [scalarRight, scalarLeft])
            , ("generic Pi nullary alias self cycle", [selfAlias, aliasSelf])
            , ("generic Pi hidden in a nullary field synonym", [fieldAlias, hiddenSelf])
            , ("generic Pi nonvacuous self cycle", [nonvacuousSelf])
            ]
            $ \(label, declarations) -> rejectsImportedKnots ("structural use of " ++ label) shapeDiagnostic (structuralUse declarations)
        let -- | Build a base constructor and a constructor with lazy fields.
            ordinaryRecursive base label fieldTypes =
                let owner = ordinaryField bool (recursiveDeclaration base label)
                 in case owner of
                        IfaceData{ifCons = IfDataTyCon typeData [con]} ->
                            owner
                                { ifCons =
                                    IfDataTyCon
                                        typeData
                                        [ con
                                            { ifConName = mkExternalName (unique (base + 2)) (nameModule (ifName owner)) (mkDataOcc ("Zero" ++ label)) noSrcSpan
                                            , ifConArgTys = []
                                            , ifConStricts = []
                                            , ifConSrcStricts = []
                                            }
                                        , con
                                            { ifConArgTys = map (\ty -> (many, ty)) fieldTypes
                                            , ifConStricts = replicate (length fieldTypes) IfNoBang
                                            , ifConSrcStricts = replicate (length fieldTypes) (IfSrcBang NoSrcUnpack NoSrcStrict)
                                            }
                                        ]
                                }
                        _ -> error "FAIL: recursive interface fixture changed shape"
            validNat = ordinaryRecursive 10000 "IfaceValidNat" [reference validNat]
            validList = ordinaryRecursive 10010 "IfaceValidBoolList" [bool, reference validList]
            validLeft = ordinaryRecursive 10020 "IfaceValidMutualNat" [reference validRight]
            validRight = ordinaryRecursive 10030 "IfaceValidMutualList" [bool, reference validLeft]
            natHolder = genericScalarDomain validNat (recursiveDeclaration 10040 "IfaceValidNatHolder")
            listHolder = genericScalarDomain validList (recursiveDeclaration 10050 "IfaceValidListHolder")
            mutualHolder = genericScalarDomain validLeft (recursiveDeclaration 10060 "IfaceValidMutualHolder")
            boolAlias = synonym 10070 "IfaceValidBoolAlias" bool
            aliasHolder = genericScalarDomain boolAlias (recursiveDeclaration 10080 "IfaceValidAliasHolder")
            boolHolder = ordinaryField (IfaceDepFunTy DepFunVisible (many, firstName, bool) (snd dependent)) (recursiveDeclaration 10090 "IfaceValidBoolPiHolder")
        forM_
            [ ("valid self Nat first", [(validNat, True), (natHolder, False)])
            , ("valid self Nat holder first", [(natHolder, False), (validNat, True)])
            , ("valid self list first", [(validList, True), (listHolder, False)])
            , ("valid self list holder first", [(listHolder, False), (validList, True)])
            , ("valid mutual Nat first", [(validLeft, True), (validRight, True), (mutualHolder, False)])
            , ("valid mutual holder first", [(mutualHolder, False), (validRight, True), (validLeft, True)])
            , ("valid nullary synonym first", [(boolAlias, True), (aliasHolder, False)])
            , ("valid nullary synonym holder first", [(aliasHolder, False), (boolAlias, True)])
            , ("ordinary field holds a nonvacuous Bool Pi", [(boolHolder, False)])
            ]
            $ \(label, declarations) -> acceptsImportedKnots label declarations
        let dataAlias = synonym 10200 "IfaceDataBackEdgeAlias" (reference aliasData)
            aliasData = ordinaryField (reference dataAlias) (recursiveDeclaration 10210 "IfaceDataBackEdge")
            aliasDataHolder = genericScalarDomain dataAlias (recursiveDeclaration 10220 "IfaceDataBackEdgeHolder")
            oneAlias = (synonym 10230 "IfaceOneAlias" (toIfaceType OneTy)){ifKind = toIfaceType multiplicityTy, ifResKind = toIfaceType multiplicityTy}
            manyAlias = (synonym 10240 "IfaceManyAlias" many){ifKind = toIfaceType multiplicityTy, ifResKind = toIfaceType multiplicityTy}
            mixedFields = changePackage
                (\con -> con{ifConArgTys = [(reference oneAlias, reference boolAlias), (reference manyAlias, reference boolAlias)], ifConStricts = [IfNoBang, IfNoBang], ifConSrcStricts = replicate 2 (IfSrcBang NoSrcUnpack NoSrcStrict)})
                (ordinaryField bool (recursiveDeclaration 10250 "IfaceMixedMultiplicityFields"))
            mixedHolder = genericScalarDomain mixedFields (recursiveDeclaration 10260 "IfaceMixedMultiplicityHolder")
            functionAlias = synonym 10270 "IfaceVacuousFunctionAlias" (IfaceDepFunTy DepFunVisible (many, firstName, bool) bool)
            functionAliasHolder = genericScalarDomain functionAlias (recursiveDeclaration 10280 "IfaceVacuousFunctionHolder")
            storedFunction = ordinaryRecursive 10430 "IfaceStoredFunction" [reference storedFunction, reference functionAlias]
            storedFunctionHolder = genericScalarDomain storedFunction (recursiveDeclaration 10440 "IfaceStoredFunctionHolder")
            functionLoop = ordinaryRecursive 10450 "IfaceFunctionLoop"
                [case toIfaceType (mkVisFunTyManyU boolTy boolTy) of
                    IfaceFunTy flag ma mult _ result -> IfaceFunTy flag ma mult (reference functionLoop) result
                    _ -> error "FAIL: ordinary function wire changed shape"]
            functionLoopHolder = genericScalarDomain functionLoop (recursiveDeclaration 10460 "IfaceFunctionLoopHolder")
            ignoredParameter = mkTyVar (mkSystemName (unique 10290) (mkTyVarOcc "ignored")) liftedTypeKind
            ignoredBinders = [mkAnonTyConBinder ignoredParameter]
            forgetful = mkSynonymTyCon
                (mkExternalName (unique 10300) (nameModule (ifName declaration)) (mkTcOcc "IfaceForgetfulDomain") noSrcSpan)
                (mkTyConKindWithMatchability unmatchableDataConTy ignoredBinders liftedTypeKind) ignoredBinders liftedTypeKind [Phantom] boolTy True True True True
            forgetfulDeclaration = tyThingToIfaceDecl (ATyCon forgetful)
            appliedForgetful = toIfaceType (mkTyConApp forgetful [intTy])
            forgetfulHolder = ordinaryField (IfaceDepFunTy DepFunVisible (many, firstName, appliedForgetful) (toIfaceType intTy)) (recursiveDeclaration 10310 "IfaceForgetfulHolder")
            aliasCycle = synonym 10320 "IfaceAliasOnlyCycle" (reference aliasCycle)
            aliasCycleHolder = genericScalarDomain aliasCycle (recursiveDeclaration 10330 "IfaceAliasOnlyHolder")
            badMultDomain = changePackage
                (\con -> con{ifConArgTys = [(scalarPi badMultDomain, bool)]})
                (ordinaryField bool (recursiveDeclaration 10340 "IfacePiMultiplicityDomain"))
            badMultHolder = genericScalarDomain badMultDomain (recursiveDeclaration 10350 "IfacePiMultiplicityHolder")
            contextDomain = changePackage
                (\con -> con{ifConCtxt = [scalarPi contextDomain]})
                (ordinaryField bool (recursiveDeclaration 10360 "IfaceContextDomain"))
            contextHolder = genericScalarDomain contextDomain (recursiveDeclaration 10370 "IfaceContextHolder")
            sessionDomain = ordinaryField bool (recursiveDeclaration 10380 "IfaceRepeatedSessionDomain")
            sessionHolder = genericScalarDomain sessionDomain (recursiveDeclaration 10390 "IfaceRepeatedSessionHolder")
            malformedSession = ordinaryField (scalarPi sessionDomain) sessionDomain
            invalidSibling = ordinaryRecursive 10410 "IfaceInvalidSibling" [reference invalidSibling, scalarPi invalidSibling]
            invalidSiblingHolder = genericScalarDomain invalidSibling (recursiveDeclaration 10420 "IfaceInvalidSiblingHolder")
        check "forgetful domain retains an applied raw synonym" $ case appliedForgetful of
            IfaceTyConApp _ IA_Arg{} -> True
            _ -> False
        forM_
            [ ("alias data back edge alias first", [(dataAlias, True), (aliasData, True), (aliasDataHolder, False)])
            , ("alias data back edge holder first", [(aliasDataHolder, False), (aliasData, True), (dataAlias, True)])
            , ("One and Many nullary aliases", [(mixedHolder, False), (mixedFields, True), (boolAlias, True), (oneAlias, False), (manyAlias, False)])
            , ("vacuous Pi alias admits function mode", [(functionAliasHolder, False), (functionAlias, False)])
            , ("stored vacuous function alias domain first", [(storedFunction, True), (functionAlias, False), (storedFunctionHolder, False)])
            , ("stored vacuous function alias holder first", [(storedFunctionHolder, False), (functionAlias, False), (storedFunction, True)])
            , ("applied forgetful alias uses typed fallback", [(forgetfulHolder, False), (forgetfulDeclaration, False)])
            , ("same Names valid session before", [(sessionHolder, False), (sessionDomain, True)])
            ]
            $ \(label, declarations) -> acceptsImportedKnots label declarations
        -- Demand holders first. A direct typed query on the alias-only cycle
        -- would bypass the raw precheck and exercise an unrelated loop.
        forM_
            [ ("alias-only cycle rejects before expansion", [aliasCycleHolder, aliasCycle])
            , ("raw Pi in field multiplicity rejects early", [badMultHolder, badMultDomain])
            , ("deferred constructor context rejects early", [contextHolder, contextDomain])
            , ("visited data edge does not hide invalid sibling", [invalidSiblingHolder, invalidSibling])
            , ("function endpoint cannot reuse an enclosing data back edge", [functionLoopHolder, functionLoop])
            , ("same Names malformed session", [sessionHolder, malformedSession])
            ]
            $ \(label, declarations) -> rejectsImportedKnots ("structural use of " ++ label) shapeDiagnostic (structuralUse declarations)
        acceptsImportedKnots "same Names valid session after" [(sessionHolder, False), (sessionDomain, True)]
        let cachedDomain = setTyConNativeDomainShape boolTyCon NativeInvalidDomain
            cachedIndex = local 10400 "cached_domain_index" (mkTyConTy cachedDomain)
            uncachedIndex = local 10401 "uncached_domain_index" boolTy
        check "nullary cache retains its attached raw view" $ case mkTyConTy cachedDomain of
            TyConApp actual [] -> case tyConNativeDomainShape actual of
                Just NativeInvalidDomain -> True
                _ -> False
            _ -> False
        cachedOpaque <- restore [cachedIndex] (IfaceTermVar (mkIfLclName (fsLit "cached_domain_index")))
        checkType dflags "opaque identity does not inspect a structural raw view" (TermVarTy cachedIndex) cachedOpaque
        let cachedHead = local 110420 "cached_domain_head" (mkVisFunTyManyU (mkTyConTy cachedDomain) boolTy)
        rejectsIface "cached raw view rejects a fixed native endpoint" "Unsupported retained domain shape." [cachedHead, cachedIndex] $
            IfaceTermAppTy (mkIfLclName (fsLit "cached_domain_head")) (IfaceTermVar (mkIfLclName (fsLit "cached_domain_index")))
        check "wired Bool has no imported raw view" $ case tyConNativeDomainShape boolTyCon of
            Nothing -> True
            _ -> False
        uncached <- restore [uncachedIndex] (IfaceTermVar (mkIfLclName (fsLit "uncached_domain_index")))
        checkType dflags "absent raw view retains typed Bool eligibility" (TermVarTy uncachedIndex) uncached
        promotionMessages <- newIORef []
        temporaryTypes <- newIORef (mkTypeEnv [AConLike (RealDataCon firstParameterizedCon)])
        let promotionModule = nameModule (dataConName firstParameterizedCon)
            promotionEnv =
                env
                    { hsc_logger = pushLogHook (\_ _ _ _ message -> modifyIORef' promotionMessages (showSDoc dflags message :)) (hsc_logger env)
                    , hsc_type_env_vars = KnotVars [promotionModule] (\modu -> if modu == promotionModule then Just temporaryTypes else Nothing)
                    }
            promotedType = IfaceTyConApp (IfaceTyCon (dataConName firstParameterizedCon) (mkIfaceTyConInfo IsPromoted IfaceNormalTyCon)) IA_Nil
        promotion <-
            try @IOEnvFailure
                $ initIfaceCheck (text "retained promotion test") promotionEnv
                $ initIfaceLcl promotionModule (text "retained promotion test") NotBoot
                $ tcIfaceExpr (IfaceType promotedType)
        case promotion of
            Left _ -> do
                diagnostics <- unlines <$> readIORef promotionMessages
                check ("imported retained constructor rejects promotion; diagnostic: " ++ diagnostics) $
                    "Retained constructors cannot be promoted." `isInfixOf` diagnostics
            Right _ -> fail "FAIL: imported retained constructor was promoted"
        ordinary <- restorePackage env =<< roundtrip NormalCompression (tyThingToIfaceDecl (ATyCon boolTyCon))
        forcePackage ordinary
        check "ordinary constructor interface has no retained metadata" (all (null . dataConRepArgBinders) (tyConDataCons ordinary))
        -- Check forall metadata through conversion, binary tables and import.
        let wireMa = mkTyVar (mkSystemName (unique 9200) (mkTyVarOcc "wire_ma")) matchabilityTy
            wireA = mkTyVar (mkSystemName (unique 9201) (mkTyVarOcc "wire_a")) liftedTypeKind
            closedForall ma = mkForAllTy matchableDataConTy (Bndr wireMa Specified) $
                mkForAllTy ma (Bndr wireA Specified) intTy
            metadataCases = [matchableDataConTy, unmatchableDataConTy, mkTyVarTy wireMa]
            shadowName = mkIfLclName (fsLit "wire_shadow")
            shadowType = IfaceForAllTy (toIfaceType matchableDataConTy)
                (mkIfaceForAllTvBndr Specified (shadowName, toIfaceType matchabilityTy)) $
                IfaceForAllTy (IfaceTyVar shadowName)
                    (mkIfaceForAllTvBndr Specified (shadowName, toIfaceType liftedTypeKind))
                    (toIfaceType intTy)
            ownType = IfaceForAllTy (IfaceTyVar shadowName)
                (mkIfaceForAllTvBndr Specified (shadowName, toIfaceType matchabilityTy))
                (toIfaceType intTy)
            maProof = proof 9202 matchableDataConTy unmatchableDataConTy
            structural role = mkForAllCo wireA Specified Specified
                (mkCoVarCo maProof) MRefl (mkReflCo role intTy)
            leftForall = mkForAllTy matchableDataConTy (Bndr wireA Specified) intTy
            rightForall = mkForAllTy unmatchableDataConTy (Bndr wireA Specified) intTy
            opaqueParent = mkCoVar (mkSystemName (unique 9203) (mkVarOcc "wire_forall_parent")) $
                mkReprEqPred leftForall rightForall
        forM_ [NormalCompression, SafeExtraCompression, MaximumCompression] $ \compression -> do
            restoredForalls <- forM metadataCases $ \ma -> do
                let originalForall = closedForall ma
                wireForall <- roundtrip compression (toIfaceType (tidyTopType originalForall))
                first <- restore [] wireForall
                second <- restore [] wireForall
                checkType dflags "forall codec preserves metadata and alpha equality" originalForall first
                checkType dflags "separate forall imports remain alpha equal" first second
                check "forall codec preserves closed scope" (isEmptyVarSet (tyCoVarsOfType first))
                lintAccepts dflags "imported forall metadata has its declared kind and scope" [] (Coercion (mkNomReflCo first))
                case (first, second) of
                    (ForAllTy _ (Bndr outer _) (ForAllTy innerMa (Bndr inner _) _),
                     ForAllTy _ (Bndr nextOuter _) (ForAllTy _ (Bndr nextInner _) _)) -> do
                        check "separate forall imports freshen both binders" $
                            outer /= wireMa && inner /= wireA && outer /= nextOuter && inner /= nextInner
                        case ma of
                            TyVarTy{} -> checkType dflags "variable metadata refers to the imported outer binder" (mkTyVarTy outer) innerMa
                            _ -> pure ()
                    _ -> fail "FAIL: forall codec lost its telescope"
                pure first
            forM_ [(left, right) | (i, left) <- zip [0 :: Int ..] restoredForalls,
                                  (j, right) <- zip [0 :: Int ..] restoredForalls, i < j] $ \(left, right) ->
                check "forall codec keeps M, U and variable metadata distinct" (not (eqType left right))
            shadow <- restore [] =<< roundtrip compression shadowType
            case shadow of
                ForAllTy _ (Bndr outer _) (ForAllTy ma (Bndr inner _) _) -> do
                    check "shadowed forall names retain distinct identities" (outer /= inner)
                    checkType dflags "forall metadata is outside its own binder scope" (mkTyVarTy outer) ma
                    lintAccepts dflags "shadowed metadata scope remains well kinded" [] (Coercion (mkNomReflCo shadow))
                _ -> fail "FAIL: shadowed forall codec lost its telescope"
            own <- roundtrip compression ownType
            rejectsIface "forall metadata cannot capture its own binder" "Iface type variable out of scope" [] own
            forM_ [Nominal, Representational, Phantom] $ \role -> do
                decoded <- roundtrip compression (IfaceCo (toIfaceCoercion (structural role)))
                restored <- restoreExpr [maProof] decoded
                case restored of
                    Coercion co -> do
                        check "forall codec retains the parent coercion role" (coercionRole co == role)
                        checkType dflags "forall coercion codec retains M on the left" leftForall (coercionLKind co)
                        checkType dflags "forall coercion codec retains U on the right" rightForall (coercionRKind co)
                        lintAccepts dflags "forall coercion codec retains nominal metadata evidence" [maProof] restored
                        check "metadata-only evidence remains live after import" (maProof `elemVarSet` tyCoVarsOfCo co)
                        let selected = mkSelCo (SelForAll SelFAMatch) co
                        check "structural metadata selection retains the actual nominal proof" $
                            coercionRole selected == Nominal && eqCoercion selected (mkCoVarCo maProof)
                        lintAccepts dflags "imported structural metadata selection" [maProof] (Coercion selected)
                    _ -> fail "FAIL: forall coercion codec returned a term"
            opaqueWire <- roundtrip compression $ IfaceCo $
                toIfaceCoercion (SelCo (SelForAll SelFAMatch) (mkCoVarCo opaqueParent))
            opaque <- restoreExpr [opaqueParent] opaqueWire
            case opaque of
                Coercion co -> do
                    check "opaque R forall metadata selection is nominal" (coercionRole co == Nominal)
                    checkType dflags "opaque selector codec retains its M endpoint" matchableDataConTy (coercionLKind co)
                    checkType dflags "opaque selector codec retains its U endpoint" unmatchableDataConTy (coercionRKind co)
                    lintAccepts dflags "opaque forall metadata selector codec" [opaqueParent] opaque
                _ -> fail "FAIL: forall selector codec returned a term"
            case toIfaceCoercion (structural Nominal) of
                IfaceForAllCo bndr visL visR _ kindCo bodyCo -> do
                    let withMetadata ma = IfaceCo (IfaceForAllCo bndr visL visR ma kindCo bodyCo)
                        ownName = mkIfLclName (fsLit "wire_own_proof")
                        ownProof = IfaceCo $ IfaceForAllCo
                            (IfaceIdBndr (toIfaceType ManyTy, ownName, toIfaceType (varType maProof)))
                            visL visR (IfaceCoVarCo ownName) kindCo bodyCo
                    forM_ [ ("non-nominal forall metadata", "Expected a nominal coercion.",
                             withMetadata (toIfaceCoercion (mkReflCo Representational unmatchableDataConTy)))
                          , ("wrong-kind forall metadata", "Invalid forall matchability in interface:",
                             withMetadata (toIfaceCoercion (mkNomReflCo intTy)))
                          , ("forall metadata proof cannot capture its own binder", "Iface id out of scope", ownProof)
                          ] $ \(label, diagnostic, input) -> do
                        decoded <- roundtrip compression input
                        rejectsIfaceExpr label diagnostic [] decoded
                _ -> fail "FAIL: forall coercion conversion lost its structural node"
        -- Check call types under the actual lexical Id environment.
        let callF = local 9100 "wire_call" (mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy)
            callN = local 9101 "wire_arg" boolTy
            callM = local 9102 "wire_arg_other" boolTy
            callCv = proof 9103 (TermVarTy callN) (TermVarTy callM)
            callAt v = TermAppTy callF (TermVarTy v)
            openCalls = toIfaceType (indexed (callAt callN))
            callName = mkIfLclName (fsLit "wire_call")
            closedCalls = mkDepFunTy callF (mkDepFunTy callN (indexed (callAt callN)))
            freeCall = toIfaceTypeX (unitVarSet callF) (callAt callN)
            nestedFreeCall = toIfaceTypeX (unitVarSet callF) (indexed (callAt callN))
            callCo = mkTermAppCo callF (mkCoVarCo callCv)
            callCoWire = toIfaceCoercion callCo
            reservedCo = IfaceTermAppCo callName (toIfaceCoercion (mkCoVarCo callCv))
        forM_ [NormalCompression, SafeExtraCompression, MaximumCompression] $ \compression -> do
            wire <- roundtrip compression openCalls
            restored <- restore [callF, callN] wire
            checkType dflags "open call codec preserves its type" (indexed (callAt callN)) restored
            case restored of
                FunTy _ _ _ (TyConApp _ [_, TermAppTy left (TermVarTy ln), TermAppTy right (TermVarTy rn)]) _ ->
                    check "repeated call nodes reuse actual scoped head and index" (left == callF && right == callF && ln == callN && rn == callN)
                _ -> fail "FAIL: open call codec lost repeated endpoints"
            other <- restore [callF, callM] =<< roundtrip compression (toIfaceType (indexed (callAt callM)))
            check "call codec does not identify distinct argument Ids" (not (eqType restored other))
            decoded <- roundtrip compression (IfaceTermAppTy callName (toIfaceType (TermVarTy callN)))
            rejectsIface "wire call cannot invent a missing head" "Iface id out of scope" [callN] decoded
            let linearHead = mkSysLocal (fsLit "wire_call") (unique 9100) OneTy (idType callF)
                scalarHead = local 9100 "wire_call" boolTy
            rejectsIface "wire call rejects a linear head" "Invalid retained call head" [linearHead, callN] decoded
            rejectsIface "wire call rejects stale scalar head metadata" "Invalid retained call head" [scalarHead, callN] decoded
            wrongDomain <- roundtrip compression (IfaceTermAppTy callName (toIfaceType intTy))
            rejectsIface "wire call rejects wrong argument domain" "Invalid retained call argument" [callF] wrongDomain
            let argument ty rest = IA_Arg ty (IfaceForAllTyFlag Required) rest
                rawCall ty = IfaceAppTy (IfaceTermVar callName) (argument ty IA_Nil)
                argumentWire = toIfaceType (TermVarTy callN)
            rawRestored <- restore [callF, callN] =<< roundtrip compression (rawCall argumentWire)
            checkType dflags "ordinary wire application has a canonical native endpoint" (callAt callN) rawRestored
            check "ordinary wire application uses the actual lexical head" $ case rawRestored of
                TermAppTy head (TermVarTy arg) -> head == callF && arg == callN
                _ -> False
            lintAccepts dflags "canonical ordinary wire application" [callF, callN] (Coercion (mkNomReflCo rawRestored))
            rejectsIface "ordinary wire application checks its argument domain" "Invalid retained function-value application" [callF] $
                rawCall (toIfaceType intTy)
            rejectsIface "ordinary wire application rejects an extra argument" "A retained call result cannot be applied" [callF, callN] $
                IfaceAppTy (IfaceTermVar callName) (argument argumentWire (argument argumentWire IA_Nil))
            rejectsIface "ordinary wire application cannot invent its head" "Iface id out of scope" [callN] (rawCall argumentWire)
            let changedDomain = proof 9112 (varType callF) (mkVisFunTyManyU boolTy (mkTyConApp maybeTyCon [boolTy]))
                functionCast co = IfaceCastTy (IfaceTermVar callName) (toIfaceCoercion co)
            castRestored <- restore [callF] =<< roundtrip compression (functionCast (mkNomReflCo (varType callF)))
            checkType dflags "reflexive function-value cast preserves identity" (TermVarTy callF) castRestored
            rejectsIface "function-value cast cannot change its fixed domain" "A retained function-value cast changes its fixed domain" [callF, changedDomain] $
                functionCast (mkCoVarCo changedDomain)
            rejectsIface "function-value cast requires nominal evidence" "A retained function-value cast changes its fixed domain" [callF] $
                functionCast (mkReflCo Representational (varType callF))
            functionPiWire <- roundtrip compression (toIfaceType (tidyTopType closedCalls))
            firstFunctionPi <- restore [] functionPiWire
            secondFunctionPi <- restore [] =<< roundtrip compression (toIfaceType (tidyTopType closedCalls))
            checkType dflags "function Pi codec preserves alpha equality" closedCalls firstFunctionPi
            checkType dflags "separate function Pi imports are alpha equivalent" firstFunctionPi secondFunctionPi
            check "function Pi codec preserves both runtime argument slots" (countFunRepArgs 2 firstFunctionPi == 2)
            case (firstFunctionPi, secondFunctionPi) of
                (DepFunTy DepFunVisible firstHead (DepFunTy DepFunVisible firstIndex firstBody), DepFunTy DepFunVisible secondHead (DepFunTy DepFunVisible secondIndex secondBody)) -> do
                    check "function Pi imports freshen both actual binders" (firstHead /= secondHead && firstIndex /= secondIndex)
                    forM_ [(firstHead, firstIndex, firstBody), (secondHead, secondIndex, secondBody)] $ \(headId, indexId, body) ->
                        case body of
                            FunTy _ _ _ (TyConApp _ [_, TermAppTy left (TermVarTy ln), TermAppTy right (TermVarTy rn)]) _ ->
                                check "function Pi endpoints share actual imported head and index" (left == headId && right == headId && ln == indexId && rn == indexId)
                            _ -> fail "FAIL: function Pi codec lost repeated call endpoints"
                _ -> fail "FAIL: function Pi codec lost its runtime telescope"
            let shadowHead = local 9110 "wire_call" (idType callF)
                shadowCalls = mkDepFunTy callF (mkDepFunTy shadowHead (mkDepFunTy callN $
                    mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [boolTy, TermAppTy callF (TermVarTy callN), TermAppTy shadowHead (TermVarTy callN)]) intTy))
            shadowRestored <- restore [] =<< roundtrip compression (toIfaceType (tidyTopType shadowCalls))
            checkType dflags "same-spelling function Pi binders avoid capture" shadowCalls shadowRestored
            case shadowRestored of
                DepFunTy DepFunVisible outer (DepFunTy DepFunVisible inner (DepFunTy DepFunVisible argument (FunTy _ _ _ (TyConApp _ [_, TermAppTy left (TermVarTy ln), TermAppTy right (TermVarTy rn)]) _))) ->
                    check "shadowed call heads retain distinct actual identities" (outer /= inner && left == outer && right == inner && ln == argument && rn == argument)
                _ -> fail "FAIL: shadowed function Pi codec changed shape"
            case functionPiWire of
                IfaceDepFunTy vis (mult, name, domain) body -> do
                    let badDomain replacement = IfaceDepFunTy vis (mult, name, replacement) body
                    rejectsIface "function Pi rejects outer multiplicity One" "Invalid retained binder" [] $
                        IfaceDepFunTy vis (toIfaceType OneTy, name, domain) body
                    rejectsIface "native call rejects an opaque inner linear arrow" "Invalid retained call head" [] $
                        badDomain (toIfaceType (mkFunctionType unmatchableDataConTy OneTy boolTy boolTy))
                    rejectsIface "native call rejects an opaque unsupported result domain" "Invalid retained call head" [] $
                        badDomain (toIfaceType (mkFunctionType unmatchableDataConTy ManyTy boolTy intTy))
                    let missingType = mkTyVar (mkSystemName (unique 9111) (mkTyVarOcc "missing_domain")) liftedTypeKind
                    rejectsIface "function Pi cannot import a free domain variable" "Iface type variable out of scope" [] $
                        badDomain (toIfaceType (mkFunctionType unmatchableDataConTy ManyTy (mkTyVarTy missingType) boolTy))
                    let boundDomainName = mkIfLclName (fsLit "bound_domain")
                        nonclosedDomain = case domain of
                            IfaceFunTy flag ma arrowMult _ result -> IfaceFunTy flag ma arrowMult (IfaceTyVar boundDomainName) result
                            _ -> error "FAIL: function Pi domain is not an ordinary arrow"
                        boundDomainPi = IfaceForAllTy (toIfaceType unmatchableDataConTy)
                            (mkIfaceForAllTvBndr Specified (boundDomainName, toIfaceType liftedTypeKind))
                            (badDomain nonclosedDomain)
                    rejectsIface "native call rejects an opaque in-scope nonclosed domain" "Invalid retained call head" [] boundDomainPi
                    rejectsIface "function-value index cannot be a retained function result" "Invalid retained function result" [] $
                        IfaceDepFunTy vis (mult, name, domain) (IfaceTermVar name)
                    rejectsIface "call argument cannot refer to a later scalar binder" "Iface id out of scope" [] $
                        IfaceDepFunTy vis (mult, name, domain) (IfaceTermAppTy name (toIfaceType (TermVarTy callN)))
                    rejectsIface "a sibling function binder is not in scope" "Iface id out of scope" [] $
                        IfaceDepFunTy vis (mult, name, domain) (IfaceTermAppTy (mkIfLclName (fsLit "sibling_head")) (toIfaceType boolTy))
                _ -> fail "FAIL: function Pi wire fixture changed shape"
            encodedCo <- roundtrip compression callCoWire
            restoredCoExpr <- restoreExpr [callF, callN, callM, callCv] (IfaceCo encodedCo)
            case restoredCoExpr of
                Coercion co -> do
                    check "call congruence roundtrip keeps nominal role and argument evidence" $
                        coercionRole co == Nominal && callCv `elemVarSet` tyCoVarsOfCo co
                    checkType dflags "call congruence roundtrip source" (callAt callN) (coercionLKind co)
                    checkType dflags "call congruence roundtrip target" (callAt callM) (coercionRKind co)
                    check "call congruence roundtrip has canonical endpoints" $ case (coercionLKind co, coercionRKind co) of
                        (TermAppTy left (TermVarTy ln), TermAppTy right (TermVarTy rn)) ->
                            left == callF && right == callF && ln == callN && rn == callM
                        _ -> False
                    lintAccepts dflags "call congruence roundtrip" [callF, callN, callM, callCv] (Coercion co)
                _ -> fail "FAIL: call congruence import did not return a coercion"
            rejectsWith "raw diagnostic call coercion serialization stays reserved" "Opaque runtime call congruence interfaces are not implemented" (roundtrip compression reservedCo)
            forM_ [("direct", freeCall), ("nested", nestedFreeCall)] $ \(label, diagnostic) ->
                rejectsWith (label ++ " diagnostic call head cannot enter binary") "Can't serialise IfaceFreeTermAppTy" (roundtrip compression diagnostic)
        rejectsIfaceExpr "raw call coercion import stays reserved" "Opaque runtime call congruence interfaces are not implemented" [callF, callN, callM, callCv] (IfaceCo reservedCo)
        rejectsIface "free diagnostic head cannot be imported" "diagnostic-only retained call head" [callF, callN] freeCall
        rejectsIface "nested free diagnostic head cannot be imported" "diagnostic-only retained call head" [callF, callN] nestedFreeCall
        rejectsWith "raw free-head writer bypass stays rejected" "Can't serialise IfaceFreeTermAppTy" $ do
            writer <- Binary.openBinMem 1024
            putIfaceType writer freeCall

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
            betaCase =
                close $ Case (App (identity 3107 boolTy) (Var b)) caseBinder (indexed index) (dictionaryAlts payload falseProof trueProof)
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
                , ("identity beta proof case", betaCase, 2)
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
                (DepFunTy DepFunVisible old _, DepFunTy DepFunVisible fresh body, DepFunTy DepFunVisible next _) -> do
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
                        $ DepFunTy DepFunVisible outer
                        $ DepFunTy DepFunVisible inner
                        $ mkFunctionType unmatchableDataConTy ManyTy (indexed (TermVarTy outer)) (indexed (TermVarTy inner))
            nestedIface <- roundtrip compression (toIfaceType nested)
            nestedResult <- restore [] nestedIface
            checkType dflags (label ++ " (nested alpha equality)") nested nestedResult
            case nestedResult of
                DepFunTy DepFunVisible outer' (DepFunTy DepFunVisible inner' body) -> do
                    check (label ++ " (nested distinct identities)") (outer' /= inner')
                    check (label ++ " (nested fresh identities)") (outer' /= outer && inner' /= inner)
                    checkType
                        dflags
                        (label ++ " (nested lexical references)")
                        (mkFunctionType unmatchableDataConTy ManyTy (indexed (TermVarTy outer')) (indexed (TermVarTy inner')))
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
                forM_ [NormalCompression, SafeExtraCompression, MaximumCompression] $ \compression -> do
                    decoded <- roundtrip compression (wrap (IfaceApp (IfaceLam retained scrut) scrut) [falseAlt, IfaceAlt trueCon [proofName] rhs])
                    restored <- restoreExpr [] decoded
                    lintAccepts dflags "interface proof accepts a checked identity beta scrutinee" [] restored
                    checkType dflags "interface identity beta proof exact type" (exprType variableCase) (exprType restored)
                let ifaceOpaqueProducer =
                        setIdUnfolding
                            (local 3108 "opaque_interface_producer" (mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy))
                            (unfolding (error "forced an interface producer unfolding"))
                opaqueProofCase <-
                    roundtrip NormalCompression $
                        wrap (IfaceApp (toIfaceExpr (Var ifaceOpaqueProducer)) scrut) [falseAlt, IfaceAlt trueCon [proofName] rhs]
                rejectsIfaceExpr
                    "unsupported opaque interface proof scrutinee"
                    "Unsupported scrutinee for native Boolean interface evidence"
                    [ifaceOpaqueProducer]
                    opaqueProofCase
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
        let hiddenModule = mkModule mainUnit (mkModuleName "HiddenRetainedInterface")
            hiddenTypes =
                map (tidyTopType . mkDepFunTyWithVis DepFunInvisible b) [intTy, keepResult (TermVarTy b)]
            hiddenDecl (u, ty) =
                (fingerprint0, IfaceId name (toIfaceType ty) IfVanillaId [])
              where
                name = mkExternalName (unique u) hiddenModule (mkVarOcc ("hidden" ++ show u)) noSrcSpan
            hiddenIface = set_mi_decls (map hiddenDecl (zip [17206, 17207] hiddenTypes)) (emptyFullModIface hiddenModule)
            newIfacePath = do
                (path, handle) <- openBinaryTempFile "." "NativeDependentCore.hidden.hi"
                hClose handle
                pure path
        bracket newIfacePath removeFile $ \path -> do
            let profile = targetProfile dflags
            writeBinIface profile QuietBinIFace NormalCompression path hiddenIface
            decoded <- readBinIface profile (hsc_NC env) CheckHiWay QuietBinIFace path
            check "versioned hidden retained interface declaration count" (length (mi_decls decoded) == length hiddenTypes)
            forM_ (zip hiddenTypes (mi_decls decoded)) $ \(hidden, (_, decl)) ->
                case decl of
                    IfaceId _ converted@(IfaceDepFunTy DepFunInvisible _ _) _ _ -> do
                        restored <- restore [] converted
                        checkType dflags "hidden retained interface round trip" hidden restored
                    _ -> fail "FAIL: the interface lost a hidden retained binder"
        case iface of
            IfaceDepFunTy DepFunVisible (multiplicity, name, domain) body -> do
                missing <- roundtrip NormalCompression (IfaceTermVar (mkIfLclName (fsLit "missing")))
                rejectsIface "missing interface index" "Iface id out of scope" [] missing
                let malformed label fragment binder = do
                        decoded <- roundtrip NormalCompression (IfaceDepFunTy DepFunVisible binder body)
                        rejectsIface label fragment [] decoded
                malformed "unlifted interface binder" "Invalid retained binder" (multiplicity, name, toIfaceType intPrimTy)
                malformed "linear interface binder" "Invalid retained binder" (toIfaceType OneTy, name, domain)
                malformed "interface binder outside its own domain scope" "Iface id out of scope" (multiplicity, name, IfaceTermVar name)
            _ -> fail "FAIL: interface conversion did not retain the Boolean binder"
        let constraintResult = DepFunTy DepFunVisible b (mkTyConApp eqTyCon [boolTy, TermVarTy b, TermVarTy b])
        constraintIface <- roundtrip NormalCompression (toIfaceType (tidyTopType constraintResult))
        rejectsIface "Constraint interface result" "Invalid retained function result" [] constraintIface
        let badType = local 305 "badType" intPrimTy
            badMultiplicity = mkSysLocal (fsLit "badMultiplicity") (unique 306) OneTy boolTy
            badJoin = asJoinId (local 110410 "badJoin" (mkVisFunTyManyU boolTy boolTy)) 1
            badCoVar = proof 110411 (constructorIndex trueDataCon) (constructorIndex trueDataCon)
        forM_ [badType, badMultiplicity, badJoin, badCoVar] $ \bad -> do
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

-- | Check opaque call indices, nominal forward congruence and actual head scope.
runOpaqueCallChecks :: DynFlags -> IO ()
runOpaqueCallChecks dflags = do
    let (nat, zero, successor) = peanoNat
        natTy = mkTyConTy nat
        zeroTy = mkTyConTy (promoteDataCon zero)
        succTy ty = mkTyConApp (promoteDataCon successor) [ty]
        n = local 6200 "call_n" natTy
        m = local 6201 "call_m" natTy
        f = local 6202 "call_head" (mkFunctionType unmatchableDataConTy ManyTy natTy boolTy)
        constraintHead = setIdType f (FunTy FTF_T_C unmatchableDataConTy ManyTy natTy boolTy)
        g = local 6203 "call_head" (idType f)
        h = local 6204 "call_head" (idType f)
        reverseHead = local 6205 "reverse_head" (mkFunctionType unmatchableDataConTy ManyTy boolTy natTy)
        b = local 6206 "call_bool" boolTy
        call = TermAppTy f (TermVarTy n)
        cv = proof 6207 (TermVarTy n) (TermVarTy m)
        co = mkTermAppCo f (mkCoVarCo cv)
        right = TermAppTy f (TermVarTy m)
        scope = [f, g, h, n, m, cv]
        apart lhs rhs = case tcUnifyTysFG alwaysBindFam alwaysBindTv [lhs] [rhs] of
            MaybeApart{} -> True
            _ -> False
    check "native function rejects constraint result classification" (not (isNativeIndexFunctionId constraintHead))
    check "mixed-domain native function is eligible" (isNativeIndexFunctionId f)
    check "native function is not a scalar index" (not (isNativeIndexId f))
    checkType dflags "Nat-to-Bool call has Bool kind" boolTy (typeKind call)
    checkType dflags "Bool-to-Nat call has Nat kind" natTy (typeKind (TermAppTy reverseHead (TermVarTy b)))
    lintAccepts dflags "mixed-domain call index" [f, n] (Coercion (mkNomReflCo call))
    lintAccepts dflags "reverse mixed-domain call index" [reverseHead, b] (Coercion (mkNomReflCo (TermAppTy reverseHead (TermVarTy b))))
    check "call congruence is nominal" (coercionRole co == Nominal)
    checkType dflags "call congruence left endpoint" call (coercionLKind co)
    checkType dflags "call congruence right endpoint" right (coercionRKind co)
    checkType dflags "call congruence kind proof has result domain" boolTy (coercionLKind (mkKindCo co))
    checkType dflags "call congruence kind proof is reflexive at result domain" boolTy (coercionRKind (mkKindCo co))
    check "call congruence tracks head and evidence" (all (`elemVarSet` tyCoVarsOfCo co) [f, cv])
    lintAccepts dflags "nominal forward call congruence" scope (Coercion co)
    lintAccepts dflags "call congruence can be weakened representationally" scope (Coercion (mkSubCo co))
    check "reflexive call congruence reduces to reflexivity" (isReflCo (mkTermAppCo f (mkNomReflCo (TermVarTy n))))
    let dependent = mkDepFunTy n (indexed call)
        renamed = mkDepFunTy m (indexed (TermAppTy f (TermVarTy m)))
    check "call indices respect retained alpha equality" (eqType dependent renamed)
    check "different same-spelled heads have distinct identity" (not (eqType call (TermAppTy g (TermVarTy n))))
    check "opaque calls do not imply argument injectivity" (apart call right)
    check "opaque calls with different heads are only maybe apart" (apart call (TermAppTy g (TermVarTy n)))
    check "opaque call versus constructor is only maybe apart" (apart call (constructorIndex trueDataCon))
    let constant = setIdUnfolding f (unfolding (Lam n (Var (dataConWorkId trueDataCon))))
    check "constant-head metadata does not decompose call arguments" (apart (TermAppTy constant (TermVarTy n)) (TermAppTy constant (TermVarTy m)))
    let template = mkTyVar (mkSystemName (unique 6208) (mkTyVarOcc "call_template")) natTy
    check "call argument is not injectively determined" (not (isInjectiveInType template (TermAppTy f (mkTyVarTy template))))
    check "type-only call keeps both actual runtime identifiers free" (all (`elemVarSet` exprFreeVars (Type (indexed call))) [f, n])
    case exprToNativeCaseIndex (App (Var f) (Var n)) of
        Just (TermAppTy actual (TermVarTy argument)) -> check "term and type use the actual head and argument" (actual == f && argument == n)
        _ -> fail "FAIL: strict-case converter lost the actual call identities"
    let replaceF = indexSubst [g, n] f (Var g)
        replaceG = indexSubst [h, n] g (Var h)
        composed = Subst.composeTCvSubst replaceG replaceF
        simultaneous = Subst.Subst (mkInScopeSet (mkVarSet [g, h, n])) (mkVarEnv [(f, Var g), (g, Var h)]) emptyVarEnv emptyVarEnv
    checkType dflags "head substitution changes the actual head" (TermAppTy g (TermVarTy n)) (Subst.substTy replaceF call)
    checkType dflags "head substitution composition changes its range" (TermAppTy h (TermVarTy n)) (Subst.substTy composed call)
    checkType dflags "stored head substitution range is used once" (TermAppTy g (TermVarTy n)) (Subst.substTy simultaneous call)
    check "head substitution range records its free function" (g `elemVarSet` Subst.getSubstRangeTyCoFVs replaceF)
    check "coercion substitution preserves the actual chosen head" (eqCoercion (mkTermAppCo g (mkCoVarCo cv)) (Subst.substCo (indexSubst [g, n, m, cv] f (Var g)) co))
    let body = mkFunctionType unmatchableDataConTy ManyTy (indexed (TermAppTy f (TermVarTy n))) (indexed (TermAppTy f (TermVarTy m)))
        captured = Subst.substTy (indexSubst [f, n] m (Var n)) (mkDepFunTy n body)
    case captured of
        DepFunTy DepFunVisible fresh result -> do
            check "call substitution freshens a reused runtime Unique" (fresh /= n)
            checkType dflags "call substitution separates bound and free arguments" (mkFunctionType unmatchableDataConTy ManyTy (indexed (TermAppTy f (TermVarTy fresh))) (indexed call)) result
            lintAccepts dflags "capture-avoiding call substitution" [f, n] (Coercion (mkNomReflCo captured))
        _ -> fail "FAIL: call substitution lost its retained binder"
    let headProof = proof 6211 call call
        headCapture = Lam g (Let (NonRec headProof (Coercion (mkNomReflCo call))) (mkIntExprInt (targetPlatform dflags) 42))
        headSubst = CoreSubst.extendIdSubst (CoreSubst.mkEmptySubst (mkInScopeSet (mkVarSet [g, n]))) f (Var g)
        headCaptured = CoreSubst.substExpr headSubst headCapture
    lintAccepts dflags "original function-head capture control" [f, n] headCapture
    case headCaptured of
        Lam fresh (Let (NonRec actualProof (Coercion actualCo)) _) -> do
            check "Core substitution freshens a capturing function binder" (fresh /= g)
            check "Core substitution preserves the free range head" (g `elemVarSet` exprFreeVars headCaptured)
            check "Core substitution excludes the fresh bound head from free variables" (not (fresh `elemVarSet` exprFreeVars headCaptured))
            checkType dflags "captured head proof uses the original actual free head" (TermAppTy g (TermVarTy n)) (coercionLKind actualCo)
            check "captured proof metadata preserves the original free head" (g `elemVarSet` tyCoVarsOfType (varType actualProof))
            lintAccepts dflags "Core substitution avoids function-head capture" [g, n] headCaptured
        _ -> fail "FAIL: Core substitution changed the head capture control shape"
    let unused = mkTyVar (mkSystemName (unique 6212) (mkTyVarOcc "unused_head_parameter")) liftedTypeKind
        unusedBinders = [mkAnonTyConBinder unused]
        erasedHead = mkSynonymTyCon (mkSystemName (unique 6213) (mkTcOcc "ErasedHeadParameter")) (mkTyConKindWithMatchability unmatchableDataConTy unusedBinders liftedTypeKind) unusedBinders liftedTypeKind [Phantom] (idType f) True True True False
        hiddenFree = mkTyConApp erasedHead [mkTyVarTy unused]
        hiddenHead = local 6214 "hidden_free_head" hiddenFree
        escapedPayload = local 6215 "head_dependent_payload" (indexed call)
    checkType dflags "unused synonym parameter hides the same closed arrow" (idType f) hiddenFree
    check "raw free head metadata is ineligible before synonym expansion" (not (isNativeIndexFunctionId hiddenHead))
    lintRejects dflags "raw free head metadata cannot enter a call" [hiddenHead, unused, n] (Coercion (mkNomReflCo (TermAppTy hiddenHead (TermVarTy n))))
    lintAccepts dflags "runtime function head is bound in a retained lambda result" [n] (Lam f (Lam escapedPayload (Var escapedPayload)))
    let symbolic = TermAppTy f (succTy zeroTy)
    lintAccepts dflags "type-node call argument need not be an atomic runtime value" [f] (Coercion (mkNomReflCo symbolic))
    let familyParameter = mkTyVar (mkSystemName (unique 6209) (mkTyVarOcc "call_family_arg")) boolTy
        familyBinders = [mkAnonTyConBinder familyParameter]
        stuckFamily = mkFamilyTyCon (mkSystemName (unique 6210) (mkTcOcc "CallArgument")) (mkTyConKindWithMatchability unmatchableDataConTy familyBinders natTy) familyBinders 0 natTy Nothing OpenTypeFamilyTyCon Nothing NotInjective
        familyArgument = mkTyConApp stuckFamily [TermVarTy b]
        nestedCall = TermAppTy f (TermAppTy reverseHead (TermVarTy b))
        poisoned = setIdUnfolding f (unfolding (error "opaque call inspected its head unfolding"))
    lintAccepts dflags "call type permits a well-kinded stuck family argument" [f, b] (Coercion (mkNomReflCo (TermAppTy f familyArgument)))
    lintAccepts dflags "call type permits nested mixed-domain calls" [f, reverseHead, b] (Coercion (mkNomReflCo nestedCall))
    let reducedFamily = mkFamilyTyCon (mkSystemName (unique 6216) (mkTcOcc "ReducedCallArgument")) (mkTyConKindWithMatchability unmatchableDataConTy familyBinders natTy) familyBinders 0 natTy Nothing (ClosedTypeFamilyTyCon (CTF (Just reducedAxiom))) Nothing NotInjective
        reducedAxiom = mkBranchedCoAxiom Nominal (mkSystemName (unique 6217) (mkTcOcc "ReducedCallArgumentAx")) reducedFamily [mkCoAxBranch [] [] [] [constructorIndex trueDataCon] zeroTy [] noSrcSpan]
        reducibleCall = TermAppTy f (mkTyConApp reducedFamily [constructorIndex trueDataCon])
        reducedCall = TermAppTy f zeroTy
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        let Reduction callCo normalized = normaliseType emptyFamInstEnvs role reducibleCall
        checkType dflags "call normalization reduces its symbolic argument" reducedCall normalized
        checkType dflags "call normalization proof source" reducibleCall (coercionLKind callCo)
        checkType dflags "call normalization proof target" reducedCall (coercionRKind callCo)
        check "call normalization keeps the requested role" (coercionRole callCo == role)
        check "call normalization retains the actual head dependency" (f `elemVarSet` tyCoVarsOfCo callCo)
        lintAccepts dflags "forward call normalization evidence" [f] (Coercion callCo)
    checkType dflags "head substitution preserves a symbolic family argument" (TermAppTy g familyArgument) (Subst.substTy (indexSubst [g, b] f (Var g)) (TermAppTy f familyArgument))
    check "head eligibility does not inspect unfoldings" (isNativeIndexFunctionId poisoned)
    checkType dflags "call kind does not inspect head unfoldings" boolTy (typeKind (TermAppTy poisoned (TermVarTy n)))
    forM_
        [ ("linear head", setIdMult f OneTy, [setIdMult f OneTy, n])
        , ("join head", asJoinId f 1, [asJoinId f 1, n])
        , ("global head", globaliseId f, [n])
        , ("missing head", f, [n])
        , ("stale head metadata", f, [setIdType f (idType reverseHead), n])
        , ("linear function arrow", setIdType f (mkFunctionType unmatchableDataConTy OneTy natTy boolTy), [setIdType f (mkFunctionType unmatchableDataConTy OneTy natTy boolTy), n])
        , ("constraint result classification", constraintHead, [constraintHead, n])
        ]
        $ \(label, badHead, badScope) -> lintRejects dflags ("call rejects " ++ label) badScope (Coercion (mkNomReflCo (TermAppTy badHead (TermVarTy n))))
    lintRejects dflags "call argument must have its head's domain" [f, b] (Coercion (mkNomReflCo (TermAppTy f (TermVarTy b))))
    lintRejects dflags "call congruence child must be nominal" scope (Coercion (TermAppCo f (mkSubCo (mkCoVarCo cv))))
    lintRejects dflags "call congruence child must have the input domain" [f, b] (Coercion (TermAppCo f (mkNomReflCo (TermVarTy b))))
    rejectsWith "head substitution cannot silently keep a lambda range" "unsupported native function range" $
        evaluate (Subst.substTy (indexSubst [n] f (Lam n (Var (dataConWorkId trueDataCon)))) call)
    rejectsWith "head substitution cannot change closed domains" "invalid native function head" $
        evaluate (Subst.substTy (indexSubst [reverseHead, n] f (Var reverseHead)) call)

-- | Check head-only retained binders, exact application and fixed-domain evidence.
runFunctionBinderChecks :: DynFlags -> IO ()
runFunctionBinderChecks dflags = do
    let (nat, zero, _) = peanoNat
        natTy = mkTyConTy nat
        zeroTy = mkTyConTy (promoteDataCon zero)
        arrow = mkFunctionType unmatchableDataConTy ManyTy
        f = local 6300 "retained_head" (arrow natTy natTy)
        g = local 6301 "retained_head" (idType f)
        cross = local 6302 "mixed_head" (arrow boolTy natTy)
        n = local 6303 "retained_n" natTy
        m = local 6304 "retained_m" natTy
        at head arg = arrow (mkTyConApp eqTyCon [natTy, TermAppTy head arg, TermAppTy head arg]) intTy
        atN head = at head (TermVarTy n)
        body = arrow (atN f) (atN f)
        retained = mkDepFunTy f body
        alpha = mkDepFunTy g (arrow (atN g) (atN g))
        payload = local 6305 "retained_payload" (atN f)
        worker = Lam f (Lam payload (Var payload))
    forM_ [f, cross] $ \head -> do
        check "supported function head is an eligible retained binder" (isNativeRetainedBinder head && isNativeRetainedBinderType (idType head))
        check "retained function stays outside scalar index eligibility" (not (isNativeIndexId head) && not (isNativeIndexType (idType head)))
        check "function-value reification preserves the actual eligible head" $ case exprToNativeIndex (Var head) of
            Just (TermVarTy actual) -> actual == head && eqType (varType actual) (varType head)
            _ -> False
        lintAccepts dflags "a retained function can occur as a scoped TermVarTy" [head] (Coercion (mkNomReflCo (TermVarTy head)))
    check "function-domain binder alpha equality" (eqType retained alpha)
    let nested = mkDepFunTy f (mkDepFunTy n body)
        nestedAlpha = mkDepFunTy g (mkDepFunTy m (arrow (at g (TermVarTy m)) (at g (TermVarTy m))))
    check "nested function and scalar binders respect alpha equality" (eqType nested nestedAlpha)
    check "nested function and scalar binders close their runtime indices" (isEmptyVarSet (tyCoVarsOfType nested))
    lintAccepts dflags "nested function and scalar retained binders" [] (Coercion (mkNomReflCo nested))
    check "function-domain binder scopes the head but preserves its argument" $
        not (f `elemVarSet` tyCoVarsOfType retained) && n `elemVarSet` tyCoVarsOfType retained
    lintAccepts dflags "function-domain retained identity" [n] worker
    checkType dflags "function-domain lambda type recovery" retained (exprType worker)
    let mixedPayload = local 6306 "mixed_payload" (at cross (constructorIndex trueDataCon))
    lintAccepts dflags "Bool-to-Nat retained binder" [] (Lam cross (Lam mixedPayload (Var mixedPayload)))
    let applied = App worker (Var g)
        expected = arrow (atN g) (atN g)
    lintAccepts dflags "exact local function operand" [g, n] applied
    checkType dflags "function operand substitutes its actual identity" expected (exprType applied)
    checkType dflags "explicit function application type recovery" expected (applyTypeToArgs retained [Var g])
    forM_
        [ ("different domain", [cross, n], Var cross)
        , ("global function", [n], Var (globaliseId g))
        , ("linear function", [setIdMult g OneTy, n], Var (setIdMult g OneTy))
        , ("missing function", [n], Var g)
        , ("lambda producer", [n], identity 6307 natTy)
        ]
        $ \(label, scope, argument) -> lintRejects dflags ("function retained operand rejects " ++ label) scope (App worker argument)
    checkType dflags "function binder masks an outer head substitution" retained (Subst.substTy (indexSubst [g, n] f (Var g)) retained)
    let pair = mkDepFunTy f (arrow (atN f) (atN g))
        captured = Subst.substTy (indexSubst [f, n] g (Var f)) pair
    case captured of
        DepFunTy DepFunVisible fresh result -> do
            check "function binder freshens a capturing range" (fresh /= f)
            checkType dflags "function capture keeps the free range head" (arrow (atN fresh) (atN f)) result
            check "function capture tracks only the actual free replacement" $
                f `elemVarSet` tyCoVarsOfType captured && not (fresh `elemVarSet` tyCoVarsOfType captured)
            lintAccepts dflags "capture-avoiding retained function substitution" [f, n] (Coercion (mkNomReflCo captured))
        _ -> fail "FAIL: function substitution lost its retained binder"
    let cv = proof 6308 (TermVarTy n) (TermVarTy m)
        callCo = mkTermAppCo f (mkCoVarCo cv)
        dictionaryCo = mkTyConAppCo Nominal eqTyCon [mkNomReflCo natTy, callCo, callCo]
        bodyCo = mkFunCoNoFTF Nominal (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) dictionaryCo (mkNomReflCo intTy)
    forM_ [(Nominal, bodyCo), (Representational, mkSubCo bodyCo), (Phantom, mkReflCo Phantom (atN f))] $ \(role, evidence) -> do
        let scoped = mkDepFunCo f evidence
        check "function binder coercion preserves its body role" (coercionRole scoped == role)
        checkType dflags "function binder coercion source" (mkDepFunTy f (coercionLKind evidence)) (coercionLKind scoped)
        checkType dflags "function binder coercion target" (mkDepFunTy f (coercionRKind evidence)) (coercionRKind scoped)
        check "function binder coercion removes its bound head" (not (f `elemVarSet` tyCoVarsOfCo scoped))
        lintAccepts dflags "fixed-domain function binder coercion" [n, m, cv] (Coercion scoped)
    let scopedCo = DepFunCo DepFunVisible DepFunVisible f MRefl bodyCo
    check "function coercion binder masks an outer head substitution" $
        eqCoercion scopedCo (Subst.substCo (indexSubst [g, n, m, cv] f (Var g)) scopedCo)
    lintAccepts dflags "Pi abstraction scopes a function head" [n, m, cv] (Coercion (mkPiCo Nominal unmatchableDataConTy f bodyCo))
    let parameter = mkTyVar (mkSystemName (unique 6309) (mkTyVarOcc "head_payload")) liftedTypeKind
        given = proof 6310 intTy boolTy
        openPi = mkDepFunTy f (arrow (atN f) (mkTyVarTy parameter))
        context = extendLiftingContext (emptyLiftingContext (mkInScopeSet (mkVarSet [n, given]))) parameter (mkCoVarCo given)
        lifted = liftCoSubst Nominal context openPi
    checkType dflags "function binder lifting source" (mkDepFunTy f (arrow (atN f) intTy)) (coercionLKind lifted)
    checkType dflags "function binder lifting target" (mkDepFunTy f (arrow (atN f) boolTy)) (coercionRKind lifted)
    lintAccepts dflags "lifting ordinary evidence under a function binder" [n, given] (Coercion lifted)
    let familyParameter = mkTyVar (mkSystemName (unique 6311) (mkTyVarOcc "head_family_arg")) natTy
        familyBinders = [mkAnonTyConBinder familyParameter]
        family = mkFamilyTyCon (mkSystemName (unique 6312) (mkTcOcc "HeadArgument")) (mkTyConKindWithMatchability unmatchableDataConTy familyBinders natTy) familyBinders 0 natTy Nothing (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal (mkSystemName (unique 6313) (mkTcOcc "HeadArgumentAx")) family [mkCoAxBranch [] [] [] [zeroTy] zeroTy [] noSrcSpan]
        original = mkDepFunTy f (at f (mkTyConApp family [zeroTy]))
        normalized = mkDepFunTy f (at f zeroTy)
        multParameter = mkTyVar (mkSystemName (unique 6315) (mkTyVarOcc "multiplicity_arg")) boolTy
        multBinders = [mkAnonTyConBinder multParameter]
        multFamily = mkFamilyTyCon (mkSystemName (unique 6316) (mkTcOcc "ArrowMultiplicity")) (mkTyConKindWithMatchability unmatchableDataConTy multBinders multiplicityTy) multBinders 0 multiplicityTy Nothing (ClosedTypeFamilyTyCon (CTF (Just multAxiom))) Nothing NotInjective
        multAxiom = mkBranchedCoAxiom Nominal (mkSystemName (unique 6317) (mkTcOcc "ArrowMultiplicityAx")) multFamily [mkCoAxBranch [] [] [] [constructorIndex trueDataCon] ManyTy [] noSrcSpan]
        ordinary = mkFunTy FTF_T_T unmatchableDataConTy (mkTyConApp multFamily [constructorIndex trueDataCon]) intTy intTy
    -- The ordinary arrow has no retained binder or runtime index.
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        let Reduction evidence actual = normaliseType emptyFamInstEnvs role ordinary
        checkType dflags "ordinary multiplicity-family normalization" (arrow intTy intTy) actual
        checkType dflags "ordinary multiplicity-family proof source" ordinary (coercionLKind evidence)
        checkType dflags "ordinary multiplicity-family proof target" actual (coercionRKind evidence)
        check "ordinary multiplicity-family normalization is nonreflexive at the requested role" (not (isReflCo evidence) && coercionRole evidence == role)
        lintAccepts dflags "ordinary multiplicity-family normalization evidence" [] (Coercion evidence)
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        let Reduction evidence actual = normaliseType emptyFamInstEnvs role original
        checkType dflags "function binder normalization reduces a call argument" normalized actual
        checkType dflags "function binder normalization proof source" original (coercionLKind evidence)
        checkType dflags "function binder normalization proof target" normalized (coercionRKind evidence)
        check "function binder normalization preserves role and scope" (coercionRole evidence == role && not (f `elemVarSet` tyCoVarsOfCo evidence))
        lintAccepts dflags "normalized function binder evidence" [] (Coercion evidence)
    let initial lhs rhs = Subst.mkEmptySubst (mkInScopeSet (tyCoVarsOfType lhs `unionVarSet` tyCoVarsOfType rhs))
        bindParameter var _ = if var == parameter then BindMe else DontBindMe
        match lhs rhs = tcMatchRetainedTyX_BM bindParameter (initial lhs rhs) lhs rhs
        target = mkDepFunTy g (arrow (atN g) intTy)
        capturingTarget = mkDepFunTy g (arrow (atN g) (atN g))
    case match openPi target of
        Just result -> do
            checkType dflags "scoped function binder matching infers an erased payload" intTy (Subst.substTy result (mkTyVarTy parameter))
            check "scoped function matching cannot export bound heads" (isEmptyVarSet (Subst.getSubstRangeTyCoFVs result))
        Nothing -> fail "FAIL: scoped function binder matching lost the payload"
    check "scoped function matching rejects head escape" $ case match openPi capturingTarget of
        Nothing -> True
        Just _ -> False
    check "default matching does not infer through a function binder" $ case tcMatchTyX_BM bindParameter (initial openPi target) openPi target of
        Nothing -> True
        Just _ -> False
    check "fine unification remains conservative under an open function binder" $ case tcUnifyTysFG alwaysBindFam alwaysBindTv [openPi] [target] of
        MaybeApart{} -> True
        _ -> False
    let indexParameter = mkTyVar (mkSystemName (unique 6314) (mkTyVarOcc "head_index")) natTy
        opaqueTemplate = mkDepFunTy f (at f (mkTyVarTy indexParameter))
        opaqueTarget = mkDepFunTy g (atN g)
    check "opt-in function matching does not invert opaque call arguments" $
        case tcMatchRetainedTyX_BM (\var _ -> if var == indexParameter then BindMe else DontBindMe) (initial opaqueTemplate opaqueTarget) opaqueTemplate opaqueTarget of
            Nothing -> True
            Just _ -> False

{- Note [Closed native function-field checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Parameter fields hold opaque lifted values. This does not make those values
eligible call heads. The original function-free endpoint policy must still
reject a recursive function over its own container.

Build forward application proofs before replacing their erased head variables
with runtime function indices. Preserve each nonreflexive head proof and its
canonical endpoints. Extracting an existing AppCo child uses stored evidence;
it does not justify projecting an opaque U or unknown application equality.
The separate source client checks actual saturated-family constructor transport.
-}

-- | Check opaque parameter fields and preservation of fixed-head forward proofs.
runFunctionFieldChecks :: DynFlags -> IO ()
runFunctionFieldChecks dflags = do
    let fnTy = mkVisFunTyManyU boolTy boolTy
        maybeFn = mkTyConApp maybeTyCon [fnTy]
        f = local 97000 "field_function" fnTy
        other = local 97001 "field_function" fnTy
        a = local 97002 "field_argument" boolTy
        b = local 97003 "field_argument" boolTy
        argTy = TermVarTy a
        otherArgTy = TermVarTy b
        call = TermAppTy f argTy
        otherCall = TermAppTy other otherArgTy
        variable n label kind = mkTyVar (mkSystemName (unique n) (mkTyVarOcc label)) kind
        open = variable 97004 "field_open" liftedTypeKind
        poly = mkTyCoForAllTy unmatchableDataConTy open Specified $
            mkVisFunTyManyU (mkTyVarTy open) (mkTyVarTy open)
        maybeOf ty = mkTyConApp maybeTyCon [ty]
    check "closed Maybe function field is an index domain" (isNativeIndexType maybeFn)
    check "regular nested function-field containers are index domains" (isNativeIndexType (mkListTy maybeFn))
    check "function values remain separate from algebraic index eligibility" $
        isNativeRetainedBinder f && not (isNativeIndexId f || isNativeIndexType fnTy)
    forM_
        [ ("M", mkVisFunTyManyM boolTy boolTy)
        , ("linear", mkVisFunTy unmatchableDataConTy OneTy boolTy boolTy)
        , ("polymorphic", poly)
        , ("open", mkVisFunTyManyU boolTy (mkTyVarTy open))
        ]
        $ \(label, ty) -> do
            check ("parameter field accepts an opaque " ++ label ++ " value") (isNativeIndexType (maybeOf ty))
            check ("opaque " ++ label ++ " value retains identity") (isNativeOpaqueValueId (setIdType f ty))
            check ("opaque " ++ label ++ " value is not a fixed native call head") (not (isNativeIndexFunctionId (setIdType f ty)))
    bounded <- timeout 1000000 (evaluate (isNativeIndexType (mkTyConTy loop)))
    check "a recursive function over its container is rejected without looping" (bounded == Just False)
    lintAccepts dflags "function-value index has its actual lexical binding" [f] (Coercion (mkNomReflCo (TermVarTy f)))
    lintRejects dflags "function-value index cannot escape" [] (Coercion (mkNomReflCo (TermVarTy f)))
    lintRejects dflags "function-value index cannot use a linear binding" [setIdMult f OneTy] (Coercion (mkNomReflCo (TermVarTy f)))
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        let Reduction evidence normalized = normaliseType emptyFamInstEnvs role (TermVarTy f)
        checkType dflags "function-value normalization keeps the exact value" (TermVarTy f) normalized
        check "function-value normalization preserves role" (coercionRole evidence == role)
        lintAccepts dflags "function-value normalization proof" [f] (Coercion evidence)
    let justExpr = mkApps (Var (dataConWorkId justDataCon)) [Type fnTy, Var f]
        justIndex = mkTyConApp (promoteDataCon justDataCon) [fnTy, TermVarTy f]
    lintAccepts dflags "Just stores a scoped opaque function value" [f] justExpr
    check "Just function-field reification preserves its index" (maybe False (eqType justIndex) (exprToNativeIndex justExpr))
    check "Just function-field reconstruction preserves its actual field" (cheapEqExpr justExpr (mapNativeIndex id justExpr))
    checkType dflags "Just function-field index has the full container kind" maybeFn (typeKind justIndex)
    forM_
        [ ("single", mkAppTy (TermVarTy f) argTy)
        , ("batched", mkAppTys (TermVarTy f) [argTy])
        , ("cast batch", mkAppTys (CastTy (TermVarTy f) (mkNomReflCo fnTy)) [argTy])
        ]
        $ \(label, actual) -> do
            checkType dflags (label ++ " function-value application matches a direct call") call actual
            check (label ++ " function-value application has one canonical form") $ case actual of
                TermAppTy head argument -> head == f && eqType argument argTy
                _ -> False
    lintRejects dflags "raw duplicate native application form is rejected" [f, a] $
        Coercion (mkNomReflCo (AppTy (TermVarTy f) argTy))

    let g = variable 97010 "generic_field_head" fnTy
        h = variable 97011 "generic_other_head" fnTy
        argumentProof = proof 97012 argTy otherArgTy
        argumentCo = mkCoVarCo argumentProof
        genericCall = AppTy (mkTyVarTy g) argTy
        genericOtherCall = AppTy (mkTyVarTy h) otherArgTy
    checkType dflags "substitution into a pre-existing application is canonical" call $
        Subst.substTyWith [g] [TermVarTy f] genericCall
    forM_ (zip [97020, 97030] [Nominal, Representational]) $ \(base, role) -> do
        let genericHead = mkCoVar (mkSystemName (unique base) (mkVarOcc "generic_head_proof")) $
                mkCoercionType role (mkTyVarTy g) (mkTyVarTy h)
            actualHead = mkCoVar (mkSystemName (unique (base + 1)) (mkVarOcc "actual_head_proof")) $
                mkCoercionType role (TermVarTy f) (TermVarTy other)
            genericCo = mkAppCo (mkCoVarCo genericHead) argumentCo
            scope = [f, other, a, b, argumentProof, actualHead]
            subst = Subst.Subst (mkInScopeSet (mkVarSet scope)) emptyVarEnv
                (mkVarEnv [(g, TermVarTy f), (h, TermVarTy other)])
                (unitVarEnv genericHead (mkCoVarCo actualHead))
            actualCo = Subst.substCo subst genericCo
        lintAccepts dflags "generic forward proof is valid before substitution" [g, h, a, b, argumentProof, genericHead] (Coercion genericCo)
        checkType dflags "generic forward proof starts at the original application" genericCall (coercionLKind genericCo)
        checkType dflags "generic forward proof ends at the original application" genericOtherCall (coercionRKind genericCo)
        checkType dflags "substituted forward proof has a canonical native source" call (coercionLKind actualCo)
        checkType dflags "substituted forward proof has a canonical native target" otherCall (coercionRKind actualCo)
        check "forward proof retains its role and nonreflexive head evidence" $
            coercionRole actualCo == role && actualHead `elemVarSet` tyCoVarsOfCo actualCo
        lintAccepts dflags "substituted N/R/P application proof" scope (Coercion actualCo)
        case splitAppCo_maybe actualCo of
            Just (storedHead, storedArg) -> do
                check "structural application splitting returns the stored proof" $
                    actualHead `elemVarSet` tyCoVarsOfCo storedHead && eqCoercion storedArg argumentCo
                lintAccepts dflags "stored application head evidence" scope (Coercion storedHead)
                lintAccepts dflags "stored application argument evidence" scope (Coercion storedArg)
            Nothing -> fail "FAIL: forward proof lost its stored application children"
    let phantomHead = mkPhantomCo (mkNomReflCo fnTy) (mkTyVarTy g) (mkTyVarTy h)
        phantomGeneric = mkAppCo phantomHead argumentCo
        phantomSubst = Subst.Subst (mkInScopeSet (mkVarSet [f, other, a, b, argumentProof])) emptyVarEnv
            (mkVarEnv [(g, TermVarTy f), (h, TermVarTy other)]) emptyVarEnv
        phantomActual = Subst.substCo phantomSubst phantomGeneric
    lintAccepts dflags "generic phantom application has ordinary kind evidence" [g, h, a, b, argumentProof] (Coercion phantomGeneric)
    checkType dflags "phantom forward proof has a canonical native source" call (coercionLKind phantomActual)
    checkType dflags "phantom forward proof has a canonical native target" otherCall (coercionRKind phantomActual)
    check "phantom application retains its role and nominal argument evidence" $
        coercionRole phantomActual == Phantom && argumentProof `elemVarSet` tyCoVarsOfCo phantomActual
    lintAccepts dflags "substituted phantom application proof" [f, other, a, b, argumentProof] (Coercion phantomActual)
    let fixedHeadCo = mkAppCo (mkNomReflCo (mkTyVarTy g)) argumentCo
        predicateCo = mkTyConAppCo Nominal eqTyCon [mkNomReflCo boolTy, fixedHeadCo, fixedHeadCo]
        bodyCo = mkFunCoNoFTF Nominal (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) predicateCo (mkNomReflCo intTy)
        quantifiedCo = mkForAllCo g Specified Specified (mkNomReflCo matchableDataConTy) MRefl bodyCo
        instantiated = mkInstCo quantifiedCo (mkNomReflCo (TermVarTy f))
    checkType dflags "InstCo preserves the substituted native source" (indexed call) (coercionLKind instantiated)
    checkType dflags "InstCo preserves the substituted native target" (indexed (TermAppTy f otherArgTy)) (coercionRKind instantiated)
    lintAccepts dflags "InstCo keeps a pre-existing forward application proof" [f, a, b, argumentProof] (Coercion instantiated)
    checkType dflags "function-value substitution keeps an eligible variable range" (TermVarTy other) $
        Subst.substTy (indexSubst [other] f (Var other)) (TermVarTy f)
    forM_ [("lambda", identity 97050 boolTy), ("cast", Cast (Var other) (mkReflCo Representational fnTy))] $ \(label, rhs) ->
        rejectsWith ("bare function substitution rejects a " ++ label ++ " range") "unsupported native function range" $
            evaluate (typeKind (Subst.substTy (indexSubst [other] f rhs) (TermVarTy f)))

    let callProof = proof 97051 call (TermAppTy f otherArgTy)
    forM_ [CLeft, CRight] $ \side ->
        lintRejects dflags "opaque native U equality cannot reveal stored children" [f, a, b, callProof] (Coercion (LRCo side (mkCoVarCo callProof)))
    let ma = variable 97052 "field_unknown_matchability" matchabilityTy
    forM_ [(97060, matchableDataConTy, True), (97070, mkTyVarTy ma, False)] $ \(base, metadata, matchable) -> do
        let head = variable base "field_inverse_head" (mkVisFunTyMany metadata boolTy boolTy)
            lhs = AppTy (mkTyVarTy head) argTy
            rhs = AppTy (mkTyVarTy head) otherArgTy
            opaque = proof (base + 1) lhs rhs
            scope = [ma, head, a, b, opaque]
        forM_ [CLeft, CRight] $ \side ->
            if matchable
                then lintAccepts dflags "ordinary M inverse application remains valid" scope (Coercion (LRCo side (mkCoVarCo opaque)))
                else lintRejects dflags "unknown inverse application remains invalid" scope (Coercion (LRCo side (mkCoVarCo opaque)))
  where
    modu = mkModule mainUnit (mkModuleName "NativeFunctionFieldAPI")
    name n occurrence = mkExternalName (unique n) modu occurrence noSrcSpan
    loop = mkAlgTyCon (name 97080 (mkTcOcc "Loop")) liftedTypeKind [] 0 liftedTypeKind [] Nothing []
        (mkDataTyConRhs [loopCon]) (VanillaAlgTyCon (name 97081 (mkVarOcc "loop_rep"))) False
    loopCon = con
      where
        occurrence = mkDataOcc "Loop"
        con = mkDataCon (name 97082 occurrence) False (name 97083 (mkVarOcc "loop_con_rep"))
            [HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict] [HsLazy] [NotMarkedStrict]
            [] [] [] noConcreteTyVars [] [] []
            [Scaled OneTy (mkVisFunTyManyU (mkTyConTy loop) boolTy)] (mkTyConTy loop)
            NoPromInfo loop 1 [] (mkDataConWorkId (name 97084 (mkDataConWorkerOcc occurrence)) con) NoDataConRep

-- | Check forall selector roles and the reductions that preserve nominal evidence.
runForAllSelectorChecks :: DynFlags -> IO ()
runForAllSelectorChecks dflags = do
    let a = mkTyVar (mkSystemName (unique 6500) (mkTyVarOcc "a")) liftedTypeKind
        b = mkTyVar (mkSystemName (unique 6501) (mkTyVarOcc "b")) boolTy
        left = mkForAllTy unmatchableDataConTy (Bndr a Specified) intTy
        right = mkForAllTy unmatchableDataConTy (Bndr b Specified) intTy
        phantom = mkPhantomCo (mkNomReflCo liftedTypeKind) left right
        sameKind = mkPhantomCo (mkNomReflCo liftedTypeKind) left (mkForAllTy unmatchableDataConTy (Bndr a Specified) boolTy)
        kindProof = proof 6502 liftedTypeKind boolTy
        nominal = proof 6503 left right
        representational = mkCoVar (mkSystemName (unique 6504) (mkVarOcc "repr_cv")) (mkReprEqPred left right)
        structural = mkForAllCo a Specified Specified (mkNomReflCo unmatchableDataConTy) (MCo (mkCoVarCo kindProof)) (mkReflCo Phantom intTy)
        visibility = mkForAllCo a Specified Required (mkNomReflCo unmatchableDataConTy) MRefl (mkReflCo Representational intTy)
    lintAccepts dflags "phantom equality permits different forall binder kinds" [] (Coercion phantom)
    lintAccepts dflags "structural phantom forall retains a nominal kind child" [kindProof] (Coercion structural)
    forM_ [structural, mkSymCo structural] $ \co -> do
        let selected = mkSelCo (SelForAll SelKind) co
        check "structural phantom selection returns nominal evidence" (coercionRole selected == Nominal)
        lintAccepts dflags "structural phantom selector reduces to its nominal child" [kindProof] (Coercion selected)
    check "structural phantom selector returns the stored child" (eqCoercion (mkSelCo (SelForAll SelKind) structural) (mkCoVarCo kindProof))
    forM_ [mkNomReflCo left, sameKind] $ \co -> do
        let selected = mkSelCo (SelForAll SelKind) co
        check "equal forall binder kinds give nominal reflexivity" (coercionRole selected == Nominal && isReflCo selected)
        checkType dflags "reflexive forall selection has the binder kind" liftedTypeKind (coercionLKind selected)
        lintAccepts dflags "equal-kind phantom selection reduces before its role check" [] (Coercion selected)
    forM_ [nominal, representational] $ \cv -> do
        let selected = mkSelCo (SelForAll SelKind) (mkCoVarCo cv)
        check "opaque nominal or representational selection remains nominal" (coercionRole selected == Nominal)
        checkType dflags "opaque forall selector left endpoint" liftedTypeKind (coercionLKind selected)
        checkType dflags "opaque forall selector right endpoint" boolTy (coercionRKind selected)
        lintAccepts dflags "nominal and representational forall selectors remain valid" [cv] (Coercion selected)
    case pushCoTyArg (mkCoVarCo representational) (mkTyConTy (promoteDataCon trueDataCon)) of
        Nothing -> fail "FAIL: representational forall type-argument pushing was declined"
        Just (functionType, argument, resultCo) -> do
            checkType dflags "type-argument pushing retains its source forall" left functionType
            checkType dflags "type-argument pushing transports the argument kind" liftedTypeKind (typeKind argument)
            lintAccepts dflags "transported type argument retains its nominal kind proof" [representational] (Coercion (mkNomReflCo argument))
            case resultCo of
                MRefl -> pure ()
                MCo co -> lintAccepts dflags "type-argument pushing retains representational result evidence" [representational] (Coercion co)
    lintAccepts dflags "required forall visibility bridge remains representational" [] (Coercion visibility)
    lintRejects dflags "raw phantom forall selector cannot prove nominal binder-kind equality" [] (Coercion (SelCo (SelForAll SelKind) phantom))
    lintRejects dflags "raw phantom forall selector requires reduction even at equal kinds" [] (Coercion (SelCo (SelForAll SelKind) sameKind))

{- Note [Arrow matchability kernel controls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary arrow can describe an unmatchable type function. AppCo still
supports forward congruence. LRCo must not invert that application. SelFun
projects the structure of the arrow itself, so it does not require M.
Use raw coercions here. Smart constructors can remove reflexive children and
hide a malformed role before Lint sees it. Native calls keep their separate
opaque representation and must not acquire LRCo projections.
Binding a template variable to a whole application does not invert that
application. Matchability must not block this binding before variable matching.
If that variable is rigid, a U or unknown application can still reduce to it.
The lack of a permitted binding does not establish definite apartness.
-}

-- | Check explicit forall metadata, its scope, and conditional CoVar form.
runForAllMatchabilityChecks :: DynFlags -> IO ()
runForAllMatchabilityChecks dflags = do
    let a = mkTyVar (mkSystemName (unique 6510) (mkTyVarOcc "forall_a")) liftedTypeKind
        m = mkTyVar (mkSystemName (unique 6511) (mkTyVarOcc "forall_ma")) matchabilityTy
        quantified ma = mkForAllTy ma (Bndr a Specified) intTy
        forallM = quantified matchableDataConTy
        forallU = quantified unmatchableDataConTy
        forallV = quantified (mkTyVarTy m)
        entries = [(forallM, 1), (forallU, 2), (forallV, 3)]
        table :: TypeMap Int
        table = foldl' (\acc (ty, value) -> Trie.insertTM ty value acc) emptyTypeMap entries
    forM_ [(forallM, forallU), (forallM, forallV), (forallU, forallV)] $ \(left, right) -> do
        check "forall metadata changes equality" (not (eqType left right))
        check "forall metadata changes ordering" (nonDetCmpType left right /= EQ)
    forM_ entries $ \(ty, value) -> do
        check "type map preserves distinct forall metadata" (Trie.lookupTM ty table == Just value)
        lintAccepts dflags "explicit forall metadata is well kinded" [m] (Coercion (mkNomReflCo ty))
        checkType dflags "tidying preserves forall metadata" ty (tidyTopType ty)
    check "forall annotation contributes free variables" (m `elemVarSet` tyCoVarsOfType forallV)
    let annotated = [(mkTyVarTy m, a), (matchableDataConTy, m)]
        ordered = scopedSortWith snd (\(ma, v) -> tyCoVarsOfType ma `unionVarSet` tyCoVarsOfType (varType v)) annotated
    check "telescope sorting includes annotation dependencies" (map snd ordered == [m, a])
    lintAccepts dflags "annotation-aware telescope sorting preserves scope" [] $
        Coercion (mkNomReflCo (mkForAllTys [(ma, Bndr v Specified) | (ma, v) <- ordered] intTy))
    checkType dflags "substitution preserves explicit invisible U" forallU $
        Subst.substTyWith [m] [unmatchableDataConTy] forallV
    checkType dflags "plural forall makers preserve per-binder U" forallU $
        mkForAllTys [(unmatchableDataConTy, Bndr a Specified)] intTy
    let nominal = proof 6514 forallV forallU
        representational = mkCoVar (mkSystemName (unique 6515) (mkVarOcc "forall_repr"))
                                  (mkReprEqPred forallV forallU)
        phantom = mkPhantomCo (mkNomReflCo liftedTypeKind) forallV forallU
    forM_ [nominal, representational] $ \parent -> do
        let selected = mkSelCo (SelForAll SelFAMatch) (mkCoVarCo parent)
        check "opaque forall metadata selector remains nominal" (coercionRole selected == Nominal)
        checkType dflags "opaque metadata selector retains its left endpoint" (mkTyVarTy m) (coercionLKind selected)
        checkType dflags "opaque metadata selector retains its right endpoint" unmatchableDataConTy (coercionRKind selected)
        lintAccepts dflags "opaque N/R forall metadata selection" [m, parent] (Coercion selected)
    lintRejects dflags "opaque P forall cannot project nominal metadata" [m] $
        Coercion (SelCo (SelForAll SelFAMatch) phantom)
    forM_ entries $ \(ty, _) -> do
        let instantiated = mkInstCo (mkNomReflCo ty) (mkNomReflCo boolTy)
        checkType dflags "forward forall instantiation does not require M" intTy (coercionLKind instantiated)
        lintAccepts dflags "forall instantiation retains valid evidence" [m] (Coercion instantiated)
    lintRejects dflags "forall annotation is outside its own binder" [] $
        Coercion (mkNomReflCo (ForAllTy (mkTyVarTy m) (Bndr m Specified) intTy))
    lintRejects dflags "forall annotation must have kind Matchability" [] $
        Coercion (mkNomReflCo (ForAllTy intTy (Bndr a Specified) intTy))

    let cv = proof 6512 liftedTypeKind liftedTypeKind
        covarForall ma = mkTyCoForAllTy ma cv Specified intTy
        cvM = covarForall matchableDataConTy
        cvV = covarForall (mkTyVarTy m)
        cvU = covarForall unmatchableDataConTy
        arrowU = mkFunctionType unmatchableDataConTy ManyTy (varType cv) intTy
        metadataProof = proof 6513 (mkTyVarTy m) unmatchableDataConTy
        structural = mkForAllCo cv Specified Specified (mkCoVarCo metadataProof)
                                MRefl (mkReflCo Phantom intTy)
    checkType dflags "unused U CoVar uses actual arrow sorts" arrowU cvU
    check "unused M CoVar retains a forall" (case cvM of ForAllTy{} -> True; _ -> False)
    check "unused unknown CoVar retains a forall" (case cvV of ForAllTy{} -> True; _ -> False)
    forM_ [cvM, cvV, cvU] $ \ty ->
        lintAccepts dflags "CoVar canonicalization preserves well-kinded types" [m] (Coercion (mkNomReflCo ty))
    checkType dflags "substitution rechecks CoVar vacuity after selecting U" arrowU $
        Subst.substTyWith [m] [unmatchableDataConTy] cvV
    lintAccepts dflags "structural forall evidence can have differently normalized endpoints" [m, metadataProof] (Coercion structural)
    check "forall metadata proof remains a live coercion dependency" $
        metadataProof `elemVarSet` tyCoVarsOfCo structural
    checkType dflags "structural forall right endpoint uses U arrow form" arrowU (coercionRKind structural)
    check "structural metadata projection returns its stored nominal proof" $
        eqCoercion (mkSelCo (SelForAll SelFAMatch) structural) (mkCoVarCo metadataProof)
    lintAccepts dflags "structural phantom metadata projection reduces before shape checks" [m, metadataProof] $
        Coercion (mkSelCo (SelForAll SelFAMatch) structural)
    lintRejects dflags "raw phantom metadata projection requires reduction" [m, metadataProof] $
        Coercion (SelCo (SelForAll SelFAMatch) structural)
    lintRejects dflags "forall metadata evidence must be nominal" [] $
        Coercion (ForAllCo a Specified Specified (mkReflCo Representational unmatchableDataConTy)
                          MRefl (mkNomReflCo intTy))

-- | Check CoVar scope in the mutable solver, including safe outer unification.
runForAllSolverChecks :: FilePath -> IO ()
runForAllSolverChecks libdir = runGhc (Just libdir) $ do
    initialFlags <- getSessionDynFlags
    _ <- setSessionDynFlags initialFlags
    env <- getSession
    dflags <- getSessionDynFlags
    liftIO $ do
        (_, outcome) <- TcM.initTcInteractive TcM.NoTcMPlugins env $ do
            let cv = mkCoVar (mkSystemName (unique 6520) (mkVarOcc "scope_cv")) (mkNomEqPred liftedTypeKind boolTy)
                other = mkCoVar (mkSystemName (unique 6521) (mkVarOcc "scope_other")) (varType cv)
                quantify binder = mkTyCoForAllTy matchableDataConTy binder Specified
                headKind = quantify cv liftedTypeKind
                solve left right = do
                    loc <- TcM.getCtLocM (TypeEqOrigin left right Nothing Nothing) (Just TypeLevel)
                    ((residual, proof), _) <- TcS.runTcS $ do
                        (wanted, _) <- TcS.newWantedEq loc emptyCoHoleSet Nominal left right
                        residual <- solveWanteds (mkSimpleWC [CtWanted wanted])
                        proof <- if isSolvedWC residual
                            then Just <$> TcS.zonkCo (wantedCtEvCoercion wanted)
                            else pure Nothing
                        pure (residual, proof)
                    pure (isSolvedWC residual, proof)
            skolInfo <- mkSkolemInfo (UnifyForAllSkol liftedTypeKind)
            headVar <- newSkolemTyVar skolInfo (mkSystemName (unique 6522) (mkTyVarOcc "scope_head")) headKind
            escaped <- newFlexiTyVar liftedTypeKind
            let left = quantify cv (mkTyVarTy escaped)
                applied = mkAppTy (mkTyVarTy headVar) (mkCoercionTy (mkCoVarCo other))
                right = quantify other applied
            (escapeSolved, _) <- solve left right
            escapedValue <- isFilledMetaTyVar_maybe escaped
            safe <- newFlexiTyVar liftedTypeKind
            (safeSolved, safeProof) <- solve (quantify cv (mkTyVarTy safe)) (quantify other intTy)
            safeValue <- isFilledMetaTyVar_maybe safe
            let equalityCv = mkCoVar (mkSystemName (unique 6523) (mkVarOcc "fc6_cv")) (mkNomEqPred intTy boolTy)
            (usedAsGiven, _) <- solve (quantify equalityCv intTy) (quantify equalityCv boolTy)
            used <- newFlexiTyVar liftedTypeKind
            -- A non-reflexive kind proof survives substitution and zonking.
            -- Select accepts the resulting Bool index and returns a lifted type.
            let usedCv = mkCoVar (mkSystemName (unique 6524) (mkVarOcc "used_cv")) (mkNomEqPred liftedTypeKind boolTy)
                usedOther = mkCoVar (mkSystemName (unique 6525) (mkVarOcc "used_other")) (varType usedCv)
                usedBody first binder = mkBoxedTupleTy
                    [first, mkTyConApp (fst selectFamily) [CastTy intTy (mkCoVarCo binder)]]
                usedLeft = quantify usedCv (usedBody (mkTyVarTy used) usedCv)
                usedRight = quantify usedOther (usedBody intTy usedOther)
            (usedSolved, usedProof) <- solve usedLeft usedRight
            usedValue <- isFilledMetaTyVar_maybe used
            pure (escapeSolved, escapedValue, safeSolved, safeValue, safeProof, usedAsGiven,
                  (usedSolved, usedValue, usedProof))
        case outcome of
            Nothing -> fail "FAIL: CoVar solver controls could not initialize TcM"
            Just (escapeSolved, escapedValue, safeSolved, safeValue, safeProof, usedAsGiven,
                  (usedSolved, usedValue, usedProof)) -> do
                check "CoVar-dependent Wanted remains unsolved without escaping its binder" (not escapeSolved)
                check "outer metavariable cannot acquire a scoped CoVar" (case escapedValue of Nothing -> True; _ -> False)
                check "CoVar-free equality still solves an outer metavariable" safeSolved
                check "safe CoVar-free unification selects Int" (maybe False (`eqType` intTy) safeValue)
                case safeProof of
                    Just proof -> lintAccepts dflags "safe CoVar forall solver evidence" [] (Coercion proof)
                    Nothing -> fail "FAIL: safe CoVar forall equality has no evidence"
                check "CoVar forall does not expose forbidden Given evidence" (not usedAsGiven)
                check "used CoVar forall equality still solves" usedSolved
                check "used CoVar equality safely selects Int" (maybe False (`eqType` intTy) usedValue)
                check "used CoVar cannot escape into the outer metavariable" $
                    maybe False (not . anyVarSet isCoVar . tyCoVarsOfType) usedValue
                case usedProof of
                    Just proof -> do
                        lintAccepts dflags "used CoVar forall solver evidence" [] (Coercion proof)
                        check "used CoVar solver evidence has no free proof variable" $
                            not (anyVarSet isCoVar (tyCoVarsOfCo proof))
                        forM_ [coercionLKind proof, coercionRKind proof] $ \endpoint ->
                            case splitForAllTyCoVar_maybe endpoint of
                                Just (_, binder, body) -> check "solver evidence retains its used CoVar binder" $
                                    isCoVar binder && binder `elemVarSet` tyCoVarsOfType body
                                Nothing -> fail "FAIL: used CoVar solver endpoint lost its forall"
                    Nothing -> fail "FAIL: used CoVar forall equality has no evidence"

-- | Check arrow metadata at substitution and coercion boundaries.
runArrowMatchabilityChecks :: DynFlags -> IO ()
runArrowMatchabilityChecks dflags = do
    let ma = mkTyVar (mkSystemName (unique 6400) (mkTyVarOcc "arrow_ma")) matchabilityTy
        mb = mkTyVar (mkSystemName (unique 6401) (mkTyVarOcc "arrow_mb")) matchabilityTy
        arrow metadata = mkFunctionType metadata ManyTy intTy boolTy
        arrowM = arrow matchableDataConTy
        arrowU = arrow unmatchableDataConTy
        arrowV = arrow (mkTyVarTy ma)
        quantified v = mkForAllTy unmatchableDataConTy (Bndr v Specified) (arrow (mkTyVarTy v))
        entries = [(arrowM, 1), (arrowU, 2), (arrowV, 3), (quantified ma, 4)]
        table :: TypeMap Int
        table = foldl' (\m (ty, value) -> Trie.insertTM ty value m) emptyTypeMap entries
    forM_ [(arrowM, arrowU), (arrowM, arrowV), (arrowU, arrowV)] $ \(lhs, rhs) -> do
        check "arrow metadata changes equality" (not (eqType lhs rhs))
        check "arrow metadata changes ordering" (nonDetCmpType lhs rhs /= EQ)
    forM_ entries $ \(ty, value) ->
        check "type map preserves distinct arrow metadata" (Trie.lookupTM ty table == Just value)
    check "metadata binders are alpha-equivalent type map keys" (Trie.lookupTM (quantified mb) table == Just 4)
    check "metadata occurrence is free before abstraction" (ma `elemVarSet` tyCoVarsOfType arrowV)
    check "metadata binder scopes its occurrence" (not (ma `elemVarSet` tyCoVarsOfType (quantified ma)))
    checkType dflags "metadata variable survives binder instantiation" (arrow (mkTyVarTy mb)) (piResultTy (quantified ma) (mkTyVarTy mb))
    checkType dflags "metadata substitution can select M" arrowM (Subst.substTyWith [ma] [matchableDataConTy] arrowV)
    checkType dflags "metadata substitution can select U" arrowU (Subst.substTyWith [ma] [unmatchableDataConTy] arrowV)
    lintAccepts dflags "metadata abstraction" [] (Coercion (mkNomReflCo (quantified ma)))
    forM_ (zip [6410, 6420, 6430] [matchableDataConTy, unmatchableDataConTy, mkTyVarTy ma]) $ \(base, metadata) -> do
        let kind = mkVisFunTyMany metadata liftedTypeKind liftedTypeKind
            f = mkTyVar (mkSystemName (unique base) (mkTyVarOcc "arrow_f")) kind
            g = mkTyVar (mkSystemName (unique (base + 1)) (mkTyVarOcc "arrow_g")) kind
            a = mkTyVar (mkSystemName (unique (base + 2)) (mkTyVarOcc "arrow_a")) liftedTypeKind
            b = mkTyVar (mkSystemName (unique (base + 3)) (mkTyVarOcc "arrow_b")) liftedTypeKind
            lhs = AppTy (mkTyVarTy f) (mkTyVarTy a)
            rhs = AppTy (mkTyVarTy g) (mkTyVarTy b)
            fg = proof (base + 4) (mkTyVarTy f) (mkTyVarTy g)
            ab = proof (base + 5) (mkTyVarTy a) (mkTyVarTy b)
            applied = proof (base + 6) lhs rhs
            whole = mkTyVar (mkSystemName (unique (base + 7)) (mkTyVarOcc "whole_arrow_application")) liftedTypeKind
            wholeTy = mkTyVarTy whole
            scope = [ma, f, g, a, b, fg, ab, applied]
            matchable = eqType metadata matchableDataConTy
        forM_ [("tcMatchTy", tcMatchTy), ("tcMatchTyKi", tcMatchTyKi)] $ \(label, match) ->
            case match wholeTy lhs >>= (`Subst.lookupTyVar` whole) of
                Just actual -> checkType dflags (label ++ " binds a whole application at any matchability") lhs actual
                Nothing -> fail ("FAIL: " ++ label ++ " rejected whole-application template binding")
        forM_ [(wholeTy, lhs), (lhs, wholeTy)] $ \(left, right) ->
            case tcUnifyTysFG alwaysBindFam alwaysBindTv [left] [right] of
                Unifiable subst -> case Subst.lookupTyVar subst whole of
                    Just actual -> checkType dflags "unification binds a whole application in either direction" lhs actual
                    Nothing -> fail "FAIL: whole-application unification omitted the variable binding"
                _ -> fail "FAIL: unification rejected whole-application variable binding"
        unless matchable $
            forM_ [(wholeTy, lhs), (lhs, wholeTy)] $ \(left, right) ->
                check "rigid variable versus opaque application is only maybe apart" $
                    case tcUnifyTysFG alwaysBindFam (\_ _ -> DontBindMe) [left] [right] of
                        MaybeApart{} -> True
                        _ -> False
        check "matching inside an application still requires M" $
            case tcMatchTy lhs (AppTy (mkTyVarTy f) (mkTyVarTy b)) of
                Just subst -> matchable && eqType (Subst.substTy subst (mkTyVarTy a)) (mkTyVarTy b)
                Nothing -> not matchable
        lintAccepts dflags "forward application congruence permits any matchability" scope (Coercion (AppCo (mkCoVarCo fg) (mkCoVarCo ab)))
        check "application injectivity requires M" (isInjectiveInType a lhs == matchable)
        forM_ [CLeft, CRight] $ \side ->
            if matchable
                then lintAccepts dflags "M application inversion" scope (Coercion (LRCo side (mkCoVarCo applied)))
                else lintRejects dflags "U or unknown application inversion" scope (Coercion (LRCo side (mkCoVarCo applied)))
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        let metadataRole = if role == Phantom then Phantom else Nominal
            structural = FunCo role FTF_T_T FTF_T_T (mkReflCo metadataRole unmatchableDataConTy) (mkReflCo metadataRole ManyTy) (mkReflCo role intTy) (mkReflCo role boolTy)
        lintAccepts dflags "raw U function coercion" [] (Coercion structural)
        forM_ [(SelMatch, metadataRole, unmatchableDataConTy), (SelMult, metadataRole, ManyTy), (SelArg, role, intTy), (SelRes, role, boolTy)] $ \(selector, expectedRole, expectedType) -> do
            let selected = SelCo (SelFun selector) structural
            check "structural arrow projection preserves its role" (coercionRole selected == expectedRole)
            checkType dflags "structural arrow projection endpoint" expectedType (coercionLKind selected)
            lintAccepts dflags "structural projection permits U" [] (Coercion selected)
    let phantom = FunCo Phantom FTF_T_T FTF_T_T (mkReflCo Phantom unmatchableDataConTy) (mkReflCo Phantom ManyTy) (mkReflCo Phantom intTy) (mkReflCo Phantom boolTy)
        forged = FunCo Phantom FTF_T_T FTF_T_T (mkNomReflCo unmatchableDataConTy) (mkReflCo Phantom ManyTy) (mkReflCo Phantom intTy) (mkReflCo Phantom boolTy)
        consumer = mkTyVar (mkSystemName (unique 6440) (mkTyVarOcc "metadata_consumer")) (mkVisFunTyManyM matchabilityTy liftedTypeKind)
        phantomMetadata = SelCo (SelFun SelMatch) phantom
    lintRejects dflags "phantom function cannot contain nominal metadata evidence" [] (Coercion forged)
    lintRejects dflags "phantom metadata projection cannot supply nominal application evidence" [consumer] (Coercion (AppCo (mkNomReflCo (mkTyVarTy consumer)) phantomMetadata))
    let parameter = mkTyVar (mkSystemName (unique 6441) (mkTyVarOcc "metadata_parameter")) boolTy
        binders = [mkAnonTyConBinder parameter]
        family = mkFamilyTyCon (mkSystemName (unique 6442) (mkTcOcc "ArrowMatchability")) (mkTyConKindWithMatchability unmatchableDataConTy binders matchabilityTy) binders 0 matchabilityTy Nothing (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal (mkSystemName (unique 6443) (mkTcOcc "ArrowMatchabilityAx")) family [mkCoAxBranch [] [] [] [constructorIndex trueDataCon] unmatchableDataConTy [] noSrcSpan]
        reducible = arrow (mkTyConApp family [constructorIndex trueDataCon])
    forM_ [Nominal, Representational, Phantom] $ \role -> do
        let Reduction evidence normalized = normaliseType emptyFamInstEnvs role reducible
        checkType dflags "normalization reduces arrow metadata" arrowU normalized
        checkType dflags "metadata normalization proof source" reducible (coercionLKind evidence)
        checkType dflags "metadata normalization proof target" arrowU (coercionRKind evidence)
        check "metadata normalization has nonreflexive evidence at the requested role" (not (isReflCo evidence) && coercionRole evidence == role)
        lintAccepts dflags "metadata normalization evidence" [] (Coercion evidence)
    let f = local 6450 "opaque_arrow_head" (mkVisFunTyManyU boolTy boolTy)
        n = local 6451 "opaque_arrow_n" boolTy
        m = local 6452 "opaque_arrow_m" boolTy
        lhs = TermAppTy f (TermVarTy n)
        rhs = TermAppTy f (TermVarTy m)
        argument = proof 6453 (TermVarTy n) (TermVarTy m)
        applied = proof 6454 lhs rhs
        scope = [f, n, m, argument, applied]
    lintAccepts dflags "native call retains forward nominal congruence" scope (Coercion (mkTermAppCo f (mkCoVarCo argument)))
    forM_ [CLeft, CRight] $ \side ->
        lintRejects dflags "native call remains opaque to LRCo" scope (Coercion (LRCo side (mkCoVarCo applied)))
    let printPartial metadata = showSDoc dflags (ppr (TyConApp fUNTyCon [metadata, ManyTy]))
        printed = map printPartial [matchableDataConTy, unmatchableDataConTy, mkTyVarTy ma]
    check "partial FUN diagnostics retain M, U and variable metadata" (and [lhs /= rhs | (i, lhs) <- zip [0 :: Int ..] printed, (j, rhs) <- zip [0 :: Int ..] printed, i < j])

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
        differentPi = mkDepFunTy other (mkFunctionType unmatchableDataConTy ManyTy (indexed otherIndex) boolTy)
        occursPi = mkDepFunTy b (mkFunctionType unmatchableDataConTy ManyTy (indexed index) wholeTy)
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
        captureTemplate = mkForAllTy unmatchableDataConTy (Bndr outer Specified) wholeTy
        captureTarget =
            mkForAllTy unmatchableDataConTy (Bndr inner Specified) $
                mkDepFunTy b (mkFunctionType unmatchableDataConTy ManyTy (mkTyVarTy inner) (indexed index))
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
            mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [varType var, TermVarTy var, TermVarTy var]) payload
        arrow = mkFunctionType unmatchableDataConTy ManyTy
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
        isRetained vis ty = case unLoc ty of
            HsForEachTy{hst_xforeach = ann, hst_bndrs = [_]} -> hsForEachVisibility ann == vis
            _ -> False
    retained <- parseWith nativeFlags source
    check "retained quantification has a distinct AST node" (isRetained DepFunVisible retained)
    reparsed <- parseWith nativeFlags (showSDoc nativeFlags (ppr retained))
    check "retained quantification printing round trip" (isRetained DepFunVisible reparsed)
    invisible <- parseWith nativeFlags "foreach (b :: Bool). Select b -> Select b"
    check "foreach-dot is retained and invisible" (isRetained DepFunInvisible invisible)
    printedInvisible <- parseWith nativeFlags (showSDoc nativeFlags (ppr invisible))
    check "foreach-dot printing round trip" (isRetained DepFunInvisible printedInvisible)
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

-- | Check visibility at both endpoints of retained function coercions.
runDepFunVisibilityChecks :: DynFlags -> IO ()
runDepFunVisibilityChecks dflags = do
    let b = local 17200 "visibility_binder" boolTy
        arg = local 17201 "visibility_argument" boolTy
    forM_ [intTy, keepResult (TermVarTy b)] $ \body -> do
        let visible = mkDepFunTy b body
            hidden = mkDepFunTyWithVis DepFunInvisible b body
            bridge = mkDepFunCoWithVis DepFunVisible DepFunInvisible b MRefl (mkReflCo Representational body)
            bad = DepFunCo DepFunVisible DepFunInvisible b MRefl (mkNomReflCo body)
            worker = local 17202 "visibility_worker" visible
            application = App (Cast (Var worker) bridge) (Var arg)
            result = Subst.substTy (indexSubst [arg] b (Var arg)) body
        lintAccepts dflags "hidden retained type" [] (identity 17203 hidden)
        lintAccepts dflags "retained visibility cast and application" [worker, arg] application
        lintRejects dflags "retained nominal coercion cannot change visibility" [] (Coercion bad)
        check "retained visibility is part of nominal equality" (not (eqType visible hidden))
        check "retained visibility coercion is not reflexive" (not (isReflCo bridge))
        check "retained visibility coercion cannot become nominal" $
            case setNominalRole_maybe Representational bridge of
                Nothing -> True
                Just _ -> False
        check "a hidden retained binder consumes one runtime argument" (countFunRepArgs 1 hidden == 1)
        checkType dflags "retained visibility cast source" visible (coercionLKind bridge)
        checkType dflags "retained visibility cast target" hidden (coercionRKind bridge)
        let optimise = optCoercion (OptCoercionOpts True Nothing) Subst.emptySubst
            reverseBridge = mkSymCo bridge
        forM_ [bridge, reverseBridge] $ \proof -> do
            let optimised = optimise proof
            lintAccepts dflags "optimised visibility cast" [] (Coercion optimised)
            check "coercion optimisation preserves the visibility cast" (not (isReflCo optimised))
            checkType dflags "optimised visibility source" (coercionLKind proof) (coercionLKind optimised)
            checkType dflags "optimised visibility target" (coercionRKind proof) (coercionRKind optimised)
        checkType dflags "symmetric retained visibility cast source" hidden (coercionLKind reverseBridge)
        checkType dflags "symmetric retained visibility cast target" visible (coercionRKind reverseBridge)
        checkType dflags "hidden retained application result" result (exprType application)
        let hiddenWorker = local 17204 "hidden_visibility_worker" hidden
            expanded = etaExpand 1 (Var hiddenWorker)
        lintAccepts dflags "eta-expanded hidden retained type" [hiddenWorker] expanded
        checkType dflags "eta expansion preserves hidden retained type" hidden (exprType expanded)

-- | Check retained congruence with a family equation and scoped nominal evidence.
runDepFunCoChecks :: DynFlags -> IO ()
runDepFunCoChecks dflags = do
    let b = local 700 "co_b" boolTy
        free = local 701 "co_free" boolTy
        other = local 702 "co_other" boolTy
        equation = mkAxInstCo Nominal (BranchedAxiom (snd selectFamily) 0) [] []
        concrete = selected (constructorIndex trueDataCon)
        cv = proof 703 concrete intTy
        argument = mkFunctionType unmatchableDataConTy ManyTy (indexed (TermVarTy b)) (indexed (TermVarTy free))
        body = mkFunCoNoFTF Nominal (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) (mkNomReflCo argument) (mkCoVarCo cv)
        raw = DepFunCo DepFunVisible DepFunVisible b MRefl body
        lifted = mkDepFunCo b body
        left = mkDepFunTy b (mkFunctionType unmatchableDataConTy ManyTy argument concrete)
        right = mkDepFunTy b (mkFunctionType unmatchableDataConTy ManyTy argument intTy)
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
        phantom = DepFunCo DepFunVisible DepFunVisible b MRefl (mkReflCo Phantom (coercionLKind body))
    check "retained coercion inherits representational role" (coercionRole representational == Representational)
    checkType dflags "representational retained coercion source" left (coercionLKind representational)
    checkType dflags "representational retained coercion target" right (coercionRKind representational)
    lintAccepts dflags "representational retained congruence" [free, cv] (Coercion representational)
    check "retained coercion inherits phantom role" (coercionRole phantom == Phantom)
    lintAccepts dflags "phantom retained reflexivity" [free] (Coercion phantom)
    let renamedBody = Subst.substCo (indexSubst [other, free, cv] b (Var other)) body
        renamed = DepFunCo DepFunVisible DepFunVisible other MRefl renamedBody
    check "retained coercion alpha equality" (eqCoercion raw renamed)
    lintAccepts dflags "alpha-renamed retained coercion" [free, cv] (Coercion renamed)
    check
        "index substitution respects a retained coercion binder"
        (eqCoercion raw (Subst.substCo (indexSubst [free, cv] b (Var (dataConWorkId trueDataCon))) raw))
    let captured = Subst.substCo (indexSubst [b, cv] free (Var b)) raw
    case captured of
        DepFunCo DepFunVisible DepFunVisible fresh MRefl _ -> do
            check "coercion substitution freshens a capturing runtime binder" (fresh /= b)
            let expectedArgument = mkFunctionType unmatchableDataConTy ManyTy (indexed (TermVarTy fresh)) (indexed (TermVarTy b))
            checkType
                dflags
                "coercion substitution keeps free and bound indices distinct"
                (mkDepFunTy fresh (mkFunctionType unmatchableDataConTy ManyTy expectedArgument concrete))
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
                (mkTyConKindWithMatchability unmatchableDataConTy binders liftedTypeKind)
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
    checkType dflags "one-sided retained endpoint preserves syntactic dependence" (DepFunTy DepFunVisible b forgotten) (coercionLKind disappearing)
    checkType dflags "one-sided retained endpoint canonicalizes the ordinary arrow" (mkFunctionType unmatchableDataConTy ManyTy boolTy intTy) (coercionRKind disappearing)
    lintAccepts dflags "a family equation removes one endpoint's retained dependence" [] (Coercion disappearing)
    let proofOnly = mkDepFunCo b (TransCo (SymCo forgetting) forgetting)
        ordinaryEndpoint = mkFunctionType unmatchableDataConTy ManyTy boolTy intTy
    check "proof-only dependence keeps the retained coercion binder" $ case proofOnly of
        DepFunCo{} -> True
        _ -> False
    checkType dflags "proof-only retained coercion left endpoint" ordinaryEndpoint (coercionLKind proofOnly)
    checkType dflags "proof-only retained coercion right endpoint" ordinaryEndpoint (coercionRKind proofOnly)
    check "proof-only retained coercion removes its bound index from free variables" (not (b `elemVarSet` tyCoVarsOfCo proofOnly))
    lintAccepts dflags "retained binder scopes an intermediate proof-only dependency" [] (Coercion proofOnly)
    lintAccepts dflags "Pi construction scopes an intermediate proof-only dependency" [] $
        Coercion (mkPiCo Nominal unmatchableDataConTy b (TransCo (SymCo forgetting) forgetting))
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
        wrongDomain = local 708 "co_unlifted" intPrimTy
        opaqueInt = local 81035 "co_opaque_int" intTy
        linear = setIdMult b OneTy
    lintAccepts dflags "retained Nat coercion binder" [] (Coercion (DepFunCo DepFunVisible DepFunVisible nat MRefl (mkNomReflCo intTy)))
    lintAccepts dflags "opaque Int coercion binder" [] (Coercion (DepFunCo DepFunVisible DepFunVisible opaqueInt MRefl (mkNomReflCo intTy)))
    forM_ [("erased variable", erased), ("coercion variable", cv), ("unlifted domain", wrongDomain), ("linear runtime binder", linear)] $ \(label, binder) ->
        lintRejects dflags ("retained coercion rejects " ++ label) [] (Coercion (DepFunCo DepFunVisible DepFunVisible binder MRefl (mkNomReflCo intTy)))
    lintRejects dflags "retained coercion rejects unlifted results" [] (Coercion (DepFunCo DepFunVisible DepFunVisible b MRefl (mkNomReflCo intPrimTy)))
    forM_ [("left", proof 709 intPrimTy intTy), ("right", proof 710 intTy intPrimTy)] $ \(side, evidence) -> do
        lintAccepts dflags ("ordinary heterogeneous " ++ side ++ " endpoint evidence") [evidence] (Coercion (mkCoVarCo evidence))
        lintRejects dflags ("retained coercion rejects an unlifted " ++ side ++ " result") [evidence] (Coercion (DepFunCo DepFunVisible DepFunVisible b MRefl (mkCoVarCo evidence)))
    lintRejects dflags "retained coercion rejects an out-of-scope runtime dependency" [cv] (Coercion raw)
    lintRejects dflags "valid raw retained coercion cannot escape a let-bound index" [cv] $
        Let (NonRec free (Var (dataConWorkId trueDataCon))) (Coercion raw)

    -- The phantom data parameter permits two supported domains. The family
    -- axiom supplies nominal evidence without making their raw types equal.
    let tokenName n occurrence = mkExternalName (unique n) (mkModule mainUnit (mkModuleName "DomainConversionAPI")) occurrence noSrcSpan
        tokenParameter = mkTyVar (mkSystemName (unique 30010) (mkTyVarOcc "phantom")) liftedTypeKind
        tokenBinders = [mkAnonTyConBinder tokenParameter]
        token =
            mkAlgTyCon
                (tokenName 30000 (mkTcOcc "DomainToken"))
                (mkTyConKind tokenBinders liftedTypeKind)
                tokenBinders
                0
                liftedTypeKind
                [Phantom]
                Nothing
                []
                (mkDataTyConRhs [tokenConstructor])
                (VanillaAlgTyCon (tokenName 30001 (mkVarOcc "domainToken_rep")))
                False
        tokenConstructor =
            mkDataCon
                (tokenName 30002 (mkDataOcc "DomainToken")) False (tokenName 30003 (mkVarOcc "domainToken_con_rep"))
                [] [] [] [] [tokenParameter] [] noConcreteTyVars [Bndr tokenParameter Specified]
                [] [] [] (mkTyConApp token [mkTyVarTy tokenParameter]) NoPromInfo token 1 []
                (mkDataConWorkId (tokenName 30004 (mkDataConWorkerOcc (mkDataOcc "DomainToken"))) tokenConstructor)
                NoDataConRep
        parameter = mkTyVar (mkSystemName (unique 30500) (mkTyVarOcc "a")) liftedTypeKind
        parameters = [mkAnonTyConBinder parameter]
        family =
            mkFamilyTyCon
                (mkSystemName (unique 30501) (mkTcOcc "DomainF"))
                (mkTyConKindWithMatchability unmatchableDataConTy parameters liftedTypeKind)
                parameters
                0
                liftedTypeKind
                Nothing
                (ClosedTypeFamilyTyCon (CTF (Just axiom)))
                Nothing
                NotInjective
        axiom =
            mkBranchedCoAxiom Nominal (mkSystemName (unique 30502) (mkTcOcc "DomainFAx")) family
                [mkCoAxBranch [] [] [] [boolTy] boolTy [] noSrcSpan]
        familyDelta = mkAxInstCo Nominal (BranchedAxiom axiom 0) [] []
        delta = mkTyConAppCo Nominal token [familyDelta]
        domainA = mkTyConApp token [mkTyConApp family [boolTy]]
        domainB = mkTyConApp token [boolTy]
        natTy = mkTyConTy (let (tc, _, _) = peanoNat in tc)
        x = local 31000 "domain_x" domainA
        y = local 31001 "domain_y" domainB
        fn = local 31002 "domain_fn" (mkFunctionType unmatchableDataConTy ManyTy domainA natTy)
        at first second = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [natTy, TermAppTy fn first, TermAppTy fn second]) intTy
        bodyType = at (TermVarTy x) (TermVarTy x)
        back = mkCastTy (TermVarTy y) (mkSymCo delta)
        expectedLeft = mkDepFunTy x bodyType
        expectedRight = mkDepFunTy y (at back back)
        converted = mkDepFunCoWithDomain x (MCo delta) (mkNomReflCo bodyType)
    check "domain conversion has two supported raw-distinct domains" $
        isNativeIndexType domainA && isNativeIndexType domainB && not (eqType domainA domainB)
    forM_ [Nominal, Representational] $ \role -> do
        let co = mkDepFunCoWithDomain x (MCo delta) (mkReflCo role bodyType)
        check "nonreflexive domain survives a reflexive body" (not (isReflCo co))
        check "domain conversion keeps the body role" (coercionRole co == role)
        checkType dflags "domain conversion left endpoint" expectedLeft (coercionLKind co)
        checkType dflags "domain conversion right endpoint" expectedRight (coercionRKind co)
        lintAccepts dflags "nominal domain conversion" [fn] (Coercion co)
    case coercionRKind converted of
        DepFunTy DepFunVisible binder (FunTy _ _ _ (TyConApp _ [_, TermAppTy headId (CastTy (TermVarTy occurrence) co), _]) _) -> do
            check "converted endpoint uses its actual binder and original head" (occurrence == binder && headId == fn)
            checkType dflags "converted occurrence metadata" domainB (varType occurrence)
            check "converted index cast is nominal" (coercionRole co == Nominal)
            checkType dflags "converted index cast starts at the right domain" domainB (coercionLKind co)
            checkType dflags "converted index casts back to the left domain" domainA (coercionRKind co)
        _ -> fail "FAIL: converted endpoint has no backward index cast"
    check "domain conversion masks its binder and keeps its opaque head" $
        not (x `elemVarSet` tyCoVarsOfCo converted) && fn `elemVarSet` tyCoVarsOfCo converted
    let alpha = local 31003 "domain_alpha" domainA
        alphaBody = at (TermVarTy alpha) (TermVarTy alpha)
        alphaCo = mkDepFunCoWithDomain alpha (MCo delta) (mkNomReflCo alphaBody)
        outer = local 31004 "domain_outer" domainA
        captureBody = at (TermVarTy x) (TermVarTy outer)
        captureCo = mkDepFunCoWithDomain x (MCo delta) (mkNomReflCo captureBody)
        capturedCo = Subst.substCo (indexSubst [x, fn] outer (Var x)) captureCo
    check "domain conversion alpha equality" (eqCoercion converted alphaCo)
    case capturedCo of
        DepFunCo DepFunVisible DepFunVisible fresh (MCo _) _ -> do
            check "domain conversion avoids substitution capture" (fresh /= x)
            check "domain conversion keeps the free replacement" (x `elemVarSet` tyCoVarsOfCo capturedCo)
            checkType dflags "captured converted right endpoint" (mkDepFunTy y (at back (TermVarTy x))) (coercionRKind capturedCo)
            lintAccepts dflags "capture-safe domain conversion" [x, fn] (Coercion capturedCo)
        _ -> fail "FAIL: domain conversion lost its binder during substitution"
    let domainProof = proof 31005 domainA domainB
        givenCo = mkDepFunCoWithDomain x (MCo (mkCoVarCo domainProof)) (mkNomReflCo bodyType)
        proofSubst = Subst.Subst (mkInScopeSet (mkVarSet [fn])) emptyVarEnv emptyVarEnv (unitVarEnv domainProof delta)
    check "domain evidence remains free outside the runtime binder" (domainProof `elemVarSet` tyCoVarsOfCo givenCo)
    check "domain evidence substitution visits the domain proof" (eqCoercion converted (Subst.substCo proofSubst givenCo))
    lintAccepts dflags "in-scope domain evidence" [fn, domainProof] (Coercion givenCo)
    lintRejects dflags "domain evidence cannot be bound by the retained binder" [fn] (Coercion givenCo)
    let forgottenParameter = mkTyVar (mkSystemName (unique 31006) (mkTyVarOcc "ignored")) domainA
        forgottenCon =
            mkSynonymTyCon
                (mkSystemName (unique 31007) (mkTcOcc "ForgetDomainIndex"))
                (mkTyConKindWithMatchability unmatchableDataConTy [mkAnonTyConBinder forgottenParameter] liftedTypeKind)
                [mkAnonTyConBinder forgottenParameter]
                liftedTypeKind
                [Phantom]
                intTy
                True
                True
                True
                True
        forgottenType = mkTyConApp forgottenCon [TermVarTy x]
        forgettingCo = TransCo (mkNomReflCo forgottenType) (mkNomReflCo intTy)
        proofOnlyCo = mkDepFunCoWithDomain x (MCo delta) (TransCo (SymCo forgettingCo) forgettingCo)
    checkType dflags "domain conversion proof-only left endpoint" (mkFunctionType unmatchableDataConTy ManyTy domainA intTy) (coercionLKind proofOnlyCo)
    checkType dflags "domain conversion proof-only right endpoint" (mkFunctionType unmatchableDataConTy ManyTy domainB intTy) (coercionRKind proofOnlyCo)
    check "proof-only converted binder stays scoped" (not (x `elemVarSet` tyCoVarsOfCo proofOnlyCo))
    lintAccepts dflags "proof-only dependency in a converted domain" [] (Coercion proofOnlyCo)
    check "reflexive explicit domain uses the fixed-domain representation" $
        case mkDepFunCoWithDomain b (MCo (mkNomReflCo boolTy)) body of
            DepFunCo DepFunVisible DepFunVisible _ MRefl _ -> True
            _ -> False
    let functionDelta = mkFunCoNoFTF Nominal (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) delta (mkNomReflCo natTy)
    forM_
        [ ("representational domain evidence", DepFunCo DepFunVisible DepFunVisible x (MCo (mkSubCo delta)) (mkNomReflCo bodyType), [fn])
        , ("reversed domain evidence", DepFunCo DepFunVisible DepFunVisible x (MCo (mkSymCo delta)) (mkNomReflCo bodyType), [fn])
        , ("wrong left domain", DepFunCo DepFunVisible DepFunVisible b (MCo delta) (mkNomReflCo intTy), [])
        , ("changed function-head domain", DepFunCo DepFunVisible DepFunVisible fn (MCo functionDelta) (mkNomReflCo intTy), [])
        ]
        $ \(label, co, scope) -> lintRejects dflags ("domain conversion rejects " ++ label) scope (Coercion co)

    -- Keep a nonreflexive result proof as well as the domain conversion.
    -- Each worker is an actual scoped assumption with the source Pi type.
    let applicationBody role =
            mkFunCoNoFTF role (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) (mkReflCo role bodyType)
                (if role == Nominal then equation else mkSubCo equation)
        applicationR = mkDepFunCoWithDomain x (MCo delta) (applicationBody Representational)
        applicationN = mkDepFunCoWithDomain x (MCo delta) (applicationBody Nominal)
        fixedHeadCo = mkDepFunCo fn (applicationBody Representational)
        actualHead = local 31008 "domain_actual_head" (varType fn)
        backward = Cast (Var y) (SubCo (mkSymCo delta))
        forward = Cast (Var x) (SubCo delta)
        roundTrip = Cast forward (SubCo (mkSymCo delta))
        firstDelta = mkTyConAppCo Nominal token [mkTyConAppCo Nominal family [familyDelta]]
        firstDomain = mkTyConApp token [mkTyConApp family [mkTyConApp family [boolTy]]]
        firstBinder = local 31210 "domain_chain_first" firstDomain
        middleBinder = local 31211 "domain_chain_middle" domainA
        firstArgument = local 31212 "domain_chain_input" firstDomain
        chainHead = local 31213 "domain_chain_head" (mkFunctionType unmatchableDataConTy ManyTy firstDomain natTy)
        chainAt index = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [natTy, TermAppTy chainHead index, TermAppTy chainHead index]) intTy
        firstBody = mkFunCoNoFTF Representational (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) (mkReflCo Representational (chainAt (TermVarTy firstBinder))) (mkSubCo equation)
        firstCo = mkDepFunCoWithDomain firstBinder (MCo firstDelta) firstBody
        middleIndex = mkCastTy (TermVarTy middleBinder) (mkSymCo firstDelta)
        secondCo = mkDepFunCoWithDomain middleBinder (MCo delta) (mkReflCo Representational (mkFunctionType unmatchableDataConTy ManyTy (chainAt middleIndex) intTy))
        chainCo = TransCo firstCo secondCo
        backwardChain = Cast backward (SubCo (mkSymCo firstDelta))
        forwardChain = Cast (Cast (Var firstArgument) (SubCo firstDelta)) (SubCo delta)
        applications =
            [ ("direct", applicationR, Var y, backward, [fn, y])
            , ("inverse", SymCo applicationR, Var x, forward, [fn, x])
            , ("composition", TransCo applicationR (SymCo applicationR), Var x, roundTrip, [fn, x])
            , ("weakening", SubCo applicationN, Var y, backward, [fn, y])
            , ("fixed function domain", fixedHeadCo, Var actualHead, Var actualHead, [x, actualHead])
            , ("two distinct domain conversions", chainCo, Var y, backwardChain, [chainHead, y])
            , ("inverse two distinct domain conversions", SymCo chainCo, Var firstArgument, forwardChain, [chainHead, firstArgument])
            ]
    check "composition uses three raw-distinct supported domains" $
        isNativeIndexType firstDomain && not (eqType firstDomain domainA) && not (eqType firstDomain domainB)
    forM_ (zip [0 ..] applications) $ \(number, (label, co, arg, expectedArg, freeScope)) -> do
        let worker = local (31100 + number) "domain_application" (coercionLKind co)
            scope = worker : freeScope
            original = App (Cast (Var worker) co) arg
            prefix = "retained application " ++ label
        lintAccepts dflags (prefix ++ " original") scope original
        case pushCoArg co arg of
            Nothing -> fail ("FAIL: " ++ prefix ++ " did not push explicit congruence")
            Just (pushedArg, result) -> do
                check (prefix ++ " preserves argument identity and cast direction") (cheapEqExpr pushedArg expectedArg)
                checkType dflags (prefix ++ " pushed argument type") (exprType expectedArg) (exprType pushedArg)
                let sourceApplication = App (Var worker) pushedArg
                    sourceResult = exprType sourceApplication
                    targetResult = exprType original
                    pushed = case result of
                        MRefl -> sourceApplication
                        MCo resultCo -> Cast sourceApplication resultCo
                case result of
                    MRefl -> checkType dflags (prefix ++ " reflexive result") sourceResult targetResult
                    MCo resultCo -> do
                        check (prefix ++ " result role") (coercionRole resultCo == Representational)
                        checkType dflags (prefix ++ " result proof left endpoint") sourceResult (coercionLKind resultCo)
                        checkType dflags (prefix ++ " result proof right endpoint") targetResult (coercionRKind resultCo)
                        unless (label == "composition") $
                            check (prefix ++ " retains the nonreflexive result proof") (not (isReflCo resultCo))
                checkType dflags (prefix ++ " full result type") targetResult (exprType pushed)
                lintAccepts dflags (prefix ++ " pushed") scope pushed
                case targetResult of
                    FunTy _ _ _ payloadType _ -> do
                        let payload = local (31250 + number) "domain_later_payload" payloadType
                            originalFull = App original (Var payload)
                            pushedFull = App pushed (Var payload)
                        checkType dflags (prefix ++ " later payload result") (exprType originalFull) (exprType pushedFull)
                        lintAccepts dflags (prefix ++ " original later payload") (payload : scope) originalFull
                        lintAccepts dflags (prefix ++ " pushed later payload") (payload : scope) pushedFull
                    _ -> fail ("FAIL: " ++ prefix ++ " lost its later dependent payload")
    let opaque =
            mkCoVar
                (mkSystemName (unique 31009) (mkVarOcc "opaque_domain_co"))
                (mkReprEqPred (coercionLKind applicationR) (coercionRKind applicationR))
        opaqueCo = mkCoVarCo opaque
        opaqueWorker = local 31110 "opaque_domain_application" (coercionLKind opaqueCo)
        opaqueOriginal = App (Cast (Var opaqueWorker) opaqueCo) (Var y)
    lintAccepts dflags "opaque native congruence original application" [fn, y, opaque, opaqueWorker] opaqueOriginal
    check "opaque native congruence has no invented Pi projection" $
        case pushCoArg opaqueCo (Var y) of
            Nothing -> True
            Just _ -> False

    -- The domain proof precedes the binder. Its free x and the body's bound
    -- x can have the same Unique. The right endpoint must keep them distinct.
    let outerParameter = mkTyVar (mkSystemName (unique 31020) (mkTyVarOcc "outer")) domainA
        outerBinders = [mkAnonTyConBinder outerParameter]
        outerAlias =
            mkSynonymTyCon
                (mkSystemName (unique 31021) (mkTcOcc "ForgetOuterDomain"))
                (mkTyConKindWithMatchability unmatchableDataConTy outerBinders liftedTypeKind)
                outerBinders
                liftedTypeKind
                [Phantom]
                domainA
                True
                False
                True
                False
        outerType = mkTyConApp outerAlias [TermVarTy x]
        forgetOuter = TransCo (Refl outerType) (Refl domainA)
        outerLoop = TransCo (SymCo forgetOuter) forgetOuter
        scopedDelta = TransCo outerLoop delta
        scopedConverted = mkDepFunCoWithDomain x (MCo scopedDelta) (mkNomReflCo bodyType)
        scopedRight = coercionRKind scopedConverted
        rightFree = tyCoVarsOfType scopedRight
    check "domain proof retains its outer same-Unique dependency" (x `elemVarSet` tyCoVarsOfCo scopedDelta)
    lintAccepts dflags "domain proof scopes its outer index before the binder" [x, fn] (Coercion scopedConverted)
    case scopedRight of
        DepFunTy DepFunVisible actual _ -> do
            check "converted endpoint freshens past the domain proof" (actual /= x)
            checkType dflags "fresh converted binder metadata" domainB (varType actual)
            check "converted endpoint masks only its fresh binder" (not (actual `elemVarSet` rightFree))
        _ -> fail "FAIL: same-Unique domain proof lost its retained endpoint"
    check "converted endpoint retains the original opaque head" (fn `elemVarSet` rightFree)
    case lookupVarSet rightFree x of
        Just outerIndex -> checkType dflags "converted endpoint keeps outer index metadata" domainA (varType outerIndex)
        Nothing -> fail "FAIL: converted endpoint captured the domain proof's outer index"
    lintAccepts dflags "converted endpoint independently preserves outer proof scope" [x, fn] (Coercion (mkNomReflCo scopedRight))

    let betaActual = local 31220 "domain_beta_actual" domainA
        betaIgnored = local 31221 "domain_beta_ignored" domainA
        betaCast proofCo = Cast (Var x) (SubCo proofCo)
        replaceBeta = indexSubst [betaActual] x (Var betaActual)
        castBetaCases =
            [ ("changed domain", App (Lam x (betaCast scopedDelta)) (Var betaActual), scopedDelta)
            , ("proof-only loop", App (Lam x (betaCast outerLoop)) (Var betaActual), outerLoop)
            , ("stored range avoids inner capture", App (Lam x (App (Lam betaActual (betaCast scopedDelta)) (Var betaIgnored))) (Var betaActual), scopedDelta)
            ]
    forM_ castBetaCases $ \(label, expression, proofCo) -> do
        let expectedProof = Subst.substCo replaceBeta proofCo
            expectedValue = Cast (Var betaActual) (SubCo expectedProof)
            expectedIndex = CastTy (TermVarTy betaActual) expectedProof
            prefix = "cast beta " ++ label
        lintAccepts dflags (prefix ++ " original") [betaActual, betaIgnored] expression
        case normaliseNativeIndexExpr expression of
            Just (normal, index) -> do
                check (prefix ++ " preserves the actual runtime range") (cheapEqExpr expectedValue normal)
                checkType dflags (prefix ++ " substitutes the nominal index proof") expectedIndex index
                checkType dflags (prefix ++ " result domain") (coercionRKind expectedProof) (typeKind index)
                check (prefix ++ " leaves no escaped beta binder") $
                    not (x `elemVarSet` exprFreeVars normal) && not (x `elemVarSet` tyCoVarsOfType index)
                check (prefix ++ " retains the actual input Id") $
                    betaActual `elemVarSet` exprFreeVars normal && betaActual `elemVarSet` tyCoVarsOfType index
                lintAccepts dflags (prefix ++ " normalized value") [betaActual] normal
                lintAccepts dflags (prefix ++ " normalized index") [betaActual] (Coercion (mkNomReflCo index))
            Nothing -> fail ("FAIL: " ++ prefix ++ " was not normalized")

    let unrelatedType = mkTyVar (mkSystemName (unique 31222) (mkTyVarOcc "unrelated")) liftedTypeKind
        unrelatedProof = proof 31223 boolTy boolTy
        emptyExpressionSubst = CoreSubst.mkEmptySubst (mkInScopeSet (mkVarSet [y]))
        expressionSubsts =
            [ ("type", CoreSubst.extendTvSubst emptyExpressionSubst unrelatedType intTy)
            , ("coercion", CoreSubst.extendCvSubst emptyExpressionSubst unrelatedProof (mkNomReflCo boolTy))
            ]
    lintAccepts dflags "converted argument before expression substitution" [y] backward
    forM_ expressionSubsts $ \(label, subst) -> do
        let rewritten = CoreSubst.substExpr subst backward
            prefix = "converted argument under unrelated " ++ label ++ " substitution"
        check (prefix ++ " uses a nonempty substitution") (not (CoreSubst.isEmptySubst subst))
        case rewritten of
            Cast (Var actual) (SubCo nominalProof) -> do
                check (prefix ++ " preserves its actual Id") (actual == y)
                check (prefix ++ " keeps nominal evidence") (coercionRole nominalProof == Nominal)
                checkType dflags (prefix ++ " cast source") domainB (coercionLKind nominalProof)
                checkType dflags (prefix ++ " cast target") domainA (coercionRKind nominalProof)
            _ -> fail ("FAIL: " ++ prefix ++ " discarded the nominal child")
        checkType dflags (prefix ++ " expression type") (exprType backward) (exprType rewritten)
        case exprToNativeIndex rewritten of
            Just index -> checkType dflags (prefix ++ " recoverable index") (CastTy (TermVarTy y) (mkSymCo delta)) index
            Nothing -> fail ("FAIL: " ++ prefix ++ " lost index conversion")
        lintAccepts dflags prefix [y] rewritten

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

-- | Check dead local removal and dependencies in native type annotations.
runOccurrenceChecks :: DynFlags -> IO ()
runOccurrenceChecks dflags = do
    let (nat, _, _) = peanoNat
        natTy = mkTyConTy nat
        dead = local 6000 "ordinary_dead" intTy
        intSeed = local 6001 "ordinary_seed" intTy
        intProducer = local 6002 "ordinary_producer" (mkVisFunTyManyU intTy intTy)
        index = local 6003 "type_only_index" natTy
        seed = local 6004 "index_seed" natTy
        producer = local 6005 "index_producer" (mkVisFunTyManyU natTy natTy)
        parameter = mkTyVar (mkSystemName (unique 6006) (mkTyVarOcc "index")) natTy
        consume = local 6007 "consume_index_type" (mkForAllTy unmatchableDataConTy (Bndr parameter Specified) intTy)
        expression =
            Let (NonRec dead (App (Var intProducer) (Var intSeed))) $
                Let (NonRec index (App (Var producer) (Var seed))) $
                    App (Var consume) (Type (TermVarTy index))
    result <- simplifyAccepts dflags "native dead local and type-only index" [intSeed, intProducer, seed, producer, consume] expression
    check "native occurrence analysis removes an ordinary dead let" $ case result of
        Let (NonRec retained _) _ -> retained == index
        _ -> False
    check "native occurrence analysis retains the type-only index" $ case result of
        Let (NonRec retained (App (Var rhsProducer) (Var rhsSeed))) (App (Var worker) (Type (TermVarTy actual))) ->
            retained == index && actual == index && rhsProducer == producer && rhsSeed == seed && worker == consume
        _ -> False

    let headId = local 6010 "type_only_head" (mkVisFunTyManyU natTy natTy)
        makeHead = local 6011 "opaque_head_producer" (mkVisFunTyManyU intTy (idType headId))
        call = TermAppTy headId (TermVarTy index)
        headExpression =
            Let (NonRec headId (App (Var makeHead) (Var intSeed)))
                $ Let (NonRec index (App (Var producer) (Var seed)))
                $ App (Var consume) (Type call)
    headResult <- simplifyAccepts dflags "native head and input used only in a type argument" [makeHead, intSeed, producer, seed, consume] headExpression
    check "type-only call keeps both actual opaque bindings" $ case headResult of
        Let (NonRec retainedHead _) (Let (NonRec retainedIndex _) (App (Var worker) (Type (TermAppTy actualHead (TermVarTy actualIndex))))) ->
            retainedHead == headId && actualHead == headId && retainedIndex == index && actualIndex == index && worker == consume
        _ -> False

    let payloadType = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [natTy, call, call]) intTy
        payloadBinder = local 6012 "later_payload" payloadType
        answer = mkIntExprInt (targetPlatform dflags) 42
        telescope = Lam headId (Lam index (Lam payloadBinder answer))
    telescopeResult <- simplifyAccepts dflags "native dependencies in later lambda domains" [] telescope
    check "later lambda domain keeps its preceding head and index" $ case telescopeResult of
        Lam retainedHead (Lam retainedIndex (Lam retainedPayload _)) ->
            retainedHead == headId
                && retainedIndex == index
                && retainedPayload == payloadBinder
                && not (isDeadBinder retainedHead || isDeadBinder retainedIndex)
                && headId `elemVarSet` tyCoVarsOfType (idType retainedPayload)
                && index `elemVarSet` tyCoVarsOfType (idType retainedPayload)
        _ -> False

    let boolIndex = local 6013 "result_only_index" boolTy
        scrutinee = local 6014 "empty_case_scrutinee" boolTy
        caseBinder = local 6015 "empty_case_binder" boolTy
        resultType = indexed (TermVarTy boolIndex)
        resultOnly = Lam boolIndex (Case (Var scrutinee) caseBinder resultType [])
    caseResult <- simplifyAccepts dflags "native index used only in a case result annotation" [scrutinee] resultOnly
    check "case result retains its enclosing index identity" $ case caseResult of
        Lam retained (Case (Var actualScrutinee) actualBinder actualType []) ->
            retained == boolIndex
                && not (isDeadBinder retained)
                && actualScrutinee == scrutinee
                && actualBinder == caseBinder
                && eqType actualType resultType
        _ -> False

    let ordinaryRhs = App (Var intProducer) (Var intSeed)
        stable = setIdUnfolding dead ((unfolding ordinaryRhs){uf_src = StableUserSrc})
        inactive = setInlinePragma dead neverInlinePragma
        loopBreaker = setIdOccInfo dead (IAmALoopBreaker False NoTailCallInfo)
        linear = setIdMult dead OneTy
        unlifted = local 6016 "unused_unlifted" intPrimTy
        unliftedSeed = local 6017 "unlifted_seed" intPrimTy
        token = local 6018 "unused_token" realWorldStatePrimTy
        tokenSeed = local 6019 "token_seed" realWorldStatePrimTy
        body = App (Var consume) (Type (TermVarTy index))
        protectedCases =
            [ ("stable unfolding", stable, ordinaryRhs, [intProducer, intSeed])
            , ("inactive binding", inactive, ordinaryRhs, [intProducer, intSeed])
            , ("loop breaker", loopBreaker, ordinaryRhs, [intProducer, intSeed])
            , ("One multiplicity", linear, ordinaryRhs, [intProducer, intSeed])
            , ("unlifted Many", unlifted, Var unliftedSeed, [unliftedSeed])
            , ("state token", token, Var tokenSeed, [tokenSeed])
            ]
    forM_ protectedCases $ \(label, binder, rhs, scope) -> do
        protected <- simplifyAccepts dflags ("native dead binding exclusion: " ++ label) (index : consume : scope) (Let (NonRec binder rhs) body)
        check ("native dead binding exclusion keeps its binding: " ++ label) $ case protected of
            Let (NonRec retained _) _ -> retained == binder && eqType (idMult retained) (idMult binder)
            _ -> False

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

-- | Check native variable substitution before any backend preparation.
runAliasChecks :: DynFlags -> IO ()
runAliasChecks dflags = do
    let (nat, zero, successor) = peanoNat
        natTy = mkTyConTy nat
        actual = local 4200 "alias_actual" natTy
        alias = local 4201 "alias" natTy
        inner = local 4202 "inner_alias" natTy
        parameter = mkTyVar (mkSystemName (unique 4203) (mkTyVarOcc "index")) natTy
        consume = local 4204 "consume_index" (mkForAllTy unmatchableDataConTy (Bndr parameter Specified) (mkVisFunTyManyU natTy (mkVisFunTyManyU natTy intTy)))
        consumeType = local 4205 "consume_index_type" (mkForAllTy unmatchableDataConTy (Bndr parameter Specified) intTy)
        seed = local 4206 "opaque_seed" natTy
        producer = local 4207 "opaque_producer" (mkVisFunTyManyU natTy natTy)
        zeroExpr = Var (dataConWorkId zero)
        successorExpr = App (Var (dataConWorkId successor)) (Var actual)
        repeated index left right = mkApps (Var consume) [Type index, Var left, Var right]
        typeOnly index = App (Var consumeType) (Type index)
        binding binder rhs = Let (NonRec binder rhs) (Var binder)
        absent label eliminated expression =
            check label (not (eliminated `elemVarSet` exprFreeVars expression))

    -- Keep this first. The M40 compiler retains this valid literal alias.
    literal <- simplifyAccepts dflags "native literal variable alias" [actual] (binding alias (Var actual))
    check "native literal variable alias disappears" $ case literal of
        Var result -> result == actual
        _ -> False

    let chained =
            Let (NonRec alias (Var actual))
                $ Let (NonRec inner (Var alias))
                $ repeated (TermVarTy inner) inner inner
    repeatedResult <- simplifyAccepts dflags "native alias updates runtime and erased arguments" [actual, consume] chained
    check "native aliases preserve the repeated actual Id" $ case repeatedResult of
        App (App (App (Var worker) (Type (TermVarTy index))) (Var first)) (Var second) ->
            worker == consume && index == actual && first == actual && second == actual
        _ -> False
    absent "first alias is absent from deep free variables" alias repeatedResult
    absent "second alias is absent from deep free variables" inner repeatedResult
    check "actual index remains in deep free variables" (actual `elemVarSet` exprFreeVars repeatedResult)

    let opaque =
            Let (NonRec actual (App (Var producer) (Var seed)))
                $ Let (NonRec alias (Var actual))
                $ typeOnly (TermVarTy alias)
    opaqueResult <- simplifyAccepts dflags "type-only alias retains its opaque outer binding" [producer, seed, consumeType] opaque
    case opaqueResult of
        Let (NonRec retained (App (Var rhsProducer) (Var rhsSeed))) body@(App (Var worker) (Type (TermVarTy index))) -> do
            check "opaque producer and seed remain shared" (retained == index && rhsProducer == producer && rhsSeed == seed && worker == consumeType)
            check "erased argument keeps the outer index live" (retained `elemVarSet` exprFreeVars body)
            absent "type-only alias has no stale deep dependency" alias body
            absent "outer opaque index remains bound" retained opaqueResult
        _ -> fail "FAIL: type-only alias lost its outer opaque binding"

    let capture =
            Let (NonRec alias (Var actual)) $
                Lam actual (repeated (TermVarTy alias) alias actual)
    captured <- simplifyAccepts dflags "native alias avoids reused-Unique lambda capture" [actual, consume] capture
    check "descendant lambda cannot capture the stored alias range" $ case captured of
        Lam fresh (App (App (App (Var worker) (Type (TermVarTy index))) (Var outer)) (Var bound)) ->
            fresh /= actual && worker == consume && index == actual && outer == actual && bound == fresh
        _ -> False

    let at index = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [natTy, index, index]) intTy
        typeParameter = mkTyVar (mkSystemName (unique 4208) (mkTyVarOcc "result")) liftedTypeKind
        consumePi = local 4209 "consume_pi" (mkForAllTy unmatchableDataConTy (Bndr typeParameter Specified) intTy)
        dependentCapture =
            Let (NonRec alias (Var actual))
                $ App (Var consumePi)
                $ Type (mkDepFunTy actual (mkVisFunTyManyU (at (TermVarTy alias)) (at (TermVarTy actual))))
    capturedType <- simplifyAccepts dflags "native alias avoids reused-Unique type capture" [actual, consumePi] dependentCapture
    case capturedType of
        App (Var worker) (Type (DepFunTy DepFunVisible fresh (FunTy _ _ _ argument result))) -> do
            check "retained type binder is fresh" (worker == consumePi && fresh /= actual)
            checkType dflags "type argument retains the outer actual index" (at (TermVarTy actual)) argument
            checkType dflags "type result uses its fresh retained binder" (at (TermVarTy fresh)) result
            absent "type capture leaves no stale alias" alias capturedType
            check "outer index survives beneath retained type" (actual `elemVarSet` exprFreeVars capturedType)
        _ -> fail "FAIL: native alias did not preserve the retained type telescope"

    let opaqueTarget = setIdUnfolding actual (unfolding (error "forced an alias target unfolding"))
    opaqueTargetResult <- simplifyAccepts dflags "alias target unfolding remains opaque" [opaqueTarget] (binding alias (Var opaqueTarget))
    check "alias uses the actual Id without its unfolding" $ case opaqueTargetResult of
        Var result -> result == opaqueTarget
        _ -> False

    let substitutedConstructor = App (Lam actual (binding alias (Var actual))) zeroExpr
    constructorResult <- simplifyAccepts dflags "constructor substitution retains the alias fallback" [] substitutedConstructor
    check "current constructor substitution is not a variable alias" $ case constructorResult of
        Let (NonRec retained (Var constructor)) (Var result) -> retained == result && constructor == dataConWorkId zero
        _ -> False

    let stableAlias = setIdUnfolding alias ((unfolding (Var actual)){uf_src = StableUserSrc})
        inactiveAlias = setInlinePragma alias neverInlinePragma
        loopAlias = setIdOccInfo alias (IAmALoopBreaker False NoTailCallInfo)
        global = mkVanillaGlobal (mkExternalName (unique 4210) (nameModule eqTyConName) (mkVarOcc "global_alias_target") noSrcSpan) natTy
        fallbackCases =
            [ ("opaque producer RHS", [actual, producer], alias, App (Var producer) (Var actual))
            , ("constructor RHS", [actual], alias, successorExpr)
            , ("cast RHS", [actual], alias, Cast (Var actual) (mkReflCo Representational natTy))
            , ("beta RHS", [actual], alias, App (Lam inner (Var inner)) (Var actual))
            , ("let RHS", [actual], alias, binding inner (Var actual))
            , ("stable alias metadata", [actual], stableAlias, Var actual)
            , ("inactive alias metadata", [actual], inactiveAlias, Var actual)
            , ("loop-breaker alias metadata", [actual], loopAlias, Var actual)
            , ("global target", [], alias, Var global)
            ]
    forM_ fallbackCases $ \(label, scope, binder, rhs) -> do
        result <- simplifyAccepts dflags ("native alias fallback: " ++ label) scope (binding binder rhs)
        check ("native alias fallback retains its binding: " ++ label) $ case result of
            Let (NonRec retained _) (Var occurrence) -> retained == occurrence
            _ -> False
    recursive <- simplifyAccepts dflags "native alias preserves recursive group" [actual] (Let (Rec [(alias, Var actual)]) (Var alias))
    check "recursive alias remains in its group" $ case recursive of
        Let (Rec [(retained, Var target)]) (Var occurrence) -> retained == occurrence && target == actual
        _ -> False

    let linearAlias = setIdMult alias OneTy
        wrongDomain = setIdType alias boolTy
        stale = local 4211 "stale_alias_index" natTy
        exported = setIdExported alias
    lintRejects dflags "linear alias index is invalid before simplification" [actual, consumeType] $
        Let (NonRec linearAlias (Var actual)) (typeOnly (TermVarTy linearAlias))
    lintRejects dflags "wrong alias domain is invalid before simplification" [actual] (binding wrongDomain (Var actual))
    lintRejects dflags "stale alias index is invalid before simplification" [actual, consumeType] $
        Let (NonRec alias (Var actual)) (typeOnly (TermVarTy stale))
    lintRejects dflags "exported local alias is invalid before simplification" [actual] (binding exported (Var actual))

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

    let sharedBody = mkFunctionType unmatchableDataConTy ManyTy (indexed (TermVarTy b)) (indexed (TermVarTy c))
        bindsB = DepFunTy DepFunVisible b sharedBody
        bindsC = DepFunTy DepFunVisible c sharedBody
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
        DepFunTy DepFunVisible renamed body -> do
            check "substitution renames a capturing binder" (renamed /= c)
            checkType
                dflags
                "substitution preserves free and bound index identities"
                (mkFunctionType unmatchableDataConTy ManyTy (indexed (TermVarTy c)) (indexed (TermVarTy renamed)))
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
        (mkFunctionType unmatchableDataConTy ManyTy boolTy intTy)
        (mkDepFunTy b intTy)
    lintRejects dflags "raw vacuous retained type is rejected" [] (identity 10 (DepFunTy DepFunVisible b intTy))
    lintRejects dflags "wrong retained argument type is rejected" [n] (App worker (Var n))
    lintRejects dflags "non-Boolean index is rejected" [n] (identity 11 (indexed (TermVarTy n)))
    lintRejects dflags "out-of-scope index is rejected" [] lambdaX
    lintRejects dflags "non-Boolean retained binder is rejected" [] (identity 12 (DepFunTy DepFunVisible n (indexed (TermVarTy n))))
    lintRejects dflags "non-Type dependent codomain is rejected" [] (identity 13 (DepFunTy DepFunVisible b (TermVarTy b)))
    lintRejects dflags "Constraint dependent codomain is rejected" [] (identity 307 (DepFunTy DepFunVisible b (mkTyConApp eqTyCon [boolTy, TermVarTy b, TermVarTy b])))
    lintRejects dflags "primitive equality dependent codomain is rejected" [] (identity 308 (DepFunTy DepFunVisible b (mkNomEqPred (TermVarTy b) (TermVarTy b))))
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
        (coercionLKind (mkPiCo Nominal unmatchableDataConTy b (mkNomReflCo (indexed (TermVarTy b)))))

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
                (mkTyConKindWithMatchability unmatchableDataConTy unusedBinders liftedTypeKind)
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
                (mkTyConKindWithMatchability unmatchableDataConTy identityBinders liftedTypeKind)
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
    check "scanner expands synonyms inside ordinary types" (hasNativeDependentExpr (Type (mkVisFunTyManyU intTy hiddenType)))
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
    funCo = mkFunCoNoFTF Representational (mkNomReflCo unmatchableDataConTy) (mkNomReflCo ManyTy) (mkSubCo predCo) (mkReflCo Representational intTy)

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
            (mkTyConKindWithMatchability unmatchableDataConTy binders liftedTypeKind)
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
    let producer = local 82 "producer" (mkFunctionType unmatchableDataConTy ManyTy boolTy boolTy)
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
        (mkDepFunTy keepBool (mkFunctionType unmatchableDataConTy ManyTy (selected keepIndex) (selected keepIndex)))
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
