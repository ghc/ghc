{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import qualified Data.List.NonEmpty as NE
import GHC (InteractiveImport (IIDecl), getSession, getSessionDynFlags, lookupName, parseImportDecl, parseName, runGhc, setContext, setSessionDynFlags)
import GHC.Builtin.WiredIn.Types (boolTy, constraintKind, liftedTypeKind, mkBoxedTupleTy, stringTy, unmatchableDataConTy)
import GHC.Core (Bind (NonRec), Expr (App, Lam, Let, Type, Var), mkApps)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Lint
    ( LintConfig, checkScopedSelectorApp, eqScopedSelectorApp, lintExpr
    , lintScopedDictionaryExpr, lintScopedDictionaryType, scopedSelectorResultKind, substScopedSelectorApp
    )
import GHC.Core.Map.Type (emptyTypeMap, extendTypeMap, lookupTypeMap)
import qualified GHC.Core.Subst as Subst
import GHC.Core.TyCo.Compare (eqType, eqTypeX, eqVarBndrs, nonDetCmpType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (Type (ClassAppTy, DepDictTy, DepFunTy, TermVarTy))
import GHC.Core.TyCo.Tidy (tidyTopType)
import GHC.Core.TyCon (FamTyConFlav (OpenTypeFamilyTyCon), Injectivity (NotInjective), mkAnonTyConBinder, mkFamilyTyCon, mkNamedTyConBinder, mkTyConKindWithMatchability, tyConDataCons)
import GHC.Core.Type (pattern ManyTy, mkForAllTys, mkFunctionType, mkTyConApp, mkTyVarTy, typeKind)
import GHC.Core.Utils (applyTypeToArgs, exprType, mkLamType)
import GHC.CoreToStg.Prep (corePrepExpr, corePrepPgm)
import GHC.CoreToIface (toIfaceExpr, toIfaceType)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Iface.Env (extendIfaceIdEnv)
import GHC.Iface.Syntax (IfaceExpr(..))
import GHC.IfaceToCore (tcIfaceExpr)
import GHC.Iface.Type (IfaceType(..), appArgsIfaceTypes, ifLclNameFS, mkIfLclName)
import GHC.Driver.Config.CoreToStg.Prep (initCorePrepConfigFor, initCorePrepPgmConfig)
import GHC.Types.Id (idName, mkSysLocal, setIdType)
import GHC.Types.Name (mkSystemName, nameModule)
import GHC.Types.Name.Occurrence (mkTcOcc, mkTyVarOcc)
import GHC.Types.TyThing (TyThing (ATyCon, AnId))
import GHC.Types.Unique (mkUniqueGrimilyWithTag)
import GHC.Types.Var (Id, DepFunTyFlag (DepFunVisible), ForAllTyFlag (Invisible), Specificity (SpecifiedSpec), VarBndr (Bndr), mkTyVar, varType)
import GHC.Types.Var.Env (emptyInScopeSet, mkInScopeSetList, mkRnEnv2)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.Tc.Utils.Monad (initIfaceLcl, initIfaceLoad)
import GHC.Unit.Types (IsBootInterface(NotBoot), mainUnit, mkModule, mkModuleName)
import GHC.Data.IOEnv (IOEnvFailure)
import GHC.Utils.Logger (initLogger)
import GHC.Utils.Outputable (ppr, showSDocUnsafe, text)
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

-- | Test prototypes in memory only.
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
        flags <- getSessionDynFlags
        _ <- setSessionDynFlags flags
        prelude <- parseImportDecl "import Prelude"
        proxyImport <- parseImportDecl "import Data.Proxy"
        setContext [IIDecl prelude, IIDecl proxyImport]
        proxyNames <- parseName "Proxy"
        proxies <- traverse lookupName proxyNames
        [proxy] <- pure [tc | Just (ATyCon tc) <- NE.toList proxies]
        eqName <- NE.head <$> parseName "Eq"
        Just (ATyCon eq) <- lookupName eqName
        enumName <- NE.head <$> parseName "Enum"
        Just (ATyCon enum) <- lookupName enumName
        succName <- NE.head <$> parseName "succ"
        Just (AnId suc) <- lookupName succName
        predName <- NE.head <$> parseName "pred"
        Just (AnId pre) <- lookupName predName
        showClassName <- NE.head <$> parseName "Show"
        Just (ATyCon showClass) <- lookupName showClassName
        showName <- NE.head <$> parseName "show"
        Just (AnId showMethod) <- lookupName showName
        let tv key label = mkTyVar (mkSystemName (unique key) (mkTyVarOcc label))
            unique = mkUniqueGrimilyWithTag 'z'
            a = tv 1 "a" liftedTypeKind
            b = tv 2 "b" liftedTypeKind
            predicate = mkTyConApp eq . pure . mkTyVarTy
            dictionary key label ty = mkSysLocal (fsLit label) (unique key) ManyTy (predicate ty)
            da = dictionary 3 "da" a
            db = dictionary 4 "db" b
            da2 = dictionary 8 "da2" a
            dk = tv 5 "predicate" constraintKind
            value = tv 6 "evidence" (mkTyVarTy dk)
            familyBinders = [mkNamedTyConBinder (Invisible SpecifiedSpec) dk, mkAnonTyConBinder value]
            result = mkFamilyTyCon (mkSystemName (unique 7) (mkTcOcc "ObserveEvidence"))
                (mkTyConKindWithMatchability unmatchableDataConTy familyBinders liftedTypeKind)
                familyBinders 2 liftedTypeKind Nothing OpenTypeFamilyTyCon Nothing NotInjective
            observe d = mkTyConApp result [varType d, TermVarTy d]
            bind = DepDictTy
            close = mkForAllTys
                [(unmatchableDataConTy, Bndr a (Invisible SpecifiedSpec)),
                 (unmatchableDataConTy, Bndr b (Invisible SpecifiedSpec))]
            scoped = close (bind db (bind da (observe da)))
            other = close (bind db (bind da (observe db)))
            sameClassFirst = close (bind da (bind da2 (observe da)))
            sameClassSecond = close (bind da (bind da2 (observe da2)))
            both = close (bind db (bind da (mkBoxedTupleTy [observe da, observe db])))
            escaped = close (bind db (observe da))
            mislabeled = close (bind db (bind da (observe (setIdType da (predicate b)))))
            cfg = initLintConfig flags []
        env <- getSession
        liftIO $ do
            pass cfg "two dictionary binders" both
            pass cfg "first dictionary reference" scoped
            pass cfg "second dictionary reference" other
            pass cfg "first evidence at the same predicate" sameClassFirst
            pass cfg "second evidence at the same predicate" sameClassSecond
            when (eqType sameClassFirst sameClassSecond) (fail "distinct evidence at one predicate compares equal")
            unless (isEmptyVarSet (tyCoVarsOfType both)) (fail "dictionary binder escaped from free variables")
            when (eqType scoped other) (fail "two dictionary references compare equal")
            let tidied = tidyTopType both
            pass cfg "tidied evidence type" tidied
            unless (eqType both tidied) (fail "tidy changed evidence scope")
            reject cfg "escaped reference" escaped
            reject cfg "forged predicate" mislabeled
            let carrier = mkSysLocal (fsLit "carrier") (unique 9) ManyTy both
            case lintExpr cfg (Lam carrier (Var carrier)) of
                Just _ -> putStrLn "normal Lint rejected prototype nodes"
                Nothing -> fail "normal Lint accepted prototype nodes"

            let valueId key label = mkSysLocal (fsLit label) (unique key) ManyTy
                enumDict key label ty = valueId key label (mkTyConApp enum [ty])
                ea = enumDict 10 "ea" (mkTyVarTy a)
                ea2 = enumDict 11 "ea2" (mkTyVarTy a)
                eb = enumDict 12 "eb" (mkTyVarTy b)
                x = valueId 13 "x" (mkTyVarTy a)
                x2 = valueId 14 "x2" (mkTyVarTy a)
                y = valueId 15 "y" (mkTyVarTy b)
                showA = valueId 19 "showA" (mkTyConApp showClass [mkTyVarTy a])
                cfgA = initLintConfig flags [a, ea, ea2, da, x, x2, showA]
                cfgB = initLintConfig flags [b, eb, y]
                cfgAB = initLintConfig flags [a, b, ea, eb, x, y]
                rn = mkRnEnv2 emptyInScopeSet
            Just left <- pure (checkScopedSelectorApp cfgA suc (mkTyVarTy a) ea x)
            Just right <- pure (checkScopedSelectorApp cfgB suc (mkTyVarTy b) eb y)
            Just otherGiven <- pure (checkScopedSelectorApp cfgA suc (mkTyVarTy a) ea2 x)
            Just otherValue <- pure (checkScopedSelectorApp cfgA suc (mkTyVarTy a) ea x2)
            Just otherMethod <- pure (checkScopedSelectorApp cfgA pre (mkTyVarTy a) ea x)
            Just printed <- pure (checkScopedSelectorApp cfgA showMethod (mkTyVarTy a) showA x)
            Just renamed <- pure (eqVarBndrs rn [a, ea, x] [b, eb, y])
            unless (eqType (scopedSelectorResultKind left) (mkTyVarTy a)
                    && eqType (scopedSelectorResultKind printed) stringTy) $
                fail "selector application changed its result kind"
            unless (eqScopedSelectorApp renamed left right) (fail "alpha-renamed selector differs")
            when (any (eqScopedSelectorApp rn left) [right, otherGiven, otherValue, otherMethod]) $
                fail "selector equality collapsed a different component"
            putStrLn "selector equality preserves the chosen binder, operand and method"
            unless (and
                [ isNothing (checkScopedSelectorApp cfgAB suc (mkTyVarTy a) eb x)
                , isNothing (checkScopedSelectorApp cfgA suc (mkTyVarTy a) da x)
                , isNothing (checkScopedSelectorApp cfgAB suc (mkTyVarTy a) ea y)
                , isNothing (checkScopedSelectorApp cfgB suc (mkTyVarTy a) ea x)
                ]) (fail "selector accepted a wrong dictionary, operand or scope")
            putStrLn "selector rejected wrong dictionaries, wrong operands and out-of-scope evidence"

            let subst = Subst.extendSubstList (Subst.mkEmptySubst (mkInScopeSetList [b, eb, y]))
                    [(a, Type (mkTyVarTy b)), (ea, Var eb), (x, Var y)]
            Just substituted <- pure (substScopedSelectorApp cfgB subst left)
            unless (eqScopedSelectorApp rn substituted right) $
                fail "substitution changed the selected dictionary or operand"
            let eBool = enumDict 16 "eBool" boolTy
                bValue = valueId 17 "bValue" boolTy
                cfgBool = initLintConfig flags [eBool, bValue]
                substBool = Subst.extendSubstList (Subst.mkEmptySubst (mkInScopeSetList [eBool, bValue]))
                    [(a, Type boolTy), (ea, Var eBool), (x, Var bValue)]
            Just instantiated <- pure (substScopedSelectorApp cfgBool substBool left)
            unless (eqType (scopedSelectorResultKind instantiated) boolTy) $
                fail "substitution did not update the result kind"
            let alias = enumDict 18 "alias" (mkTyVarTy b)
                computed = Subst.extendIdSubst subst ea (Let (NonRec alias (Var eb)) (Var alias))
            let scopedSubst = Subst.extendSubstInScopeList subst [a, ea, x]
                wrongDictionary = Subst.extendIdSubst scopedSubst ea (Var ea)
                wrongOperand = Subst.extendIdSubst scopedSubst x (Var x)
            unless (and
                [ isNothing (substScopedSelectorApp cfgB computed left)
                , isNothing (substScopedSelectorApp cfgAB wrongDictionary left)
                , isNothing (substScopedSelectorApp cfgAB wrongOperand left)
                ]) (fail "substitution accepted computed evidence or changed an argument type")
            putStrLn "selector substitution preserves variable evidence and rechecks its kind"

            let selectorAt selector t evidence value resultKind =
                    ClassAppTy selector t (TermVarTy evidence) (TermVarTy value) resultKind
                selectA = selectorAt suc (mkTyVarTy a) ea x (mkTyVarTy a)
                selectB = selectorAt suc (mkTyVarTy b) eb y (mkTyVarTy b)
                showIndex = selectorAt showMethod (mkTyVarTy a) showA x stringTy
                resultUnder index = mkTyConApp proxy [typeKind index, index]
                qualified evidence value index =
                    DepDictTy evidence (DepFunTy DepFunVisible value (resultUnder index))
                sigma = close (qualified ea x selectA)
                alpha = mkForAllTys
                    [(unmatchableDataConTy, Bndr b (Invisible SpecifiedSpec)),
                     (unmatchableDataConTy, Bndr a (Invisible SpecifiedSpec))]
                    (qualified eb y selectB)
                stringResult = close (qualified showA x showIndex)
                differentEvidence = close (DepDictTy ea (qualified ea2 x selectA))
                otherEvidence = close (DepDictTy ea (qualified ea2 x
                    (selectorAt suc (mkTyVarTy a) ea2 x (mkTyVarTy a))))
                wrongResult = selectorAt suc (mkTyVarTy a) ea x boolTy
                wrongDict = selectorAt suc (mkTyVarTy a) eb x (mkTyVarTy a)
                wrongValue = selectorAt suc (mkTyVarTy a) ea y (mkTyVarTy a)
            pass cfg "typed dependent selector signature" sigma
            pass cfg "typed selector signature with fixed result kind" stringResult
            pass cfg "first of two exact Givens" differentEvidence
            pass cfg "second of two exact Givens" otherEvidence
            unless (isEmptyVarSet (tyCoVarsOfType sigma)) (fail "typed signature leaked a binder")
            unless (eqType (typeKind sigma) liftedTypeKind) (fail "dependent signature has wrong kind")
            unless (eqType sigma (tidyTopType sigma)) (fail "tidying changed the typed signature")
            unless (eqType
                (Subst.substTy
                    (Subst.extendTvSubst (Subst.mkEmptySubst (mkInScopeSetList [b, ea, x]))
                        a (mkTyVarTy b))
                    (qualified ea x selectA))
                (qualified eb y selectB)) (fail "type substitution captured a source binder")
            unless (eqType sigma alpha && nonDetCmpType sigma alpha == EQ
                    && eqTypeX renamed selectA selectB) (fail "typed selector alpha-renaming failed")
            when (eqType differentEvidence otherEvidence) (fail "typed signature erased Given identity")
            let oneKey = extendTypeMap emptyTypeMap sigma (1 :: Int)
                manyKeys = extendTypeMap (extendTypeMap oneKey differentEvidence 2) otherEvidence 3
            unless (lookupTypeMap oneKey alpha == Just 1
                    && lookupTypeMap manyKeys alpha == Just 1
                    && lookupTypeMap manyKeys differentEvidence == Just 2
                    && lookupTypeMap manyKeys otherEvidence == Just 3) $
                fail "type-map keys lost alpha-equivalence or dictionary identity"
            putStrLn "type-map keys preserve renamed and distinct dictionaries"
            reject cfgAB "typed selector rejects wrong dictionary" (resultUnder wrongDict)
            reject cfgAB "typed selector rejects wrong operand" (resultUnder wrongValue)
            reject cfgAB "typed selector rejects forged result kind" (resultUnder wrongResult)
            reject (initLintConfig flags [a, x]) "typed selector rejects an escaped dictionary"
                (resultUnder selectA)
            let typedCarrier = valueId 20 "typedCarrier" sigma
            when (isNothing (lintExpr cfg (Lam typedCarrier (Var typedCarrier)))) $
                fail "normal Lint accepted the typed selector signature"
            rejectsPhase "typed substitution of computed evidence"
                "unsupported experimental dictionary substitution"
                (case Subst.substTy computed selectA of
                    ClassAppTy _ _ dictionary _ _ -> dictionary
                    _ -> error "substitution lost the selector application")
            let free = tyCoVarsOfType selectA
            unless (all (`elemVarSet` free) [a, ea, x]) (fail "selector missed a free variable")
            case toIfaceType sigma of
                IfaceForAllTy _ _ (IfaceForAllTy _ _ (IfaceDepDictTy (_, dict, _)
                    (IfaceDepFunTy _ (_, arg, _) (IfaceTyConApp _ args)))) ->
                    case [ (method, dict_ref, arg_ref)
                         | IfaceClassAppTy method _ (IfaceTermVar dict_ref) (IfaceTermVar arg_ref)
                             <- appArgsIfaceTypes args ] of
                        [refs] | refs == (idName suc, dict, arg) ->
                            putStrLn "interface selector references its two binders"
                        _ -> fail "interface selector lost a lexical reference"
                _ -> fail "interface type lost a scoped dictionary binder"
            rejectsPhase "runtime application with mismatched evidence"
                "invalid dependent dictionary argument"
                (applyTypeToArgs (qualified ea x selectA) [Var eb])

            let [proxyCon] = tyConDataCons proxy
                proxyFor index = mkApps (Var (dataConWorkId proxyCon)) [Type (typeKind index), Type index]
                abstract = Lam a (Lam ea (Lam x (proxyFor selectA)))
                abstractType = mkLamType a (qualified ea x selectA)
                ordinary = Lam a (Lam ea (Lam x (mkApps (Var suc) [Type (mkTyVarTy a), Var ea, Var x])))
                arrow = mkFunctionType unmatchableDataConTy ManyTy
                ordinaryType = mkLamType a (arrow (varType ea) (arrow (mkTyVarTy a) (mkTyVarTy a)))
                apply evidence operand = mkApps abstract [Type (varType operand), evidence, Var operand]
                applied = apply (Var eb) y
            passExpr cfg "abstract dictionary and retained operand" abstract abstractType
            passExpr cfgB "apply dependent dictionary after type substitution" applied (resultUnder selectB)
            unless (isNothing (lintExpr cfg ordinary)) (fail "ordinary Lint rejected a constraint arrow")
            passExpr cfg "ordinary dictionary abstraction" ordinary ordinaryType
            let ordinaryCall = mkApps ordinary [Type (mkTyVarTy b), Var eb, Var y]
            passExpr cfgB "ordinary dictionary application" ordinaryCall (mkTyVarTy b)
            rejectExpr cfgAB "reject wrong dictionary for instantiated class"
                (apply (Var ea) y) (resultUnder selectB)
            rejectExpr (initLintConfig flags [b, y]) "reject an unbound dictionary argument"
                applied (mkTyConApp proxy [liftedTypeKind, mkTyVarTy b])
            rejectExpr cfgB "reject a computed dictionary expression"
                (apply (Let (NonRec alias (Var eb)) (Var alias)) y) (resultUnder selectB)

            let inner = enumDict 21 "sameName" (mkTyVarTy a)
                outer = enumDict 22 "sameName" (mkTyVarTy a)
                selected d = selectorAt suc (mkTyVarTy a) d x (mkTyVarTy a)
                innerLambda = Lam inner (Lam x (proxyFor (selected inner)))
                shadow = Lam outer (App innerLambda (Var outer))
                shadowType = qualified outer x (selected outer)
                nextType = DepFunTy DepFunVisible x (resultUnder (selected ea2))
            let shadowCfg = initLintConfig flags [a, ea2]
            passExpr shadowCfg "shadowed abstraction uses the outer dictionary after application" shadow shadowType
            passExpr shadowCfg "the second call substitutes a new same-predicate dictionary"
                (App shadow (Var ea2)) nextType

            when (isNothing (lintExpr cfg abstract)) $
                fail "ordinary Core Lint accepted a dependent dictionary lambda"
            let receiver = valueId 23 "receiver" (qualified ea x selectA)
            case toIfaceExpr (App (Var receiver) (Var ea2)) of
                IfaceDictApp (IfaceLcl _) dict | ifLclNameFS dict == fsLit "ea2" ->
                    putStrLn "interface dictionary application retains the actual binder"
                _ -> fail "interface dictionary application lost its binder"
            rejectsPhase "interface conversion of mismatched dictionary evidence"
                "invalid scoped dictionary application"
                (toIfaceExpr (App (Var receiver) (Var eb)))
            let input name = IfaceDictApp (IfaceLcl (mkIfLclName (fsLit "receiver"))) (mkIfLclName (fsLit name))
                restore scope expression =
                    initIfaceLoad env $
                    initIfaceLcl (nameModule (idName suc)) (text "scoped dictionary application") NotBoot $
                    extendIfaceIdEnv scope $
                    tcIfaceExpr expression
            restored <- restore [receiver, ea2] (input "ea2")
            case restored of
                App (Var f) (Var dict) | f == receiver && dict == ea2 -> pure ()
                _ -> fail "the interface reader changed the applied dictionary"
            forM_ ["missing", "eb"] $ \dict -> do
                rejected <- try (restore [receiver, ea2, eb] (input dict))
                    :: IO (Either IOEnvFailure (Expr Id))
                case rejected of
                    Left _ -> putStrLn ("interface reader rejected dictionary " ++ dict)
                    Right _ -> fail ("interface reader accepted dictionary " ++ dict)

            prep <- initCorePrepConfigFor True env
            logger <- initLogger
            rejectsIOPhase "CorePrep rejects dependent dictionary expressions"
                "CorePrep does not support experimental dependent dictionaries"
                (corePrepExpr logger prep abstract)
            let top = valueId 24 "top" abstractType
                thisModule = mkModule mainUnit (mkModuleName "ScopedDictionaryFixture")
            rejectsIOPhase "CorePrep rejects a dependent dictionary program"
                "CorePrep does not support experimental dependent dictionaries"
                (corePrepPgm logger prep (initCorePrepPgmConfig flags []) thisModule [NonRec top abstract])

pass :: LintConfig -> String -> Type -> IO ()
pass cfg label ty = case lintScopedDictionaryType cfg ty of
    Nothing -> putStrLn (label ++ ": accepted")
    Just err -> fail (label ++ ": " ++ showSDocUnsafe (ppr err))

reject :: LintConfig -> String -> Type -> IO ()
reject cfg label ty = case lintScopedDictionaryType cfg ty of
    Just _ -> putStrLn (label ++ ": rejected")
    Nothing -> fail (label ++ ": accepted")

rejectsPhase :: String -> String -> a -> IO ()
rejectsPhase label diagnostic = rejectsIOPhase label diagnostic . evaluate

rejectsIOPhase :: String -> String -> IO a -> IO ()
rejectsIOPhase label diagnostic action = do
    checked <- try action
    case checked of
        Left err | diagnostic `isInfixOf` displayException (err :: SomeException) ->
            putStrLn (label ++ ": rejected")
        Left err -> fail (label ++ ": " ++ displayException err)
        Right _ -> fail (label ++ ": accepted")

passExpr :: LintConfig -> String -> Expr Id -> Type -> IO ()
passExpr cfg label expr ty = do
    unless (isNothing (lintScopedDictionaryExpr cfg expr ty) && eqType (exprType expr) ty) $
        fail (label ++ ": rejected or has the wrong result type")
    putStrLn (label ++ ": accepted")

rejectExpr :: LintConfig -> String -> Expr Id -> Type -> IO ()
rejectExpr cfg label expr ty =
    if isNothing (lintScopedDictionaryExpr cfg expr ty)
    then fail (label ++ ": accepted")
    else putStrLn (label ++ ": rejected")
