{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import qualified Data.List.NonEmpty as NE
import GHC (InteractiveImport (IIDecl), getSessionDynFlags, lookupName, parseImportDecl, parseName, runGhc, setContext, setSessionDynFlags)
import GHC.Builtin.WiredIn.Types (boolTy, constraintKind, liftedTypeKind, mkBoxedTupleTy, stringTy, unmatchableDataConTy)
import GHC.Core (Bind (NonRec), Expr (Lam, Let, Type, Var))
import GHC.Core.Lint
    ( LintConfig, checkScopedSelectorApp, eqScopedSelectorApp, lintExpr
    , lintScopedDictionaryType, scopedSelectorResultKind, substScopedSelectorApp
    )
import qualified GHC.Core.Subst as Subst
import GHC.Core.TyCo.Compare (eqType, eqVarBndrs)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (Type (DepFunTy, TermVarTy))
import GHC.Core.TyCo.Tidy (tidyTopType)
import GHC.Core.TyCon (FamTyConFlav (OpenTypeFamilyTyCon), Injectivity (NotInjective), mkAnonTyConBinder, mkFamilyTyCon, mkNamedTyConBinder, mkTyConKindWithMatchability)
import GHC.Core.Type (pattern ManyTy, mkForAllTys, mkTyConApp, mkTyVarTy)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Types.Id (mkSysLocal, setIdType)
import GHC.Types.Name (mkSystemName)
import GHC.Types.Name.Occurrence (mkTcOcc, mkTyVarOcc)
import GHC.Types.TyThing (TyThing (ATyCon, AnId))
import GHC.Types.Unique (mkUniqueGrimilyWithTag)
import GHC.Types.Var (DepFunTyFlag (DepFunInvisible), ForAllTyFlag (Invisible), Specificity (SpecifiedSpec), VarBndr (Bndr), mkTyVar, varType)
import GHC.Types.Var.Env (emptyInScopeSet, mkInScopeSetList, mkRnEnv2)
import GHC.Types.Var.Set (isEmptyVarSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import System.Environment (getArgs)

-- | Test prototypes in memory only.
main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
        flags <- getSessionDynFlags
        _ <- setSessionDynFlags flags
        prelude <- parseImportDecl "import Prelude"
        setContext [IIDecl prelude]
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
            bind = DepFunTy DepFunInvisible
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
            reject cfg "escaped reference" "Invalid runtime index" escaped
            reject cfg "forged predicate" "Invalid runtime index" mislabeled
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

pass :: LintConfig -> String -> Type -> IO ()
pass cfg label ty = case lintScopedDictionaryType cfg ty of
    Nothing -> putStrLn (label ++ ": accepted")
    Just err -> fail (label ++ ": " ++ showSDocUnsafe (ppr err))

reject :: LintConfig -> String -> String -> Type -> IO ()
reject cfg label diagnostic ty = case lintScopedDictionaryType cfg ty of
    Just err | diagnostic `isInfixOf` showSDocUnsafe (ppr err) -> putStrLn (label ++ ": rejected")
    other -> fail (label ++ ": " ++ showSDocUnsafe (ppr other))
