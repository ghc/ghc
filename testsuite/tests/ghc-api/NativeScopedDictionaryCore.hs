{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import GHC (InteractiveImport (IIDecl), getSessionDynFlags, lookupName, parseImportDecl, parseName, runGhc, setContext, setSessionDynFlags)
import GHC.Builtin.WiredIn.Types (constraintKind, liftedTypeKind, mkBoxedTupleTy, unmatchableDataConTy)
import GHC.Core (Expr (Lam, Var))
import GHC.Core.Lint (LintConfig, lintExpr, lintScopedDictionaryType)
import GHC.Core.TyCo.Compare (eqType)
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
import GHC.Types.TyThing (TyThing (ATyCon))
import GHC.Types.Unique (mkUniqueGrimilyWithTag)
import GHC.Types.Var (DepFunTyFlag (DepFunInvisible), ForAllTyFlag (Invisible), Specificity (SpecifiedSpec), VarBndr (Bndr), mkTyVar, varType)
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

pass :: LintConfig -> String -> Type -> IO ()
pass cfg label ty = case lintScopedDictionaryType cfg ty of
    Nothing -> putStrLn (label ++ ": accepted")
    Just err -> fail (label ++ ": " ++ showSDocUnsafe (ppr err))

reject :: LintConfig -> String -> String -> Type -> IO ()
reject cfg label diagnostic ty = case lintScopedDictionaryType cfg ty of
    Just err | diagnostic `isInfixOf` showSDocUnsafe (ppr err) -> putStrLn (label ++ ": rejected")
    other -> fail (label ++ ": " ++ showSDocUnsafe (ppr other))
