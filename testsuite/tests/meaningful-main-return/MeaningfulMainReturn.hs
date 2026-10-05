{-# OPTIONS_GHC -dcore-lint #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}

module MeaningfulMainReturn (
  module MeaningfulMainReturn,
  Imported.main,
  Imported.mainInvalid
) where

import qualified Imported
import Data.Void (Void)
import System.Exit (ExitCode(..))

mainUnit :: IO ()
mainUnit = pure ()

mainVoid :: IO Void
mainVoid = pure (error "mainVoid result evaluated")

mainInt :: IO Int
mainInt = pure 1

mainExitSuccess :: IO ExitCode
mainExitSuccess = pure ExitSuccess

mainExitFailure :: IO ExitCode
mainExitFailure = pure (ExitFailure 7)

mainRecursive = mainRecursive

mainPolymorphic :: IO a
mainPolymorphic = pure (error "mainPolymorphic result evaluated")

mainExitCodeBottom :: IO ExitCode
mainExitCodeBottom = pure (error "mainExitCodeBottom result evaluated")

type ExitCodeAlias = ExitCode

mainExitCodeAlias :: IO ExitCodeAlias
mainExitCodeAlias = pure (ExitFailure 8)

newtype ExitCodeNewtype = ExitCodeNewtype ExitCode

mainExitCodeNewtype :: IO ExitCodeNewtype
mainExitCodeNewtype = pure (ExitCodeNewtype (ExitFailure 9))

type family FamilyResult a where
  FamilyResult Bool = ExitCode
  FamilyResult Int  = ()
  FamilyResult Char = Int

mainFamilyExitCode :: IO (FamilyResult Bool)
mainFamilyExitCode = pure (ExitFailure 10)

mainFamilyUnit :: IO (FamilyResult Int)
mainFamilyUnit = pure ()

mainFamilyInvalid :: IO (FamilyResult Char)
mainFamilyInvalid = pure 11

mainFamilyUnconstrained :: IO (FamilyResult a)
mainFamilyUnconstrained = pure (error "mainFamilyUnconstrained result evaluated")

type family AlwaysExit a where
  AlwaysExit a = ExitCode

mainFamilyAlwaysExit :: IO (AlwaysExit a)
mainFamilyAlwaysExit = pure (ExitFailure 12)

mainConstrained :: (a ~ ExitCode) => IO a
mainConstrained = pure (ExitFailure 13)

mainImportedWrapped = Imported.main

readExit :: (Read a) => IO a
readExit = pure (read "ExitFailure 15")

mainTypeApp = readExit @ExitCode
