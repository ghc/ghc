module Main where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import GHC
import GHC.Builtin.CoercionRules (boxedPairCompositionRule)
import GHC.Builtin.WiredIn.Types (boolTy, falseDataCon, promotedTupleDataCon, trueDataCon)
import GHC.Core.Coercion (mkNomReflCo, mkRepReflCo)
import GHC.Core.DataCon (promoteDataCon)
import GHC.Core.Lint (lintTcCoercion)
import GHC.Core.TyCo.Rep (Coercion(AxiomCo))
import GHC.Core.Type (mkTyConApp)
import GHC.Data.Bag (isEmptyBag)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Id (idDetails)
import GHC.Types.Id.Info (IdDetails(SourceTableId))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import System.Environment (getArgs)

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
      flags <- getSessionDynFlags
      setSessionDynFlags flags
      target <- guessTarget "NativeOpenPairCoercionWorker.hs" Nothing Nothing
      setTargets [target]
      Succeeded <- load LoadAllTargets
      modl <- findModule (mkModuleName "NativeOpenPairCoercionWorker") Nothing
      Just worker <- getModuleInfo modl
      first <- family worker "first"
      second <- family worker "second"
      exchange <- family worker "exchange"
      duplicate <- family worker "duplicate"
      let false = mkTyConApp (promoteDataCon falseDataCon) []
          true = mkTyConApp (promoteDataCon trueDataCon) []
          pair = mkTyConApp (promotedTupleDataCon Boxed 2) [boolTy, boolTy, false, true]
          call f arg = mkTyConApp f [boolTy, arg]
          lhs = call first (call exchange pair)
          rhs = call second pair
          evidence l r = AxiomCo boxedPairCompositionRule [mkNomReflCo l, mkNomReflCo r]
          lint co = isEmptyBag (snd (lintTcCoercion (initLintConfig flags []) co))
          wrongRole = AxiomCo boxedPairCompositionRule [mkNomReflCo lhs, mkRepReflCo rhs]
      liftIO $ do
        unless (lint (evidence lhs rhs)) (fail "Rejected the checked exchange")
        when (lint (evidence lhs (call first pair))) (fail "Accepted the same projection")
        when (lint wrongRole) (fail "Accepted a representational premise")
        when (lint (evidence (call first (call duplicate pair)) rhs)) $
          fail "Accepted the duplicated field"

family :: ModuleInfo -> String -> Ghc TyCon
family info occ = do
    name <- maybe (fail ("Missing worker " ++ occ)) pure $
      find ((== occ) . occNameString . nameOccName) (modInfoExports info)
    Just (AnId owner) <- modInfoLookupName info name
    case idDetails owner of
      SourceTableId _ table -> pure table
      _ -> fail ("No checked source family for " ++ occ)
