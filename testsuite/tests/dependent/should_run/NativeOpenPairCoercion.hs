module Main where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import GHC
import GHC.Builtin.CoercionRules
  (boxedPairCompositionRule, boxedPairReconstructionRule, matchBoxedPairComposition, proveBoxedPairProjection)
import GHC.Builtin.WiredIn.Types (boolTy, falseDataCon, promotedTupleDataCon, trueDataCon)
import GHC.Core.Coercion (mkNomReflCo, mkRepReflCo)
import GHC.Core.DataCon (promoteDataCon)
import GHC.Core.Lint (lintTcCoercion)
import GHC.Core.TyCo.Rep (Coercion(AxiomCo))
import GHC.Core.Type (mkTyConApp)
import GHC.Data.Bag (isEmptyBag)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Id (sourceTableId_maybe)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import System.Environment (getArgs)

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
      flags <- getSessionDynFlags
      setSessionDynFlags flags
      target <- guessTarget "NativeOpenPairCoercionUsage.hs" Nothing Nothing
      setTargets [target]
      Succeeded <- load LoadAllTargets
      modl <- findModule (mkModuleName "NativeOpenPairCoercionWorker") Nothing
      Just worker <- getModuleInfo modl
      first <- family worker "first"
      second <- family worker "second"
      exchange <- family worker "exchange"
      duplicate <- family worker "duplicate"
      same <- family worker "same"
      duplicateRight <- family worker "duplicateRight"
      let false = mkTyConApp (promoteDataCon falseDataCon) []
          true = mkTyConApp (promoteDataCon trueDataCon) []
          pair = mkTyConApp (promotedTupleDataCon Boxed 2) [boolTy, boolTy, false, true]
          call f arg = mkTyConApp f [boolTy, arg]
          callPair f arg = mkTyConApp f [boolTy, boolTy, arg]
          lhs = call first (call exchange pair)
          rhs = call second pair
          lhsDuplicate = call second (call duplicate pair)
          lhsDuplicateRight = call first (callPair duplicateRight pair)
          lhsSame = call first (callPair same pair)
          lhsDirect = call first pair
          commonInput = call exchange pair
          repeated = call duplicate commonInput
          commonUses = map fst . snd <$> proveBoxedPairProjection (call first repeated)
                                                                   (call second repeated)
          selectedRule l r = do
              (AxiomCo rule _, _) <- proveBoxedPairProjection l r
              pure rule
          evidence rule l r = AxiomCo rule [mkNomReflCo l, mkNomReflCo r]
          lint co = isEmptyBag (snd (lintTcCoercion (initLintConfig flags []) co))
          wrongRole = AxiomCo boxedPairReconstructionRule [mkNomReflCo lhs, mkRepReflCo rhs]
      liftIO $ do
        case matchBoxedPairComposition lhs rhs of
          Just [_, _, _] -> pure ()
          _ -> fail "The original exchange matcher lost a source dependency"
        unless (lint (evidence boxedPairCompositionRule lhs rhs)) (fail "Rejected an old exchange proof")
        unless (selectedRule lhs rhs == Just boxedPairCompositionRule) (fail "Changed the exchange rule key")
        unless (selectedRule lhsDuplicate lhsDirect == Just boxedPairReconstructionRule) $
          fail "Rejected the selected binder of a duplicated field"
        unless (lint (evidence boxedPairReconstructionRule lhsDuplicate lhsDirect)) $
          fail "Rejected a valid duplicate-field projection"
        unless (lint (evidence boxedPairReconstructionRule lhsDuplicateRight rhs)) $
          fail "Rejected a valid second-field reconstruction"
        unless (lint (evidence boxedPairReconstructionRule lhsSame lhsDirect)) $
          fail "Rejected a valid identity reconstruction"
        when (lint (evidence boxedPairReconstructionRule lhs lhsDirect)) $
          fail "Accepted the same projection after exchange"
        case commonUses of
          Just uses | duplicate `elem` uses, exchange `notElem` uses -> pure ()
          _ -> fail "The proof must use duplication and leave its common input opaque"
        when (lint wrongRole) (fail "Accepted a representational premise")
        when (lint (evidence boxedPairReconstructionRule lhsDuplicate rhs)) $
          fail "Accepted the other field after duplication"
        when (lint (evidence boxedPairCompositionRule lhsDuplicate lhsDirect)) $
          fail "Accepted new evidence under the exchange-only key"

family :: ModuleInfo -> String -> Ghc TyCon
family info occ = do
    name <- maybe (fail ("Missing worker " ++ occ)) pure $
      find ((== occ) . occNameString . nameOccName) (modInfoExports info)
    Just (AnId owner) <- modInfoLookupName info name
    maybe (fail ("No checked source family for " ++ occ)) pure (sourceTableId_maybe owner)
