{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module CtIdPlugin where

-- base
import Data.Maybe
import Data.Traversable

-- ghc
import GHC.Core.Class
import GHC.Core.Coercion
import GHC.Core.DataCon
import GHC.Core.Make
import GHC.Core.Predicate
import GHC.Core.TyCo.Rep
import GHC.Plugins
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Types.Unique.DSet

-- common
import Common

--------------------------------------------------------------------------------

-- This plugin simplifies Given and Wanted 'CtId' constraints.
-- To do this, we just look through the Givens and Wanteds,
-- find any irreducible constraint whose TyCon matches that of 'CtId',
-- in which case we substitute it for its argument:
-- We create a new Given or Wanted and remove the old one using a cast.

plugin :: Plugin
plugin = mkPlugin solver don'tRewrite

-- Solve "CtId".
solver :: [String]
       -> PluginDefs -> EvBindsVar -> [Ct] -> [Ct]
       -> TcPluginM TcPluginSolveResult
solver _args defs ev givens wanteds = do
  let pluginCo = mkUnivCo (PluginProv "CtIdPlugin" emptyUniqDSet) Representational  -- Empty is fine. This plugin does not use "givens".
  let substEvidence ct ct' =
        evCast (ctEvExpr $ ctEvidence ct') $ pluginCo (ctPred ct') (ctPred ct)

  if null wanteds
    then do
      newGivenPredTypes <- traverse (solveCt defs) givens
      newGivens <- for (zip newGivenPredTypes givens) \case
        (Nothing, _) -> return Nothing
        (Just pred, ct) ->
          let EvExpr expr =
                evCast (ctEvExpr $ ctEvidence ct) $ pluginCo (ctPred ct) pred
          in Just . mkNonCanonical <$> newGiven ev (ctLoc ct) pred expr
      let removedGivens =
            [ (substEvidence ct ct', ct)
            | (Just ct', ct) <- zip newGivens givens
            ]
      pure $ TcPluginOk removedGivens (catMaybes newGivens)
    else do
      newWantedPredTypes <- traverse (solveCt defs) wanteds
      newWanteds <- for (zip newWantedPredTypes wanteds) \case
        (Nothing, _) -> return Nothing
        (Just pred, ct) -> do
          evidence <- newWanted (ctLoc ct) pred
          return $ Just (mkNonCanonical evidence)
      let removedWanteds =
            [ (substEvidence ct ct', ct)
            | (Just ct', ct) <- zip newWanteds wanteds
            ]
      pure $ TcPluginOk removedWanteds (catMaybes newWanteds)

solveCt :: PluginDefs -> Ct -> TcPluginM (Maybe PredType)
solveCt (PluginDefs {..}) ct@(classifyPredType . ctPred -> IrredPred pred)
  | Just (tyCon, [arg]) <- splitTyConApp_maybe pred
  , tyCon == ctIdFam
  = pure $ Just arg
solveCt _ ct = pure Nothing
