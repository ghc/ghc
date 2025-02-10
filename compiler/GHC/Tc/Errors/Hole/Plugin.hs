{-# LANGUAGE ExistentialQuantification #-}
module GHC.Tc.Errors.Hole.Plugin(CandPlugin, FitPlugin, HoleFitPlugin (..), HoleFitPluginR (..)) where

import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Types ( TcRef, TcM )


-- | A plugin for modifying the candidate hole fits *before* they're checked.
type CandPlugin = TypedHole -> [HoleFitCandidate] -> TcM [HoleFitCandidate]

-- | A plugin for modifying hole fits  *after* they've been found.
type FitPlugin =  TypedHole -> [HoleFit] -> TcM [HoleFit]

-- | A HoleFitPlugin is a pair of candidate and fit plugins.
data HoleFitPlugin = HoleFitPlugin
  { candPlugin :: CandPlugin
  , fitPlugin :: FitPlugin }

-- | HoleFitPluginR adds a TcRef to hole fit plugins so that plugins can
-- track internal state. Note the existential quantification, ensuring that
-- the state cannot be modified from outside the plugin.
data HoleFitPluginR = forall s. HoleFitPluginR
  { hfPluginInit :: TcM (TcRef s)
    -- ^ Initializes the TcRef to be passed to the plugin
  , hfPluginRun :: TcRef s -> HoleFitPlugin
    -- ^ The function defining the plugin itself
  , hfPluginStop :: TcRef s -> TcM ()
    -- ^ Cleanup of state, guaranteed to be called even on error
  }
