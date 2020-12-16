module GHC.Core.Unfold where

import GHC.Prelude

data UnfoldingOpts

defaultUnfoldingOpts :: UnfoldingOpts

updateCreationThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
updateUseThreshold      :: Int -> UnfoldingOpts -> UnfoldingOpts
updateFunAppDiscount    :: Int -> UnfoldingOpts -> UnfoldingOpts
updateDictDiscount      :: Int -> UnfoldingOpts -> UnfoldingOpts
updateVeryAggressive    :: Bool -> UnfoldingOpts -> UnfoldingOpts
updateCaseThreshold     :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCaseScaling       :: Int -> UnfoldingOpts -> UnfoldingOpts
