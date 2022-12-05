module GHC.Core.Unfold where

import GHC.Prelude
import GHC.Types.Basic ( StateHackFlag )

data UnfoldingOpts

defaultUnfoldingOpts :: StateHackFlag -> UnfoldingOpts

updateCreationThreshold :: Int -> UnfoldingOpts -> UnfoldingOpts
updateUseThreshold      :: Int -> UnfoldingOpts -> UnfoldingOpts
updateFunAppDiscount    :: Int -> UnfoldingOpts -> UnfoldingOpts
updateDictDiscount      :: Int -> UnfoldingOpts -> UnfoldingOpts
updateVeryAggressive    :: Bool -> UnfoldingOpts -> UnfoldingOpts
updateCaseThreshold     :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCaseScaling       :: Int -> UnfoldingOpts -> UnfoldingOpts
