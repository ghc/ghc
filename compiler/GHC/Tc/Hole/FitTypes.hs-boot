-- This boot file is in place to break the loop where:
-- + GHC.Tc.Utils needs 'HoleFitPlugin',
-- + which needs 'GHC.Tc.Hole.FitTypes'
-- + which needs 'GHC.Tc.Utils'
module GHC.Tc.Hole.FitTypes where

-- Build ordering
import GHC.Base()

data HoleFitPlugin
