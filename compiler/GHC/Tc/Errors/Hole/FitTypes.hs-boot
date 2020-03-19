-- This boot file is in place to break the loop where:
-- + GHC.Tc.Types needs 'HoleFitPlugin',
-- + which needs 'GHC.Tc.Errors.Hole.FitTypes'
-- + which needs 'GHC.Tc.Types'
module GHC.Tc.Errors.Hole.FitTypes where

-- Build ordering
import GHC.Base()

data HoleFitPlugin
