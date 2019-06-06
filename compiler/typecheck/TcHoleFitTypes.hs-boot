-- This boot file is in place to break the loop where:
-- + TcRnTypes needs 'HoleFitPlugin',
-- + which needs 'TcHoleFitTypes'
-- + which needs 'TcRnTypes'
module TcHoleFitTypes where

-- Build ordering
import GHC.Base()

data HoleFitPlugin
