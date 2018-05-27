-- This boot file is in place to break the loop where:
-- + TcSimplify calls 'TcErrors.reportUnsolved',
-- + which calls 'TcHoleErrors.findValidHoleFits`
-- + which calls 'TcSimplify.simpl_top'
module TcHoleErrors where

import TcRnTypes  ( TcM, Ct, Implication )
import Outputable ( SDoc )
import VarEnv     ( TidyEnv )

findValidHoleFits :: TidyEnv -> [Implication] -> [Ct] -> Ct
                  -> TcM (TidyEnv, SDoc)
