module LiftTInstance where

import Language.Haskell.TH.Lib.Internal

-- User defined instances for LiftT should be banned.
instance LiftT () where

