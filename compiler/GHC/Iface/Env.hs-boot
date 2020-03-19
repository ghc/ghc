module GHC.Iface.Env where

import GHC.Types.Module
import GHC.Types.Name.Occurrence
import GHC.Tc.Utils.Monad
import GHC.Types.Name
import GHC.Types.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
