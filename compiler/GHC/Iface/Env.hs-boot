module GHC.Iface.Env where

import GHC.Types.Module
import GHC.Types.Name.Occurrence
import TcRnMonad
import GHC.Types.Name
import GHC.Types.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
