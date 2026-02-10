module GHC.Iface.Env where

import GHC.Unit.Module
import GHC.Types.Name.Occurrence
import GHC.Tc.Utils.Monad
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Name.Cache (ContainsNameCache)
import GHC.Utils.Logger (ContainsLogger)

newGlobalBinder :: (ContainsNameCache top, ContainsLogger top) => Module -> OccName -> SrcSpan -> TcRnIfBase top a b Name
