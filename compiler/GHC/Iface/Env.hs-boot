module GHC.Iface.Env where

import GHC.Unit.Module
import GHC.Types.Name.Occurrence
import GHC.Tc.Utils.Monad
import GHC.Types.Name
import Language.Haskell.Textual.Location

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
