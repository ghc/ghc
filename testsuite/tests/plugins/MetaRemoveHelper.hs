module MetaRemoveHelper where

import Language.Haskell.TH

clear :: Q [Dec] -> Q [Dec]
clear _ = return []
