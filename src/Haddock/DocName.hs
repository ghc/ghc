--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.DocName where


import Haddock.GHC.Utils

import GHC
import OccName
import Name
import Binary
import Outputable


data DocName = Documented Name Module | Undocumented Name


docNameOcc :: DocName -> OccName
docNameOcc = nameOccName . docNameOrig


docNameOrig :: DocName -> Name
docNameOrig (Documented name _) = name
docNameOrig (Undocumented name) = name


instance Binary DocName where
  put_ bh (Documented name mod) = do
    putByte bh 0
    put_ bh name
    put_ bh mod
  put_ bh (Undocumented name) = do
    putByte bh 1
    put_ bh name

  get bh = do
    h <- getByte bh
    case h of
      0 -> do
        name <- get bh
        mod  <- get bh
        return (Documented name mod)
      1 -> do
        name <- get bh
        return (Undocumented name)
