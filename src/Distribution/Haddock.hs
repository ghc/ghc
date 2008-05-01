--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Distribution.Haddock (
  readInterfaceFile,
  InterfaceFile(..),
  LinkEnv,
  InstalledInterface(..),
  DocName(..)
) where


import Haddock.InterfaceFile
import Haddock.Types
import Haddock.DocName
