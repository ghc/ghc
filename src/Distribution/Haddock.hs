--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Distribution.Haddock (
  readInterfaceFile,
  nameCacheFromGhc,
  freshNameCache,
  NameCacheAccessor,
  InterfaceFile(..),
  LinkEnv,
  InstalledInterface(..),
  module Haddock.DocName
) where


import Haddock.InterfaceFile
import Haddock.Types
import Haddock.DocName
