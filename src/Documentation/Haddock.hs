--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Documentation.Haddock (
  readInterfaceFile,
  nameCacheFromGhc,
  freshNameCache,
  NameCacheAccessor,
  InterfaceFile(..),
  LinkEnv,
  InstalledInterface(..),
  DocName(..),
  docNameOcc
) where


import Haddock.InterfaceFile
import Haddock.Types
