-----------------------------------------------------------------------------
-- |
-- Module      :  Documentation.Haddock
-- Copyright   :  (c) David Waern 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- The Haddock API: A rudimentory, highly experimental API exposing some of
-- the internals of Haddock. Don't expect it to be stable.
-----------------------------------------------------------------------------
module Documentation.Haddock (

  -- * Interface
  Interface(..),
  InstalledInterface(..),
  createInterfaces,
  processModules,

  -- * Export items & declarations
  ExportItem(..),
  Decl,
  DeclInfo,
  DocForDecl,
  FnArgsDoc,

  -- * Hyperlinking
  LinkEnv,
  DocName(..),

  -- * Instances
  DocInstance,
  InstHead,

  -- * Documentation comments
  Doc(..),
  Example(..),
  DocMarkup(..),
  HaddockModInfo(..),
  markup,

  -- * Interface files
  -- | (.haddock files)
  InterfaceFile(..),
  readInterfaceFile,
  nameCacheFromGhc,
  freshNameCache,
  NameCacheAccessor,

  -- * Flags and options
  Flag(..),
  DocOption(..)

) where


import Haddock.InterfaceFile
import Haddock.Interface
import Haddock.Types
import Haddock.Options
import Haddock.Utils
import Main


-- | Create 'Interface' structures from a given list of Haddock command-line
-- flags and file or module names (as accepted by 'haddock' executable).  Flags
-- that control documentation generation or show help or version information
-- are ignored.
createInterfaces
  :: [Flag]         -- ^ A list of command-line flags
  -> [String]       -- ^ File or module names
  -> IO [Interface] -- ^ Resulting list of interfaces
createInterfaces flags modules = do
  (_, ifaces, _) <- readPackagesAndProcessModules flags modules
  return ifaces

