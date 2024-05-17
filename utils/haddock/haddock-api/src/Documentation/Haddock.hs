-----------------------------------------------------------------------------

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
-- The Haddock API: A rudimentary, highly experimental API exposing some of
-- the internals of Haddock. Don't expect it to be stable.
module Documentation.Haddock
  ( -- * Interface
    Interface (..)
  , InstalledInterface (..)
  , toInstalledIface
  , createInterfaces
  , processModules

    -- * Export items & declarations
  , ExportItem (..)
  , DocForDecl
  , FnArgsDoc

    -- * Cross-referencing
  , LinkEnv
  , DocName (..)

    -- * Instances
  , DocInstance
  , InstHead

    -- * Documentation comments
  , Doc
  , MDoc
  , DocH (..)
  , Example (..)
  , Hyperlink (..)
  , DocMarkup
  , DocMarkupH (..)
  , Documentation (..)
  , ArgMap
  , WarningMap
  , DocMap
  , HaddockModInfo (..)
  , markup

    -- * Interface files
  , InterfaceFile (..)
  , readInterfaceFile
  , freshNameCache

    -- * Flags and options
  , Flag (..)
  , DocOption (..)

    -- * Error handling
  , HaddockException (..)

    -- * Program entry point
  , haddock
  , haddockWithGhc
  , getGhcDirs
  , withGhc
  ) where

import Documentation.Haddock.Markup (markup)
import Haddock
import Haddock.Interface
import Haddock.InterfaceFile
import Haddock.Options
import Haddock.Types

-- | Create 'Interface' structures from a given list of Haddock command-line
-- flags and file or module names (as accepted by 'haddock' executable).  Flags
-- that control documentation generation or show help or version information
-- are ignored.
createInterfaces
  :: [Flag]
  -- ^ A list of command-line flags
  -> [String]
  -- ^ File or module names
  -> IO [Interface]
  -- ^ Resulting list of interfaces
createInterfaces flags modules = do
  (_, ifaces, _) <- withGhc flags (readPackagesAndProcessModules flags modules)
  return ifaces
