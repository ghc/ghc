module Manifest where

import DynFlags
import PackageConfig

import System.FilePath

-- | Generate the appropriate Manifest file for program inclusion.
mkManifest
   :: DynFlags
   -> [PackageConfig]                   -- dependencies of this link object
   -> FilePath                          -- filename of executable
   -> IO [FilePath]                     -- extra objects to embed, maybe