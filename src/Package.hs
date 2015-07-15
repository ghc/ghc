{-# LANGUAGE DeriveGeneric #-}

module Package (Package (..), library, topLevel, setCabal) where

import Base
import Util
import GHC.Generics
import Development.Shake.Classes

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName  :: String,   -- Examples: "deepseq", "Cabal/Cabal"
         pkgPath  :: FilePath, -- "libraries/deepseq", "libraries/Cabal/Cabal"
         pkgCabal :: FilePath  -- "deepseq.cabal", "Cabal.cabal" (relative)
     }
     deriving Generic

instance Show Package where
    show = pkgName

instance Eq Package where
    (==) = (==) `on` pkgName

instance Ord Package where
    compare = compare `on` pkgName

-- TODO: check if unifyPath is actually needed
library :: String -> Package
library name =
    Package name (unifyPath $ "libraries" </> name) (name <.> "cabal")

topLevel :: String -> Package
topLevel name = Package name name (name <.> "cabal")

setCabal :: Package -> FilePath -> Package
setCabal pkg cabalName = pkg { pkgCabal = cabalName }

-- Instances for storing in the Shake database
instance Binary Package
instance Hashable Package where
    hashWithSalt salt = hashWithSalt salt . show
