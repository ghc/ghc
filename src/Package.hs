module Package (Package, library, setCabal) where

import Base
import Util

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName  :: String,   -- Examples: "deepseq", "Cabal/Cabal"
         pkgPath  :: FilePath, -- "libraries/deepseq", "libraries/Cabal/Cabal"
         pkgCabal :: FilePath  -- "deepseq.cabal", "Cabal.cabal"
     }

instance Eq Package where
    (==) = (==) `on` pkgName

libraryPackage :: String -> String -> Package
libraryPackage name cabalName =
    Package
        name
        (unifyPath $ "libraries" </> name)
        cabalName

library :: String -> Package
library name = libraryPackage name (name <.> "cabal")

setCabal :: Package -> FilePath -> Package
setCabal pkg cabalName = pkg { pkgCabal = cabalName }
