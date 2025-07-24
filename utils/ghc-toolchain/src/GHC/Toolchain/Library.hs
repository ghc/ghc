module GHC.Toolchain.Library
  ( Library(..)
  )
  where

import System.FilePath
import GHC.Toolchain.Prelude

data Library = Library { libName :: String
                       , includePath :: Maybe FilePath
                       , libraryPath :: Maybe FilePath
                       }
    deriving (Read, Eq, Ord)

instance Show Library where
  -- Normalise filepaths before showing to aid with diffing the target files.
  show (Library n i l) = unwords
    [ "Library { libName = ", show n
    , ", includePath = ", show (normalise <$> i)
    , ", libraryPath =", show (normalise <$> l)
    , "}"]

