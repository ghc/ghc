module Haddock.Backends.Meta where

import Haddock.Utils.Json
import Haddock.Version

import Data.ByteString.Builder (hPutBuilder)
import System.FilePath ((</>))
import System.IO (withFile, IOMode (WriteMode))

-- | Writes a json encoded file containing additional
-- information about the generated documentation. This
-- is useful for external tools (e.g. hackage).
writeHaddockMeta :: FilePath -> IO ()
writeHaddockMeta odir = do
  let
    meta_json :: Value
    meta_json = object [
        "haddock_version" .= String projectVersion
      ]

  withFile (odir </> "meta.json") WriteMode $ \h ->
    hPutBuilder h (encodeToBuilder meta_json)