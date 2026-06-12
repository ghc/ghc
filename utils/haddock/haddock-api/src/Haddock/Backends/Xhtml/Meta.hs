{-# LANGUAGE OverloadedStrings #-}

module Haddock.Backends.Xhtml.Meta where

import Data.ByteString.Builder (hPutBuilder)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withFile)

import Haddock.Utils.Json
import Haddock.Version

-- | Everytime breaking changes to the Quckjump api
-- happen this needs to be modified.
quickjumpVersion :: Int
quickjumpVersion = 1

-- | Writes a json encoded file containing additional
-- information about the generated documentation. This
-- is useful for external tools (e.g., Hackage).
writeHaddockMeta :: FilePath -> Bool -> IO ()
writeHaddockMeta odir withQuickjump = do
  let
    meta_json :: Value
    meta_json =
      object
        ( concat
            [ ["haddock_version" .= T.pack projectVersion]
            , ["quickjump_version" .= quickjumpVersion | withQuickjump]
            ]
        )

  withFile (odir </> "meta.json") WriteMode $ \h ->
    hPutBuilder h (encodeToBuilder meta_json)
