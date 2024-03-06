module GHC.Driver.MakeFile.JSON
  ( writeJSONFile,
    JsonOutput (..),
    mkJsonOutput,
    updateJson,
    writeJsonOutput,
    DepJSON,
    initDepJSON,
    updateDepJSON,
    OptJSON,
    initOptJSON,
    updateOptJSON,
  )
where

import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Prelude
import GHC.Utils.Json
import GHC.Utils.Misc
import GHC.Utils.Outputable
import System.FilePath
import Data.Foldable (traverse_)

--------------------------------------------------------------------------------
-- Output helpers
--------------------------------------------------------------------------------

writeJSONFile :: ToJson a => a -> FilePath -> IO ()
writeJSONFile doc p = do
  withAtomicRename p
    $ \tmp -> writeFile tmp $ showSDocUnsafe $ renderJSON $ json doc

--------------------------------------------------------------------------------
-- Output interface for json dumps
--------------------------------------------------------------------------------

-- | Resources for a json dump option, used in "GHC.Driver.MakeFile".
-- The flags @-dep-json@ and @-opt-json@ add an additional output target for
-- dependency diagnostics.
data JsonOutput a =
  JsonOutput {
    -- | This ref is updated in @processDeps@ incrementally, using a
    -- flag-specific type.
    json_ref :: IORef a,

    -- | The output file path specified as argument to the flag.
    json_path :: FilePath
  }

-- | Allocate an 'IORef' with the given function if the 'FilePath' is 'Just',
-- indicating that the userspecified @-*-json@.
mkJsonOutput ::
  IO (IORef a) ->
  Maybe FilePath ->
  IO (Maybe (JsonOutput a))
mkJsonOutput mk_ref =
  traverse $ \ json_path -> do
    json_ref <- mk_ref
    pure JsonOutput {json_ref, json_path}

-- | Update the dump data in 'json_ref' if the output target is present.
updateJson :: Maybe (JsonOutput a) -> (a -> a) -> IO ()
updateJson out f = traverse_ (flip modifyIORef' f . json_ref) out

-- | Write a json object to the flag-dependent file if the output target is
-- present.
writeJsonOutput ::
  ToJson a =>
  Maybe (JsonOutput a) ->
  IO ()
writeJsonOutput =
  traverse_ $ \ JsonOutput {json_ref, json_path} -> do
    payload <- readIORef json_ref
    writeJSONFile payload json_path

--------------------------------------------------------------------------------
-- Payload for -dep-json
--------------------------------------------------------------------------------

newtype DepJSON = DepJSON (Map.Map FilePath (Set.Set FilePath))

instance ToJson DepJSON where
  json (DepJSON m) =
    JSObject
      [ (target, JSArray [JSString dep | dep <- Set.toList deps])
        | (target, deps) <- Map.toList m
      ]

initDepJSON :: IO (IORef DepJSON)
initDepJSON = newIORef $ DepJSON Map.empty

updateDepJSON :: [FilePath] -> FilePath -> DepJSON -> DepJSON
updateDepJSON targets dep (DepJSON m0) =
  DepJSON
    $ foldl'
      ( \acc target ->
          Map.insertWith
            Set.union
            (normalise target)
            (Set.singleton $ normalise dep)
            acc
      )
      m0
      targets

--------------------------------------------------------------------------------
-- Payload for -opt-json
--------------------------------------------------------------------------------

newtype OptJSON = OptJSON (Map.Map FilePath [String])

instance ToJson OptJSON where
  json (OptJSON m) =
    JSObject
      [ (src_file, JSArray [JSString opt | opt <- opts])
        | (src_file, opts) <- Map.toList m
      ]

initOptJSON :: IO (IORef OptJSON)
initOptJSON = newIORef $ OptJSON Map.empty

updateOptJSON :: FilePath -> [String] -> OptJSON -> OptJSON
updateOptJSON src_file opts (OptJSON m0) =
  OptJSON $ Map.insert (normalise src_file) opts m0
