module Mini
  ( benchmarks -- :: [Benchmark]
  ) where
import           System.FilePath

import           Criterion.Main
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BS

import           Macro.DeepSeq ()
import qualified Macro.PkgBinary as PkgBinary
import qualified Macro.PkgCereal as PkgCereal
import qualified Macro.PkgStore  as PkgStore
import qualified Macro.CBOR as CBOR


benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "decode-index"
      [ bgroup "binary"
          [ envBinary $ \v ->
              bench "deserialise" $ nf PkgBinary.deserialise v
          ]
      , bgroup "cereal"
          [ envCereal $ \v ->
              bench "deserialise" $ nf PkgCereal.deserialise v
          ]
      , bgroup "store"
          [ envStore $ \v ->
              bench "deserialise" $ nf PkgStore.deserialise v
          ]
      , bgroup "cbor"
          [ envCBOR $ \v ->
              bench "deserialise" $ nf CBOR.deserialise v
          ]
      ]
  , bgroup "decode-index-noaccum"
      [ bgroup "binary"
          [ envBinary $ \v ->
              bench "deserialise" $ nf PkgBinary.deserialiseNull v
          ]
      , bgroup "cereal"
          [ envCereal $ \v ->
              bench "deserialise" $ nf PkgCereal.deserialiseNull v
          ]
      , bgroup "cbor"
          [ envCBOR $ \v ->
              bench "deserialise" $ nf CBOR.deserialiseNull v
          ]
      ]
  ]
  where
    -- Helpers for using Criterion environments.
    envBinary = env (fmap BS.fromStrict (readBin "binary"))
    envCereal = env (fmap BS.fromStrict (readBin "cereal"))
    envStore  = env (readBin "store")
    envCBOR   = env (fmap BS.fromStrict (readBin "cbor"))

    -- | Read one of the pre-encoded binary files out of the
    -- data directory.
    readBin :: FilePath -> IO B.ByteString
    readBin f = B.readFile ("bench" </> "data" </> f <.> "bin")
