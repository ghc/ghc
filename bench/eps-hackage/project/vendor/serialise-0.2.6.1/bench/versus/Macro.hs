module Macro
  ( benchmarks -- :: [Benchmark]
  ) where
import           Data.Int

import           Criterion.Main
import           Control.DeepSeq
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BS
import qualified Codec.Compression.GZip as GZip

import qualified Macro.Types     as Types
import qualified Macro.MemSize
import           Macro.DeepSeq ()
import qualified Macro.Load as Load

import qualified Macro.ReadShow  as ReadShow
import qualified Macro.PkgBinary as PkgBinary
import qualified Macro.PkgCereal as PkgCereal
import qualified Macro.PkgAesonGeneric as PkgAesonGeneric
import qualified Macro.PkgAesonTH as PkgAesonTH
import qualified Macro.PkgStore as PkgStore

import qualified Macro.CBOR as CBOR

readBigTestData :: IO [Types.GenericPackageDescription]
readBigTestData = do
    Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                        (BS.readFile "bench/data/01-index.tar.gz")
    let tstdata  = take 100 pkgs_
    return tstdata

benchmarks :: [Benchmark]
benchmarks =
  [ env readBigTestData $ \tstdata ->
    bgroup "reference"
      [ bench "deepseq" (whnf rnf tstdata)
      , bench "memSize" (whnf (flip Macro.MemSize.memSize 0) tstdata)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "encoding"
      [ bench "binary"        (whnf perfEncodeBinary       tstdata)
      , bench "cereal"        (whnf perfEncodeCereal       tstdata)
      , bench "aeson generic" (whnf perfEncodeAesonGeneric tstdata)
      , bench "aeson TH"      (whnf perfEncodeAesonTH      tstdata)
      , bench "read/show"     (whnf perfEncodeReadShow     tstdata)
      , bench "cbor"          (whnf perfEncodeCBOR         tstdata)
      , bench "store"         (whnf perfEncodeStore        tstdata)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "decoding whnf"
      [ env (return $ combineChunks $ PkgBinary.serialise tstdata)
        $ \tstdataB -> bench "binary" (whnf perfDecodeBinary tstdataB)
      , env (return $ combineChunks $ PkgCereal.serialise tstdata)
        $ \tstdataC -> bench "cereal"  (whnf perfDecodeCereal tstdataC)
      , env (return $ combineChunks $ PkgAesonTH.serialise tstdata)
        $ \tstdataA -> bgroup "aeson"
            [ bench "generic"   (whnf perfDecodeAesonGeneric tstdataA)
            , bench "TH"        (whnf perfDecodeAesonTH      tstdataA)
            ]
      , env (return $ combineChunks $ ReadShow.serialise tstdata)
        $ \tstdataS -> bench "read/show" (whnf perfDecodeReadShow tstdataS)
      , env (return $ PkgStore.serialise tstdata)
        $ \tstdataR -> bench "store" (whnf perfDecodeStore tstdataR)
      , env (return $ combineChunks $ CBOR.serialise tstdata)
        $ \tstdataR -> bench "cbor" (whnf perfDecodeCBOR tstdataR)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "decoding nf"
      [ env (return $ combineChunks $ PkgBinary.serialise tstdata)
      $ \tstdataB -> bench "binary" (nf perfDecodeBinary tstdataB)
      , env (return $ combineChunks $ PkgCereal.serialise tstdata)
      $ \tstdataC -> bench "cereal" (nf perfDecodeCereal tstdataC)
      , env (return $ combineChunks $ PkgAesonTH.serialise tstdata)
      $ \tstdataA -> bgroup "aeson"
          [ bench "generic"   (nf perfDecodeAesonGeneric tstdataA)
          , bench "TH"        (nf perfDecodeAesonTH      tstdataA)
          ]

      , env (return $ combineChunks $ ReadShow.serialise tstdata)
      $ \tstdataS -> bench "read/show" (nf perfDecodeReadShow tstdataS)
      , env (return $ PkgStore.serialise tstdata)
      $ \tstdataR -> bench "store" (nf perfDecodeStore tstdataR)
      , env (return $ combineChunks $ CBOR.serialise tstdata)
      $ \tstdataR -> bench "cbor" (nf perfDecodeCBOR tstdataR)
      ]
  ]
  where
    perfEncodeBinary, perfEncodeCereal, perfEncodeAesonGeneric,
      perfEncodeAesonTH, perfEncodeReadShow,
      perfEncodeCBOR
      :: [Types.GenericPackageDescription] -> Int64


    perfEncodeBinary       = BS.length . PkgBinary.serialise
    perfEncodeCereal       = BS.length . PkgCereal.serialise
    perfEncodeAesonGeneric = BS.length . PkgAesonGeneric.serialise
    perfEncodeAesonTH      = BS.length . PkgAesonTH.serialise
    perfEncodeReadShow     = BS.length . ReadShow.serialise
    perfEncodeCBOR         = BS.length . CBOR.serialise

    perfDecodeBinary, perfDecodeCereal, perfDecodeAesonGeneric,
      perfDecodeAesonTH, perfDecodeReadShow,
      perfDecodeCBOR
      :: BS.ByteString -> [Types.GenericPackageDescription]

    perfDecodeBinary       = PkgBinary.deserialise
    perfDecodeCereal       = PkgCereal.deserialise
    perfDecodeAesonGeneric = PkgAesonGeneric.deserialise
    perfDecodeAesonTH      = PkgAesonTH.deserialise
    perfDecodeReadShow     = ReadShow.deserialise
    perfDecodeCBOR        = CBOR.deserialise

    perfDecodeStore :: B.ByteString -> [Types.GenericPackageDescription]
    perfDecodeStore = PkgStore.deserialise
    perfEncodeStore :: [Types.GenericPackageDescription] -> Int
    perfEncodeStore = B.length . PkgStore.serialise

    -- Convert any lazy ByteString to ByteString lazy bytestring
    -- that have only single chunk.
    combineChunks :: BS.ByteString -> BS.ByteString
    combineChunks = BS.fromStrict . BS.toStrict
