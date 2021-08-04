{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import Data.Byteable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Hash.SHA3 as SHA3
import Crypto.Hash

sha1F = ( "sha1"
        , SHA1.hash
        , SHA1.finalize . SHA1.update SHA1.init
        , toBytes . (hash :: B.ByteString -> Digest SHA1)
        )

sha512F = ( "sha512"
        , SHA512.hash
        , SHA512.finalize . SHA512.update SHA512.init
        , toBytes . (hash :: B.ByteString -> Digest SHA512)
        )

main = do
    let !bs32     = B.replicate 32 0
        !bs256    = B.replicate 256 0
        !bs4096   = B.replicate 4096 0
        !bs1M     = B.replicate (1*1024*1024) 0
    let !lbs64x256 = (map (const (B.replicate 64 0)) [0..3])
        !lbs64x4096 = (map (const (B.replicate 64 0)) [0..63])

    let (fname, fHash, fIncr, fAPI) = sha512F
    let benchName ty z = fname ++ "." ++ ty ++ " " ++ show z
    defaultMain
        [ bgroup "digest hex"
            [ bench "hex" $ whnf digestToHexByteString (hashsha1 B.empty)
            ]
        , bcompare
            [ bench (benchName "hash" 0) $ whnf fHash B.empty
            , bench (benchName "incr" 0) $ whnf fIncr B.empty
            , bench (benchName "api" 0)  $ whnf fAPI B.empty
            ]
        , bcompare
            [ bench (benchName "hash" 32) $ whnf SHA1.hash bs32
            , bench (benchName "incr" 32) $ whnf fIncr bs32
            , bench (benchName "api" 32)  $ whnf fAPI bs32
            ]
        , bcompare
            [ bench (benchName "hash" 256) $ whnf SHA1.hash bs256
            , bench (benchName "incr" 256) $ whnf fIncr bs256
            , bench (benchName "api" 256)  $ whnf fAPI bs256
            ]
        , bcompare
            [ bench (benchName "hash" 4096) $ whnf SHA1.hash bs4096
            , bench (benchName "incr" 4096) $ whnf fIncr bs4096
            , bench (benchName "api" 4096)  $ whnf fAPI bs4096
            ]
        ]
    where hashsha1 = hash :: B.ByteString -> Digest SHA1
