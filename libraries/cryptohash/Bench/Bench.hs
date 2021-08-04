{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.MD2 as MD2
import qualified Crypto.Hash.MD4 as MD4
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA224 as SHA224
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA384 as SHA384
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Hash.SHA512t as SHA512t
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160
import qualified Crypto.Hash.Tiger as Tiger
import qualified Crypto.Hash.Skein256 as Skein256
import qualified Crypto.Hash.Skein512 as Skein512
import qualified Crypto.Hash.Whirlpool as Whirlpool

hashmany (i,u,f) = f . foldl u i

allHashs =
    [ ("MD2",MD2.hash, hashmany (MD2.init,MD2.update,MD2.finalize))
    , ("MD4",MD4.hash, hashmany (MD4.init,MD4.update,MD4.finalize))
    , ("MD5",MD5.hash, hashmany (MD5.init,MD5.update,MD5.finalize))
    , ("SHA1",SHA1.hash, hashmany (SHA1.init,SHA1.update,SHA1.finalize))
    , ("SHA2-224",SHA224.hash, hashmany (SHA224.init,SHA224.update,SHA224.finalize))
    , ("SHA2-256",SHA256.hash, hashmany (SHA256.init,SHA256.update,SHA256.finalize))
    , ("SHA2-384",SHA384.hash, hashmany (SHA384.init,SHA384.update,SHA384.finalize))
    , ("SHA2-512",SHA512.hash, hashmany (SHA512.init,SHA512.update,SHA512.finalize))
    , ("SHA2-512t-512",SHA512t.hash 512, hashmany (SHA512t.init 512,SHA512t.update,SHA512t.finalize))
    , ("SHA3-224",SHA3.hash 224, hashmany (SHA3.init 224,SHA3.update,SHA3.finalize))
    , ("SHA3-256",SHA3.hash 256, hashmany (SHA3.init 256,SHA3.update,SHA3.finalize))
    , ("SHA3-384",SHA3.hash 384, hashmany (SHA3.init 384,SHA3.update,SHA3.finalize))
    , ("SHA3-512",SHA3.hash 512, hashmany (SHA3.init 512,SHA3.update,SHA3.finalize))
    , ("RIPEMD160",RIPEMD160.hash, hashmany (RIPEMD160.init,RIPEMD160.update,RIPEMD160.finalize))
    , ("Tiger",Tiger.hash, hashmany (Tiger.init,Tiger.update,Tiger.finalize))
    , ("Skein256-256",Skein256.hash 256, hashmany (Skein256.init 256,Skein256.update,Skein256.finalize))
    , ("Skein512-512",Skein512.hash 512, hashmany (Skein512.init 512,Skein512.update,Skein512.finalize))
    , ("Whirlpool",Whirlpool.hash, hashmany (Whirlpool.init,Whirlpool.update,Whirlpool.finalize))
    ]

benchHash :: a -> (a -> B.ByteString) -> Pure
benchHash bs f = whnf f bs

withHashesFilter out f = map f $ filter (\(n,_,_) -> not (n `elem` out)) allHashs
withHashes f = map f allHashs

main = do
    let !bs32     = B.replicate 32 0
        !bs256    = B.replicate 256 0
        !bs4096   = B.replicate 4096 0
        !bs1M     = B.replicate (1*1024*1024) 0
    let !lbs64x256 = (map (const (B.replicate 64 0)) [0..3])
        !lbs64x4096 = (map (const (B.replicate 64 0)) [0..63])
    defaultMain
        [ bgroup "hash-32b" (withHashes (\(name, f,_) -> bench name $ benchHash bs32 f))
        , bgroup "hash-256b" (withHashes (\(name, f,_) -> bench name $ benchHash bs256 f))
        , bgroup "hash-4Kb" (withHashes (\(name, f,_) -> bench name $ benchHash bs4096 f))
        , bgroup "hash-1Mb" (withHashesFilter ["MD2"] (\(name, f,_) -> bench name $ benchHash bs1M f))
        , bgroup "iuf-64x256" (withHashes (\(name, _,f) -> bench name $ benchHash lbs64x256 f))
        , bgroup "iuf-64x4096" (withHashes (\(name, _,f) -> bench name $ benchHash lbs64x4096 f))
        ]
