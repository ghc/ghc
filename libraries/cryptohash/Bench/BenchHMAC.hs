
import Criterion.Main
import Crypto.Hash
import qualified Data.ByteString as B
import Data.Byteable

main = do
    let b32 = B.replicate 32 0
    defaultMain
        [ bench "hmac-md5" $ whnf (toBytes . hmacAlg MD5 b32) b32
        , bench "hmac-sha1" $ whnf (toBytes . hmacAlg SHA1 b32) b32
        ]
