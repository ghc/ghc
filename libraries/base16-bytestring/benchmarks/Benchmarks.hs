module Main
( main
) where


import Criterion
import Criterion.Main

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B

generate :: Int -> B.ByteString
generate n = B.pack . take n . cycle $ [0..255]

main = defaultMain
  [ case bs of
      ~(a,b,c,d,e) -> bgroup "encode"
        [ bench "25" $ whnf B16.encode a
        , bench "100" $ whnf B16.encode b
        , bench "1000" $ whnf B16.encode c
        , bench "10000" $ whnf B16.encode d
        , bench "100000" $ whnf B16.encode e
        ]
  , case bs of
      ~(a,b,c,d,e) -> bgroup "decode"
        [ bench "25" $ whnf B16.decode a
        , bench "100" $ whnf B16.decode b
        , bench "1000" $ whnf B16.decode c
        , bench "10000" $ whnf B16.decode d
        , bench "100000" $ whnf B16.decode e
        ]
  ]
  where
    bs =
      let a = generate 25
          b = generate 100
          c = generate 1000
          d = generate 10000
          e = generate 100000
      in (a,b,c,d,e)

    bs' =
      let a = generate 25
          b = generate 100
          c = generate 1000
          d = generate 10000
          e = generate 100000
          f = B16.encode
      in (f a, f b, f c, f d, f e)
