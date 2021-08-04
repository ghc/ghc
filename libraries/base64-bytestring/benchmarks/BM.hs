{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

#if __GLASGOW_HASKELL__ > 702
#if !MIN_VERSION_bytestring(0,10,0)
import Control.DeepSeq (NFData(rnf))
#endif

import Criterion
import Criterion.Main

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
#endif

main :: IO ()
main =
#if __GLASGOW_HASKELL__ < 704
  return ()
#else
  defaultMain
    [ env bs $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "encode"
      [ bgroup "25"
        [ bench "base64" $ whnf B64.encode bs25
        ]
      , bgroup "100"
        [ bench "base64" $ whnf B64.encode bs100
        ]
      , bgroup "1k"
        [ bench "base64" $ whnf B64.encode bs1k
        ]
      , bgroup "10k"
        [ bench "base64" $ whnf B64.encode bs10k
        ]
      , bgroup "100k"
        [ bench "base64" $ whnf B64.encode bs100k
        ]
      , bgroup "1mm"
        [ bench "base64" $ whnf B64.encode bs1mm
        ]
      ]
    , env bs' $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "decode"
      [ bgroup "25"
        [ bench "base64" $ whnf B64.decode bs25
        ]
      , bgroup "100"
        [ bench "base64" $ whnf B64.decode bs100
        ]
      , bgroup "1k"
        [ bench "base64" $ whnf B64.decode bs1k
        ]
      , bgroup "10k"
        [ bench "base64" $ whnf B64.decode bs10k
        ]
      , bgroup "100k"
        [ bench "base64" $ whnf B64.decode bs100k
        ]
      , bgroup "1mm"
        [ bench "base64" $ whnf B64.decode bs1mm
        ]
      ]
    ]
  where
    bss :: BS.ByteString
    bss = "ab%de^ghi*"

    bs = do
      let !a = BS.concat $ replicate 3 bss
          !b = BS.concat $ replicate 10 bss
          !c = BS.concat $ replicate 100 bss
          !d = BS.concat $ replicate 1000 bss
          !e = BS.concat $ replicate 10000 bss
          !f = BS.concat $ replicate 100000 bss
      return (a,b,c,d,e,f)

    bs' = do
      let !a = B64.encode (BS.concat $ replicate 3 bss)
          !b = B64.encode (BS.concat $ replicate 10 bss)
          !c = B64.encode (BS.concat $ replicate 100 bss)
          !d = B64.encode (BS.concat $ replicate 1000 bss)
          !e = B64.encode (BS.concat $ replicate 10000 bss)
          !f = B64.encode (BS.concat $ replicate 100000 bss)
      return (a,b,c,d,e,f)

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData BS.ByteString where
    rnf bs = bs `seq` ()
#endif
#endif
