module HeadersByteString (headers) where

import Common (pathTo, rechunkBS)
import Criterion.Main (bench, bgroup, nf, nfIO)
import Criterion.Types (Benchmark)
import HeadersByteString.Atto (request, response)
import Network.Wai.Handler.Warp.RequestHeader (parseHeaderLines)
import qualified Data.Attoparsec.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B

-- Note: In the benchmarks for parsing an http request
-- from a strict bytestring, we consider warp's implementation,
-- which has highly optimized code for handling the first
-- line in particular. It's treatment of the headers
-- is more relaxed from the treatment they are given by the
-- attoparsec parser benchmarked here. Consequently, it
-- is should not be possible to match its performance since
-- it accepts header names with disallowed characters.

headers :: IO Benchmark
headers = do
  req <- B.readFile =<< pathTo "http-request.txt"
  resp <- B.readFile =<< pathTo "http-response.txt"
  let reql    = rechunkBS 4 req
      respl   = rechunkBS 4 resp
  return $ bgroup "headers" [
      bgroup "B" [
        bench "request" $ nf (B.parseOnly request) req
      , bench "warp" $ nfIO (parseHeaderLines (B.split '\n' req))
      , bench "response" $ nf (B.parseOnly response) resp
      ]
    , bgroup "BL" [
        bench "request" $ nf (BL.parse request) reql
      , bench "response" $ nf (BL.parse response) respl
      ]
    ]
