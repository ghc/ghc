
module T1792_imports (B.putStr, z, zipWith) where

import qualified Data.ByteString as B (putStr, readFile, zip, zipWith)

z = B.readFile
