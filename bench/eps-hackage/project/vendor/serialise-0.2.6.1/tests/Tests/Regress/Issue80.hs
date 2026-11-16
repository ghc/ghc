module Tests.Regress.Issue80 ( testTree ) where

import qualified Data.Vector.Storable       as S
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Codec.Serialise as Serialise
import           Test.Tasty
import           Test.Tasty.HUnit

repro :: Bool
repro =
    let evilChunker bs i =
            -- Split strict bytestring into chunks and return as lazy one
            let (b1, b2) = BS.splitAt i bs in BL.fromChunks [b1, b2]
        -- Test case
        value = [S.replicate 128 (0 :: Double)]
        serialised  = (BS.concat . BL.toChunks . Serialise.serialise) value
        deserialised = Serialise.deserialise . evilChunker serialised
    in all (\i -> value == deserialised i) [1 .. BS.length serialised - 1]

testTree :: TestTree
testTree =
    testGroup "Issue 80 - Vector chunkingd"
        [ testCase "simple reproduction case"   (True @=? repro) ]
