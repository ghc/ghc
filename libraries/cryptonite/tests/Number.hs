{-# LANGUAGE OverloadedStrings #-}
module Number (tests) where

import Imports

import Data.ByteArray (Bytes)
import qualified Data.ByteArray as B
import Crypto.Number.Basic
import Crypto.Number.Generate
import qualified Crypto.Number.Serialize    as BE
import qualified Crypto.Number.Serialize.LE as LE
import Crypto.Number.Prime
import Crypto.Number.ModArithmetic
import Data.Bits

serializationVectors :: [(Int, Integer, ByteString)]
serializationVectors =
    [ (128, 468189858948067662094510918729062682059955669513914188715630930503497261316361784677177564296207557978182700664806717692596876084916561811001371208806217360635705059859428069669992937334724312890015700331031248133952795914192719979937664050389500162437642525331653766885896869239678885404647468665996400635, "\x00\xaa\xae\x74\xc8\xec\x3c\x36\x06\x5e\x46\xca\x8e\x57\xab\x09\x87\xfd\xcd\x1f\xa4\xe7\xf9\xd2\x60\xd5\x4a\x1b\x74\xdc\xa8\x75\xd8\xdd\xff\x2b\x74\x28\x14\x59\x67\x6c\x82\xae\xa3\xa5\x1d\x3f\xb4\xb7\xfe\x5c\xd2\xf0\x7f\xd8\xd9\xa9\xb0\xce\x26\xc1\x26\x74\x96\xf5\xf6\x4c\x8f\x66\x7f\x5d\xf1\x68\x38\xd4\x03\x62\xe9\x30\xc8\xa1\xc1\x84\x97\x62\x20\xfd\xd7\x03\x35\xc1\x25\x45\x1b\x86\x81\x3d\xa4\x92\xc0\xd3\xdd\xfa\x86\x1d\xdf\x0a\xbb\xf4\xc0\x56\xf7\xa2\xb0\x3b\x52\xf7\xa5\x89\x4c\x69\x34\x91\x46\xd9\x57\xfb")
    , (128, 40031303476923779996794876613623495515025748694978019540894726181695410095832601107261950025830235596060960914255795497479135806963313279476038687192202016132891881954743054164975707083302554941058329647014950354509055121290280892911153779672733723699997592027662953953692834215577119173225643193201177329, "\x00\x0e\x97\xf9\xd5\x79\xb9\x90\x7c\x85\x48\x49\x01\x19\x64\xfb\x76\x31\xcd\x51\xfb\x8a\x9d\x55\xe5\xd3\x7b\x87\x2d\xad\x63\x2d\x6b\x1c\x84\x3f\x65\x95\xb6\xf3\x1a\xa9\x43\x3f\x06\x46\x7b\xf8\xf3\x35\x45\x84\x11\x56\x91\x53\x43\xd7\xe1\x6d\x80\x64\x14\x45\x35\x4e\x93\x7d\x5e\x48\xec\xe0\x79\x7b\x44\x8e\xab\x0f\xc4\x5f\xc6\xa1\x71\xee\x37\xb1\x55\x51\x98\x44\x57\xe3\xc3\x56\x3a\x50\x27\xaf\xa5\x1d\x1a\x0a\x90\x19\x0d\x14\xed\x3d\x93\x40\x62\x76\xa3\xaa\x00\x23\x86\xca\x98\xb2\x6e\x02\x43\xa7\xbc\xb1\xb2\xf1")
    , (128, 75152325976543603337003024341071663845101857195436434620947904288957274825323005869230041326941600298094896018190395352332646796347130114769768242670539699217743549573961461985255265474392937773768121046339453584830072421569334022498680626938734088755136253492360177084153487115846920446085149631919580041, "\x00\x1b\x65\xb1\x73\x74\xed\xd2\xcb\xb8\xf3\x6b\x3f\xc2\x05\xaa\x91\xab\x48\x5b\x03\x30\xae\x24\xa3\xec\x7a\x6a\xf0\x34\x73\x18\x04\xea\xe4\xd6\x19\x97\xc4\xc1\x13\x7d\x12\x0d\xd5\xcb\xbd\x18\x05\xc2\xce\x87\x66\x84\x12\xe8\x24\xa3\x31\x69\xfa\xf4\x2c\x21\x53\xa6\x04\x74\x78\xc4\x93\x0d\x38\x7f\x28\xfe\x80\x8e\xd2\x7b\x20\xc8\xf5\x1f\x0f\x73\x68\xb2\xe5\x08\xf1\x94\xa1\xe6\xcf\x3a\x2c\x12\x63\xda\x08\x3a\x78\x12\xb8\x11\x23\x3c\x38\x38\x10\x94\x2b\xac\x64\x5d\x67\x0c\xb6\x0d\xc3\x9a\x45\x39\x50\x8a\x63\x89")
    , (128, 132094272981815297755209818914225029878347650582749561568514551350741192910991391836297682842650690115955454061006435646226436379226218676796260483719213285072886626400953065229934239690821114513313427305727000011361769875430428291375851099221794646192854831002408178061474948738788927399080262963320752452, "\x00\x30\x27\xe0\xbf\x46\xec\x77\x2d\xc6\x06\x77\xbc\x68\x87\x3c\x1b\x2e\xc7\xb7\x6c\x88\x25\xec\x8c\x95\xbf\x74\xe5\x37\x01\x25\x96\xe1\x70\x33\x5c\x7d\xab\x1f\xc2\x9c\xad\xf7\xca\x26\x85\x2d\xfc\x8f\xc7\xab\x49\x28\xa4\x47\xe6\xd5\x6e\xfa\x0a\xbb\x57\xe4\xa2\x51\xc7\xc6\x12\x0f\xa9\x98\x69\xb8\x05\x84\xc5\xe3\x28\x86\x0f\x54\x1d\xf9\x92\x42\x9f\xb1\x77\x2b\x58\x89\xe2\xfc\x22\xb0\x1e\x71\x78\xea\x39\xc1\x87\x4f\xd4\x83\x2c\x96\x1d\xea\xd5\xf9\xf9\xb9\x7b\x86\xfa\xf6\xad\x5b\xb1\x3c\xe7\x11\xd7\x96\x89\x44")
    , (128, 577245873336454863811643140721674509319073059708446946821011267146688442860798353087462545395033001525475835015592425207995480357299993009193426638306801669333644226765032464458284920004140299209138389393494751627076239104390434285377314678827349631962212281858308570255468721491493027423799738158196939966, "\x00\xd2\x70\x41\xdb\x3d\xb5\xfe\x8c\xef\x79\xcf\x5b\x7b\x37\xb0\x05\xb8\x5a\x9b\x7d\x01\x28\xc7\xf5\x5a\x02\xba\xce\xbc\xf5\x8e\x91\x59\xd0\x42\x6f\x04\x82\x4b\x78\xb0\xdd\x91\x2e\x15\x9d\xea\x4f\x0c\x21\xc0\x67\x54\xa2\x39\xa8\xe1\x13\x8f\xa9\xff\x46\x2d\x11\x56\x04\xa0\xde\x64\xc8\x0f\xf4\x2c\xd2\x31\xdf\x2a\xfd\xac\xc7\x25\x58\xc8\xea\xfd\x47\x6e\xdd\x2a\x53\x02\x77\x49\xa7\x0d\x18\xfb\x05\x18\x4b\x28\xd3\xa2\x39\x8c\x83\x80\x90\xd1\xa8\x81\x56\x6f\xd1\x94\x9d\x65\x34\x95\x79\xc1\x27\xbc\x76\xc3\x5c\xbe")
    ]

tests = testGroup "number"
    [ testProperty "num-bits" $ \(Int1_2901 i) ->
        and [ (numBits (2^i-1) == i)
            , (numBits (2^i) == i+1)
            , (numBits (2^i + (2^i-1)) == i+1)
            ]
    , testProperty "num-bits2" $ \(Positive i) ->
        not (i `testBit` numBits i) && (i `testBit` (numBits i - 1))
    , testProperty "generate-param" $ \testDRG (Int1_2901 bits)  ->
        let r = withTestDRG testDRG $ generateParams bits (Just SetHighest) False
         in r >= 0 && numBits r == bits && testBit r (bits-1)
    , testProperty "generate-param2" $ \testDRG (Int1_2901 m1bits) ->
        let bits = m1bits + 1 -- make sure minimum is 2
            r = withTestDRG testDRG $ generateParams bits (Just SetTwoHighest) False
         in r >= 0 && numBits r == bits && testBit r (bits-1) && testBit r (bits-2)
    , testProperty "generate-param-odd" $ \testDRG (Int1_2901 bits) ->
        let r = withTestDRG testDRG $ generateParams bits Nothing True
         in r >= 0 && odd r
    , testProperty "generate-range" $ \testDRG (Positive range) ->
        let r = withTestDRG testDRG $ generateMax range
         in 0 <= r && r < range
    , testProperty "generate-prime" $ \testDRG (Int0_2901 baseBits') ->
        let baseBits = baseBits' `mod` 800
            bits  = 5 + baseBits -- generating lower than 5 bits causes an error ..
            prime = withTestDRG testDRG $ generatePrime bits
         in bits == numBits prime
    , testProperty "generate-safe-prime" $ \testDRG (Int0_2901 baseBits') ->
        let baseBits = baseBits' `mod` 200
            bits = 6 + baseBits
            prime = withTestDRG testDRG $ generateSafePrime bits
         in bits == numBits prime
    , testProperty "as-power-of-2-and-odd" $ \n ->
        let (e, a1) = asPowerOf2AndOdd n
         in n == (2^e)*a1
    , testProperty "squareRoot" $ \testDRG (Int0_2901 baseBits') -> do
        let baseBits = baseBits' `mod` 500
            bits = 5 + baseBits -- generating lower than 5 bits causes an error ..
            p = withTestDRG testDRG $ generatePrime bits
        g <- choose (1, p - 1)
        let square x = (x * x) `mod` p
            r = square <$> squareRoot p g
        case jacobi g p of
            Just   1  -> return $ Just g `assertEq` r
            Just (-1) -> return $ Nothing `assertEq` r
            _         -> error "invalid jacobi result"
    , testProperty "marshalling-be" $ \qaInt ->
        getQAInteger qaInt == BE.os2ip (BE.i2osp (getQAInteger qaInt) :: Bytes)
    , testProperty "marshalling-le" $ \qaInt ->
        getQAInteger qaInt == LE.os2ip (LE.i2osp (getQAInteger qaInt) :: Bytes)
    , testProperty "be-rev-le" $ \qaInt ->
        getQAInteger qaInt == LE.os2ip (B.reverse (BE.i2osp (getQAInteger qaInt) :: Bytes))
    , testProperty "be-rev-le-40" $ \qaInt ->
        getQAInteger qaInt == LE.os2ip (B.reverse (BE.i2ospOf_ 40 (getQAInteger qaInt) :: Bytes))
    , testProperty "le-rev-be" $ \qaInt ->
        getQAInteger qaInt == BE.os2ip (B.reverse (LE.i2osp (getQAInteger qaInt) :: Bytes))
    , testProperty "le-rev-be-40" $ \qaInt ->
        getQAInteger qaInt == BE.os2ip (B.reverse (LE.i2ospOf_ 40 (getQAInteger qaInt) :: Bytes))
    , testGroup "marshalling-kat-to-bytearray" $ zipWith toSerializationKat [katZero..] serializationVectors
    , testGroup "marshalling-kat-to-integer" $ zipWith toSerializationKatInteger [katZero..] serializationVectors
    ]
  where
    toSerializationKat i (sz, n, ba) = testCase (show i) (ba @=? BE.i2ospOf_ sz n)
    toSerializationKatInteger i (_, n, ba) = testCase (show i) (n @=? BE.os2ip ba)
