{-# LANGUAGE OverloadedStrings #-}
module Salsa (tests) where

import qualified Data.ByteString as B
import qualified Crypto.Cipher.Salsa as Salsa

import           Imports

type Vector = (Int, B.ByteString, B.ByteString, [(Int, B.ByteString)])

vectors :: [Vector]
vectors =
    [ (20, key, iv
      , [ (0, "\x99\xA8\xCC\xEC\x6C\x5B\x2A\x0B\x6E\x33\x6C\xB2\x06\x52\x24\x1C\x32\xB2\x4D\x34\xAC\xC0\x45\x7E\xF6\x79\x17\x8E\xDE\x7C\xF8\x05\x80\x5A\x93\x05\xC7\xC4\x99\x09\x68\x3B\xD1\xA8\x03\x32\x78\x17\x62\x7C\xA4\x6F\xE8\xB9\x29\xB6\xDF\x00\x12\xBD\x86\x41\x83\xBE")
        , (192, "\x2D\x22\x6C\x11\xF4\x7B\x3C\x0C\xCD\x09\x59\xB6\x1F\x59\xD5\xCC\x30\xFC\xEF\x6D\xBB\x8C\xBB\x3D\xCC\x1C\xC2\x52\x04\xFC\xD4\x49\x8C\x37\x42\x6A\x63\xBE\xA3\x28\x2B\x1A\x8A\x0D\x60\xE1\x3E\xB2\xFE\x59\x24\x1A\x9F\x6A\xF4\x26\x68\x98\x66\xED\xC7\x69\xE1\xE6\x48\x2F\xE1\xC1\x28\xA1\x5C\x11\x23\xB5\x65\x5E\xD5\x46\xDF\x01\x4C\xE0\xC4\x55\xDB\xF5\xD3\xA1\x3D\x9C\xD4\xF0\xE2\xD1\xDA\xB9\xF1\x2F\xB6\x8C\x54\x42\x61\xD7\xF8\x8E\xAC\x1C\x6C\xBF\x99\x3F\xBB\xB8\xE0\xAA\x85\x10\xBF\xF8\xE7\x38\x35\xA1\xE8\x6E\xAD\xBB")
        , (448, "\x05\x97\x18\x8A\x1C\x19\x25\x57\x69\xBE\x1C\x21\x03\x99\xAD\x17\x2E\xB4\x6C\x52\xF9\x2F\xD5\x41\xDF\x2E\xAD\x71\xB1\xFF\x8E\xA7\xAD\xD3\x80\xEC\x71\xA5\xFD\x7A\xDB\x51\x81\xEA\xDD\x18\x25\xEC\x02\x77\x9A\x45\x09\xBE\x58\x32\x70\x8C\xA2\x83\x6C\x16\x93\xA5")
        ])
    , (20
      , "\x00\x53\xA6\xF9\x4C\x9F\xF2\x45\x98\xEB\x3E\x91\xE4\x37\x8A\xDD\x30\x83\xD6\x29\x7C\xCF\x22\x75\xC8\x1B\x6E\xC1\x14\x67\xBA\x0D"
      , "\x0D\x74\xDB\x42\xA9\x10\x77\xDE"
      , [ (0, "\xF5\xFA\xD5\x3F\x79\xF9\xDF\x58\xC4\xAE\xA0\xD0\xED\x9A\x96\x01\xF2\x78\x11\x2C\xA7\x18\x0D\x56\x5B\x42\x0A\x48\x01\x96\x70\xEA\xF2\x4C\xE4\x93\xA8\x62\x63\xF6\x77\xB4\x6A\xCE\x19\x24\x77\x3D\x2B\xB2\x55\x71\xE1\xAA\x85\x93\x75\x8F\xC3\x82\xB1\x28\x0B\x71")
        , (65472, "\xB7\x0C\x50\x13\x9C\x63\x33\x2E\xF6\xE7\x7A\xC5\x43\x38\xA4\x07\x9B\x82\xBE\xC9\xF9\xA4\x03\xDF\xEA\x82\x1B\x83\xF7\x86\x07\x91\x65\x0E\xF1\xB2\x48\x9D\x05\x90\xB1\xDE\x77\x2E\xED\xA4\xE3\xBC\xD6\x0F\xA7\xCE\x9C\xD6\x23\xD9\xD2\xFD\x57\x58\xB8\x65\x3E\x70\x81\x58\x2C\x65\xD7\x56\x2B\x80\xAE\xC2\xF1\xA6\x73\xA9\xD0\x1C\x9F\x89\x2A\x23\xD4\x91\x9F\x6A\xB4\x7B\x91\x54\xE0\x8E\x69\x9B\x41\x17\xD7\xC6\x66\x47\x7B\x60\xF8\x39\x14\x81\x68\x2F\x5D\x95\xD9\x66\x23\xDB\xC4\x89\xD8\x8D\xAA\x69\x56\xB9\xF0\x64\x6B\x6E")
        , (131008, "\xA1\x3F\xFA\x12\x08\xF8\xBF\x50\x90\x08\x86\xFA\xAB\x40\xFD\x10\xE8\xCA\xA3\x06\xE6\x3D\xF3\x95\x36\xA1\x56\x4F\xB7\x60\xB2\x42\xA9\xD6\xA4\x62\x8C\xDC\x87\x87\x62\x83\x4E\x27\xA5\x41\xDA\x2A\x5E\x3B\x34\x45\x98\x9C\x76\xF6\x11\xE0\xFE\xC6\xD9\x1A\xCA\xCC")
        ])
    ]
  where
        key :: B.ByteString
        key = "\xEA\xEB\xEC\xED\xEE\xEF\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFB\xFC\xFD\xFE\xFF\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09"

        iv  = B.replicate 8 0

newtype RandomVector = RandomVector Vector
    deriving (Show,Eq)

instance Arbitrary RandomVector where
    arbitrary = RandomVector <$> elements vectors

tests = testGroup "Salsa"
    [ testGroup "KAT" $
        zipWith (\i (r,k,n,e) -> testCase (show (i :: Int)) $ salsaRunSimple e r k n) [1..] vectors
    , testProperty "generate-combine" salsaGenerateCombine
    , testProperty "chunking-generate" salsaGenerateChunks
    , testProperty "chunking-combine" salsaCombineChunks
    ]
  where
        salsaRunSimple expected rounds key nonce =
            let salsa = Salsa.initialize rounds key nonce
             in map snd expected @=? salsaLoop 0 salsa expected

        salsaLoop _       _     [] = []
        salsaLoop current salsa (r@(ofs,expectBs):rs)
            | current < ofs  =
                let (_, salsaNext) = Salsa.generate salsa (ofs - current) :: (ByteString, Salsa.State)
                 in salsaLoop ofs salsaNext (r:rs)
            | current == ofs =
                let (e, salsaNext) = Salsa.generate salsa (B.length expectBs)
                 in e : salsaLoop (current + B.length expectBs) salsaNext rs
            | otherwise = error "internal error in salsaLoop"

        salsaGenerateCombine :: ChunkingLen0_127 -> RandomVector -> Int0_2901 -> Bool
        salsaGenerateCombine (ChunkingLen0_127 ckLen) (RandomVector (rounds, key, iv, _)) (Int0_2901 nbBytes) =
            let initSalsa    = Salsa.initialize rounds key iv
             in loop nbBytes ckLen initSalsa
          where loop n []     salsa = loop n ckLen salsa
                loop 0 _      _     = True
                loop n (x:xs) salsa =
                    let len        = min x n
                        (c1, next) = Salsa.generate salsa len
                        (c2, _)    = Salsa.combine salsa (B.replicate len 0)
                     in if c1 == c2 then loop (n - len) xs next else False

        salsaGenerateChunks :: ChunkingLen -> RandomVector -> Bool
        salsaGenerateChunks (ChunkingLen ckLen) (RandomVector (rounds, key, iv, _)) =
            let initSalsa    = Salsa.initialize rounds key iv
                nbBytes      = 1048
                (expected,_) = Salsa.generate initSalsa nbBytes
                chunks       = loop nbBytes ckLen (Salsa.initialize rounds key iv)
             in expected == B.concat chunks

          where loop n []     salsa = loop n ckLen salsa
                loop 0 _      _     = []
                loop n (x:xs) salsa =
                    let len       = min x n
                        (c, next) = Salsa.generate salsa len
                     in c : loop (n - len) xs next

        salsaCombineChunks :: ChunkingLen -> RandomVector -> ArbitraryBS0_2901 -> Bool
        salsaCombineChunks (ChunkingLen ckLen) (RandomVector (rounds, key, iv, _)) (ArbitraryBS0_2901 wholebs) =
            let initSalsa    = Salsa.initialize rounds key iv
                (expected,_) = Salsa.combine initSalsa wholebs
                chunks       = loop wholebs ckLen initSalsa
             in expected `propertyEq` B.concat chunks

          where loop bs []     salsa = loop bs ckLen salsa
                loop bs (x:xs) salsa
                    | B.null bs = []
                    | otherwise =
                        let (bs1, bs2) = B.splitAt (min x (B.length bs)) bs
                            (c, next)  = Salsa.combine salsa bs1
                         in c : loop bs2 xs next
