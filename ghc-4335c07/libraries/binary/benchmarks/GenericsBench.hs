{-# LANGUAGE DeriveGeneric, StandaloneDeriving, BangPatterns #-}
module Main where

import qualified Data.ByteString.Lazy            as L
import           Distribution.PackageDescription

import           Criterion.Main

import qualified Data.Binary                     as Binary
import           Data.Binary.Get                 (Get)
import qualified Data.Binary.Get                 as Binary

import           GenericsBenchCache

main :: IO ()
main = benchmark =<< readPackageDescriptionCache 100

benchmark :: [PackageDescription] -> IO ()
benchmark pds = do
  let lbs = encode pds
      !_ = L.length lbs
      str = show pds
      !_ = length str
  defaultMain [
      bench "encode" (nf encode pds)
    , bench "decode" (nf decode lbs)
    , bench "decode null" (nf decodeNull lbs)
    , bgroup "embarrassment" [
          bench "read" (nf readPackageDescription str)
        , bench "show" (nf show pds)
      ]
    ]

encode :: [PackageDescription] -> L.ByteString
encode = Binary.encode

decode :: L.ByteString -> Int
decode = length . (Binary.decode :: L.ByteString -> [PackageDescription])

decodeNull :: L.ByteString -> ()
decodeNull =
  Binary.runGet $ do
    n <- Binary.get :: Get Int
    go n
  where
    go 0 = return ()
    go i = do
      x <- Binary.get :: Get PackageDescription
      x `seq` go (i-1)

readPackageDescription :: String -> Int
readPackageDescription = length . (read :: String -> [PackageDescription])
