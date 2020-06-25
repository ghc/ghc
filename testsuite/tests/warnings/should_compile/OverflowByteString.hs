{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

main :: IO ()
main = do
    BS.putStr "\331"
    LBS.putStr "\331"
    BS.putStr $ SBS.fromShort "\331"
    putStrLn ""
