{- # OPTIONS_GHC -Wall -Werror #-}
module Main (main) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

main :: IO ()
main = (S8.concat (map S.singleton (S.unpack (S8.pack "<foo>"))) == S8.empty) `seq` return ()
