{-# LANGUAGE BangPatterns, OverloadedStrings, RankNTypes #-}

module Main (
  main
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import System.IO (hFlush, stdout)
import Timer (timer)

type BM = Text -> ()

bm :: forall a. (Text -> a) -> BM
bm f t = f t `seq` ()

benchmarks :: [(String, Text.Text -> ())]
benchmarks = [
    ("find_first", bm $ Text.isInfixOf "en:Benin")
  , ("find_index", bm $ Text.findIndex (=='c'))
  ]

main :: IO ()
main = do
  !contents <- decodeUtf8 `fmap` B.readFile "../tests/text-test-data/yiwiki.xml"
  forM_ benchmarks $ \(name, bmark) -> do
    putStr $ name ++ " "
    hFlush stdout
    putStrLn =<< (timer 100 contents bmark)
