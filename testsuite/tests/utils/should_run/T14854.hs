{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import GHC.Data.FastString

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as Char
import Data.ByteString.Lazy (toStrict)
import Data.List
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Time
import GHC.Conc
import System.IO
import System.Random
import Text.Printf

data Options = Options
  { optThreads :: Int   -- ^ the number of threads to run concurrently
  , optRepeat :: Int    -- ^ how many times do we create the same 'FastString'
  , optCount :: Int     -- ^ the total number of different 'FastString's
  , optPrefix :: Int    -- ^ the length of prefix in each 'FastString'
  }

defOptions :: Options
defOptions = Options
  { optThreads = 8
  , optRepeat = 16
  , optCount = 10000
  , optPrefix = 0
  }

run :: [[ByteString]] -> (ByteString -> Int) -> IO Int
run jobs op = do
  mvars <- forM ([0 ..] `zip` jobs) $ \(i, job) -> do
    mvar <- newEmptyMVar
    forkOn i $ do
      uniq <- evaluate $ force $ maximum $ map op job
      putMVar mvar uniq
    return mvar
  uniqs <- mapM takeMVar mvars
  evaluate $ force $ maximum uniqs - 603979775

summary :: IO [[[a]]] -> IO Int
summary getTable = do
  table <- getTable
  evaluate $ force $ length $ concat $ concat table

timeIt :: String -> IO a -> IO a
timeIt name io = do
  before <- getCurrentTime
  ret <- io
  after <- getCurrentTime
  hPrintf stderr "%s: %.2fms\n" name
    (realToFrac $ diffUTCTime after before * 1000 :: Double)
  return ret

main :: IO ()
main = do
  seed <- randomIO
  let Options{..} = defOptions
      shuffle (i:is) s
        | Seq.null s = []
        | otherwise = m: shuffle is (l <> r)
        where
          (l, m Seq.:< r) = Seq.viewl <$> Seq.splitAt (i `rem` Seq.length s) s
      inputs =
        shuffle (randoms $ mkStdGen seed) $
        mconcat $ replicate optRepeat $
        Seq.fromFunction optCount $ \i -> toStrict $ toLazyByteString $
          byteString (Char.replicate optPrefix '_') <> intDec i
  jobs <- evaluate $ force $ transpose $
    map (take optThreads) $
    takeWhile (not . null) $
    iterate (drop optThreads) inputs
  setNumCapabilities (length jobs)
  -- The maximum unique may be greater than 'optCount'
  u <- timeIt "run" $ run jobs $ uniqueOfFS . mkFastStringByteString
  print $ optCount <= u && u <= min optThreads optRepeat * optCount
  -- But we should never have duplicate 'FastString's in the table
  n <- timeIt "summary" $ summary getFastStringTable
  print $ n == optCount
