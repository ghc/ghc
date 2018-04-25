{-# LANGUAGE PatternGuards #-}
module Main (main) where

import qualified Data.Map as M
import Data.Map (Map)

import System.Environment
import System.IO
import Text.Regex
import Data.List
import Control.Monad
import Data.Maybe

type RawResults = Map (String,Int) [Double]

main = do
  [time,speedup] <- getArgs
  s <- getContents
  let raw = slurp_raw (lines s)
      avg = M.map calc_average raw  

  let keys = M.keys avg
      modes = nub (map fst keys)
      cores = sort (nub (map snd keys))
  
  h <- openFile time WriteMode
  forM modes $ \m -> do
   hPutStrLn h (m ++ concatMap (',':) [ show (fromJust (M.lookup (m,n) avg))
                                      | n <- cores ])
  hClose h

  let
      baselines = M.fromList [ (m, fromJust (M.lookup (m,1) avg)) | m <- modes ]
      spd = M.mapWithKey (calc_speedup baselines) avg

  h <- openFile speedup WriteMode
  forM modes $ \m -> do
   hPutStrLn h (m ++ concatMap (',':) [ show (fromJust (M.lookup (m,n) spd))
                                      | n <- cores ])
  hClose h

calc_average :: [Double] -> Double
calc_average = foldl f 0 . zip [(1::Int)..]
  where f mean (i,x) = mean + (x - mean) / fromIntegral i

calc_speedup :: Map String Double -> (String,Int) -> Double -> Double
calc_speedup baselines (mode,_) result = base / result
  where Just base = M.lookup mode baselines

slurp_raw :: [String] -> RawResults
slurp_raw lines = add_results ("",1) M.empty lines
  where
    add_results m results [] = results
    add_results m results (line:lines)
      | Just [mode,cores] <- matchRegex header line
      = add_results (mode, read cores :: Int) results lines
      | Just [time] <- matchRegex result line
      = let t = read time :: Double in
        add_results m (M.insertWith (\_ o -> t:o) m [t] results) lines
      | otherwise
      = add_results m results lines

header = mkRegex "run\\.([^\\.]*)\\.N([0-9]*)"
result = mkRegex "time: ([0-9\\.]+)s"
