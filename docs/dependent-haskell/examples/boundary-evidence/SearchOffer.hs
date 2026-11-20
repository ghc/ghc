{-# LANGUAGE NoExperimentalDependentTypes #-}

module SearchOffer (main) where

import Data.Bits (shiftR)
import Data.Word (Word32)
import Numeric.Natural (Natural)
import OrdinaryOffer (offered)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | Write a candidate position to stdout and its replay data to stderr.
main :: IO ()
main = do
    args <- getArgs
    case traverse readMaybe args of
        Just [seed, input, target] -> search seed input target
        _ -> die "usage: search-offer SEED INPUT LABEL"

-- | Sample at most two positions with replacement and observe their labels.
search :: Natural -> Natural -> Natural -> IO ()
search seed input target =
    case break ((== target) . snd) trials of
        (misses, candidate@(position, _) : _) -> do
            hPutStrLn stderr (report (misses ++ [candidate]))
            putStr (show position)
        (misses, []) -> die (report misses ++ " no candidate")
  where
    labels = offered input
    count = length labels
    trials =
        [ (position, label)
        | count > 0
        , state <- take 2 (iterate advance (advance (fromIntegral seed)))
        , let position = fromIntegral (state `shiftR` 16) `mod` count
        , label <- take 1 (drop position labels)
        ]
    report observed =
        unwords
            [ "seed=" ++ show seed
            , "input=" ++ show input
            , "label=" ++ show target
            , "offer=" ++ show labels
            , "tried=" ++ show observed
            ]

    -- Use fixed-width arithmetic so a seed has the same draws on every host.
    advance :: Word32 -> Word32
    advance state = 1664525 * state + 1013904223
