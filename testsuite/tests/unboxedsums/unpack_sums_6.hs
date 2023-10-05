{-# LANGUAGE BangPatterns #-}
-- This perhaps overly simple test check if code involving
-- unbacked sums is faster than non-unpacked ones which at
-- least in this case we expect to be the case.
-- However this test isn't quite robust, should it fail in
-- the future we might want to redo it or mark it fragile.
import Data.Time.Clock

import Data.Int
import System.Exit

data A = ANothing | AJust {-# UNPACK #-} !Int64
data B = BNothing | BJust {-# UNPACK #-} !A
data C = CNothing | CJust {-# UNPACK #-} !B
data D = DNothing | DJust {-# UNPACK #-} !C

data Unlayered = Unlayered {-# UNPACK #-} !D

data Layered = Layered !(Maybe (Maybe (Maybe (Maybe Int64))))

makeUnlayered :: Int64 -> [Unlayered]
makeUnlayered n = Unlayered . DJust . CJust . BJust . AJust <$> [1..n]

makeLayered :: Int64 -> [Layered]
makeLayered n = Layered . Just . Just . Just . Just <$> [1..n]

sumUnlayered :: [Unlayered] -> Int64
sumUnlayered = go 0
 where
  go !n [] = n
  go !n (w:ws) = case w of
    Unlayered (DJust (CJust (BJust (AJust i)))) -> go (n+i) ws
    Unlayered _ -> go n ws

sumLayered :: [Layered] -> Int64
sumLayered = go 0
 where
  go !n [] = n
  go !n (w:ws) = case w of
    Layered (Just (Just (Just (Just i)))) -> go (n+i) ws
    Layered _ -> go n ws

main :: IO ()
main = do
  let magnitude = 10000000
      unlayeredInts = makeUnlayered magnitude
      layeredInts = makeLayered magnitude
  now <- getCurrentTime
  print $ sumUnlayered unlayeredInts
  unlayeredTime <- getCurrentTime
  print $ sumLayered layeredInts
  layeredTime <- getCurrentTime
  case (unlayeredTime `diffUTCTime` now) < (layeredTime `diffUTCTime` unlayeredTime) of
    True -> exitSuccess
    False -> exitFailure
