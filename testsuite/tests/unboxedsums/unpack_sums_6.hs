{-# LANGUAGE BangPatterns #-}
import Data.Time.Clock

import System.Exit

data A = ANothing | AJust {-# UNPACK #-} !Int
data B = BNothing | BJust {-# UNPACK #-} !A
data C = CNothing | CJust {-# UNPACK #-} !B
data D = DNothing | DJust {-# UNPACK #-} !C

data Unlayered = Unlayered {-# UNPACK #-} !D

data Layered = Layered !(Maybe (Maybe (Maybe (Maybe Int))))

makeUnlayered :: Int -> [Unlayered]
makeUnlayered n = Unlayered . DJust . CJust . BJust . AJust <$> [1..n]

makeLayered :: Int -> [Layered]
makeLayered n = Layered . Just . Just . Just . Just <$> [1..n]

sumUnlayered :: [Unlayered] -> Int
sumUnlayered = go 0
 where
  go !n [] = n
  go !n (w:ws) = case w of
    Unlayered (DJust (CJust (BJust (AJust i)))) -> go (n+i) ws
    Unlayered _ -> go n ws

sumLayered :: [Layered] -> Int
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
  case (unlayeredTime `diffUTCTime` now) < (layeredTime `diffUTCTime` now) of
    True -> exitSuccess
    False -> exitFailure
