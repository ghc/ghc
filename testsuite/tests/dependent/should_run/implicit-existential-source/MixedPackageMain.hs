{-# LANGUAGE ImpredicativeTypes, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.IORef
import MixedPackageProvider

named :: Mixed
named = inner

applied :: Mixed
applied = returnInner inner

bottomPayload :: Mixed
bottomPayload = lazyInner

readViaBar :: Mixed -> Int
readViaBar p = readInner p

readViaProducer :: Bool -> Int
readViaProducer flag = readInner (chooseMixed flag)

readInsideQuickLook :: Bool -> Int
readInsideQuickLook flag = id ((readInner) (chooseMixed flag))

ignoreViaBar :: Mixed -> Bool
ignoreViaBar p = ignoreInner p

ignoreViaProducer :: Bool -> Bool
ignoreViaProducer flag = ignoreInner (chooseMixed flag)

readViaRank :: RankedMixed -> (Int, Bool)
readViaRank p = readRankInner p

main :: IO ()
main = do
  print (readMixed named, readMixed applied, readMixed written, readKind kindMixed)
  print (ignoreMixed bottomPayload, ignoreMixed (error "whole package" :: Mixed))
  result <- try (evaluate (readMixed bottomPayload))
  print (either (\(ErrorCall message) -> message == "mixed payload") (const False) result)
  flag <- newIORef True
  input <- readIORef flag
  let first = readViaProducer input
  if first == readViaBar (chooseMixed input)
    then pure ()
    else fail "Different result through a named mixed package"
  writeIORef flag False
  second <- readInsideQuickLook <$> readIORef flag
  print (first, second, readViaRank rankedMixed, readKindInner kindMixed)
  print (ignoreViaBar (error "unused package") && ignoreViaProducer (error "unused producer input"))
