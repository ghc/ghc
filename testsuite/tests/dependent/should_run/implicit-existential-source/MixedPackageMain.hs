{-# LANGUAGE ImpredicativeTypes, NoExperimentalDependentTypes, RankNTypes #-}
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
readViaProducer flag = readInner (id (chooseMixed flag))

forward :: Show a => a -> b -> b
forward flag payload = show flag `seq` payload

readInsideQuickLook :: Bool -> Int
readInsideQuickLook flag = id ((readInner) (forward flag (chooseMixed flag)))

ignoreViaBar :: Mixed -> Bool
ignoreViaBar p = ignoreInner p

ignoreViaProducer :: Bool -> Bool
ignoreViaProducer flag = ignoreInner (chooseMixed flag)

readViaRank :: RankedMixed -> (Int, Bool)
readViaRank p = readRankInner p

readNamedBoth :: BothMixed -> Int
readNamedBoth p = readBoth p

readProducedBoth :: Bool -> Int
readProducedBoth flag = id (readBoth (id (makeBoth flag)))

readRankedBoth :: Bool -> Int
readRankedBoth flag = withRankedId (\transform ->
  readBoth (relayRanked transform flag (makeBoth flag)))

readSelection :: Bool -> BothMixed -> BothMixed -> Int
readSelection flag left right = withRankedId (\transform ->
  readBoth (selectRanked transform flag left right))

readProducedSelection :: Bool -> Int
readProducedSelection flag = readBoth (selectRanked id flag (makeBoth True) alternateBoth)

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
  third <- readProducedBoth <$> readIORef flag
  print (readNamedBoth (makeBoth input), third, readDependent depMixed,
         ignoreBoth (makeBoth (error "unused multi-parameter producer input")))
  fourth <- readRankedBoth <$> readIORef flag
  print (readRankedBoth input, fourth, ignoreBoth (relayRanked id (error "unused label" :: Bool)
         (makeBoth (error "unused ranked producer input"))))
  selected <- readIORef flag
  let unused :: BothMixed
      unused = error "unselected neighbouring package"
  print (readProducedSelection input, readProducedSelection selected,
         readSelection input (makeBoth True) unused,
         readSelection selected unused (makeBoth False))
