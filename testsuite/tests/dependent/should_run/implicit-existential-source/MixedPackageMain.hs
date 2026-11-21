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

readInsideQuickLook :: Mixed -> Int
readInsideQuickLook p = id ((readInner) p)

ignoreViaBar :: Mixed -> Bool
ignoreViaBar p = ignoreInner p

readViaRank :: RankedMixed -> (Int, Bool)
readViaRank p = readRankInner p

main :: IO ()
main = do
  print (readMixed named, readMixed applied, readMixed written, readKind kindMixed)
  print (ignoreMixed bottomPayload, ignoreMixed (error "whole package" :: Mixed))
  result <- try (evaluate (readMixed bottomPayload))
  print (either (\(ErrorCall message) -> message == "mixed payload") (const False) result)
  flag <- newIORef True
  first <- readViaBar . chooseMixed <$> readIORef flag
  writeIORef flag False
  second <- readInsideQuickLook . chooseMixed <$> readIORef flag
  print (first, second, readViaRank rankedMixed, readKindInner kindMixed)
  print (ignoreViaBar (error "unused package"))
