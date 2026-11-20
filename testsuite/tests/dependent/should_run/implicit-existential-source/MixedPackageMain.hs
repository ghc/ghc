{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import MixedPackageProvider

named :: Mixed
named = inner

applied :: Mixed
applied = returnInner inner

bottomPayload :: Mixed
bottomPayload = lazyInner

main :: IO ()
main = do
  print (readMixed named, readMixed applied, readMixed written, readKind kindMixed)
  print (ignoreMixed bottomPayload, ignoreMixed (error "whole package" :: Mixed))
  result <- try (evaluate (readMixed bottomPayload))
  print (either (\(ErrorCall message) -> message == "mixed payload") (const False) result)
