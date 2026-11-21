{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import NativeLinearRetainedPackageProvider

main :: IO ()
main = do
    let unused = error "unused field"
    print ( takeChoice (First False unused (12 :: Int))
          , takeChoice (Other True unused (13 :: Int))
          , takeRefined (Slot True () (14 :: Int))
          , takeRefined (Slot False unused (15 :: Int))
          , takeOverlap (Slot False True (16 :: Int))
          , takeOverlap (Slot False False (17 :: Int))
          , takeTwo (Two False True unused () (18 :: Int))
          , takeTwo (Two True False () True (19 :: Int)) )
    print =<< mapM throws
        [ takeRefined (error "outer package")
        , takeRefined (makeIndex (error "retained index") 20)
        , takeRefined (Slot True (error "matched unit") 21)
        , snd (takeOverlap (Slot False (error "matched flag") 22)) ]
  where
    throws :: Int -> IO Bool
    throws n = either (const True) (const False) <$> try @ErrorCall (evaluate n)
