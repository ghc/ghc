{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import GHC.Sigma (Sigma(Pair))
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
    print ( takeStep (next unused True 23 (24 :: Int))
          , takeStep (next (S Z) False 25 (26 :: Int))
          , snd (takeStep (next unused False unused (27 :: Int))) )
    print ( inspect (packetTrue unused 30 () (31 :: Int))
          , inspect (IndexFirst (S Z) False (at Z 32) True (33 :: Int))
          , inspect (FlagFirst (S Z) True () (at Z 34) (35 :: Int))
          , inspect (packetFalse unused False 36 (37 :: Int))
          , inspect (packetZero unused (38 :: Int))
          , snd (inspect (packetFalse unused unused unused (39 :: Int))) )
    print ( chooseThree (makeTriple unused 1 True True True (1 :: Int))
          , chooseThree (makeTriple unused 2 True True False (2 :: Int))
          , chooseThree (makeTriple unused 3 True False unused (3 :: Int))
          , chooseThree (makeTriple unused 4 False unused unused (4 :: Int))
          , snd (chooseThree (makeTriple unused unused False unused unused (5 :: Int))) )
    let bothBottom = makeTriple unused unused (error "ordinary first")
                                 (error "retained later") unused (0 :: Int)
    order <- try @ErrorCall (evaluate (snd (chooseThree bothBottom)))
    print (either ((== "ordinary first") . takeWhile (/= '\n') . displayException) (const False) order)
    print ( chooseMark (makeMark unused (Pair True ()) True (1 :: Int))
          , chooseMark (makeMark unused (Pair False True) False (2 :: Int))
          , snd (chooseMark (makeMark unused (unknownMark unused) True (3 :: Int))) )
    print =<< mapM demanded
        [ snd (chooseMark (makeMark unused (error "marker") (error "branch") False))
        , fst (chooseMark (makeMark unused (unknownMark (error "witness")) True False))
        , fst (chooseMark (makeMark unused (Pair False (error "payload")) False False)) ]
    print =<< mapM throws
        [ takeRefined (error "outer package")
        , takeRefined (makeIndex (error "retained index") 20)
        , takeRefined (Slot True (error "matched unit") 21)
        , snd (takeOverlap (Slot False (error "matched flag") 22))
        , fst (takeStep (next Z False (error "matched payload") (28 :: Int)))
        , snd (takeStep (next Z (error "matched Bool") 29 30))
        , snd (inspect (packetUnknown (error "second retained field") 0))
        , snd (inspect (packetTrue Z 0 (error "unit payload") 0))
        , fst (fst (inspect (packetFalse Z False (error "index payload") (0 :: Int)))) ]
  where
    throws :: Int -> IO Bool
    throws n = either (const True) (const False) <$> try @ErrorCall (evaluate n)

    demanded :: Bool -> IO String
    demanded value = either (takeWhile (/= '\n') . displayException) (const "ok")
                  <$> try @ErrorCall (evaluate value)
