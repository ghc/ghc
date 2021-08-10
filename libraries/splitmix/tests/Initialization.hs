module Main (main) where

import Control.Monad (forM_, replicateM)
import Data.List     (tails)
import Test.HUnit    (assertFailure)

import qualified System.Random.SplitMix   as SM
import qualified System.Random.SplitMix32 as SM32

main :: IO ()
main = do
    g64 <- replicateM 10 (fmap show SM.initSMGen)
    putStrLn $ unlines g64
    forM_ (tails g64) $ \xs' -> case xs' of
        []     -> return ()
        (x:xs) ->
            if all (x /=) xs
            then return ()
            else assertFailure "ERROR: duplicate"

    g32 <- replicateM 10 (fmap show SM32.initSMGen)
    putStrLn $ unlines g32
    forM_ (tails g32) $ \xs' -> case xs' of
        []     -> return ()
        (x:xs) ->
            if all (x /=) xs
            then return ()
            else assertFailure "ERROR: duplicate"
