{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf)
import NativeRetainedCastDomainWorker

main :: IO ()
main = do
    observe >>= print
    fails "missing worker" forceWorker
    fails "demanded token" (evaluate forceIndex >> pure ())

fails :: String -> IO () -> IO ()
fails label action = do
    r <- try action :: IO (Either ErrorCall ())
    case r of
        Left err | label `isInfixOf` show err -> putStrLn label
        _ -> fail ("did not force " ++ label)
