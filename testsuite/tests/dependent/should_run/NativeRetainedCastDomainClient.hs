{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf)
import NativeRetainedCastDomainWorker
import PackedVecLibrary (Nat(..), Vec)

main :: IO ()
main = do
    observe >>= print
    fails "missing worker" forceWorker
    fails "demanded token" (evaluate forceIndex >> pure ())
    let empty = error "unused empty vector" :: Vec Bool 'Z
        singleton = error "unused singleton vector" :: Vec Bool ('S 'Z)
    print (observeBranch Z empty, observeBranch (S Z) singleton)

fails :: String -> IO () -> IO ()
fails label action = do
    r <- try action :: IO (Either ErrorCall ())
    case r of
        Left err | label `isInfixOf` show err -> putStrLn label
        _ -> fail ("did not force " ++ label)
