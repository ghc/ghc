{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
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
        pair = error "unused two-cell vector" :: Vec Bool ('S ('S 'Z))
    print (observeBranch Z empty, observeBranch (S Z) singleton)
    print (literalCut undefined)
    print (observePair Z empty empty, observePair (S Z) singleton singleton,
           observePair (S (S Z)) pair pair)

literalCut :: Vec Bool ('S ('S ('S 'Z))) -> Bool
literalCut xs = case branchVector @('S 'Z) (S (S Z)) xs of
    BranchVector -> True

fails :: String -> IO () -> IO ()
fails label action = do
    r <- try action :: IO (Either ErrorCall ())
    case r of
        Left err | label `isInfixOf` show err -> putStrLn label
        _ -> fail ("did not force " ++ label)
