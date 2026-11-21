{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes, BangPatterns, GADTs, ScopedTypeVariables, Strict #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import RetainedTuple (keep)
import TupleProvider (At(..), Tuple(Tup2))

defaultStrict :: Tuple Bool Bool -> Bool
defaultStrict ~value =
    let tuple = value
    in case keep tuple of At -> True

writtenStrict :: Tuple Bool Bool -> Bool
writtenStrict ~value =
    let !tuple = value
    in case keep tuple of At -> True

writtenLazy :: Tuple Bool Bool -> Bool
writtenLazy ~value =
    let ~tuple = value
    in case keep tuple of At -> True

forces :: (Tuple Bool Bool -> Bool) -> IO Bool
forces inspect = do
    result <- try (evaluate (inspect (error "strict binding")))
    pure (case result of Left (_ :: ErrorCall) -> True; Right _ -> False)

main :: IO ()
main = do
    defaultForced <- forces defaultStrict
    writtenForced <- forces writtenStrict
    print (defaultStrict (Tup2 (error "unused field") False),
           defaultForced, writtenForced, writtenLazy (error "lazy binding"))
