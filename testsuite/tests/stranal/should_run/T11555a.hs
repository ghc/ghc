module Main(main) where

import Control.Monad
import Control.Exception
import Control.Monad.Trans.Cont
import GHC.Exts


type RAW a = ContT () IO a

-- See https://gitlab.haskell.org/ghc/ghc/issues/11555
catchSafe1, catchSafe2 :: IO a -> (SomeExceptionWithLocation -> IO a) -> IO a
catchSafe1 a b = lazy a `catch` b
catchSafe2 a b = join (evaluate a) `catch` b

-- | Run and then call a continuation.
runRAW1, runRAW2 :: RAW a -> (Either SomeExceptionWithLocation a -> IO ()) -> IO ()
runRAW1 m k = m `runContT` (k . Right) `catchSafe1` \e -> k $ Left e
runRAW2 m k = m `runContT` (k . Right) `catchSafe2` \e -> k $ Left e

{-# NOINLINE run1 #-}
run1 :: RAW ()-> IO ()
run1 rs = do
    runRAW1 rs $ \x -> case x of
        Left e -> putStrLn "CAUGHT"
        Right x -> return x

{-# NOINLINE run2 #-}
run2 :: RAW ()-> IO ()
run2 rs = do
    runRAW2 rs $ \x -> case x of
        Left e -> putStrLn "CAUGHT"
        Right x -> return x

main :: IO ()
main = do
    run1 $ error "MISSED"
    run2 $ error "MISSED"
