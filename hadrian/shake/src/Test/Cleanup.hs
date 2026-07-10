{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- Initially copied from https://raw.githubusercontent.com/snoyberg/conduit/master/resourcet/test/main.hs
-- on 2018-10-11
module Test.Cleanup(main) where

import General.Cleanup
import Data.Typeable
import Control.Monad
import Data.IORef
import Control.Exception
import Test.Type


main = testSimple $ do
    do -- survives releasing bottom
        x <- newIORef (0 :: Int)
        handle (\(_ :: SomeException) -> pure ()) $ withCleanup $ \cleanup -> do
            _ <- register cleanup $ modifyIORef x (+1)
            release undefined
        (=== 1) =<< readIORef x

    do -- early release
        x <- newIORef (0 :: Int)
        withCleanup $ \cleanup -> do
            undo <- register cleanup $ modifyIORef x (+1)
            release undo
            (=== 1) =<< readIORef x
        (=== 1) =<< readIORef x

    do -- unprotect keeps resource from being cleared
        x <- newIORef (0 :: Int)
        _ <- withCleanup $ \cleanup -> do
            key <- register cleanup $ writeIORef x 1
            unprotect key
        (=== 0) =<< readIORef x


    do -- cleanup actions are masked https://github.com/snoyberg/conduit/issues/144
        let checkMasked name = do
                ms <- getMaskingState
                unless (ms == MaskedUninterruptible) $
                    error $ show (name, ms)
        withCleanup $ \cleanup -> do
            register cleanup (checkMasked "release") >>= release
            register cleanup (checkMasked "normal")
        Left Dummy <- try $ withCleanup $ \cleanup -> do
            register cleanup (checkMasked "exception")
            throwIO Dummy
        pure ()


data Dummy = Dummy
    deriving (Show, Typeable)
instance Exception Dummy
