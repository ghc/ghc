{-# LANGUAGE MagicHash, ScopedTypeVariables, UnboxedTuples #-}

module T12373 where

import GHC.MVar
import GHC.Prim
import GHC.Types

main :: IO ()
main = IO (\rw -> newMVar# rw) >> return ()
