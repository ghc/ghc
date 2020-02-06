-- The simplifier changes the shapes of closures that we expect.
{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE MagicHash, UnboxedTuples, LambdaCase #-}

import GHC.Exts.Heap
import GHC.IORef
import GHC.Weak
import System.Mem

main :: IO ()
main = do
    key <- newIORef "key"
    let val = "val"
    wk@(Weak w) <- mkWeak key val Nothing

    getClosureData w >>= \case
      WeakClosure{} -> putStrLn "OK"
      _ -> error "Weak is not a WeakClosure"

    deRefWeak wk >>= \case
      Nothing -> error "Weak dead when key alive"
      Just _ -> pure ()

    readIORef key >>= putStrLn

    performMajorGC

    deRefWeak wk >>= \case
      Nothing -> pure ()
      Just _ -> error "Weak alive when key dead"

    getClosureData w >>= \case
      ConstrClosure{} -> putStrLn "OK"
      _ -> error "dead Weak should be a ConstrClosure"
