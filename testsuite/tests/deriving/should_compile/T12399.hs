{-# LANGUAGE DeriveFunctor, MagicHash, UnboxedTuples #-}
module T12399 where

import GHC.Exts

newtype RmLoopsM a = RmLoopsM { runRmLoops :: Int# -> (# Int#, a #) }
  deriving Functor
