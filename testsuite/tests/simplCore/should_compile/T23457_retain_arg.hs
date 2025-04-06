{-# LANGUAGE MagicHash, UnboxedTuples #-}
module T23457_retain_arg where

import GHC.Exts
import GHC.IO

trickyFun :: (a -> Addr#) -> (a -> Int#) -> a -> IO Word
trickyFun ptrFun ofsFun x = IO $ \s ->
  case readWordOffAddr# (ptrFun x) (ofsFun x) s of
    (# s', ans #) -> (# s', W# ans #)


trickyFun_ :: (a -> Addr#) -> (a -> Int#) -> a -> IO ()
trickyFun_ ptrFun ofsFun x = () <$ trickyFun ptrFun ofsFun x
  -- This function can discard the readWordOffAddr# when
  -- optimized but must still evaluate 'ptrFun x' and
  -- ofsFun x' because those may not be ok-to-discard.
