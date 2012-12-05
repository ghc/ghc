{-# LANGUAGE BangPatterns, MagicHash #-}
import Control.Exception
import System.Environment
import GHC.Exts

main = do
  args <- getArgs
  foo args

foo _ = let !e = toException (ErrorCall "test") in
        raise# e
