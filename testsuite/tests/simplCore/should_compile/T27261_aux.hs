{-# LANGUAGE BangPatterns #-}

module T27261_aux (myError) where

myError :: [String] -> String -> a
myError !_ _ = undefined
{-# NOINLINE myError #-}
