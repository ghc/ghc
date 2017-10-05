{-# LANGUAGE UnboxedSums #-}

module T14051a where

func :: s -> (# Bool | Bool #)
func _ = (# True | #)
