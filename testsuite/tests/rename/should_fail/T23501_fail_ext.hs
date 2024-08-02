{-# LANGUAGE NoTypeAbstractions #-}

module T23501_fail_ext where

type Const a _ = a

type F k (_ :: k) = k
