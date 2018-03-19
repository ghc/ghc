{-# LANGUAGE BangPatterns #-}

module T14761b where

data A = A { a :: ! Maybe Int}
