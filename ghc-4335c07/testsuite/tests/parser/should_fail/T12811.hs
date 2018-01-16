{-# LANGUAGE RankNTypes #-}
module Bug where

foo :: foral a. a -> a
foo x = x
