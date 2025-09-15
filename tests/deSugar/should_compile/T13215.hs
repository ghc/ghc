{-# LANGUAGE BangPatterns #-}
module T13215 where

newtype F = F Int

foo !(F {}) = ()
