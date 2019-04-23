{-# LANGUAGE Arrows #-}
module CmdFail005 where

f = proc x -> x@(_ -< _)
