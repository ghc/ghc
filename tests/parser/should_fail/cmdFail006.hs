{-# LANGUAGE Arrows #-}
module CmdFail006 where

f = proc x -> ~(_ -< _)
