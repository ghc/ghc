{-# LANGUAGE Arrows #-}
module CmdFail008 where

f = proc x -> (! (_ -< _))
