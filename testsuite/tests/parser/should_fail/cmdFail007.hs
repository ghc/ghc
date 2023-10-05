{-# LANGUAGE Arrows #-}
module CmdFail007 where

f = proc x ->
      (_ -< _) { a = _ -< _,
                 b = _ -< _,
                 c = _ -< _ }
