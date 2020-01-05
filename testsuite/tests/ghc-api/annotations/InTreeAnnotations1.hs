{-# LANGUAGE BangPatterns #-}
module InTreeAnnotations1 where

foo a@(_,_) !"a" ~x = undefined
