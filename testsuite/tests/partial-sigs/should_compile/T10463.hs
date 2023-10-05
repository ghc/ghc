{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}

module T10463 where

f (x :: _) = x ++ ""
