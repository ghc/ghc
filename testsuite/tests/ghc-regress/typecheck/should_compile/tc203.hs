{-# OPTIONS -fglasgow-exts #-}

-- Check that we can have a forall after a forall

module Foo4 where

type AnyE a = forall err. Either err a

foo :: Monad m => AnyE (m t)
foo = undefined
