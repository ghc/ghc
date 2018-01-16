
{-# LANGUAGE ParallelListComp #-}

module Foo where

foo = [ ()
      | () <- foo
      | () <- foo
      ]

