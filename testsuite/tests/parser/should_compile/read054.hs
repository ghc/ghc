
{-# OPTIONS_GHC -XParallelListComp #-}

module Foo where

foo = [ ()
      | () <- foo
      | () <- foo
      ]

