-- Trac #2082
-- If we :i D or C, we should see parentheses around (Int -> a)

{-# LANGUAGE ExistentialQuantification #-}

module Foo where

data D = forall a . C (Int -> a) Char

