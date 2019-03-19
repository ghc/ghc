{-# LANGUAGE RebindableSyntax #-}

-- Test that rebindable clash warnings are displayed.

module Main where

import Prelude

catMaybes xs = do
    Just x <- xs
    return x

main = return ()
