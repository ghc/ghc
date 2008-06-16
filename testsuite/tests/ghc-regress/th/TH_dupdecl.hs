{-# LANGUAGE TemplateHaskell #-}

-- Two sliced declarations bind the same variable.
-- This test checks that there's a reasonable error message

module ShouldCompile where

$( [d| x = 1 |] )

$( [d| x = 2 |] )
