{-# OPTIONS -fwarn-unused-binds #-}

-- Test reports of unused bindings

module ShouldCompile( t ) where

f x = f x	-- Unused

g x = h x	-- Unused
h x = g x

t x = t x	-- Used by export list

