{-# OPTIONS -fimplicit-params #-}

module ShouldCompile where

bar :: (Show a, ?c::a) => String
-- This type should not be reported as ambiguous
-- See the call in 
bar = show ?c

foo = let { ?c = 'x' } in bar


