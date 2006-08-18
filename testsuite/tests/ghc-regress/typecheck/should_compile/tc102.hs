{-# OPTIONS -fglasgow-exts #-}

-- !!! Caused ghc-4.04proto to report a bogus type error
-- !!! (as reported by Keith)

-- The type error arose from a mistake in tcMatches.tc_match

-- Involves pattern type signatures

module ShouldCompile where

p :: forall a. a -> a
p = let y = p in \ (x::a) -> x
