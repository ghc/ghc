{-# OPTIONS -fglasgow-exts #-}

-- We want to get the type
--   	g :: forall a b c.  C a (b,c) => a -> b
--but GHC 6.0 bogusly gets
--   	g :: forall a b.  C a (b,()) => a -> b

module ShouldCompile where

class C a b where { op :: a -> b }

f x = fst (op x)
