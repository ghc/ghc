{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- We want to get the type
--   	g :: forall a b c.  C a (b,c) => a -> b
--but GHC 6.0 bogusly gets
--   	g :: forall a b.  C a (b,()) => a -> b

-- Having done this, we reject f on the grounds
-- that its type is ambiguous: adding the type
-- signature   g :: C a (b,c) => a -> b
-- would fail

module ShouldCompile where

class C a b where { op :: a -> b }

g x = fst (op x)
