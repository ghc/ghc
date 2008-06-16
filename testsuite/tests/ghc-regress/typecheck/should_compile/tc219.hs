{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction #-}

module ShouldCompile where

-- c.f. tc218.hs, only no type signature here
-- Instead, the NoMonomorphismRestriction language
bar = show ?c

foo = let { ?c = 'x' } in bar
