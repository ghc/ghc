{-# OPTIONS -fimplicit-params -fno-monomorphism-restriction #-}

module ShouldCompile where

-- c.f. tc218.hs, only no type signature here
-- Instead, the -fno-monomorphism-restriction flag
bar = show ?c

foo = let { ?c = 'x' } in bar
