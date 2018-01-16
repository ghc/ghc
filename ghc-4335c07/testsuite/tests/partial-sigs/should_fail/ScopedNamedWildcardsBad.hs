{-# LANGUAGE PartialTypeSignatures, NamedWildCards, ScopedTypeVariables #-}

module ScopedNamedWildcardsBad where

-- If named wildcards are properly scoped, this should lead to
-- a constraint (Bool ~ Char)
foo :: _a -> _
foo x = let v = not x
            g :: _a -> _a
            g x = x
        in (g 'x')
