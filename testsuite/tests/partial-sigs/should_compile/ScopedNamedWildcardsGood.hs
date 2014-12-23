{-# LANGUAGE PartialTypeSignatures, NamedWildCards #-}

module ScopedNamedWildcardsGood where

-- The named wildcards aren't scoped as the ScopedTypeVariables extension
-- isn't enabled, of which the behaviour is copied. Thus, the _a annotation of
-- x, which must be Bool, isn't the same as the _a in g, which is now
-- generalised over.
foo :: _a -> _
foo x = let v = not x
            g :: _a -> _a
            g x = x
        in (g 'x')
