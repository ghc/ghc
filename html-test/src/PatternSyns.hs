{-# LANGUAGE PatternSynonyms, PolyKinds, TypeOperators #-}

-- | Testing some pattern synonyms
module PatternSyns where

-- | FooType doc
data FooType x = FooCtor x

-- | Pattern synonym for 'Foo' x
pattern Foo x = FooCtor x

-- | Pattern synonym for 'Bar' x
pattern Bar x = FooCtor (Foo x)

-- | Pattern synonym for (':<->')
pattern x :<-> y = (Foo x, Bar y)

-- | Doc for ('><')
data (a :: *) >< b = Empty

-- | Pattern for 'Empty'
pattern E = Empty

-- | Earlier ghc versions didn't allow explicit signatures
-- on pattern synonyms.
pattern PatWithExplicitSig :: Eq somex => somex -> FooType somex
pattern PatWithExplicitSig x = FooCtor x
