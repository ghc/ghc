{-# LANGUAGE Haskell2010 #-}
{-# language PatternSynonyms #-}
module Bug1067A ( Foo(P) ) where

-- | A foo
data Foo = Foo

-- | A pattern
pattern P :: Foo
pattern P = Foo
