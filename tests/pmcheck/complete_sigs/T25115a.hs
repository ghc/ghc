{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T25115a ( data Foo, ABC ) where

pattern Foo :: a
pattern Foo <- _unused
{-# COMPLETE Foo #-}

data ABC = A | B | C
