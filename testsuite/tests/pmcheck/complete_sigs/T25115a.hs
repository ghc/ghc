{-# LANGUAGE PatternSynonyms #-}

module T25115a ( pattern Foo, ABC ) where

pattern Foo :: a
pattern Foo <- _unused
{-# COMPLETE Foo #-}

data ABC = A | B | C
