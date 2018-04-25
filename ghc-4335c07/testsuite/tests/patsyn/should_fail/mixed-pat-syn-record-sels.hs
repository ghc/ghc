{-# LANGUAGE PatternSynonyms #-}
module Foo where


pattern A { a } = Just a
pattern B { b } = Just b

foo :: Maybe a -> Maybe Bool
foo x = x { a = True, b = False }
