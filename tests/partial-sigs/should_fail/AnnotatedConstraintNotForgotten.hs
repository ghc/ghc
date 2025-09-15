{-# LANGUAGE PartialTypeSignatures #-}
module AnnotatedConstraintNotForgotten where

foo :: (Eq a, _) => a -> String
foo x = show x


data Foo = Foo deriving Show


-- Foo doesn't implement Eq, so `foo Foo` should fail. This is to
-- verify that the final type of foo didn't forget about the annotated
-- `Eq a` constraint.
test :: String
test = foo Foo
