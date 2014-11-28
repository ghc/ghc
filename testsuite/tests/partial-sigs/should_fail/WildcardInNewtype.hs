{-# LANGUAGE PartialTypeSignatures #-}
module WildcardInNewtype where

-- Currently handled by the same checks as for ADTs, but in case this
-- changes in the future, add at least one test.

newtype Foo a = Foo (Either _ a)
