-- | This module caused a duplicate instance in the documentation for the Foo
-- type.
module Bug7  where

-- | The Foo datatype
data Foo = Foo

-- | The Bar class
class Bar x y

-- | Just one instance
instance Bar Foo Foo
