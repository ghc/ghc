module NamedWildcardsNotEnabled where

foo :: _a -> _b
foo x = not x

-- with the NamedWildcards extension enabled this would lead to the
-- type Bool -> Bool.
