-- Both blocks are illegal Haskell 98, because of the un-saturated
-- type synonym, but (rather obscurely) at one point (GHC 6.3), we
-- acceped 'blah', but rejected 'blah1'

module ShouldFail where

data T = T

-- This was erroneously accepted
type Foo a = String -> Maybe a
type Bar m = m Int
blah = undefined :: Bar Foo


type Foo1 a = Maybe a
type Bar1 m = m Int
blah1 = undefined :: Bar1 Foo1


