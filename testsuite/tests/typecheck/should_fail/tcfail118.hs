-- Gave a bad error message in a version of 6.3, as a 
-- result of 6.3's new duplicate-instance reporting structure
--
--	Foo.hs:4:5:
--	    No instance for `Eq Foo'
--	    When deriving the `Eq' instance for type `Bar'

module ShouldFail where

data Bar = Bar Foo deriving Eq
data Foo = Foo deriving Eq

instance Eq Foo where
    Foo == Foo = True

