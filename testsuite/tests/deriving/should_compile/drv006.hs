-- !!! simple examples of deriving Ix
--
module ShouldSucceed where
import Data.Ix

data Foo = Foo1 | Foo2 | Foo3 | Foo4 | Foo5 | Foo6 | Foo7 | Foo8
    	 deriving (Eq, Ord, Ix, Show)

data Bar a b = MkBar a Int b Integer a
