--!!! simple examples of deriving Ix
--
module ShouldSucceed where
import Ix

data Foo = Foo1 | Foo2 | Foo3 | Foo4 | Foo5 | Foo6 | Foo7 | Foo8
    	 deriving Ix

data Bar a b = MkBar a Int b Integer a
