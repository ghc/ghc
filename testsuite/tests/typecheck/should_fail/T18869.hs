{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module T18869 where

data Foo where
  MkFoo :: { foo :: !a } -> Foo

testFoo :: Foo
testFoo = MkFoo {}

data Bar where
  MkBar :: ( a ~ Int ) => { bar :: !a } -> Bar

testBar :: Bar
testBar = MkBar {}

data Baz where
  MkBaz :: { baz1 :: !a, baz2 :: !a } -> Baz

testBaz :: Baz
testBaz = MkBaz { baz1 = False }

type family TQuux x where
  TQuux Int = Bool
data Quux a where
  MkQuux :: { quux :: !( TQuux a ) } -> Quux a

testQuux :: Quux Int
testQuux = MkQuux {}
