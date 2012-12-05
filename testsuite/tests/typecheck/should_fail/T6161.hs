{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module T6161 where

data family Fam  a

data instance Fam Float = FamFloat Float

class Super a where
  testSup :: a -> Float

class Super a => Duper a where
  testDup :: a -> Float

--class ( Super (Fam a) ) => Foo a where
class Duper (Fam a) => Foo a where
  testFoo :: Fam a -> Float

instance Foo a => Duper (Fam a) where
  testDup x = testFoo x + testSup x

--instance Super (Fam Float) where
--  testSup (FamFloat x) = x

instance Foo Float where
  testFoo (FamFloat _) = 5.0

testProg :: Float
testProg = testDup (FamFloat 3.0)
