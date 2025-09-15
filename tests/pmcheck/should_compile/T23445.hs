{-# LANGUAGE GADTs #-}

module T23445 where

data GADT a where
  IsUnit :: GADT ()

data Foo b where
  FooUnit :: Foo ()
  FooInt  :: Foo Int

data SomeRec = SomeRec { fld :: () }

bug :: GADT a -> Foo a -> SomeRec -> SomeRec
bug IsUnit foo r =
  r { fld = case foo of { FooUnit -> () } }
