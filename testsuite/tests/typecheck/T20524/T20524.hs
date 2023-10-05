{-#LANGUAGE DerivingVia, GeneralizedNewtypeDeriving#-}

module T20524 where
import UT

data Foo = Foo
  deriving Tupleable via Boring Foo

newtype Bar a = Bar ()
  deriving newtype Tupleable

