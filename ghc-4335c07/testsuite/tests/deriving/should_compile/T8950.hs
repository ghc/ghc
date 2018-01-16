{-# LANGUAGE AutoDeriveTypeable, DataKinds, StandaloneDeriving #-}

module T8950 where

import Data.Typeable

data Foo = Bar
  deriving (Eq)

data Baz = Quux

deriving instance Typeable Baz  -- shouldn't error
deriving instance Typeable Quux

rep1 = typeRep (Proxy :: Proxy Bool)
rep2 = typeRep (Proxy :: Proxy 'True)
rep3 = typeRep (Proxy :: Proxy Foo)
rep4 = typeRep (Proxy :: Proxy Bar)
