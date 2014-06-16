{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

module Annfail06 where
-- Testing that we don't accept Typeable or Data instances defined in the same module

import Annfail06_Help

import Data.Data
import Data.Typeable

deriving instance Typeable InstancesInWrongModule

instance Data InstancesInWrongModule where
    gfoldl = undefined
    gunfold = undefined

{-# ANN module InstancesInWrongModule #-}

{-# ANN type Foo InstancesInWrongModule #-}
data Foo = Bar

{-# ANN f InstancesInWrongModule #-}
f x = x