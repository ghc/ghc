{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
module Foo( T ) where

-- Trac 2378

import Data.Data

newtype T f = MkT Int

deriving instance Typeable T
