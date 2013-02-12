{-# LANGUAGE TypeFamilies, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}

module T1769 where

import Data.Typeable

data family T a
deriving instance Typeable T
-- deriving instance Functor T

data instance T [b] = T1 | T2 b 
deriving instance Eq b => Eq (T [b])
