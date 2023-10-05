{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module T7947 where

import Data.Data
import Data.Typeable

import T7947a
import qualified T7947b as B

deriving instance Typeable A
deriving instance Typeable B.B

deriving instance Data A
deriving instance Data B.B
