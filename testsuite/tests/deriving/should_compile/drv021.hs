{-# OPTIONS_GHC  -XDeriveDataTypeable -XStandaloneDeriving #-}

-- See Trac #1825
-- Test stand-alone deriving for Typeable
-- Horridly, one needs to define instance for Typeable1 etc

module ShouldCompile where

import Data.OldTypeable

data T1 a   = T1 a
data T2 a b = T2 a b 

deriving instance Typeable1 T1
deriving instance Typeable2 T2

