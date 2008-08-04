{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Test Trac #2433

module T2433 where

 import Data.Typeable(Typeable1)
 import T2433_Help( T )

 deriving instance Typeable1 T
