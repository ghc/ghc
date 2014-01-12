{-# OPTIONS_GHC  -XDeriveDataTypeable -XStandaloneDeriving #-}

-- See Trac #1825

module ShouldFail where
import Data.OldTypeable

data T1 a = T1 a deriving( Typeable1 )

data T2 a b = T2 a b 

deriving instance (Typeable a, Typeable b) => Typeable (T2 a b)
	-- c.f. drv021.hs
