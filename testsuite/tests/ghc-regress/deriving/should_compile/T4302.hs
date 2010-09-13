{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable, EmptyDataDecls #-}  
module T4302 where  

import Ix
import Data.Typeable
import Data.Data
import Data.Foldable
import Data.Traversable

data Test a

deriving instance Eq (Test a) 
deriving instance Ord (Test a) 
deriving instance Typeable1 Test
deriving instance Data a => Data (Test a) 
deriving instance Functor Test 
deriving instance Foldable Test 
deriving instance Traversable Test  
