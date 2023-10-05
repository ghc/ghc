{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies, DeriveDataTypeable, StandaloneDeriving #-}

module T4896 where

import Data.Data
import Data.Typeable

--instance Typeable1 Bar where
--  typeOf1 _ = mkTyConApp (mkTyCon "Main.Bar") []
deriving instance Typeable Bar

class Foo a where
  data Bar a

data D a b = D Int a deriving (Typeable, Data)

instance Foo (D a b) where
  data Bar (D a b) = B { l :: a } deriving (Eq, Ord, Read, Show, Data)

