{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}

import Data.Typeable

class C a where
    data T a :: * 

instance C Int where
    data T Int = A1 deriving (Typeable)

instance C Bool where
    data T Bool = A2 deriving (Typeable)
