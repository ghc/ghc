{-# language TypeFamilies, DataKinds #-}

module ListTuplePunsFail2 where

import Data.Kind (Type)

type family F1 (a :: Type) :: Type where

type T1 = F1 (Int, Double)
