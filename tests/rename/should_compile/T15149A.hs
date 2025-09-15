{-# LANGUAGE TypeFamilies #-}
module T15149A where

import Data.Kind (Type)

data family An c :: Type
