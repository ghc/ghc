{-# LANGUAGE TypeFamilyDependencies, UndecidableInstances #-}
module T17405a where

import Data.Kind

data D (a :: Type)
type family F a = r | r -> a
type instance F (D a) = D (F a)
