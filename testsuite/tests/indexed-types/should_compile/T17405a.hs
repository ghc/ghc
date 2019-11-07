{-# LANGUAGE TypeFamilyDependencies, UndecidableInstances #-}
module T17405a where

data D a
type family F a = r | r -> a
type instance F (D a) = D (F a)
