{-# LANGUAGE TypeFamilies #-}
module A where
type family F a
type instance F Int = Bool
