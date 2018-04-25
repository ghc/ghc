{-# LANGUAGE TypeFamilies #-}
module A2 where
type family F a
type instance F Int = Bool
