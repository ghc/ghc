{-# LANGUAGE TypeFamilies #-}
module T25417 where
type family T a
type instance T [a] = a
