{-# LANGUAGE TypeFamilies #-}
module T7102C where
import T7102A
type instance T a b = b
to :: T a b -> b
to = id
