{-# LANGUAGE TypeFamilies #-}
module T7102B where
import T7102A
type instance T a b = a
from :: a -> T a b
from = id
