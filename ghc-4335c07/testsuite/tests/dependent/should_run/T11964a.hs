{-# LANGUAGE TypeInType #-}
module T11964a where
import Data.Kind
type Star = Type
newtype T k (t :: k) = T ()
