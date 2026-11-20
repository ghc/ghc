{-# LANGUAGE KindSignatures, TypeFamilies #-}
module DefaultCallback (Callback) where

import Data.Kind (Type)

-- | Give an element type a callback type.
class OptionalCallback (a :: Type) where
    type Callback a :: Type
    type Callback a = a -> Maybe a

instance OptionalCallback Bool
