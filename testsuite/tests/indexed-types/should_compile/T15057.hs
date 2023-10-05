{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module T15057 where

type T f = f Int
type S a = a -> a

type family TF a
type instance TF Int = T S
  -- Ensure that -dcore-lint doesn't trip up on this unsaturated use
  -- of the type synonym S
