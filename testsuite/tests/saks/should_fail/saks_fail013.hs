{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module SAKS_Fail013 where

import Data.Kind (Type)

type T :: forall (k :: Type) -> Type
data T j = MkT (j -> k)
