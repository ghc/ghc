{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module TLKS_Fail015 where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type T :: forall k. k -> Type
data T a = MkT (Proxy (a :: k))   -- 'k' is not brought into scope by ScopedTypeVariables
