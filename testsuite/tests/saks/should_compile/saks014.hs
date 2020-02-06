{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module SAKS_014 where

import Data.Kind (Type)

type T :: (k -> Type) -> (k -> Type)
data T m a = MkT (m a) (T Maybe (m a))
