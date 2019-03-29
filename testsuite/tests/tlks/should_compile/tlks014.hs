{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module TLKS_014 where

import Data.Kind (Type)

type T :: (k -> Type) -> (k -> Type)
data T m a = MkT (m a) (T Maybe (m a))
