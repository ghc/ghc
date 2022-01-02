{-# LANGUAGE TypeFamilies #-}

module SAKS_Fail026 where

import Data.Kind (Type)

type F3 :: forall kx. kx -> Type
type family F3 (b :: Type) where
