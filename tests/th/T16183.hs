{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T16183 where

import Data.Kind

$([d| type F1 = (Maybe :: Type -> Type) Int
      type F2 = (Int :: Type) -> (Int :: Type)
      type family F3 a where
        F3 (a :: Type) = Int
      newtype F4 = MkF4 (Int :: Type) |])
