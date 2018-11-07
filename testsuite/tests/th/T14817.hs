{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T14817 where

import Data.Kind (Type)

$([d| data family Foo :: Type
      data instance Foo :: Type |])
