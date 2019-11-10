{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module T17461 where

import Data.Kind

$([d| type (:+:) :: Type -> Type -> Type
      type (:+:) = Either
    |])
