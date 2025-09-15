{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T14845_fail2 where

import Data.Coerce
import Data.Kind

data A :: Type -> Type where
  MkA :: Coercible a Int => A a

data SA :: forall a. A a -> Type where
  SMkA :: SA MkA
