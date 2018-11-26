{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T15852 where

import Data.Kind
import Data.Proxy

data family DF a (b :: k)
data instance DF (Proxy c) :: Proxy j -> Type
