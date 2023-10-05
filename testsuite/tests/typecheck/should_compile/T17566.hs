{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T17566 where

import Data.Kind
import Data.Proxy
import T17566a

type family F1 (x :: Proxy a) :: Proxy a
instance C1 Proxy z where
  type T1 x = F1 x
  data D1 x

type family F2 (x :: Proxy a) :: Proxy a
instance C2 Proxy z where
  type T2 x = F2 x
  data D2 x
