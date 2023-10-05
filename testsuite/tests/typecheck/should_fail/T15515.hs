{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15515 where

import Data.Kind
import Data.Proxy

class C a where
  c :: Proxy a

type family F

data D :: F -> Type

instance C (D :: F -> Type) where
  c = Proxy

c' :: Proxy (D :: F -> Type)
c' = c @(D :: F -> Type)
