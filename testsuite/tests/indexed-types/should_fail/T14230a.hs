{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T14230a where

import Data.Kind

class C a where
  data CD k (a :: k) :: k -> Type

instance C (Maybe a) where
  data CD k (a :: Type) :: Type -> Type
