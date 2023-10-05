{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T13895 where

import Data.Data (Data, Typeable)
import Data.Kind

dataCast1 :: forall (a :: Type).
             Data a
          => forall (c :: Type -> Type)
                    (t :: forall (k :: Type). Typeable k => k -> Type).
             Typeable t
          => (forall d. Data d => c (t d))
          -> Maybe (c a)
dataCast1 _ = undefined
