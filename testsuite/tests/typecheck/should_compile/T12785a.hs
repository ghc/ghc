{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
module T12785a where

import Data.Kind (Type)

foo :: forall (dk :: Type) (c :: Type -> Type) (t :: dk -> Type) (a :: Type).
       (dk ~ Type)
    => (forall (d :: dk). c (t d)) -> Maybe (c a)
foo _ = Nothing
