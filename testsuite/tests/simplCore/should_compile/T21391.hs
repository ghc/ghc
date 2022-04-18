{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Web.Routing.SafeRouting where

import Control.DeepSeq (NFData (..))
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)

class FromHttpApiData a where

data PolyMap (c :: Type -> Constraint) (f :: Type -> Type) (a :: Type) where
  PMNil :: PolyMap c f a
  PMCons :: (Typeable p, c p) => f (p -> a) -> PolyMap c f a -> PolyMap c f a

rnfHelper :: (forall p. c p => f (p -> a) -> ()) -> PolyMap c f a -> ()
rnfHelper _ PMNil = ()
rnfHelper h (PMCons v pm) = h v `seq` rnfHelper h pm

data PathMap x =
  PathMap [x] (PolyMap FromHttpApiData PathMap x)

instance NFData x => NFData (PathMap x) where
  rnf (PathMap a b) = rnf a `seq` rnfHelper rnf b
