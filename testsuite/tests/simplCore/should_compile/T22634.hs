module T226334 where

import Data.Kind
import Type.Reflection

fromDynamic :: forall (a :: Type) (b :: Type). Typeable a => TypeRep b -> Maybe (a :~~: b)
fromDynamic t = typeRep `eqTypeRep` t

recursiveStrategy :: forall (a :: Type) (b :: Type). Typeable a
                  => TypeRep b -> Maybe ((Bool -> a) :~~: b)
recursiveStrategy = fromDynamic
