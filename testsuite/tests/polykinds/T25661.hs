{-# Language TypeFamilyDependencies #-}
{-# Language RequiredTypeArguments #-}
module T25661 where

import Data.Kind
import Control.Category (Category(id, (.)))
import Prelude hiding (id, (.))

type Cat :: Type -> Type
type Cat k = k -> k -> Type
-- type    Op :: (k -> j -> Type) -> (j -> k -> Type)
-- newtype Op cat b a = Op (cat a b)

-- instance Category cat => Category (Op @k @k cat) where
--   id = Op id
--   Op f . Op g = Op (g . f)

type NaturalTransformation :: Cat s -> Cat t -> Cat (s -> t)
data NaturalTransformation src tgt f g where
 -- NaturalTransformationId :: NaturalTransformation src tgt f f
 NaturalTransformation :: (FunctorOf src tgt f, FunctorOf src tgt g) => { getNaturalTransformation :: forall x. f x `tgt` g x } -> NaturalTransformation src tgt f g

type
  FunctorOf :: Cat s -> Cat t -> (s -> t) -> Constraint
class    (NewFunctor f, Source f ~ src, Target f ~ tgt) => FunctorOf src tgt f
instance (NewFunctor f, Source f ~ src, Target f ~ tgt) => FunctorOf src tgt f

type
  NewFunctor :: (s -> t) -> Constraint
class (Category (Source f), Category (Target f)) => NewFunctor (f :: s -> t) where
  type Source (f :: s -> t) :: Cat s
  type Target (f :: s -> t) :: Cat t
  newmap :: Source f a a' -> Target f (f a) (f a')


newmapVis :: NewFunctor f => forall source -> source ~ Source f
          => forall target -> target ~ Target f => source a a' -> target (f a) (f a')
newmapVis source = undefined
