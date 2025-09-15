{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T14158_vdq where

import Data.Kind
import qualified Control.Category as Cat

civ ::
  forall {k}.
  forall (cat :: k -> k -> Type) ->
  forall (a :: k).
  Cat.Category cat =>
  cat a a
civ (type cat) = Cat.id @cat

f = (civ (type (->)) >>=)