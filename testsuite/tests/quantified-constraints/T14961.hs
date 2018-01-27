{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE QuantifiedConstraints     #-}

module T14961 where

import           Data.Kind

import           Control.Arrow              (left, right, (&&&), (|||))
import           Control.Category
import           Prelude                    hiding (id, (.))

import           Data.Coerce

class    (forall x. f x => g x) => f ~=> g
instance (forall x. f x => g x) => f ~=> g

type family (#) (p :: Type -> Type -> Type) (ab :: (Type, Type))
  = (r :: Type) | r -> p ab where
  p # '(a, b) = p a b

newtype Glass
  :: ((Type -> Type -> Type) -> Constraint)
  -> (Type, Type) -> (Type, Type) -> Type where
  Glass :: (forall p. z p => p # ab -> p # st) -> Glass z st ab

data A_Prism

type family ConstraintOf (tag :: Type)
  = (r :: (Type -> Type -> Type) -> Constraint) where
  ConstraintOf A_Prism = Choice

_Left0
  :: Glass Choice
       '(Either a x, Either b x)
       '(a, b)
_Left0 = Glass left'

_Left1
  :: c ~=> Choice
  => Glass c '(Either a x, Either b x) '(a, b)
_Left1 = Glass left'

-- fails with
-- • Could not deduce (Choice p)
-- _Left2
--   :: (forall p. c p => ConstraintOf A_Prism p)
--   => Glass c '(Either a x, Either b x) '(a, b)
-- _Left2 = Glass left'

_Left3
  :: d ~ ConstraintOf A_Prism
  => (forall p . c p => d p)
  => Glass c
       '(Either a x, Either b x)
       '(a, b)
_Left3 = Glass left'

-- fails to typecheck unless at least a partial type signature is provided
-- l :: c ~=> Choice => Glass c _ _
-- l = _Left1 . _Left1

newtype Optic o st ab where
  Optic
    :: (forall c d. (d ~ ConstraintOf o, c ~=> d) => Glass c st ab)
    -> Optic o st ab

_Left
  :: Optic A_Prism
       '(Either a x, Either b x)
       '(a, b)
_Left = Optic _Left1

instance Category (Glass z) where
  id :: Glass z a a
  id = Glass id

  (.) :: Glass z uv ab -> Glass z st uv -> Glass z st ab
  Glass abuv . Glass uvst = Glass (uvst . abuv)

class Profunctor (p :: Type -> Type -> Type) where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  rmap :: (b -> c) -> p a b -> p a c

class Profunctor p => Choice (p :: Type -> Type -> Type) where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
