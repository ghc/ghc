{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}

module WithSpineVDQ_LintErr (uSSZ) where

import Data.Kind

data Peano = Z | S Peano

type WithSpine :: Peano -> Constraint
class WithSpine n where
  -- Single-method class (newtype dict)
  onSpine ::
    forall n' -> (n' ~ n) =>  -- workaround b/c it's not possible to make `n` visible
    (forall m -> (n ~ S m) => WithSpine m => ()) ->
    ()

instance WithSpine Z where
  onSpine (type n) _ = ()

instance forall m. WithSpine m => WithSpine (S m) where
  onSpine (type n) f = f (type m)

getUnit :: forall n -> WithSpine n => ()
getUnit (type n) = onSpine (type n) (\(type n') -> getUnit (type n'))

-- uSSZ :: ()
uSSZ = getUnit (type (S (S Z)))
