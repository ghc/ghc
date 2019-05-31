{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies #-}
{-# LANGUAGE TopLevelKindSignatures #-}
{-# OPTIONS_GHC -fwarn-unticked-promoted-constructors #-}

module RaeBlogPost where

import Data.Kind

-- a Proxy type with an explicit kind
data Proxy k (a :: k) = P
prox :: Proxy Type Bool
prox = P

prox2 :: Proxy Bool 'True
prox2 = P

-- implicit kinds still work
data A
data B :: A -> Type
data C :: B a -> Type
data D :: C b -> Type
data E :: D c -> Type
-- note that E :: forall (a :: A) (b :: B a) (c :: C b). D c -> Type

-- a kind-indexed GADT
type TypeRep :: k -> Type
data TypeRep a where
  TInt   :: TypeRep Int
  TMaybe :: TypeRep Maybe
  TApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

zero :: TypeRep a -> a
zero TInt            = 0
zero (TApp TMaybe _) = Nothing

data Nat = Zero | Succ Nat
type family a + b where
  'Zero     + b = b
  ('Succ a) + b = 'Succ (a + b)

type Vec :: Type -> Nat -> Type
data Vec a n where
  Nil  :: Vec a 'Zero
  (:>) :: a -> Vec a n -> Vec a ('Succ n)
infixr 5 :>

-- promoted GADT, and using + as a "kind family":
type (++) :: Vec a n -> Vec a m -> Vec a (n + m)
type family x ++ y where
  'Nil      ++ y = y
  (h ':> t) ++ y = h ':> (t ++ y)

-- datatype that mentions Type
data U = Star (Type)
       | Bool Bool

-- kind synonym
type Monadish = Type -> Type
class MonadTrans (t :: Monadish -> Monadish) where
  lift :: Monad m => m a -> t m a
data Free :: Monadish where
  Return :: a -> Free a
  Bind   :: Free a -> (a -> Free b) -> Free b

-- yes, Type really does have type Type.
type Star = (Type :: (Type :: (Type :: Type)))
