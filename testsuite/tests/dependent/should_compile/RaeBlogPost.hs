{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies,
             TypeInType #-}
{-# OPTIONS_GHC -fwarn-unticked-promoted-constructors #-}

module RaeBlogPost where

import Data.Kind

-- a Proxy type with an explicit kind
data Proxy k (a :: k) = P
prox :: Proxy * Bool
prox = P

prox2 :: Proxy Bool 'True
prox2 = P

-- implicit kinds still work
data A
data B :: A -> *
data C :: B a -> *
data D :: C b -> *
data E :: D c -> *
-- note that E :: forall (a :: A) (b :: B a) (c :: C b). D c -> *

-- a kind-indexed GADT
data TypeRep (a :: k) where
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

data Vec :: * -> Nat -> * where
  Nil  :: Vec a 'Zero
  (:>) :: a -> Vec a n -> Vec a ('Succ n)
infixr 5 :>

-- promoted GADT, and using + as a "kind family":
type family (x :: Vec a n) ++ (y :: Vec a m) :: Vec a (n + m) where
  'Nil      ++ y = y
  (h ':> t) ++ y = h ':> (t ++ y)

-- datatype that mentions *
data U = Star (*)
       | Bool Bool

-- kind synonym
type Monadish = * -> *
class MonadTrans (t :: Monadish -> Monadish) where
  lift :: Monad m => m a -> t m a
data Free :: Monadish where
  Return :: a -> Free a
  Bind   :: Free a -> (a -> Free b) -> Free b

-- yes, * really does have type *.
type Star = (* :: (* :: (* :: *)))
