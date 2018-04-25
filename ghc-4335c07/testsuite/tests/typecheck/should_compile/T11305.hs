{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Profunctor.Strong where

import Control.Arrow
import Control.Category
import Data.Tuple
import Prelude hiding (id,(.))

infixr 0 :->
type p :-> q = forall a b. p a b -> q a b

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

class ProfunctorFunctor t where
  promap    :: Profunctor p => (p :-> q) -> t p :-> t q

class ProfunctorFunctor t => ProfunctorMonad t where
  proreturn :: Profunctor p => p       :-> t p
  projoin   :: Profunctor p => t (t p) :-> t p

class ProfunctorFunctor t => ProfunctorComonad t where
  proextract   :: Profunctor p => t p :-> p
  produplicate :: Profunctor p => t p :-> t (t p)

class Profunctor p => Strong p where
  first' :: p a b  -> p (a, c) (b, c)
  first' = dimap swap swap . second'

  second' :: p a b -> p (c, a) (c, b)
  second' = dimap swap swap . first'

----------------------------------------------------------------------------

newtype Tambara p a b = Tambara { runTambara :: forall c. p (a, c) (b, c) }

instance Profunctor p => Profunctor (Tambara p) where
  dimap f g (Tambara p) = Tambara $ dimap (first f) (first g) p

instance ProfunctorFunctor Tambara where
  promap f (Tambara p) = Tambara (f p)

instance ProfunctorComonad Tambara where
  proextract (Tambara p) = dimap (\a -> (a,())) fst p

  produplicate (Tambara p) = Tambara (Tambara $ dimap hither yon p)
    where
      hither :: ((a, b), c) -> (a, (b, c))
      hither ~(~(x,y),z) = (x,(y,z))

      yon    :: (a, (b, c)) -> ((a, b), c)
      yon    ~(x,~(y,z)) = ((x,y),z)

instance Profunctor p => Strong (Tambara p) where
  first' = runTambara . produplicate
