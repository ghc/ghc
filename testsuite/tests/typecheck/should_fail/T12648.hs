{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module T12648 where

import GHC.Exts (Constraint)
import Unsafe.Coerce (unsafeCoerce)

type family Skolem (p :: k -> Constraint) :: k
type family Forall (p :: k -> Constraint) :: Constraint
type instance Forall p = Forall_ p
class p (Skolem p) => Forall_ (p :: k -> Constraint)
instance p (Skolem p) => Forall_ (p :: k -> Constraint)

inst :: forall p a. Forall p :- p a
inst = unsafeCoerce (Sub Dict :: Forall p :- p (Skolem p))

data Dict :: Constraint -> * where
  Dict :: a => Dict a

newtype a :- b = Sub (a => Dict b)

infixl 1 \\ -- required comment

(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

class (Applicative b, Applicative m, Monad b, Monad m) => MonadBase b m | m -> b

instance MonadBase IO IO -- where liftBase = id

class MonadBase b m => MonadBaseControl b m | m -> b where
  type StM m a :: *
  liftBaseWith :: (RunInBase m b -> b a) -> m a

type RunInBase m b = forall a. m a -> b (StM m a)

instance MonadBaseControl IO IO where
    type StM IO a = a
    liftBaseWith f = f id
    {-# INLINABLE liftBaseWith #-}

class    (StM m a ~ a) => IdenticalBase m a
instance (StM m a ~ a) => IdenticalBase m a

newtype UnliftBase b m = UnliftBase { unliftBase :: forall a. m a -> b a }

mkUnliftBase :: forall m a b. (Forall (IdenticalBase m), Monad b)
             => (forall c. m c -> b (StM m c)) -> m a -> b a
mkUnliftBase r act = r act \\ (inst :: Forall (IdenticalBase m) :- IdenticalBase m a)

class    (MonadBaseControl b m, Forall (IdenticalBase m)) => MonadBaseUnlift b m | m -> b
instance (MonadBaseControl b m, Forall (IdenticalBase m)) => MonadBaseUnlift b m

askUnliftBase :: forall b m. (MonadBaseUnlift b m) => m (UnliftBase b m)
askUnliftBase = liftBaseWith unlifter
  where
    unlifter :: (forall c. m c -> b (StM m c)) -> b (UnliftBase b m)
    unlifter r = return $ UnliftBase (mkUnliftBase r)

f :: (MonadBaseUnlift m IO) => m a
f = do

 _ <- askUnliftBase

 return ()
