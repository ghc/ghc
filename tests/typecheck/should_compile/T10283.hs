{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}
{-# LANGUAGE ImpredicativeTypes #-}

module T9834 where
import Control.Applicative
import Data.Functor.Identity

type Nat f g = forall a. f a -> g a

newtype Comp p q a = Comp (p (q a))

liftOuter :: (Functor p, Applicative q) => p a -> (Comp p q) a
liftOuter pa = Comp (pure <$> pa)

runIdComp :: Functor p => Comp p Identity a -> p a
runIdComp (Comp p) = runIdentity <$> p

wrapIdComp :: Applicative p => (forall q. Applicative q => Nat (Comp p q) (Comp p q)) -> p a -> p a
wrapIdComp f = runIdComp . f . liftOuter

class Applicative p => ApplicativeFix p where
  afix :: (forall q. Applicative q => (Comp p q) a -> (Comp p q) a) -> p a
  afix f = wrapIdComp f
