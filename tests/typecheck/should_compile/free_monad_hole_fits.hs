{-# OPTIONS_GHC -frefinement-level-hole-fits=2 #-}
{-# LANGUAGE MonoLocalBinds #-}
module TG where
import Prelude ( Monad, Applicative, Functor
               , fmap, (<*>), pure, (>>=)
               , (=<<), ($), (<$>) )

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f = go where
      go (Pure a) =  Pure (f a)
      -- Should suggest (fmap)
      go (Free fa) = Free (_a go fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure (a b)
  Pure a <*> Free mb = Free (fmap a <$> mb)
  Free ma <*> b = Free ((<*> b) <$> ma)

instance Applicative f => Monad (Free f) where
    Pure a >>= f = f a
      -- Should suggest ((=<< (_ :: a -> Free f b))
    Free f >>= g = Free (fmap _a f)
