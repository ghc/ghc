{-# LANGUAGE CPP #-}

-- | We expect 'liftA2' to inline into 'f1'. We test this by
-- grepping for occurrences of the 'Applicative' dictionary.
module RWST where

import Data.Functor.Identity
import Control.Applicative
import Control.Monad

newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (w, s, a) }

instance Functor m => Functor (RWST r w s m) where
  fmap f (RWST m) = RWST (\r s -> fmap (\ ~(w,s',x) -> (w,s',f x)) (m r s))

instance (Monoid w, Monad m) => Applicative (RWST r w s m) where
  pure x = RWST (\_ s -> pure (mempty, s, x))
  RWST mf <*> RWST mx = RWST (\r s -> do
    ~(w, s', f) <- mf r s
    ~(w', s'', x) <- mx r s'
    pure (w <> w', s'', f x))

type Pairing m a b = m a -> m b -> m (a, b)

f1 :: Pairing (RWST r [a] s Identity) a b
f1 x y = liftA2 (,) x y

