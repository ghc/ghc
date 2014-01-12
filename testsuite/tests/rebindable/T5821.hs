{-# LANGUAGE
    ExplicitForAll
  , GADTs
  , RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module T5821a 
       ( Writer
       , runWriter
       , execWriter
       , WriterT
       , runWriterT
       , execWriterT
       , tell
       ) where

import Control.Category (Category (id), (>>>))

import Prelude hiding (Monad (..), id)
import qualified Prelude

newtype Identity a = Identity { runIdentity :: a }

class Monad m where
  (>>=) :: forall e ex x a b . m e ex a -> (a -> m ex x b) -> m e x b
  (>>) :: forall e ex x a b . m e ex a -> m ex x b -> m e x b
  return :: a -> m ex ex a
  fail :: String -> m e x a
  
  {-# INLINE (>>) #-}
  m >> k = m >>= \ _ -> k
  fail = error

type Writer w = WriterT w Identity

runWriter :: Writer w e x a -> (a, w e x)
runWriter = runIdentity . runWriterT

execWriter :: Writer w e x a -> w e x
execWriter m = snd (runWriter m)

newtype WriterT w m e x a = WriterT { runWriterT :: m (a, w e x) }

execWriterT :: Prelude.Monad m => WriterT w m e x a -> m (w e x)
execWriterT m = do
  ~(_, w) <- runWriterT m
  return w
  where
    (>>=) = (Prelude.>>=)
    return = Prelude.return

instance (Category w, Prelude.Monad m) => Monad (WriterT w m) where
  return a = WriterT $ return (a, id)
    where
      return = Prelude.return
  m >>= k = WriterT $ do
    ~(a, w) <- runWriterT m
    ~(b, w') <- runWriterT (k a)
    return (b, w >>> w')
    where
      (>>=) = (Prelude.>>=)
      return = Prelude.return
  fail msg = WriterT $ fail msg
    where
      fail = Prelude.fail

tell :: (Category w, Prelude.Monad m) => w e x -> WriterT w m e x ()
tell w = WriterT $ return ((), w)
  where
    return = Prelude.return


