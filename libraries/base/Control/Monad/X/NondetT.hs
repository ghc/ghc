module Control.Monad.X.NondetT
  (NondetT,
   runNondet,
   runNondets,
   mapNondetT,
   MonadPlus(..),
   module T
  ) where

import Prelude 
import Monad(liftM,MonadPlus(..))

import Control.Monad.X.Trans as T
import Control.Monad.X.Utils
import Control.Monad.X.Types(NondetT(..),T(..))


instance MonadTrans NondetT where
  lift m            = N (liftM single m)

instance Monad m => Functor (NondetT m) where
  fmap              = liftM

instance Monad m => Monad (NondetT m) where
  return            = return'
  m >>= f           = N (do x <- unN m
                            case x of
                              Empty -> return Empty 
                              Cons a xs -> unN (mplus (f a) (xs >>= f)))

instance HasBaseMonad m n => HasBaseMonad (NondetT m) n where
  inBase            = inBase'


-- misc functions
instance Monad m => Functor (T m) where
  fmap f Empty      = Empty
  fmap f (Cons a m) = Cons (f a) (fmap f m)


single x            = Cons x mzero

flatten             :: Monad m => T m a -> m [a]
flatten Empty       = return []
flatten (Cons a m)  = liftM (a :) (runNondets m)


runNondet m         = do t <- unN m
                         case t of
                           Empty -> return Nothing
                           Cons a _ -> return (Just a)

runNondets m        = flatten =<< unN m 

mapNondetT f (N m)  = N (f m)


-- other features.

instance MonadReader r m => MonadReader r (NondetT m) where
  ask               = ask'
  local             = local' mapNondetT

instance MonadWriter w m => MonadWriter w (NondetT m) where
  tell              = tell'
  listen            = listen1' N unN (\w -> fmap (\a -> (a,w)))

instance MonadState s m => MonadState s (NondetT m) where
  get               = get'
  put               = put'

instance MonadError e m => MonadError e (NondetT m) where
  throwError        = throwError'
  catchError        = catchError1' N unN

instance Monad m => MonadPlus (NondetT m) where
  mzero             = N (return Empty)
  mplus m n         = N (do x <- unN m
                            case x of
                              Empty -> unN n
                              Cons a m' -> return (Cons a (mplus m' n)))

instance Monad m => MonadNondet (NondetT m) where
  findAll m         = lift (runNondets m)
  commit m          = N (do x <- unN m
                            case x of
                              Empty -> return Empty
                              Cons a _ -> return (single a))

-- ergh, what does this do?
instance (MonadCont m) => MonadCont (NondetT m) where
  callCC            = callCC1' N unN single

   




