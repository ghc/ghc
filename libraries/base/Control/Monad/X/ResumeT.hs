module Control.Monad.X.ResumeT
  (ResumeT,
   hyper,
   module T
  ) where

import Prelude(Functor(..),Monad(..),error)
import Control.Monad(liftM,MonadPlus(..))

import Control.Monad.X.Trans as T
import Control.Monad.X.Utils
import Control.Monad.X.Types (ResumeT(..), Res(..))

-- resumptions:
-- a transformer for explicit "lazyness"


instance MonadTrans ResumeT where
  lift m  = Re (liftM Value m) 

instance Monad m => Functor (ResumeT m) where
  fmap    = liftM

instance Monad m => Monad (ResumeT m) where
  return  = return'
  m >>= f = Re (do x <- unRe m
                   case x of
                     Value a -> unRe (f a)
                     Delay m -> return (Delay (m >>= f)))

instance HasBaseMonad m n => HasBaseMonad (ResumeT m) n where
  inBase    = inBase'

instance Monad m => Functor (Res m) where
  fmap f (Value a)      = Value (f a)
  fmap f (Delay m)      = Delay (liftM f m)



hyper       :: Monad m => ResumeT m a -> m a
hyper m     = do x <- unRe m
                 case x of
                   Value a -> return a
                   Delay m -> hyper m

mapResumeT f m  = Re (f (unRe m))

instance MonadReader r m => MonadReader r (ResumeT m) where
  ask       = ask'
  local     = local' mapResumeT

instance MonadWriter w m => MonadWriter w (ResumeT m) where
  tell      = tell'
  listen    = listen1' Re unRe (\w -> fmap (\a -> (a,w)))

instance MonadState s m => MonadState s (ResumeT m) where
  get       = get'
  put       = put'

instance MonadError e m => MonadError e (ResumeT m) where
  throwError  = throwError'
  catchError  = catchError1' Re unRe

instance MonadPlus m => MonadPlus (ResumeT m) where
  mzero     = mzero'
  mplus     = mplus1' Re unRe

instance MonadNondet m => MonadNondet (ResumeT m) where
  findAll   = error "findAll ResumeT TODO"
  commit    = mapResumeT commit

instance Monad m => MonadResume (ResumeT m) where
  delay m   = Re (return (Delay m))
  force m   = Re (do x <- unRe m
                     case x of
                       Value a  -> return (Value a)
                       Delay m' -> unRe m')

instance MonadCont m => MonadCont (ResumeT m) where
  callCC = callCC1' Re unRe Value



