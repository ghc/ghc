module Control.Monad.X.ErrorT (
	ErrorT,
        runError,
        runErrorT,
	mapErrorT,
	module T
  ) where

import Prelude(Functor(..),Monad(..),Either(..),either,(.),id,error)

import Control.Monad(MonadPlus(..),liftM)

import Control.Monad.X.Trans as T
import Control.Monad.X.Utils
import Control.Monad.X.Types(ErrorT(..))


instance MonadTrans (ErrorT e) where
  lift m    = E (liftM Right m)

instance HasBaseMonad m n => HasBaseMonad (ErrorT e m) n where
  inBase    = inBase'

instance (Monad m) => Functor (ErrorT e m) where
  fmap      = liftM

instance (Monad m) => Monad (ErrorT e m) where
  return    = return'
  m >>= k   = E (do a <- unE m
                    case a of
                      Left  l -> return (Left l)
                      Right r -> unE (k r))
  fail      = fail'   -- use 'throwErorr' to throw errors.


--------------------------------------------------------------------------------

runError    = unE
runErrorT   = unE

mapErrorT :: (m (Either e a) -> n (Either e' b)) -> ErrorT e m a -> ErrorT e' n b
mapErrorT f m = E (f (unE m))

--------------------------------------------------------------------------------

instance (MonadReader r m) => MonadReader r (ErrorT e m) where
  ask       = ask'
  local     = local' mapErrorT 

instance (MonadWriter w m) => MonadWriter w (ErrorT e m) where
  tell      = tell'
  listen    = listen1' E unE (\w -> either Left (\r -> Right (r,w)))

instance (MonadState s m) => MonadState s (ErrorT e m) where
  get       = get'
  put       = put'

instance (Monad m) => MonadError e (ErrorT e m) where
  throwError       = E . return . Left 
  m `catchError` h = E (do a <- unE m
                           case a of
                             Left  l -> unE (h l)
                             Right r -> return (Right r))

-- MonadPlus is used for Nondet, these should be moved in the nondet class
instance MonadPlus m => MonadPlus (ErrorT e m) where
  mzero       = mzero'
  mplus       = mplus1' E unE

-- `findAll` is like catMaybes, it will aways succeed, but will only return 
-- results that didn't raise an exception.
-- if all results a required, use handle to turn the failures into (tagged) successes.
instance MonadNondet m => MonadNondet (ErrorT e m) where
  findAll     = mapErrorT (liftM res . findAll)
    where res xs = Right [ x | Right x <- xs ]
  commit      = mapErrorT commit

instance MonadResume m => MonadResume (ErrorT e m) where
  delay       = mapErrorT delay
  force       = mapErrorT force

instance (MonadCont m) => MonadCont (ErrorT e m) where
  callCC            = callCC1' E unE Right



