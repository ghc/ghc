module System.Console.Haskeline.Monads(
                module System.Console.Haskeline.MonadException,
                MonadTrans(..),
                MonadIO(..),
                ReaderT,
                runReaderT,
                runReaderT',
                mapReaderT,
                asks,
                StateT,
                runStateT,
                evalStateT',
                mapStateT,
                gets,
                modify,
                update,
                MonadReader(..),
                MonadState(..),
                MaybeT(MaybeT),
                runMaybeT,
                orElse
                ) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT),runMaybeT)
import Control.Monad.Trans.Reader hiding (ask,asks)
import qualified Control.Monad.Trans.Reader as Reader
import Data.IORef
#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif

import System.Console.Haskeline.MonadException

class Monad m => MonadReader r m where
    ask :: m r

instance Monad m => MonadReader r (ReaderT r m) where
    ask = Reader.ask

instance Monad m => MonadReader s (StateT s m) where
    ask = get

instance (MonadReader r m, MonadTrans t, Monad (t m)) => MonadReader r (t m) where
    ask = lift ask

asks :: MonadReader r m => (r -> a) -> m a
asks f = liftM f ask

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

gets :: MonadState s m => (s -> a) -> m a
gets f = liftM f get

modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= put . f

update :: MonadState s m => (s -> (a,s)) -> m a
update f = do
    s <- get
    let (x,s') = f s
    put s'
    return x

runReaderT' :: Monad m => r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

newtype StateT s m a = StateT { getStateTFunc 
                                    :: forall r . s -> m ((a -> s -> r) -> r)}

instance Monad m => Functor (StateT s m) where
    fmap  = liftM

instance Monad m => Applicative (StateT s m) where
    pure x = StateT $ \s -> return $ \f -> f x s
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
    return = pure
    StateT f >>= g = StateT $ \s -> do
        useX <- f s
        useX $ \x s' -> getStateTFunc (g x) s'

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        x <- m
        return $ \f -> f x s

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

mapStateT :: (forall b . m b -> n b) -> StateT s m a -> StateT s n a
mapStateT f (StateT m) = StateT (\s -> f (m s))

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT f s = do
    useXS <- getStateTFunc f s
    return $ useXS $ \x s' -> (x,s')

makeStateT :: Monad m => (s -> m (a,s)) -> StateT s m a
makeStateT f = StateT $ \s -> do
                            (x,s') <- f s
                            return $ \g -> g x s'

instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> return $ \f -> f s s
    put s = s `seq` StateT $ \_ -> return $ \f -> f () s

instance (MonadState s m, MonadTrans t, Monad (t m)) => MonadState s (t m) where
    get = lift get
    put = lift . put

-- ReaderT (IORef s) is better than StateT s for some applications,
-- since StateT loses its state after an exception such as ctrl-c.
instance MonadIO m => MonadState s (ReaderT (IORef s) m) where
    get = ask >>= liftIO . readIORef
    put s = ask >>= liftIO . flip writeIORef s

evalStateT' :: Monad m => s -> StateT s m a -> m a
evalStateT' s f = liftM fst $ runStateT f s

instance MonadException m => MonadException (StateT s m) where
    controlIO f = makeStateT $ \s -> controlIO $ \run ->
                    fmap (flip runStateT s) $ f $ stateRunIO s run
      where
        stateRunIO :: s -> RunIO m -> RunIO (StateT s m)
        stateRunIO s (RunIO run) = RunIO (\m -> fmap (makeStateT . const)
                                        $ run (runStateT m s))

orElse :: Monad m => MaybeT m a -> m a -> m a
orElse (MaybeT f) g = f >>= maybe g return
