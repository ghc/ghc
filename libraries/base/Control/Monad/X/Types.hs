module Control.Monad.X.Types where

import Control.Monad(MonadPlus(..))

newtype ReaderT r m a = R { unR :: r -> m a }
newtype WriterT w m a = W { unW :: m (a, w) }
newtype StateT s m a  = S { unS :: s -> m (a,s) }
newtype ErrorT e m a  = E { unE :: m (Either e a) }
newtype NondetT m a   = N { unN :: m (T m a) }
newtype ResumeT m a   = Re { unRe :: m (Res m a) }
newtype ContT r m a   = C { unC :: (a -> m r) -> m r }

data T m a            = Empty | Cons a (NondetT m a)
data Res m a          = Value a | Delay (ResumeT m a)


