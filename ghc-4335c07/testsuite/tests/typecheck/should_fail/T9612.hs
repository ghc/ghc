{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module T9612 where
import Data.Monoid
import Control.Monad.Trans.Writer.Lazy( Writer, WriterT )
import Data.Functor.Identity( Identity )

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    writer :: (a,w) -> m a
    tell   :: w -> m ()
    listen :: m a -> m (a, w)
    pass   :: m (a, w -> w) -> m a

f ::(Eq a) => a -> (Int, a) -> Writer [(Int, a)] (Int, a)
f y (n,x) {- | y == x    = return (n+1, x)
             | otherwise = -}
   = do tell (n,x)
        return (1,y)


instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
