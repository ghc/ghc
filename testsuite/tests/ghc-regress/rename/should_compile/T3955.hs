{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T3955 where

newtype Reader r a = Reader { runReader :: r -> a }
class (Monad m) => MonadReader r m where
    ask   :: m r

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r
instance MonadReader r (Reader r) where
    ask       = Reader id

newtype T a x = T (Reader a x)
    deriving (Monad, MonadReader a)

{-  Trac #3955 reported
[1 of 1] Compiling Main             ( bug.hs, interpreted )
mkUsageInfo: internal name? a{tv amy}
Ok, modules loaded: Main.
-}
