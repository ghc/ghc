{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, OverlappingInstances, UndecidableInstances #-}
module Main where

class (Monad m) => MonadIO m where
    -- | Lift a computation from the 'IO' monad.
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

class XMLGenerator m where
    genElement :: (Maybe String, String) -> m ()

newtype IdentityT m a = IdentityT { runIdentityT :: m a }
    deriving (Monad, MonadIO)

instance (MonadIO m) => (XMLGenerator (IdentityT m)) where
    genElement _ = liftIO $ putStrLn "in genElement"

main :: IO ()
main = 
    do runIdentityT web
       putStrLn "done."

class (Widgets x) => MonadRender x
class (XMLGenerator m)  => Widgets m
-- instance Widgets (IdentityT IO) -- if you uncomment this, it will work
instance MonadRender m => Widgets m
instance MonadRender (IdentityT IO)

web :: ( MonadIO m
       , Widgets m
       , XMLGenerator m
       ) => m ()
web =
    do liftIO $ putStrLn "before"
       genElement (Nothing, "p")
       return ()
