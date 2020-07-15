
module T10176(buggy) where

{-# NOINLINE error2Args #-}
error2Args :: () -> () -> a
error2Args _ _ = error "here"

newtype ReaderT r a = ReaderT { runReaderT :: r -> IO a }

instance Functor (ReaderT r) where
    fmap = undefined

instance Applicative (ReaderT r) where
    pure    = liftReaderT . pure
    f <*> v = undefined
    m *> k  = ReaderT $ \r -> do runReaderT m r; runReaderT k r

instance Monad (ReaderT r) where
    m >>= k  = undefined

liftReaderT :: IO a -> ReaderT r a
liftReaderT m = ReaderT (const m)

{-# NOINLINE buggy #-}
buggy fun unit bool =
    runReaderT (do
        if bool then liftReaderT $ print () else pure ()
        case fun unit of
            True -> do
                error2Args unit unit
                pure ()
            _ -> pure ()
        ) () :: IO ()
