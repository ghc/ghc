{-# LANGUAGE GADTs, RankNTypes #-}

module T12427b where

newtype Acquire a = Acquire {unAcquire :: (forall b. b -> b) -> IO a}

instance Functor Acquire where
    fmap = undefined

instance Applicative Acquire where
    pure = undefined
    (<*>) = undefined

instance Monad Acquire where
    Acquire f >>= g' = Acquire $ \restore -> do
        x <- f restore
        let Acquire g = g' x
        -- let g = unAcquire (g' x)
        g restore

