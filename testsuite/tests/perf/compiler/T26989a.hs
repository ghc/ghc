{-# LANGUAGE DeriveFunctor #-}

module T26989a where

import Control.Monad (ap)

newtype WriterT a = WriterT (IO (a, [()]))
    deriving Functor

instance Monad WriterT where
    WriterT m >>= k  = WriterT $ do
        (a, w)  <- m
        let WriterT m' = k a
        (b, w') <- m'
        return (b, w ++ w')

instance Applicative WriterT where
    pure a = WriterT $ return (a, [])
    (<*>) = ap

{-# OPAQUE spec #-}
spec :: WriterT ()
spec = WriterT $ return ((), [()])
