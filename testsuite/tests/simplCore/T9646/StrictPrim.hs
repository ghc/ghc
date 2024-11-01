{-# LANGUAGE BangPatterns, MagicHash, NoImplicitPrelude, RankNTypes,
    TypeFamilies, UnboxedTuples, UnliftedFFITypes #-}

module StrictPrim
    ( StrictPrim
    , PrimMonad (..)
    , runStrictPrim
    ) where


import GHC.Base

newtype StrictPrim s a
    = StrictPrim (State# s -> (# State# s, a #))

instance Applicative (StrictPrim s) where
    {-# INLINE pure #-}
    pure !x = StrictPrim ( \ !s -> (# s, x #))

    {-# INLINE (*>) #-}
    (!m) *> (!k) = do { _ <- m ;  k }

    {-# INLINE (<*>) #-}
    (<*>) a b = do f <- a ; v <- b ; return $! (f $! v)

instance Functor (StrictPrim s) where
    {-# INLINE fmap #-}
    fmap !f (StrictPrim !m) = StrictPrim $ \ !s ->
        case m s of
            (# !new_s,!r #) -> (# new_s, f $! r #)


instance Monad (StrictPrim s) where

    {-# INLINE (>>=) #-}
    (StrictPrim !m) >>= (!k) =
        StrictPrim ( \ !s ->
            case m s of
                (# new_s, r #) -> case k r of
                    StrictPrim k2 -> k2 new_s
            )

instance PrimMonad (StrictPrim s) where
    type PrimState (StrictPrim s) = s
    {-# INLINE primitive #-}
    primitive = StrictPrim


{-# INLINE runStrictPrim #-}
runStrictPrim :: (forall s. StrictPrim s a) -> a
runStrictPrim !st =
    case st of
        StrictPrim st_rep ->
            case st_rep realWorld# of
                (# _, !r #) -> r

class Monad m => PrimMonad m where
    type PrimState m
    primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a
