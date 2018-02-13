{-# LANGUAGE RankNTypes #-}

module T14618 where

safeCoerce :: a -> b
safeCoerce = f'
    where
        f :: d -> forall c. d
        f x = x

        f' = f
