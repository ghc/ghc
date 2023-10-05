{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ApplicativeDo #-}

module T12490 where

import Prelude (Int, String, Functor(..), ($), undefined, (+))

join :: Monad f => f (f a) -> f a
join = undefined

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
    return :: a -> f a
    (>>=) :: f a -> (a -> f b) -> f b
    fail :: String -> f a

f_app :: Applicative f => f Int -> f Int -> f Int
f_app a b = do
    a' <- a
    b' <- b
    pure (a' + b')

f_monad :: Monad f => f Int -> f Int -> f Int
f_monad a b = do
    a' <- a
    b' <- b
    return $ a' + b'
