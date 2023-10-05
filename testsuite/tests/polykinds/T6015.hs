{-# LANGUAGE DataKinds, MultiParamTypeClasses, FunctionalDependencies,
             PolyKinds, UndecidableInstances, ScopedTypeVariables #-}

module T6015 where

data Proxy a = Proxy

class Value a t | a -> t where value :: Proxy a -> t

instance Value True Bool where value _ = True

instance Value a t => Value (Just a) (Maybe t)
    where value _ = Just (value (Proxy :: Proxy a))

test = value (Proxy :: Proxy (Just True))
