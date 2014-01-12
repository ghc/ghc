{-# LANGUAGE TypeFamilies, RankNTypes #-}

module T7804 where

type family F f a

data Proxy a = P

sDFMap :: (forall a. Proxy f -> Proxy a -> Proxy (F f a)) -> Int
sDFMap _ = 3
