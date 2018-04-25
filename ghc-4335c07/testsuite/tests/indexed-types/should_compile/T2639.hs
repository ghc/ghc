{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

module T2639 where

data Eps

data family Work a v
data instance Work Eps v = Eps v

type family Dual a
type instance Dual Eps = Eps

class Connect s where
    connect :: (Dual s ~ c, Dual c ~ s) => Work s a -> Work c b -> (a,b)

instance Connect Eps where
    connect (Eps a) (Eps b) = (a,b)
