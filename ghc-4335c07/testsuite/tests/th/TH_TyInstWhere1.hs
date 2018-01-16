{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies #-}

module TH_TyInstWhere1 where

$([d| type family F (a :: k) (b :: k) :: Bool where
        F a a = True
        F a b = False |])

data Proxy a = P

f :: Proxy True -> Proxy (F Int Int)
f x = x

g :: Proxy False -> Proxy (F Int Bool)
g x = x 