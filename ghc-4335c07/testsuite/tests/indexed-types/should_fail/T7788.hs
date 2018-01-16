{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T7788 where

data Proxy a = Proxy

foo :: Proxy (F (Fix Id)) -> ()
foo = undefined

newtype Fix a = Fix (a (Fix a))
newtype Id a = Id a

type family F a
type instance F (Fix a) = F (a (Fix a))
type instance F (Id a) = F a

main :: IO ()
main = print $ foo Proxy
