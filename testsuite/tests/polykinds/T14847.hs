{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14847 where

data Proxy k (a :: k) = MkProxy
data Proxy2 k a = MkP (Proxy k a)

data Proxy2' k a where
  MkP' :: Proxy k a -> Proxy2' k a

data T a where
  T :: Int -> T Bool

type family F a where
  F Int = True
  F _   = False
