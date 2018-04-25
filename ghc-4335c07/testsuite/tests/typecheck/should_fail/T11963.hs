{-# LANGUAGE GADTs, PolyKinds, RankNTypes #-}

module T11963 where

-- this module should be rejected without TypeInType

import Data.Proxy

-- see code in RnTypes.extract_hs_tv_bndrs which checks for these bad cases

  -- bndr_kvs vs body_tvs
data Typ k t  where
    Typ :: (forall (a :: k -> *). a t -> a t) -> Typ k t

  -- bndr_kvs vs acc_tvs
foo :: (forall (t :: k). Proxy t) -> Proxy k
foo _ = undefined

  -- locals vs body_kvs
bar :: forall k. forall (t :: k). Proxy t
bar = undefined

  -- body_kvs vs acc_tvs
quux :: (forall t. Proxy (t :: k)) -> Proxy k
quux _ = undefined

  -- body_tvs vs acc_kvs
blargh :: (forall a. a -> Proxy k) -> Proxy (t :: k)
blargh _ = undefined
