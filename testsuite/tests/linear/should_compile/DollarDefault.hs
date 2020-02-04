{-# LANGUAGE LinearTypes #-}
module DollarDefault where

class C p where
  cid :: p a a -> p a a

instance C (->) where
  cid = id

foo = (cid $ id) $ ()
