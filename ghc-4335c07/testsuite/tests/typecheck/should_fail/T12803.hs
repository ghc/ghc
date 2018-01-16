{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
             TypeFamilies, FunctionalDependencies #-}

module T10778 where

type family F a :: *
class C a b | a -> b

instance C p (F q) => C p [q]
-- This instance should fail the (liberal) coverage condition
