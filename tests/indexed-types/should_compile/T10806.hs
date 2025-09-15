{-# LANGUAGE GADTs, ExplicitNamespaces, TypeOperators, DataKinds  #-}

module T10806 where

import GHC.TypeLits (Nat, type (<=))

data Q a where
    Q :: (a <= b, b <= c) => proxy a -> proxy b -> Q c

triggersLoop :: Q b -> Q b -> Bool
triggersLoop (Q _ _) (Q _ _) = print 'x' 'y'
