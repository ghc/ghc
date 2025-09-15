{-# LANGUAGE GADTs #-}

module T2151 where

data Type a where
    Func :: Type a -> Type b -> Type (a -> b)
    PF :: Type a -> Type (PF a)

data PF a where
    ID :: PF (a -> a)

test :: Type a -> a -> a
test (PF (Func _ _)) ID = ID
