{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls #-}

module T2627b where

data R a b
data W a b
data Z

type family Dual a
type instance Dual Z = Z
type instance Dual (R a b) = W a (Dual b)
type instance Dual (W a b) = R a (Dual b)

data Comm a where
    Rd :: (a -> Comm b) -> Comm (R a b)
    Wr :: a  -> Comm b  -> Comm (W a b)
    Fin :: Int -> Comm Z

conn :: (Dual a ~ b, Dual b ~ a) => Comm a -> Comm b -> (Int, Int)
conn (Rd k) (Wr a r) = conn undefined undefined
