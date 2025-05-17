{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls #-}

module T2627b where

import Data.Kind

data R a b
data W a b
data Z

type family Dual a
type instance Dual Z = Z
type instance Dual (R a (b :: Type)) = W a (Dual b)
type instance Dual (W a (b :: Type)) = R a (Dual b)

data Comm a where
    Rd :: (c -> Comm d) -> Comm (R c d)
    Wr :: e  -> Comm f  -> Comm (W e f)
    Fin :: Int -> Comm Z

conn :: (Dual a ~ b, Dual b ~ a) => Comm a -> Comm b -> (Int, Int)
conn (Rd k) (Wr a r) = conn undefined undefined

{-
 [G] a ~ R c d
 [G] b ~ W e f
 [W] Dual alpha ~ beta, [W] Dual beta ~ alpha
-}