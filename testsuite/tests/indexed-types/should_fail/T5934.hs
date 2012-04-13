{-# LANGUAGE RankNTypes, TypeFamilies, KindSignatures #-}

module T5934 where
import Control.Monad.ST

data Gen s
type GenST s = Gen (PrimState (ST s))

run :: (forall s. GenST s) -> Int
run = 0 

type family PrimState (m :: * -> *)
