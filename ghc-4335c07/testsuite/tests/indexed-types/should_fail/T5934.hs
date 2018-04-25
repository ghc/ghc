{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, TypeFamilies, KindSignatures #-}

-- The type of 'run' is actually ambiguous

module T5934 where
import Control.Monad.ST

data Gen s
type GenST s = Gen (PrimState (ST s))

run :: (forall s. GenST s) -> Int
run = 0 

type family PrimState (m :: * -> *)
