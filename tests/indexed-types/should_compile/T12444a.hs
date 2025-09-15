{-# LANGUAGE KindSignatures, TypeFamilies, GADTs, DataKinds #-}

module T12444a where

type family F a :: *
type instance F (Maybe x) = Maybe (F x)

foo :: a -> Maybe (F a)
foo = undefined

-- bad :: (F (Maybe t) ~ t) => Maybe t -> [Maybe t]
bad x = [x, foo x]
