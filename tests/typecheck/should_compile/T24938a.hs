{-# LANGUAGE TypeFamilies, GADTs #-}

module T24938a where

type family F a

data T b where
   MkT :: forall a b. F b ~ a => a -> T b
          -- This equality is a let-bound skolem

f (MkT x) = True
