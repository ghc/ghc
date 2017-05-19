{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE KindSignatures, MultiParamTypeClasses, TypeFamilies,
             TypeOperators #-}

module T13784 where

import Data.Monoid ((<>))

data Product :: [*] -> * where
    (:*) :: a -> Product as -> Product (a : as)
    Unit :: Product '[]
infixr 5 :*

instance Show (Product '[]) where
    show Unit = "Unit"

instance (Show a, Show (Product as)) => Show (Product (a : as)) where
    show (a :* as) = show a <> " :* " <> show as

class Divideable a as where
    type Divide a as :: [*]
    divide :: Product as -> (a, Product (Divide a as))

instance Divideable a (a : as) where
    -- type Divide a (a : as) = as
    -- Conflicting type family instances, seems like OVERLAPS isn't a thing for
    -- type families.
    divide (a :* as) = (a, as)

instance Divideable b as => Divideable b (a : as) where
    type Divide b (a : as) = a : Divide b as
    divide (a :* as) = a :* divide as
