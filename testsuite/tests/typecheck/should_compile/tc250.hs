{-# LANGUAGE TypeFamilies #-}
module ShouldCompile where

import Data.Kind (Type)

class Cls a where
    type Fam a :: Type
    type Fam a = Maybe a

instance Cls Int where
    -- Gets type family from default

inc :: (Fam a ~ Maybe Int) => a -> Fam a -> Fam a
inc _proxy (Just x) = Just (x + 1)
inc _proxy Nothing  = Just 0

foo :: Maybe Int -> Maybe Int
foo = inc (undefined :: Int)
