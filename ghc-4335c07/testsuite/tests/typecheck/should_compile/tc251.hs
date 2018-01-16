{-# LANGUAGE TypeFamilies #-}
module ShouldCompile where

import Tc251_Help

instance Cls Int where

inc :: (Fam a ~ Maybe Int) => a -> Fam a -> Fam a
inc _proxy (Just x) = Just (x + 1)
inc _proxy Nothing  = Just 0

foo :: Maybe Int -> Maybe Int
foo = inc (undefined :: Int)
