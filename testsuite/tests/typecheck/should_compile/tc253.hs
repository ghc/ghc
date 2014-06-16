{-# LANGUAGE TypeFamilies #-}
module ShouldCompile where

class Cls a where
    type Fam a b :: *
    -- Multiple defaults!
    type Fam a Bool = Maybe a
    type Fam a Int = (String, a)

instance Cls Int where
    -- Gets type family from default

inc :: (Fam a Bool ~ Maybe Int, Fam a Int ~ (String, Int)) => a -> Fam a Bool -> Fam a Int -> Fam a Bool
inc _proxy (Just x) (_, y) = Just (x + y + 1)
inc _proxy Nothing  (_, y) = Just y

foo :: Maybe Int -> (String, Int) -> Maybe Int
foo = inc (undefined :: Int)
