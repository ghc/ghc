{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
  -- this is needed because |FamHelper a x| /< |Fam a x|
  -- This file compiled with -dunique-increment=-1 made GHC crash before

module T11361 where

import Data.Kind (Type)

class Cls a where
    type Fam a b :: Type
    -- Multiple defaults!
    type Fam a x = FamHelper a x

type family FamHelper a x
type instance FamHelper a Bool = Maybe a
type instance FamHelper a Int  = (String, a)

instance Cls Int where
    -- Gets type family from default

inc :: (Fam a Bool ~ Maybe Int, Fam a Int ~ (String, Int)) => a -> Fam a Bool -> Fam a Int -> Fam a Bool
inc _proxy (Just x) (_, y) = Just (x + y + 1)
inc _proxy Nothing  (_, y) = Just y

foo :: Maybe Int -> (String, Int) -> Maybe Int
foo = inc (undefined :: Int)
