{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module T9371 where

import Data.Monoid

class C x where
    data D x :: *
    makeD :: D x

instance {-# OVERLAPPABLE #-} Monoid x => C x where
    data D x = D1 (Either x ())
    makeD = D1 (Left mempty)

instance (Monoid x, Monoid y) => C (x, y) where
    data D (x,y) = D2 (x,y)
    makeD = D2 (mempty, mempty)

instance Show x => Show (D x) where
    show (D1 x) = show x


main = print (makeD :: D (String, String))
