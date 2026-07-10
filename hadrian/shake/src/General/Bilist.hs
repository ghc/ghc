
-- | List type that supports O(1) amortized 'cons', 'snoc', 'uncons' and 'isEmpty'.
module General.Bilist(
    Bilist, cons, snoc, uncons, toList, isEmpty
    ) where

import Data.Semigroup
import Prelude


data Bilist a = Bilist [a] [a]

toList :: Bilist a -> [a]
toList (Bilist as bs) = as ++ reverse bs

isEmpty :: Bilist a -> Bool
isEmpty (Bilist as bs) = null as && null bs

instance Eq a => Eq (Bilist a) where
    a == b = toList a == toList b

instance Semigroup (Bilist a) where
    a <> b = Bilist (toList a ++ toList b) []

instance Monoid (Bilist a) where
    mempty = Bilist [] []
    mappend = (<>)

cons :: a -> Bilist a -> Bilist a
cons x (Bilist as bs) = Bilist (x:as) bs

snoc :: Bilist a -> a -> Bilist a
snoc (Bilist as bs) x = Bilist as (x:bs)

uncons :: Bilist a -> Maybe (a, Bilist a)
uncons (Bilist [] []) = Nothing
uncons (Bilist (a:as) bs) = Just (a, Bilist as bs)
uncons (Bilist [] bs) = uncons $ Bilist (reverse bs) []
