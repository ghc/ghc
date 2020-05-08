{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provide trees (of instructions), so that lists of instructions can be
-- appended in linear time.
module GHC.Data.OrdList (
        OrdList,
        nilOL, isNilOL, unitOL, appOL, consOL, snocOL, concatOL, lastOL,
        headOL, unConsOL, unSnocOL, viewSingle,
        mapOL, fromOL, toOL, foldrOL, foldlOL, reverseOL, fromOLReverse,
        strictlyEqOL, strictlyOrdOL, strictlyZipWith
) where

import GHC.Prelude
import GHC.Exts (IsList (..))
import Data.Data
import Data.Foldable
import qualified Data.Semigroup as Semigroup

import GHC.Utils.Outputable


infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = None
  | One a
  | Many [a]          -- Invariant: non-empty
  | Cons a (OrdList a)
  | Snoc (OrdList a) a
  | Two (OrdList a) -- Invariant: non-empty
        (OrdList a) -- Invariant: non-empty
  deriving (Data, Functor)

instance Outputable a => Outputable (OrdList a) where
  ppr ol = ppr (fromOL ol)  -- Convert to list and print that

instance Semigroup (OrdList a) where
  (<>) = appOL

instance Monoid (OrdList a) where
  mempty = nilOL
  mappend = (Semigroup.<>)
  mconcat = concatOL

instance Foldable OrdList where
  foldMap f = \case
    None -> mempty
    One a -> f a
    Many as -> foldMap f as
    Cons a as -> f a Semigroup.<> foldMap f as
    Snoc as a -> foldMap f as Semigroup.<> f a
    Two as bs -> foldMap f as Semigroup.<> foldMap f bs
  foldr   = foldrOL
  foldl'  = foldlOL
  toList  = fromOL
  null    = isNilOL
  length  = lengthOL

instance Traversable OrdList where
  traverse f xs = toOL <$> traverse f (fromOL xs)

instance IsList (OrdList a) where
  type Item (OrdList a) = a
  fromList = toOL
  toList = fromOL

nilOL    :: OrdList a
isNilOL  :: OrdList a -> Bool

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a
headOL   :: OrdList a   -> a
lastOL   :: OrdList a   -> a
unConsOL :: OrdList a -> Maybe (a, OrdList a)
unSnocOL :: OrdList a -> Maybe (OrdList a, a)
lengthOL :: OrdList a   -> Int

nilOL        = None
unitOL as    = One as
snocOL as   b    = Snoc as b
consOL a    bs   = Cons a bs
concatOL aas = foldr appOL None aas

headOL = maybe (panic "headOL") fst . unConsOL

unConsOL None           = Nothing
unConsOL (One a)        = Just (a, None)
unConsOL (Many ~(a:as)) = Just (a, toOL as)
unConsOL (Cons a as)    = Just (a, as)
unConsOL (Snoc as _)    = unConsOL as
unConsOL (Two as _)     = unConsOL as

lastOL = maybe (panic "headOL") snd . unSnocOL

unSnocOL None        = Nothing
unSnocOL (One a)     = Just (None, a)
unSnocOL (Many as)   = Just (toOL $ init as, last as)
unSnocOL (Cons _ as) = unSnocOL as
unSnocOL (Snoc as a) = Just (as, a)
unSnocOL (Two _ as)  = unSnocOL as

lengthOL None        = 0
lengthOL (One _)     = 1
lengthOL (Many as)   = length as
lengthOL (Cons _ as) = 1 + length as
lengthOL (Snoc as _) = 1 + length as
lengthOL (Two as bs) = length as + length bs

isNilOL None = True
isNilOL _    = False

-- | @Just@ if there is only a single item in the list. This is O(1) unlike
-- other accessors which would do needless work in our @Nothing@ case.
viewSingle :: OrdList a -> Maybe a
viewSingle = \case
  None        -> Nothing
  One a       -> Just a
  Many as     -> Just $ head as
  Cons a None -> Just a
  Cons _ _    -> Nothing
  Snoc None a -> Just a
  Snoc _ _    -> Nothing
  Two _ _     -> Nothing

None  `appOL` b     = b
a     `appOL` None  = a
One a `appOL` b     = Cons a b
a     `appOL` One b = Snoc a b
a     `appOL` b     = Two a b

fromOL :: OrdList a -> [a]
fromOL a = go a []
  where go None       acc = acc
        go (One a)    acc = a : acc
        go (Cons a b) acc = a : go b acc
        go (Snoc a b) acc = go a (b:acc)
        go (Two a b)  acc = go a (go b acc)
        go (Many xs)  acc = xs ++ acc

fromOLReverse :: OrdList a -> [a]
fromOLReverse a = go a []
        -- acc is already in reverse order
  where go :: OrdList a -> [a] -> [a]
        go None       acc = acc
        go (One a)    acc = a : acc
        go (Cons a b) acc = go b (a : acc)
        go (Snoc a b) acc = b : go a acc
        go (Two a b)  acc = go b (go a acc)
        go (Many xs)  acc = reverse xs ++ acc

mapOL :: (a -> b) -> OrdList a -> OrdList b
mapOL = fmap

foldrOL :: (a->b->b) -> b -> OrdList a -> b
foldrOL _ z None        = z
foldrOL k z (One x)     = k x z
foldrOL k z (Cons x xs) = k x (foldrOL k z xs)
foldrOL k z (Snoc xs x) = foldrOL k (k x z) xs
foldrOL k z (Two b1 b2) = foldrOL k (foldrOL k z b2) b1
foldrOL k z (Many xs)   = foldr k z xs

-- | Strict left fold.
foldlOL :: (b->a->b) -> b -> OrdList a -> b
foldlOL _ z None        = z
foldlOL k z (One x)     = k z x
foldlOL k z (Cons x xs) = let !z' = (k z x) in foldlOL k z' xs
foldlOL k z (Snoc xs x) = let !z' = (foldlOL k z xs) in k z' x
foldlOL k z (Two b1 b2) = let !z' = (foldlOL k z b1) in foldlOL k z' b2
foldlOL k z (Many xs)   = foldl' k z xs

toOL :: [a] -> OrdList a
toOL [] = None
toOL [x] = One x
toOL xs = Many xs

reverseOL :: OrdList a -> OrdList a
reverseOL None = None
reverseOL (One x) = One x
reverseOL (Cons a b) = Snoc (reverseOL b) a
reverseOL (Snoc a b) = Cons b (reverseOL a)
reverseOL (Two a b)  = Two (reverseOL b) (reverseOL a)
reverseOL (Many xs)  = Many (reverse xs)

-- | Compare not only the values but also the structure of two lists
strictlyEqOL :: Eq a => OrdList a   -> OrdList a -> Bool
strictlyEqOL None         None       = True
strictlyEqOL (One x)     (One y)     = x == y
strictlyEqOL (Cons a as) (Cons b bs) = a == b && as `strictlyEqOL` bs
strictlyEqOL (Snoc as a) (Snoc bs b) = a == b && as `strictlyEqOL` bs
strictlyEqOL (Two a1 a2) (Two b1 b2) = a1 `strictlyEqOL` b1 && a2 `strictlyEqOL` b2
strictlyEqOL (Many as)   (Many bs)   = as == bs
strictlyEqOL _            _          = False

-- | Compare not only the values but also the structure of two lists
strictlyOrdOL :: Ord a => OrdList a   -> OrdList a -> Ordering
strictlyOrdOL None         None       = EQ
strictlyOrdOL None         _          = LT
strictlyOrdOL (One x)     (One y)     = compare x y
strictlyOrdOL (One _)      _          = LT
strictlyOrdOL (Cons a as) (Cons b bs) =
  compare a b `mappend` strictlyOrdOL as bs
strictlyOrdOL (Cons _ _)   _          = LT
strictlyOrdOL (Snoc as a) (Snoc bs b) =
  compare a b `mappend` strictlyOrdOL as bs
strictlyOrdOL (Snoc _ _)   _          = LT
strictlyOrdOL (Two a1 a2) (Two b1 b2) =
  (strictlyOrdOL a1 b1) `mappend` (strictlyOrdOL a2 b2)
strictlyOrdOL (Two _ _)    _          = LT
strictlyOrdOL (Many as)   (Many bs)   = compare as bs
strictlyOrdOL (Many _ )   _           = GT

-- | Zip to ord lists relying on them having the same structure not just the
-- same length
strictlyZipWith :: (a -> b -> c) -> OrdList a -> OrdList b -> OrdList c
strictlyZipWith f x y = case (x, y) of
  (None, None) -> None
  (One a, One b) -> One $ f a b
  (Many as, Many bs) -> Many $ zipWith f as bs          -- Invariant: non-empty
  (Cons a as, Cons b bs) -> Cons (f a b) (strictlyZipWith f as bs)
  (Snoc as a, Snoc bs b) -> Snoc (strictlyZipWith f as bs) (f a b)
  (Two a0 a1, Two b0 b1) -> Two (strictlyZipWith f a0 b0) (strictlyZipWith f a1 b1)
  (_, _) -> None
