{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


This is useful, general stuff for the Native Code Generator.

Provide trees (of instructions), so that lists of instructions
can be appended in linear time.
-}
{-# LANGUAGE DeriveFunctor #-}

{-# LANGUAGE BangPatterns #-}

module OrdList (
        OrdList,
        nilOL, isNilOL, unitOL, appOL, consOL, snocOL, concatOL, lastOL,
        headOL,
        initOL, tailOL, unsnocOL, unconsOL,
        mapOL, fromOL, toOL, foldrOL, foldlOL, reverseOL, fromOLReverse,
        strictlyEqOL, strictlyOrdOL
) where

import GhcPrelude
import Data.Foldable

import Outputable

import qualified Data.Semigroup as Semigroup

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
  deriving (Functor)

instance Outputable a => Outputable (OrdList a) where
  ppr ol = ppr (fromOL ol)  -- Convert to list and print that

instance Semigroup (OrdList a) where
  (<>) = appOL

instance Monoid (OrdList a) where
  mempty = nilOL
  mappend = (Semigroup.<>)
  mconcat = concatOL

instance Foldable OrdList where
  foldr   = foldrOL
  foldl'  = foldlOL
  toList  = fromOL
  null    = isNilOL
  length  = lengthOL

instance Traversable OrdList where
  traverse f xs = toOL <$> traverse f (fromOL xs)

nilOL    :: OrdList a
isNilOL  :: OrdList a -> Bool

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a
headOL   :: OrdList a   -> a
lastOL   :: OrdList a   -> a
lengthOL :: OrdList a   -> Int
initOL   :: OrdList a   -> OrdList a
tailOL   :: OrdList a   -> OrdList a
unsnocOL :: OrdList a   -> (OrdList a, a)
unconsOL :: OrdList a   -> (a, OrdList a)

nilOL        = None
unitOL as    = One as
snocOL as   b    = Snoc as b
consOL a    bs   = Cons a bs
concatOL aas = foldr appOL None aas

headOL None        = panic "headOL"
headOL (One a)     = a
headOL (Many as)   = head as
headOL (Cons a _)  = a
headOL (Snoc as _) = headOL as
headOL (Two as _)  = headOL as

lastOL None        = panic "lastOL"
lastOL (One a)     = a
lastOL (Many as)   = last as
lastOL (Cons _ as) = lastOL as
lastOL (Snoc _ a)  = a
lastOL (Two _ as)  = lastOL as

initOL None                = panic "initOL"
initOL (One _)             = None
initOL (Many [_])          = None
initOL (Many as)           = Many (init as)
initOL (Cons a (Many [_])) = One a
initOL (Cons a (One _))    = One a
initOL (Cons a as)         = Cons a (initOL as)
initOL (Snoc as _)         = as
initOL (Two as (Many [_])) = as
initOL (Two as (One _))    = as
initOL (Two as bs)         = Two as (initOL bs)

tailOL None                = panic "initOL"
tailOL (One _)             = None
tailOL (Many [_])          = None
tailOL (Many as)           = Many (tail as)
tailOL (Cons _ as)         = as
tailOL (Snoc (Many [_]) b) = One b
tailOL (Snoc (One _) b)    = One b
tailOL (Snoc as b)         = Snoc (tailOL as) b
tailOL (Two (Many [_]) bs) = bs
tailOL (Two (One _)    bs) = bs
tailOL (Two as bs)         = Two (tailOL as) bs

unconsOL None      = panic "unconsOL"
unconsOL as        = (headOL as, tailOL as)

unsnocOL None      = panic "unsnocOL"
unsnocOL as        = (initOL as, lastOL as)

lengthOL None        = 0
lengthOL (One _)     = 1
lengthOL (Many as)   = length as
lengthOL (Cons _ as) = 1 + length as
lengthOL (Snoc as _) = 1 + length as
lengthOL (Two as bs) = length as + length bs

isNilOL None = True
isNilOL _    = False

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


