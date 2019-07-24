{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


This is useful, general stuff for the Native Code Generator.

Provide trees (of instructions), so that lists of instructions
can be appended in linear time.
-}
{-# LANGUAGE DeriveFunctor #-}

module OrdList (
        OrdList,
        nilOL, isNilOL,
        unitOL, appOL, consOL, snocOL, concatOL, lastOL, headOL,
        mapOL, fromOL, toOL, foldrOL, foldlOL, reverseOL, fromOLReverse,
        NonEmptyOrdList,
        unitOLNE, appOLNE, consOLNE, snocOLNE, concatOLNE, lastOLNE, headOLNE,
        mapOLNE, fromOLNE, toOLNE, foldrOLNE, foldlOLNE, reverseOLNE, fromOLReverseNE
) where

import GhcPrelude

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL

import Outputable

import qualified Data.Semigroup as Semigroup

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = None
  | NE {-# UNPACK #-} !(NonEmptyOrdList a)
  deriving (Functor)

data NonEmptyOrdList a
  = One a
  | Many (NonEmpty a)
  | Cons a (OrdList a)
  | Snoc (OrdList a) a
  | Two (NonEmptyOrdList a)
        (NonEmptyOrdList a)
  deriving (Functor)

instance Outputable a => Outputable (OrdList a) where
  ppr ol = ppr (fromOL ol)  -- Convert to list and print that

instance Outputable a => Outputable (NonEmptyOrdList a) where
  ppr ol = ppr (fromOLNE ol)  -- Convert to list and print that

instance Semigroup (OrdList a) where
  (<>) = appOL

instance Semigroup (NonEmptyOrdList a) where
  (<>) = appOLNE

instance Monoid (OrdList a) where
  mempty = nilOL
  mappend = (Semigroup.<>)
  mconcat = concatOL

instance Foldable OrdList where
  foldr = foldrOL

instance Foldable NonEmptyOrdList where
  foldr = foldrOLNE

instance Traversable OrdList where
  traverse f xs = toOL <$> traverse f (fromOL xs)

instance Traversable NonEmptyOrdList where
  traverse f xs = toOLNE <$> traverse f (fromOLNE xs)

nilOL    :: OrdList a
isNilOL  :: OrdList a -> Bool

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a
headOL   :: OrdList a   -> a
lastOL   :: OrdList a   -> a

unitOLNE   :: a           -> NonEmptyOrdList a
snocOLNE   :: OrdList a   -> a         -> NonEmptyOrdList a
consOLNE   :: a           -> OrdList a -> NonEmptyOrdList a
appOLNE    :: NonEmptyOrdList a   -> NonEmptyOrdList a -> NonEmptyOrdList a
concatOLNE :: NonEmpty (NonEmptyOrdList a) -> NonEmptyOrdList a
headOLNE   :: NonEmptyOrdList a   -> a
lastOLNE   :: NonEmptyOrdList a   -> a


nilOL        = None
unitOL as    = NE $ unitOLNE as
snocOL as   b    = NE $ snocOLNE as b
consOL a    bs   = NE $ consOLNE a bs
concatOL aas = foldr appOL None aas

unitOLNE as    = One as
snocOLNE as   b    = Snoc as b
consOLNE a    bs   = Cons a bs
concatOLNE aas = foldr1 appOLNE aas

headOL None        = panic "headOL"
headOL (NE ol)     = headOLNE ol

headOLNE (One a)     = a
headOLNE (Many as)   = NEL.head as
headOLNE (Cons a _)  = a
headOLNE (Snoc as _) = headOL as
headOLNE (Two as _)  = headOLNE as

lastOL None        = panic "lastOL"
lastOL (NE ol)     = lastOLNE ol

lastOLNE (One a)     = a
lastOLNE (Many as)   = NEL.last as
lastOLNE (Cons _ as) = lastOL as
lastOLNE (Snoc _ a)  = a
lastOLNE (Two _ as)  = lastOLNE as

isNilOL None = True
isNilOL _    = False

None  `appOL` b     = b
a     `appOL` None  = a
NE a  `appOL` NE b  = NE $ a `appOLNE` b

One a `appOLNE` b     = Cons a (NE b)
a     `appOLNE` One b = Snoc (NE a) b
a     `appOLNE` b     = Two a b

fromOL :: OrdList a -> [a]
fromOL a = fromOLAcc a []

fromOLNE :: NonEmptyOrdList a -> NonEmpty a
fromOLNE a = fromOLAccNE a []

fromOLReverse :: OrdList a -> [a]
fromOLReverse a = fromOLReverseAcc a []

fromOLReverseNE :: NonEmptyOrdList a -> NonEmpty a
fromOLReverseNE a = fromOLReverseAccNE a []

fromOLAcc :: OrdList a -> [a] -> [a]
fromOLAcc None       acc = acc
fromOLAcc (NE ol)    acc = toList $ fromOLAccNE ol acc

fromOLAccNE :: NonEmptyOrdList a -> [a] -> NonEmpty a
fromOLAccNE (One a)    acc = a :| acc
fromOLAccNE (Cons a b) acc = a :| fromOLAcc b acc
fromOLAccNE (Snoc a b) acc = x :| xs
  where
    -- incomplete is OK because accum is non-empty
    x : xs = fromOLAcc a (b : acc)
fromOLAccNE (Two a b)  acc = fromOLAccNE a (toList $ fromOLAccNE b acc)
fromOLAccNE (Many (x :| xs)) acc = x :| (xs ++ acc)

-- acc is already in reverse order
fromOLReverseAcc :: OrdList a -> [a] -> [a]
fromOLReverseAcc None       acc = acc
fromOLReverseAcc (NE ol)    acc = toList $ fromOLReverseAccNE ol acc

fromOLReverseAccNE :: NonEmptyOrdList a -> [a] -> NonEmpty a
fromOLReverseAccNE (One a)    acc = a :| acc
fromOLReverseAccNE (Cons a b) acc = x :| xs
  where
    -- incomplete is OK because accum is non-empty
    x : xs = fromOLReverseAcc b (a : acc)
fromOLReverseAccNE (Snoc a b) acc = b :| fromOLReverseAcc a acc
fromOLReverseAccNE (Two a b)  acc = fromOLReverseAccNE b (toList $ fromOLReverseAccNE a acc)
fromOLReverseAccNE (Many xs)  acc = x' :| (xs' ++ acc)
  where x' :| xs' = NEL.reverse xs

mapOL :: (a -> b) -> OrdList a -> OrdList b
mapOL = fmap

mapOLNE :: (a -> b) -> NonEmptyOrdList a -> NonEmptyOrdList b
mapOLNE = fmap

foldrOL :: (a->b->b) -> b -> OrdList a -> b
foldrOL _ z None        = z
foldrOL k z (NE ol)     = foldrOLNE k z ol

foldrOLNE :: (a->b->b) -> b -> NonEmptyOrdList a -> b
foldrOLNE k z (One x)     = k x z
foldrOLNE k z (Cons x xs) = k x (foldrOL k z xs)
foldrOLNE k z (Snoc xs x) = foldrOL k (k x z) xs
foldrOLNE k z (Two b1 b2) = foldrOLNE k (foldrOLNE k z b2) b1
foldrOLNE k z (Many xs)   = foldr k z xs

foldlOL :: (b->a->b) -> b -> OrdList a -> b
foldlOL _ z None        = z
foldlOL k z (NE ol)     = foldlOLNE k z ol

foldlOLNE :: (b->a->b) -> b -> NonEmptyOrdList a -> b
foldlOLNE k z (One x)     = k z x
foldlOLNE k z (Cons x xs) = foldlOL k (k z x) xs
foldlOLNE k z (Snoc xs x) = k (foldlOL k z xs) x
foldlOLNE k z (Two b1 b2) = foldlOLNE k (foldlOLNE k z b1) b2
foldlOLNE k z (Many xs)   = foldl k z xs

toOL :: [a] -> OrdList a
toOL [] = None
toOL (x : xs) = NE $ toOLNE $ x :| xs

toOLNE :: NonEmpty a -> NonEmptyOrdList a
toOLNE (x :| []) = One x
toOLNE xs = Many xs

reverseOL :: OrdList a -> OrdList a
reverseOL None = None
reverseOL (NE ol) = NE $ reverseOLNE ol

reverseOLNE :: NonEmptyOrdList a -> NonEmptyOrdList a
reverseOLNE (One x) = One x
reverseOLNE (Cons a b) = Snoc (reverseOL b) a
reverseOLNE (Snoc a b) = Cons b (reverseOL a)
reverseOLNE (Two a b)  = Two (reverseOLNE b) (reverseOLNE a)
reverseOLNE (Many xs)  = Many (NEL.reverse xs)
