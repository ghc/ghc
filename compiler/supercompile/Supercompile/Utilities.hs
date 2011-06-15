module Supercompile.Utilities (
    module Supercompile.Utilities,

    module UniqSupply,
    module Unique,
    module Outputable,
    
    module Control.Arrow,
    module Control.Applicative,
    module Control.Monad,
    
    module Data.Maybe,
    module Data.List
  ) where

#include "HsVersions.h"

import UniqSupply
import Unique (Unique, getKey)
import Outputable

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative (Applicative(..))
import Control.Monad hiding (join)

import Data.Function (on)
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Ord
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import System.IO.Unsafe (unsafePerformIO)


-- | Copointed functors. The defining property is:
--
--   extract (fmap f a) == f (extract a)
class Functor f => Copointed f where
    extract :: f a -> a

instance Copointed ((,) a) where
    extract = snd


newtype Wrapper1 f a = Wrapper1 { unWrapper1 :: f a }


class Show1 f where
    showsPrec1 :: Show a => Int -> f a -> ShowS

instance (Show1 f, Show a) => Show (Wrapper1 f a) where
    showsPrec prec = showsPrec1 prec . unWrapper1


class Eq1 f where
    eq1 :: Eq a => f a -> f a -> Bool

instance (Eq1 f, Eq a) => Eq (Wrapper1 f a) where
    (==) = eq1 `on` unWrapper1


class Eq1 f => Ord1 f where
    compare1 :: Ord a => f a -> f a -> Ordering

instance (Ord1 f, Ord a) => Ord (Wrapper1 f a) where
    compare = compare1 `on` unWrapper1


class Outputable1 f where
    pprPrec1 :: Outputable a => Rational -> f a -> SDoc
    
    ppr1 :: Outputable a => f a -> SDoc
    ppr1 = pprPrec1 noPrec

instance (Outputable1 f, Outputable a) => Outputable (Wrapper1 f a) where
    pprPrec prec = pprPrec1 prec . unWrapper1

-- | Parenthesize an value if the boolean is true.
prettyParen :: Bool -> SDoc -> SDoc
prettyParen False = id
prettyParen True = parens

appPrec, opPrec, noPrec :: Num a => a
appPrec = 2    -- Argument of a function application
opPrec  = 1    -- Argument of an infix operator
noPrec  = 0    -- Others


angles, bananas :: SDoc -> SDoc
angles d = Outputable.char '<' <> d <> Outputable.char '>'
bananas d = text "(|" <> d <> text "|)"


newtype PrettyFunction = PrettyFunction (Rational -> SDoc)

instance Outputable PrettyFunction where
    pprPrec prec (PrettyFunction f) = f prec

asPrettyFunction :: Outputable a => a -> PrettyFunction
asPrettyFunction x = PrettyFunction (\prec -> pprPrec prec x)


instance Outputable IS.IntSet where
    ppr xs = braces $ hsep (punctuate comma (map ppr $ IS.toList xs))

instance Outputable a => Outputable (S.Set a) where
    ppr xs = braces $ hsep (punctuate comma (map ppr $ S.toList xs))


newtype PrettyDoc = PrettyDoc SDoc

instance Outputable PrettyDoc where
    ppr (PrettyDoc doc) = doc


newtype Identity a = I { unI :: a }

instance Copointed Identity where
    extract = unI

instance Monad Identity where
    return = I
    mx >>= fxmy = fxmy (unI mx)

instance Functor Identity where
    fmap f (I x) = I (f x)

instance Foldable.Foldable Identity where
    foldMap f (I x) = f x

instance Traversable.Traversable Identity where
    traverse f (I x) = pure I <*> f x

instance Show1 Identity where
    showsPrec1 prec (I x) = showParen (prec >= appPrec) (showString "Identity" . showsPrec appPrec x)

instance Eq1 Identity where
    eq1 (I x1) (I x2) = x1 == x2

instance Ord1 Identity where
    compare1 (I x1) (I x2) = x1 `compare` x2

instance Outputable1 Identity where
    pprPrec1 prec (I x) = pprPrec prec x


newtype (O f g) a = Comp { unComp :: f (g a) }

infixr 9 `O`

instance (Copointed f, Copointed g) => Copointed (O f g) where
    extract = extract . extract . unComp

instance (Functor f, Show1 f, Show1 g) => Show1 (O f g) where
    showsPrec1 prec (Comp x) = showParen (prec >= appPrec) (showString "Comp" . showsPrec1 appPrec (fmap Wrapper1 x))

instance (Functor f, Eq1 f, Eq1 g) => Eq1 (O f g) where
    eq1 (Comp x1) (Comp x2) = fmap Wrapper1 x1 `eq1` fmap Wrapper1 x2

instance (Functor f, Ord1 f, Ord1 g) => Ord1 (O f g) where
    compare1 (Comp x1) (Comp x2) = fmap Wrapper1 x1 `compare1` fmap Wrapper1 x2

instance (Functor f, Outputable1 f, Outputable1 g) => Outputable1 (O f g) where
    pprPrec1 prec (Comp x) = pprPrec1 prec (fmap Wrapper1 x)

instance (Functor f, Outputable1 f, Outputable1 g, Outputable a) => Outputable (O f g a) where
    pprPrec = pprPrec1

instance (Functor f, Functor g) => Functor (O f g) where
    fmap f (Comp x) = Comp (fmap (fmap f) x)

instance (Foldable.Foldable f, Foldable.Foldable g) => Foldable.Foldable (O f g) where
    foldMap f = Foldable.foldMap (Foldable.foldMap f) . unComp

instance (Traversable.Traversable f, Traversable.Traversable g) => Traversable.Traversable (O f g) where
    traverse f = fmap Comp . Traversable.traverse (Traversable.traverse f) . unComp


-- | Natural numbers on the cheap (for efficiency reasons)
type Nat = Int


newtype Fin = Fin { unFin :: Int } deriving (Eq, Ord)

instance Show Fin where
    show (Fin x) = show x

instance Outputable Fin where
    pprPrec prec (Fin x) = pprPrec prec x


type FinSet = IS.IntSet
type FinMap = IM.IntMap


data Tag = TG { tagFin :: Fin, tagOccurrences :: Nat } deriving (Eq, Ord, Show)

instance Outputable Tag where
    ppr (TG i occs) = ppr i <> brackets (ppr occs)

mkTag :: Int -> Tag
mkTag i = TG (Fin i) 1

injectTag :: Int -> Tag -> Tag
injectTag cls (TG (Fin i) occs) = TG (Fin (cls * i)) occs

tagInt :: Tag -> Int
tagInt = unFin . tagFin


data Tagged a = Tagged { tag :: !Tag, tagee :: !a }

instance Copointed Tagged where
    extract = tagee

instance Functor Tagged where
    fmap f (Tagged tg x) = Tagged tg (f x)

instance Foldable.Foldable Tagged where
    foldMap f (Tagged _ x) = f x

instance Traversable.Traversable Tagged where
    traverse f (Tagged tg x) = pure (Tagged tg) <*> f x

instance Show1 Tagged where
    showsPrec1 prec (Tagged tg x) = showParen (prec >= appPrec) (showString "Tagged" . showsPrec appPrec tg . showsPrec appPrec x)

instance Eq1 Tagged where
    eq1 (Tagged tg1 x1) (Tagged tg2 x2) = tg1 == tg2 && x1 == x2

instance Ord1 Tagged where
    compare1 (Tagged tg1 x1) (Tagged tg2 x2) = (tg1, x1) `compare` (tg2, x2)

instance Outputable1 Tagged where
    pprPrec1 prec (Tagged tg x) = braces (ppr tg) <+> pprPrec prec x


type Size = Int

data Sized a = Sized { size :: !Size, sizee :: !a }

instance Copointed Sized where
    extract = sizee

instance Functor Sized where
    fmap f (Sized sz x) = Sized sz (f x)

instance Foldable.Foldable Sized where
    foldMap f (Sized _ x) = f x

instance Traversable.Traversable Sized where
    traverse f (Sized sz x) = pure (Sized sz) <*> f x

instance Show1 Sized where
    showsPrec1 prec (Sized sz x) = showParen (prec >= appPrec) (showString "Sized" . showsPrec appPrec sz . showsPrec appPrec x)

instance Eq1 Sized where
    eq1 (Sized sz1 x1) (Sized sz2 x2) = sz1 == sz2 && x1 == x2

instance Ord1 Sized where
    compare1 (Sized sz1 x1) (Sized sz2 x2) = (sz1, x1) `compare` (sz2, x2)

instance Outputable1 Sized where
    pprPrec1 prec (Sized sz x) = bananas (text (show sz)) <> pprPrec prec x


pPrint :: Outputable a => a -> SDoc
pPrint = ppr

pPrintPrec :: Outputable a => Rational -> a -> SDoc
pPrintPrec = pprPrec


newtype Down a = Down { unDown :: a } deriving (Eq)

instance Ord a => Ord (Down a) where
    Down a `compare` Down b = b `compare` a


fmapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
fmapEither f g = either (Left . f) (Right . g)

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

plusMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
plusMaybe f (Just x) (Just y) = Just (f x y)
plusMaybe _ Nothing  mb_y     = mb_y
plusMaybe _ mb_x     Nothing  = mb_x


first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (a, b, c) = (f a, b, c)

second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (a, b, c) = (a, f b, c)

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (a, b, c) = (a, b, f c)

first4 :: (a -> e) -> (a, b, c, d) -> (e, b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)

second4 :: (b -> e) -> (a, b, c, d) -> (a, e, c, d)
second4 f (a, b, c, d) = (a, f b, c, d)

third4 :: (c -> e) -> (a, b, c, d) -> (a, b, e, d)
third4 f (a, b, c, d) = (a, b, f c, d)

fourth4 :: (d -> e) -> (a, b, c, d) -> (a, b, c, e)
fourth4 f (a, b, c, d) = (a, b, c, f d)


uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

splitBy :: [b] -> [a] -> ([a], [a])
splitBy []     xs     = ([], xs)
splitBy (_:ys) (x:xs) = first (x:) $ splitBy ys xs

splitManyBy :: [[b]] -> [a] -> [[a]]
splitManyBy []       xs = [xs]
splitManyBy (ys:yss) xs = case splitBy ys xs of (xs1, xs2) -> xs1 : splitManyBy yss xs2

listContexts :: [a] -> [([a], a, [a])]
listContexts xs = zipWith (\is (t:ts) -> (is, t, ts)) (inits xs) (init (tails xs))

bagContexts :: [a] -> [(a, [a])]
bagContexts xs = [(x, is ++ ts) | (is, x, ts) <- listContexts xs]

takeWhileJust :: (a -> Maybe b) -> [a] -> ([b], [a])
takeWhileJust f = go
  where
    go [] = ([], [])
    go (x:xs) = case f x of
        Nothing -> ([], x:xs)
        Just y  -> first (y:) $ go xs

accumLN :: (acc -> (acc, a)) -> acc -> Int -> (acc, [a])
accumLN f = go
  where
    go acc n | n <= 0            = (acc, []) 
             | (acc, x) <- f acc = second (x:) (go acc (n - 1))

sumMap :: (Foldable.Foldable f, Num b) => (a -> b) -> f a -> b
sumMap f = Foldable.foldr (\x n -> f x + n) 0


class (Functor t, Foldable.Foldable t) => Accumulatable t where
    mapAccumT  ::            (acc -> x ->   (acc, y)) -> acc -> t x ->   (acc, t y)
    mapAccumTM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> t x -> m (acc, t y)
    
    mapAccumT f acc x = unI (mapAccumTM (\acc' x' -> I (f acc' x')) acc x)

fmapDefault :: (Accumulatable t) => (a -> b) -> t a -> t b
fmapDefault f = snd . mapAccumT (\() x -> ((), f x)) ()

foldMapDefault :: (Accumulatable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = fst . mapAccumT (\acc x -> (f x `mappend` acc, ())) mempty

instance Accumulatable [] where
    mapAccumT  = mapAccumL
    mapAccumTM = mapAccumLM

mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM f = go []
  where
    go ys acc []     = return (acc, reverse ys)
    go ys acc (x:xs) = do
      (acc, y) <- f acc x
      go (y:ys) acc xs


zipWithEqualM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithEqualM _ []     []     = return []
zipWithEqualM f (x:xs) (y:ys) = liftM2 (:) (f x y) (zipWithEqualM f xs ys)
zipWithEqualM _ _ _ = fail "zipWithEqualM"


-- | Splits up a number evenly across several partitions in proportions to weights given to those partitions.
--
-- > sum (apportion n weights) == n
--
-- Annoyingly, it is important that this works properly if n is negative as well -- these can occur
-- when we have turned off deed checking. I don't care about handling negative weights.
apportion :: Int -> [Int] -> [Int]
apportion _      []        = error "apportion: empty list"
apportion orig_n weighting
  | orig_n < 0 = map negate $ apportion (negate orig_n) weighting
  | otherwise  = result
  where
    fracs :: [Rational]
    fracs = ASSERT2(denominator /= 0, text "apportion: must have at least one non-zero weight")
            map (\numerator -> fromIntegral numerator / denominator) weighting
      where denominator = fromIntegral (sum weighting)
    
    -- Here is the idea:
    --  1) Do one pass through the list of fractians
    --  2) Start by allocating the floor of the number of "n" that we should allocate to this weight of the fraction
    --  3) Accumulate the fractional pieces and the indexes that generated them
    --  4) Use circular programming to feed the list of fractional pieces that we actually allowed the allocation
    --     of back in to the one pass we are doing over the list
    ((_, remaining, final_deserving), result) = mapAccumL go (0 :: Int, orig_n, []) fracs
    go (i, n, deserving) frac = ((i + 1, n - whole, (i, remainder) : deserving),
                                 whole + if i `elem` final_deserving_allowed then 1 else 0)
      where (whole, remainder) = properFraction (frac * fromIntegral orig_n)
    
    -- We should prefer to allocate pieces to those bits of the fraction where the error (i.e. the fractional part) is greatest.
    -- We cannot allocate more of these "fixup" pieces than we had "n" left at the end of the first pass.
    final_deserving_allowed = map fst (take remaining (sortBy (comparing (Down . snd)) final_deserving))


{-# NOINLINE prettyUniqSupply #-}
supercompileUniqSupply, parseUniqSupply, expandUniqSupply, reduceUniqSupply, tagUniqSupply, prettyUniqSupply, matchUniqSupply, splitterUniqSupply :: UniqSupply
supercompileUniqSupply = unsafePerformIO $ mkSplitUniqSupply 'p'
(parseUniqSupply:expandUniqSupply:reduceUniqSupply:tagUniqSupply:prettyUniqSupply:matchUniqSupply:splitterUniqSupply:_) = listSplitUniqSupply supercompileUniqSupply
