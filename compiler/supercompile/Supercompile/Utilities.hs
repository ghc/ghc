{-# LANGUAGE RankNTypes #-} -- For callCC
module Supercompile.Utilities (
    module Supercompile.Utilities,

    module UniqSupply,
    module Unique,
    module Outputable,
    
    module Control.Arrow,
    module Control.Applicative,
    module Control.Monad,
    
    module Data.Foldable,
    module Data.Traversable,
    module Data.Maybe,
    module Data.List
  ) where

#include "HsVersions.h"

import UniqSupply
import UniqSet
import Unique (Uniquable(..), Unique, getKey)
import UniqFM (UniqFM, eltsUFM)
import Maybes (expectJust)
import Outputable hiding (Depth)
import State hiding (mapAccumLM)

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Exception (bracket)
import Control.Monad hiding (join)

import Data.Function (on)
import Data.Maybe
import Data.Ord
import Data.List
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Graph as G
import qualified Data.Foldable as Foldable
import Data.Monoid (Monoid(mappend, mempty))

import System.Directory
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process


{-# NOINLINE pprPreview2 #-}
pprPreview2 :: String -> SDoc -> SDoc -> a -> a
pprPreview2 fp_base doc1 doc2 x = unsafePerformIO $ do
    withTempFile (fp_base ++ "-1") $ \fp1 h1 -> do
        hPutStrLn h1 (showSDoc doc1) >> hFlush h1
        withTempFile (fp_base ++ "-2") $ \fp2 h2 -> do
            hPutStrLn h2 (showSDoc doc2) >> hFlush h2
            ec <- system $ "$EDITOR " ++ fp1 ++ " " ++ fp2
            case ec of ExitSuccess   -> return x
                       ExitFailure c -> error $ "pprPreview2(" ++ fp_base ++ "): preview failed with exit code " ++ show c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile fp_base act = do
    tmp_dir <- getTemporaryDirectory
    bracket (openTempFile tmp_dir fp_base) (hClose . snd) (uncurry act)



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


-- Because we have this class, we can define Outputable for
-- Supercompile.Core.Syntax.TermF without UndecidableInstances
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

asPrettyFunction1 :: (Outputable1 f, Outputable a) => f a -> PrettyFunction
asPrettyFunction1 = asPrettyFunction . Wrapper1


instance Outputable IS.IntSet where
    ppr xs = braces $ hsep (punctuate comma (map ppr $ IS.toList xs))


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

instance Applicative Identity where
    pure = I
    I f <*> I x = I (f x)

instance Foldable Identity where
    foldMap f (I x) = f x

instance Traversable Identity where
    traverse f (I x) = pure I <*> f x

instance Show1 Identity where
    showsPrec1 prec (I x) = showParen (prec >= appPrec) (showString "Identity" . showsPrec appPrec x)

instance Eq1 Identity where
    eq1 (I x1) (I x2) = x1 == x2

instance Ord1 Identity where
    compare1 (I x1) (I x2) = x1 `compare` x2

instance Outputable1 Identity where
    pprPrec1 prec (I x) = pprPrec prec x

instance Show a => Show (Identity a) where
    showsPrec = showsPrec1

instance Eq a => Eq (Identity a) where
    (==) = eq1

instance Ord a => Ord (Identity a) where
    compare = compare1

instance Outputable a => Outputable (Identity a) where
    pprPrec = pprPrec1


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

instance (Functor f, Show1 f, Show1 g, Show a) => Show (O f g a) where
    showsPrec = showsPrec1

instance (Functor f, Eq1 f, Eq1 g, Eq a) => Eq (O f g a) where
    (==) = eq1

instance (Functor f, Ord1 f, Ord1 g, Ord a) => Ord (O f g a) where
    compare = compare1

instance (Functor f, Outputable1 f, Outputable1 g, Outputable a) => Outputable (O f g a) where
    pprPrec = pprPrec1

instance (Functor f, Functor g) => Functor (O f g) where
    fmap f (Comp x) = Comp (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (O f g) where
    pure = Comp . pure . pure
    mf <*> mx = Comp $ liftA2 (<*>) (unComp mf) (unComp mx)

instance (Foldable f, Foldable g) => Foldable (O f g) where
    foldMap f = foldMap (foldMap f) . unComp

instance (Traversable f, Traversable g) => Traversable (O f g) where
    traverse f = fmap Comp . traverse (traverse f) . unComp


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

{-# INLINE injectTag #-} -- Was accounting for 2% of allocations
injectTag :: Int -> Tag -> Tag
injectTag cls (TG (Fin i) occs) = TG (Fin (cls * i)) occs

tagInt :: Tag -> Int
tagInt = unFin . tagFin


data Tagged a = Tagged { tag :: !Tag, tagee :: !a }

instance Copointed Tagged where
    extract = tagee

instance Functor Tagged where
    fmap f (Tagged tg x) = Tagged tg (f x)

instance Foldable Tagged where
    foldMap f (Tagged _ x) = f x

instance Traversable Tagged where
    traverse f (Tagged tg x) = pure (Tagged tg) <*> f x

instance Show1 Tagged where
    showsPrec1 prec (Tagged tg x) = showParen (prec >= appPrec) (showString "Tagged" . showsPrec appPrec tg . showsPrec appPrec x)

instance Eq1 Tagged where
    eq1 (Tagged tg1 x1) (Tagged tg2 x2) = tg1 == tg2 && x1 == x2

instance Ord1 Tagged where
    compare1 (Tagged tg1 x1) (Tagged tg2 x2) = (tg1, x1) `compare` (tg2, x2)

instance Outputable1 Tagged where
    pprPrec1 prec (Tagged tg x) = braces (ppr tg) <+> pprPrec prec x

instance Show a => Show (Tagged a) where
    showsPrec = showsPrec1

instance Eq a => Eq (Tagged a) where
    (==) = eq1

instance Ord a => Ord (Tagged a) where
    compare = compare1

instance Outputable a => Outputable (Tagged a) where
    pprPrec = pprPrec1


type Size = Int

data Sized a = Sized { size :: !Size, sizee :: !a }

instance Copointed Sized where
    extract = sizee

instance Functor Sized where
    fmap f (Sized sz x) = Sized sz (f x)

instance Foldable Sized where
    foldMap f (Sized _ x) = f x

instance Traversable Sized where
    traverse f (Sized sz x) = pure (Sized sz) <*> f x

instance Show1 Sized where
    showsPrec1 prec (Sized sz x) = showParen (prec >= appPrec) (showString "Sized" . showsPrec appPrec sz . showsPrec appPrec x)

instance Eq1 Sized where
    eq1 (Sized sz1 x1) (Sized sz2 x2) = sz1 == sz2 && x1 == x2

instance Ord1 Sized where
    compare1 (Sized sz1 x1) (Sized sz2 x2) = (sz1, x1) `compare` (sz2, x2)

instance Outputable1 Sized where
    pprPrec1 prec (Sized sz x) = bananas (text (show sz)) <> pprPrec prec x

instance Show a => Show (Sized a) where
    showsPrec = showsPrec1

instance Eq a => Eq (Sized a) where
    (==) = eq1

instance Ord a => Ord (Sized a) where
    compare = compare1

instance Outputable a => Outputable (Sized a) where
    pprPrec = pprPrec1


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

extractJusts :: (a -> Maybe b) -> [a] -> ([b], [a])
extractJusts p = foldr step ([], [])
  where step x rest | Just y <- p x = first  (y:) rest
                    | otherwise     = second (x:) rest


checkEqual :: Eq a => a -> a -> Maybe a
checkEqual = checkEqualBy (==)

checkEqualBy :: (a -> a -> Bool) -> a -> a -> Maybe a
checkEqualBy eq x y | x `eq` y  = Just y
                    | otherwise = Nothing


secondM :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
secondM f (x, y) = fmap ((,) x) (f y)

-- Stricter than 'first'
first2 :: (a -> c) -> (a, b) -> (c, b)
first2 f (a, b) = (f a, b)

-- Stricter than 'second'
second2 :: (b -> c) -> (a, b) -> (a, c)
second2 f (a, b) = (a, f b)

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

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

expectHead :: String -> [a] -> a
expectHead s = expectJust s . safeHead

splitBy :: [b] -> [a] -> ([a], Either [b] [a])
splitBy []     xs     = ([], Right xs)
splitBy (y:ys) []     = ([], Left (y:ys))
splitBy (_:ys) (x:xs) = first (x:) $ splitBy ys xs

splitByReverse :: [b] -> [a] -> (Either [b] [a], [a])
splitByReverse ys xs = case splitBy (reverse ys) (reverse xs) of (xs1, ei_ys1_xs2) -> (either (Left . reverse) (Right . reverse) ei_ys1_xs2, reverse xs1)

listContexts :: [a] -> [([a], a, [a])]
listContexts xs = zipWith (\is (t:ts) -> (is, t, ts)) (inits xs) (init (tails xs))

bagContexts :: [a] -> [(a, [a])]
bagContexts xs = [(x, is ++ ts) | (is, x, ts) <- listContexts xs]

dropLastWhile :: (a -> Bool) -> [a] -> [a]
dropLastWhile p = reverse . dropWhile p . reverse

takeFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
takeFirst f = go []
  where go _   []     = Nothing
        go acc (x:xs) = if f x
                        then Just (x, reverse acc ++ xs)
                        else go (x:acc) xs

takeWhileJust :: (a -> Maybe b) -> [a] -> ([b], [a])
takeWhileJust f = go
  where
    go [] = ([], [])
    go (x:xs) = case f x of
        Nothing -> ([], x:xs)
        Just y  -> first (y:) $ go xs

maximumsComparing :: Ord b => (a -> b) -> [a] -> [a]
maximumsComparing _ []     = error "maximumsComparing: empty input"
maximumsComparing f (x:xs) = go (f x) [x] xs
  where
    go _       maxs []     = reverse maxs
    go the_max maxs (x:xs) = case this `compare` the_max of
        LT -> go the_max maxs     xs
        EQ -> go the_max (x:maxs) xs
        GT -> go this    [x]      xs
      where this = f x

accumLN :: (acc -> (acc, a)) -> acc -> Int -> (acc, [a])
accumLN f = go
  where
    go acc n | n <= 0            = (acc, []) 
             | (acc, x) <- f acc = second (x:) (go acc (n - 1))

sumMap :: (Foldable f, Num b) => (a -> b) -> f a -> b
sumMap f = Foldable.foldr (\x n -> f x + n) 0

sumMapMonoid :: (Foldable f, Monoid b) => (a -> b) -> f a -> b
sumMapMonoid f = Foldable.foldr (\x n -> f x `mappend` n) mempty

{-# INLINE groups #-}
groups :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
groups f g xs = runs f g (sortBy (comparing f) xs)

{-# INLINE runs #-}
runs :: Eq b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
runs _ _ []     = []
runs f g (x:xs) = go (f x) [g x] xs
  where go b pending []     = [(b, reverse pending)]
        go b pending (x:xs)
          | b == b'   = go b (g x:pending) xs
          | otherwise = (b, reverse pending) : go b' [g x] xs
          where b' = f x

distinct :: Ord a => [a] -> Bool
distinct xs = length xs == S.size (S.fromList xs)


-- | Orders elements of a map into dependency order insofar as that is possible.
--
-- This function ignores any elements reported as reachable that are not present in the input.
--
-- An element (b1 :: b) strictly precedes (b2 :: b) in the output whenever b1 is reachable from b2 but not vice versa.
-- Element b1 occurs in the same SCC as b2 whenever both b1 is reachable from b2 and b1 is reachable from b2.
topologicalSort :: Ord a => (b -> UniqFM a) -> M.Map a b -> [M.Map a b]
topologicalSort f got = [M.fromList [(a, b) | (b, a, _) <- G.flattenSCC scc] | scc <- G.stronglyConnCompR [(b, a, eltsUFM (f b)) | (a, b) <- M.toList got]]


restrict :: Ord k => M.Map k v -> S.Set k -> M.Map k v
-- restrict m s
--   | M.size m < S.size s = M.filterWithKey (\k _ -> k `S.member` s) m                                                   -- O(m * log s)
--   | otherwise           = S.fold (\k out -> case M.lookup k m of Nothing -> out; Just v -> M.insert k v out) M.empty s -- O(s * log m)
restrict m s = M.fromDistinctAscList $ merge (M.toAscList m) (S.toAscList s)
  where
    -- Theoretically O(m + s), so should outperform previous algorithm...
    merge _              []       = []
    merge []             _        = []
    merge ((k_m, v):kvs) (k_s:ks) = case compare k_m k_s of
        LT ->          merge kvs            (k_s:ks)
        EQ -> (k_m, v):merge kvs            ks
        GT ->          merge ((k_m, v):kvs) ks

exclude :: Ord k => M.Map k v -> S.Set k -> M.Map k v
--exclude m s = M.filterWithKey (\k _ -> k `S.notMember` s) m -- O(m * log s)
exclude m s = M.fromDistinctAscList $ merge (M.toAscList m) (S.toAscList s)
  where
    -- Theoretically O(m + s), so should outperform previous algorithm...
    merge kvs            []       = kvs
    merge []             _        = []
    merge ((k_m, v):kvs) (k_s:ks) = case compare k_m k_s of
        LT -> (k_m, v):merge kvs            (k_s:ks)
        EQ ->          merge kvs            ks
        GT ->          merge ((k_m, v):kvs) ks


dataSetToVarSet :: Uniquable a => S.Set a -> UniqSet a
dataSetToVarSet = mkUniqSet . S.toList

varSetToDataMap :: Ord a => b -> UniqSet a -> M.Map a b
varSetToDataMap v = M.fromList . map (flip (,) v) . uniqSetToList

restrictDataMapVarSet :: (Ord k, Uniquable k) => M.Map k v -> UniqSet k -> M.Map k v
restrictDataMapVarSet m s = M.filterWithKey (\k _v -> k `elementOfUniqSet` s) m


class (Functor t, Foldable t) => Accumulatable t where
    mapAccumT  ::            (acc -> x ->   (acc, y)) -> acc -> t x ->   (acc, t y)
    mapAccumTM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> t x -> m (acc, t y)
    
    mapAccumT f acc x = unI (mapAccumTM (\acc' x' -> I (f acc' x')) acc x)

instance Accumulatable [] where
    mapAccumT  = mapAccumL
    mapAccumTM = mapAccumLM

{-# INLINE mapAccumLM #-}
mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM f = go []
  where
    go ys acc []     = return (acc, reverse ys)
    go ys acc (x:xs) = do
      (acc, y) <- f acc x
      go (y:ys) acc xs

{-# INLINE foldToMapAccumL #-}
foldToMapAccumL :: (forall acc'. (x -> acc' -> acc') -> acc' -> f_x -> acc')
                -> (acc -> x -> (acc, y))
                -> acc -> f_x -> (acc, [y])
foldToMapAccumL fold f init_acc xs = fold (\x (acc, ys) -> case f acc x of (acc', y) -> (acc', y:ys)) (init_acc, []) xs


traverseAll :: Traversable t => ([a] -> (c, [b])) -> t a -> (c, t b)
traverseAll f t = if null used_as' then (c, t') else error "traverseAll: replacing with too many elements"
  where
    (t', (rev_as, used_as')) = runState (traverse go t) ([], as')
    (c, as') = f (reverse rev_as)
    go a = State $ \(as, ~(a':as')) -> (# a', (a:as, as') #)

traverseSome :: Traversable t => (a -> Bool) -> ([a] -> (c, [a])) -> t a -> (c, t a)
traverseSome p f t = if null used_as' then (c, t') else error "traverseSome: replacing with too many elements"
  where
    (t', (rev_as, used_as')) = runState (traverse go t) ([], as')
    (c, as') = f (reverse rev_as)
    go a | p a       = State $ \(as, ~(a':as')) -> (# a', (a:as, as') #)
         | otherwise = return a


{-# INLINE zipWithEqualM #-}
zipWithEqualM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithEqualM f = go
  where
    go []     []     = return []
    go (x:xs) (y:ys) = liftM2 (:) (f x y) (go xs ys)
    go _ _ = fail "zipWithEqualM"

{-# INLINE foldZipEqualM #-}
foldZipEqualM :: Monad m => (a -> b -> c -> m a) -> a -> [b] -> [c] -> m a
foldZipEqualM f = go
  where
   go acc []     []     = return acc
   go acc (x:xs) (y:ys) = f acc x y >>= \acc' -> go acc' xs ys
   go _ _ _ = fail "foldZipEqualM"


zipMaybeWithEqual :: String
                  -> (a -> b -> c)
                  -> Maybe a -> Maybe b -> Maybe c
zipMaybeWithEqual _ _ Nothing   Nothing = Nothing
zipMaybeWithEqual _ f (Just x) (Just y) = Just (f x y)
zipMaybeWithEqual msg _ _ _ = error ("zipMaybeWithEqual:" ++ msg)


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
hFunctionsUniqSupply, supercompileUniqSupply, anfUniqSupply, expandUniqSupply, reduceUniqSupply, tagUniqSupply, prettyUniqSupply, matchUniqSupply, splitterUniqSupply :: UniqSupply
supercompileUniqSupply = unsafePerformIO $ mkSplitUniqSupply 'p'
(hFunctionsUniqSupply:anfUniqSupply:expandUniqSupply:reduceUniqSupply:tagUniqSupply:prettyUniqSupply:matchUniqSupply:splitterUniqSupply:_) = listSplitUniqSupply supercompileUniqSupply


infixr 5 `Car`

data Train a b = Car a (Train a b) | Loco b

instance (Outputable a, Outputable b) => Outputable (Train a b) where
    ppr xs = brackets (fsep (punctuate comma (trainFoldr (\a -> (ppr a :)) (\b -> (ppr b :)) [] xs)))

{-# INLINE trainAppend #-}
trainAppend :: Train a b -> (b -> Train a b') -> Train a b'
trainAppend init_abs mk_tl = go init_abs
  where go (Car a abs) = Car a (go abs)
        go (Loco b)    = mk_tl b

{-# INLINE fmapTrain #-}
fmapTrain :: (a -> a') -> (b -> b') -> Train a b -> Train a' b'
fmapTrain f g = go
  where go (Car a abs) = Car (f a) (go abs)
        go (Loco b)    = Loco (g b)

fmapCars :: (a -> a') -> Train a b -> Train a' b
fmapCars f = fmapTrain f id

fmapLoco :: (b -> b') -> Train a b -> Train a b'
fmapLoco f abs = trainAppend abs (Loco . f)

nullTrain :: Train a b -> Bool
nullTrain (Car _ _) = False
nullTrain (Loco _)  = True

unconsTrain :: Train a b -> Maybe (a, Train a b)
unconsTrain (Car a abs) = Just (a, abs)
unconsTrain (Loco _)    = Nothing

trainToList :: Train a b -> ([a], b)
trainToList (Car a abs) = first (a:) (trainToList abs)
trainToList (Loco b)    = ([], b)

trainLoco :: Train a b -> b
trainLoco (Car _ abs) = trainLoco abs
trainLoco (Loco b)    = b

trainCars :: Train a b -> [a]
trainCars (Car a abs) = a : trainCars abs
trainCars (Loco _)    = []

trainCarFoldl' :: (c -> a -> c) -> c -> Train a b -> c
trainCarFoldl' f_car = trainFoldl' f_car (\s _a -> s)

{-# INLINE trainFoldl' #-}
trainFoldl' :: (c -> a -> c) -> (c -> b -> d) -> c -> Train a b -> d
trainFoldl' f_car f_loco = go
  where go s (Loco b)    = s `seq` f_loco s b
        go s (Car a abs) = s `seq` go (f_car s a) abs

trainCarFoldr :: (a -> c -> c) -> c -> Train a b -> c
trainCarFoldr f_car = trainFoldr f_car (\_b s -> s)

{-# INLINE trainFoldr #-}
trainFoldr :: (a -> d -> d) -> (b -> c -> d) -> c -> Train a b -> d
trainFoldr f_car f_loco = go
  where go s (Loco b)    = f_loco b s
        go s (Car a abs) = f_car a (go s abs)

trainCarMapAccumL :: (acc -> a -> (acc, a')) -> acc -> Train a b -> (acc, Train a' b)
trainCarMapAccumL f_car = trainMapAccumL f_car (,)

{-# INLINE trainMapAccumL #-}
trainMapAccumL :: (acc -> a -> (acc, a')) -> (acc -> b -> (acc, b')) -> acc -> Train a b -> (acc, Train a' b')
trainMapAccumL f_car f_loco = go
  where go s (Loco b) = (s', Loco b')
          where (s', b') = f_loco s b
        go s (Car a abs) = second (Car a') (go s' abs)
          where (s', a') = f_car s a

{-# INLINE trainLeftExtensionBy #-}
trainLeftExtensionBy :: (a1 -> a2 -> Maybe a)
                     -> (b1 -> b2 -> Maybe b)
                     -> Train a1 b1 -- ^ Longer list
                     -> Train a2 b2 -- ^ Shorter list
                     -> Maybe ([a1], Train a b) -- Pair of the prefix present in the longer list and the common suffix (== shorter list)
trainLeftExtensionBy f_car f_loco xs ys = do
    loco <- f_loco xs_loco ys_loco
    go (reverse xs_cars) (reverse ys_cars) (Loco loco)
  where
    (xs_cars, xs_loco) = trainToList xs
    (ys_cars, ys_loco) = trainToList ys

    go xs_cars         []              train = Just (reverse xs_cars, train)
    go []              _               _     = Nothing
    go (x_car:xs_cars) (y_car:ys_cars) train = do
        car <- f_car x_car y_car
        go xs_cars ys_cars (Car car train)

trainLength :: Train a b -> Int
trainLength = trainCarFoldl' (\n _ -> n + 1) 0


data Stream a = a :< Stream a

listToStream :: [a] -> Stream a
listToStream []     = error "listToStream"
listToStream (x:xs) = x :< listToStream xs


class MonadTrans t where
    lift :: Monad m => m a -> t m a


newtype StateT s m a = StateT { unStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f mx = StateT $ \s -> fmap (first f) (unStateT mx s)

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
    return x = StateT $ \s -> return (x, s)
    mx >>= fxmy = StateT $ \s -> unStateT mx s >>= \(x, s) -> unStateT (fxmy x) s

instance MonadTrans (StateT s) where
    lift mx = StateT $ \s -> liftM (flip (,) s) mx


newtype ReaderT r m a = ReaderT { unReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f mx = ReaderT $ \r -> fmap f (unReaderT mx r)

instance Applicative m => Applicative (ReaderT r m) where
    pure x = ReaderT $ \_ -> pure x
    mf <*> mx = ReaderT $ \r -> unReaderT mf r <*> unReaderT mx r

instance Monad m => Monad (ReaderT r m) where
    return x = ReaderT $ \_ -> return x
    mx >>= fxmy = ReaderT $ \r -> unReaderT mx r >>= \x -> unReaderT (fxmy x) r

instance MonadTrans (ReaderT r) where
    lift mx = ReaderT $ \_ -> mx

runReaderT :: r -> ReaderT r m a -> m a
runReaderT = flip unReaderT


newtype ContT r m a = ContT { unContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
    fmap f mx = ContT $ \k -> unContT mx (k . f)

instance Applicative (ContT r m) where
    pure = return
    (<*>) = ap

instance Monad (ContT r m) where
    return x = ContT $ \k -> k x
    mx >>= fxmy = ContT $ \k -> unContT mx $ \x -> unContT (fxmy x) k

instance MonadTrans (ContT r) where
    lift mx = ContT $ \k -> mx >>= k

runContT :: Monad m => ContT r m r -> m r
runContT mx = unContT mx return

callCC :: ((forall b. a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \k -> unContT (f (\a -> ContT $ \_k -> k a)) k

