#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- Uses multi-param type classes
--

import Test.QuickCheck
import Text.Show.Functions

import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word

import System.IO
import System.Environment
import System.IO.Unsafe
import System.Random

import Control.Monad        ( liftM2 )

import Text.Printf
import Debug.Trace

import Foreign.Ptr

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base as L (LazyByteString(..))

import Data.ByteString.Fusion
import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Char8      as PC
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString       as P
import qualified Data.ByteString.Base  as P
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as D
import Data.ByteString.Fusion

import Prelude hiding (abs)

-- Enable this to get verbose test output. Including the actual tests.
debug = False

mytest :: Testable a => a -> Int -> IO ()
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO ()
mycheck config a =
  do let rnd = mkStdGen 99
     mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
mytests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK," ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps =
  do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------

instance Arbitrary Char where
    arbitrary     = choose ('a', 'i')
    coarbitrary c = variant (ord c `rem` 4)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PairS a b) where
  arbitrary             = liftM2 (:*:) arbitrary arbitrary
  coarbitrary (a :*: b) = coarbitrary a . coarbitrary b

instance Arbitrary Word8 where
    arbitrary = choose (97, 105)
    coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 4))

instance Arbitrary Int64 where
  arbitrary     = sized $ \n -> choose (-fromIntegral n,fromIntegral n)
  coarbitrary n = variant (fromIntegral (if n >= 0 then 2*n else 2*(-n) + 1))

instance Arbitrary a => Arbitrary (MaybeS a) where
  arbitrary            = do a <- arbitrary ; elements [NothingS, JustS a]
  coarbitrary NothingS = variant 0
  coarbitrary _        = variant 1 -- ok?

{-
instance Arbitrary Char where
  arbitrary = choose ('\0', '\255') -- since we have to test words, unlines too
  coarbitrary c = variant (ord c `rem` 16)

instance Arbitrary Word8 where
  arbitrary = choose (minBound, maxBound)
  coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 16))
-}

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int64 where
  randomR = integralRandomR
  random  = randomR (minBound,maxBound)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . L.LPS . filter (not. P.null) -- maintain the invariant.
    coarbitrary s = coarbitrary (L.unpack s)

instance Arbitrary P.ByteString where
  arbitrary = P.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (P.unpack s)

instance Functor ((->) r) where
    fmap = (.)

instance Monad ((->) r) where
    return = const
    f >>= k = \ r -> k (f r) r

instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

------------------------------------------------------------------------
--
-- We're doing two forms of testing here. Firstly, model based testing.
-- For our Lazy and strict bytestring types, we have model types:
--
--  i.e.    Lazy    ==   Byte
--              \\      //
--                 List 
--
-- That is, the Lazy type can be modeled by functions in both the Byte
-- and List type. For each of the 3 models, we have a set of tests that
-- check those types match.
--
-- The Model class connects a type and its model type, via a conversion
-- function. 
--
--
class Model a b where
  model :: a -> b  -- get the abstract vale from a concrete value

--
-- Connecting our Lazy and Strict types to their models. We also check
-- the data invariant on Lazy types.
--
-- These instances represent the arrows in the above diagram
--
instance Model B P      where model = abstr . checkInvariant
instance Model P [W]    where model = P.unpack
instance Model P [Char] where model = PC.unpack
instance Model B [W]    where model = L.unpack  . checkInvariant
instance Model B [Char] where model = LC.unpack . checkInvariant

-- Types are trivially modeled by themselves
instance Model Bool  Bool         where model = id
instance Model Int   Int          where model = id
instance Model Int64 Int64        where model = id
instance Model Int64 Int          where model = fromIntegral
instance Model Word8 Word8        where model = id
instance Model Ordering Ordering  where model = id

-- More structured types are modeled recursively, using the NatTrans class from Gofer.
class (Functor f, Functor g) => NatTrans f g where
    eta :: f a -> g a

-- The transformation of the same type is identity
instance NatTrans [] []             where eta = id
instance NatTrans Maybe Maybe       where eta = id
instance NatTrans ((->) X) ((->) X) where eta = id
instance NatTrans ((->) W) ((->) W) where eta = id

-- We have a transformation of pairs, if the pairs are in Model
instance Model f g => NatTrans ((,) f) ((,) g) where eta (f,a) = (model f, a)

-- And finally, we can take any (m a) to (n b), if we can Model m n, and a b
instance (NatTrans m n, Model a b) => Model (m a) (n b) where model x = fmap model (eta x)

------------------------------------------------------------------------

-- In a form more useful for QC testing (and it's lazy)
checkInvariant :: L.ByteString -> L.ByteString
checkInvariant (L.LPS lps) = L.LPS (check lps)
  where check []     = []
        check (x:xs) | P.null x  = error ("invariant violation: " ++ show lps)
                     | otherwise = x : check xs

abstr :: L.ByteString -> P.ByteString
abstr (L.LPS []) = P.empty
abstr (L.LPS xs) = P.concat xs

-- Some short hand.
type X = Int
type W = Word8
type P = P.ByteString
type B = L.ByteString

------------------------------------------------------------------------
--
-- These comparison functions handle wrapping and equality.
--
-- A single class for these would be nice, but note that they differe in
-- the number of arguments, and those argument types, so we'd need HList
-- tricks. See here: http://okmij.org/ftp/Haskell/vararg-fn.lhs
--

eq1 f g = \a         ->
    model (f a)         == g (model a)
eq2 f g = \a b       ->
    model (f a b)       == g (model a) (model b)
eq3 f g = \a b c     ->
    model (f a b c)     == g (model a) (model b) (model c)
eq4 f g = \a b c d   ->
    model (f a b c d)   == g (model a) (model b) (model c) (model d)
eq5 f g = \a b c d e ->
    model (f a b c d e) == g (model a) (model b) (model c) (model d) (model e)

--
-- And for functions that take non-null input
--
eqnotnull1 f g = \x     -> (not (isNull x)) ==> eq1 f g x
eqnotnull2 f g = \x y   -> (not (isNull y)) ==> eq2 f g x y
eqnotnull3 f g = \x y z -> (not (isNull z)) ==> eq3 f g x y z

class    IsNull t            where isNull :: t -> Bool
instance IsNull L.ByteString where isNull = L.null
instance IsNull P.ByteString where isNull = P.null

------------------------------------------------------------------------

--
-- These are miscellaneous tests left over. Or else they test some
-- property internal to a type (i.e. head . sort == minimum), without
-- reference to a model type.
--

invariant :: L.ByteString -> Bool
invariant (L.LPS []) = True
invariant (L.LPS xs) = all (not . P.null) xs

prop_invariant = invariant

prop_eq_refl  x     = x        == (x :: ByteString)
prop_eq_symm  x y   = (x == y) == (y == (x :: ByteString))

prop_eq1 xs      = xs == (unpack . pack $ xs)
prop_eq2 xs      = xs == (xs :: ByteString)
prop_eq3 xs ys   = (xs == ys) == (unpack xs == unpack ys)

prop_compare1 xs   = (pack xs        `compare` pack xs) == EQ
prop_compare2 xs c = (pack (xs++[c]) `compare` pack xs) == GT
prop_compare3 xs c = (pack xs `compare` pack (xs++[c])) == LT

prop_compare4 xs    = (not (null xs)) ==> (pack xs  `compare` L.empty) == GT
prop_compare5 xs    = (not (null xs)) ==> (L.empty `compare` pack xs) == LT
prop_compare6 xs ys = (not (null ys)) ==> (pack (xs++ys)  `compare` pack xs) == GT

prop_compare7 x  y  = x  `compare` y  == (L.singleton x `compare` L.singleton y)
prop_compare8 xs ys = xs `compare` ys == (L.pack xs `compare` L.pack ys)

prop_empty1 = L.length L.empty == 0
prop_empty2 = L.unpack L.empty == []

prop_packunpack s = (L.unpack . L.pack) s == id s
prop_unpackpack s = (L.pack . L.unpack) s == id s

prop_null xs = null (L.unpack xs) == L.null xs

prop_length1 xs = fromIntegral (length xs) == L.length (L.pack xs)

prop_length2 xs = L.length xs == length1 xs
  where length1 ys
            | L.null ys = 0
            | otherwise = 1 + length1 (L.tail ys)

prop_cons1 c xs = unpack (L.cons c (pack xs)) == (c:xs)
prop_cons2 c    = L.singleton c == (c `L.cons` L.empty)
prop_cons3 c    = unpack (L.singleton c) == (c:[])
prop_cons4 c    = (c `L.cons` L.empty)  == pack (c:[])

prop_snoc1 xs c = xs ++ [c] == unpack ((pack xs) `L.snoc` c)

prop_head  xs = (not (null xs)) ==> head xs == (L.head . pack) xs
prop_head1 xs = not (L.null xs) ==> L.head xs == head (L.unpack xs)

prop_tail xs  = not (L.null xs) ==> L.tail xs == pack (tail (unpack xs))
prop_tail1 xs = (not (null xs)) ==> tail xs   == (unpack . L.tail . pack) xs

prop_last xs  = (not (null xs)) ==> last xs    == (L.last . pack) xs

prop_init xs  =
    (not (null xs)) ==>
    init xs   == (unpack . L.init . pack) xs

prop_append1 xs    = (xs ++ xs) == (unpack $ pack xs `L.append` pack xs)
prop_append2 xs ys = (xs ++ ys) == (unpack $ pack xs `L.append` pack ys)
prop_append3 xs ys = L.append xs ys == pack (unpack xs ++ unpack ys)

prop_map1 f xs   = L.map f (pack xs)    == pack (map f xs)
prop_map2 f g xs = L.map f (L.map g xs) == L.map (f . g) xs
prop_map3 f xs   = map f xs == (unpack . L.map f .  pack) xs

prop_filter1 c xs = (filter (/=c) xs) == (unpack $ L.filter (/=c) (pack xs))
prop_filter2 p xs = (filter p xs) == (unpack $ L.filter p (pack xs))

prop_reverse  xs = reverse xs          == (unpack . L.reverse . pack) xs
prop_reverse1 xs = L.reverse (pack xs) == pack (reverse xs)
prop_reverse2 xs = reverse (unpack xs) == (unpack . L.reverse) xs

prop_transpose xs = (transpose xs) == ((map unpack) . L.transpose . (map pack)) xs

prop_foldl f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char

prop_foldr f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char

prop_foldl_1 xs = L.foldl (\xs c -> c `L.cons` xs) L.empty xs == L.reverse xs
prop_foldr_1 xs = L.foldr (\c xs -> c `L.cons` xs) L.empty xs == id xs

prop_foldl1_1 xs =
    (not . L.null) xs ==>
    L.foldl1 (\x c -> if c > x then c else x)   xs ==
    L.foldl  (\x c -> if c > x then c else x) 0 xs

prop_foldl1_2 xs =
    (not . L.null) xs ==>
    L.foldl1 const xs == L.head xs

prop_foldl1_3 xs =
    (not . L.null) xs ==>
    L.foldl1 (flip const) xs == L.last xs

prop_foldr1_1 xs =
    (not . L.null) xs ==>
    L.foldr1 (\c x -> if c > x then c else x)   xs ==
    L.foldr  (\c x -> if c > x then c else x) 0 xs

prop_foldr1_2 xs =
    (not . L.null) xs ==>
    L.foldr1 (flip const) xs == L.last xs

prop_foldr1_3 xs =
    (not . L.null) xs ==>
    L.foldr1 const xs == L.head xs

prop_concat1 xs = (concat [xs,xs]) == (unpack $ L.concat [pack xs, pack xs])
prop_concat2 xs = (concat [xs,[]]) == (unpack $ L.concat [pack xs, pack []])
prop_concat3 xss = L.concat (map pack xss) == pack (concat xss)

prop_concatMap xs = L.concatMap L.singleton xs == (pack . concatMap (:[]) . unpack) xs

prop_any xs a = (any (== a) xs) == (L.any (== a) (pack xs))
prop_all xs a = (all (== a) xs) == (L.all (== a) (pack xs))

prop_maximum xs = (not (null xs)) ==> (maximum xs) == (L.maximum ( pack xs ))
prop_minimum xs = (not (null xs)) ==> (minimum xs) == (L.minimum ( pack xs ))

prop_replicate1 n c =
    (n >= 0) ==> unpack (L.replicate (fromIntegral n) c) == replicate n c

prop_replicate2 c = unpack (L.replicate 0 c) == replicate 0 c

prop_take1 i xs = L.take (fromIntegral i) (pack xs) == pack (take i xs)
prop_drop1 i xs = L.drop (fromIntegral i) (pack xs) == pack (drop i xs)

prop_splitAt i xs = collect (i >= 0 && i < length xs) $
    L.splitAt (fromIntegral i) (pack xs) == let (a,b) = splitAt i xs in (pack a, pack b)

prop_takeWhile f xs = L.takeWhile f (pack xs) == pack (takeWhile f xs)
prop_dropWhile f xs = L.dropWhile f (pack xs) == pack (dropWhile f xs)

prop_break f xs = L.break f (pack xs) ==
    let (a,b) = break f xs in (pack a, pack b)

prop_breakspan xs c = L.break (==c) xs == L.span (/=c) xs

prop_span xs a = (span (/=a) xs) == (let (x,y) = L.span (/=a) (pack xs) in (unpack x, unpack y))

-- prop_breakByte xs c = L.break (== c) xs == L.breakByte c xs

-- prop_spanByte c xs = (L.span (==c) xs) == L.spanByte c xs

prop_split c xs = (map L.unpack . map checkInvariant . L.split c $ xs)
               == (map P.unpack . P.split c . P.pack . L.unpack $ xs)

prop_splitWith f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map L.length splits) == L.length xs - l2
  where splits = L.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = L.length (L.filter f xs)

prop_joinsplit c xs = L.join (pack [c]) (L.split c xs) == id xs

prop_group xs       = group xs == (map unpack . L.group . pack) xs
-- prop_groupBy  f xs  = groupBy f xs == (map unpack . L.groupBy f . pack) xs

-- prop_joinjoinByte xs ys c = L.joinWithByte c xs ys == L.join (L.singleton c) [xs,ys]

prop_index xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == L.pack xs `L.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

prop_elemIndex xs c = (elemIndex c xs) == fmap fromIntegral (L.elemIndex c (pack xs))

prop_elemIndices xs c = elemIndices c xs == map fromIntegral (L.elemIndices c (pack xs))

prop_count c xs = length (L.elemIndices c xs) == fromIntegral (L.count c xs)

prop_findIndex xs f = (findIndex f xs) == fmap fromIntegral (L.findIndex f (pack xs))
prop_findIndicies xs f = (findIndices f xs) == map fromIntegral (L.findIndices f (pack xs))

prop_elem    xs c = (c `elem` xs)    == (c `L.elem` (pack xs))
prop_notElem xs c = (c `notElem` xs) == (L.notElem c (pack xs))
prop_elem_notelem xs c = c `L.elem` xs == not (c `L.notElem` xs)

-- prop_filterByte  xs c = L.filterByte c xs == L.filter (==c) xs
-- prop_filterByte2 xs c = unpack (L.filterByte c xs) == filter (==c) (unpack xs)

-- prop_filterNotByte  xs c = L.filterNotByte c xs == L.filter (/=c) xs
-- prop_filterNotByte2 xs c = unpack (L.filterNotByte c xs) == filter (/=c) (unpack xs)

prop_find p xs = find p xs == L.find p (pack xs)

prop_find_findIndex p xs =
    L.find p xs == case L.findIndex p xs of
                                Just n -> Just (xs `L.index` n)
                                _      -> Nothing

prop_isPrefixOf xs ys = isPrefixOf xs ys == (pack xs `L.isPrefixOf` pack ys)

{-
prop_sort1 xs = sort xs == (unpack . L.sort . pack) xs
prop_sort2 xs = (not (null xs)) ==> (L.head . L.sort . pack $ xs) == minimum xs
prop_sort3 xs = (not (null xs)) ==> (L.last . L.sort . pack $ xs) == maximum xs
prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (L.head . L.sort) (L.append (pack xs) (pack ys)) == min (minimum xs) (minimum ys)

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (L.last . L.sort) (L.append (pack xs) (pack ys)) == max (maximum xs) (maximum ys)

-}

------------------------------------------------------------------------
-- Misc ByteString properties

prop_nil1BB = P.length P.empty == 0
prop_nil2BB = P.unpack P.empty == []

prop_tailSBB xs = not (P.null xs) ==> P.tail xs == P.pack (tail (P.unpack xs))

prop_nullBB xs = null (P.unpack xs) == P.null xs

prop_lengthBB xs = P.length xs == length1 xs
    where
        length1 ys
            | P.null ys = 0
            | otherwise = 1 + length1 (P.tail ys)

prop_lengthSBB xs = length xs == P.length (P.pack xs)

prop_indexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.index` i
  where indices = choose (0, length xs -1)

prop_unsafeIndexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_mapfusionBB f g xs = P.map f (P.map g xs) == P.map (f . g) xs

prop_filterBB f xs = P.filter f (P.pack xs) == P.pack (filter f xs)

prop_filterfusionBB f g xs = P.filter f (P.filter g xs) == P.filter (\c -> f c && g c) xs

prop_elemSBB x xs = P.elem x (P.pack xs) == elem x xs

prop_takeSBB i xs = P.take i (P.pack xs) == P.pack (take i xs)
prop_dropSBB i xs = P.drop i (P.pack xs) == P.pack (drop i xs)

prop_splitAtSBB i xs = -- collect (i >= 0 && i < length xs) $
    P.splitAt i (P.pack xs) ==
    let (a,b) = splitAt i xs in (P.pack a, P.pack b)

prop_foldlBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_scanlfoldlBB f z xs = not (P.null xs) ==> P.last (P.scanl f z xs) == P.foldl f z xs

prop_foldrBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_takeWhileSBB f xs = P.takeWhile f (P.pack xs) == P.pack (takeWhile f xs)
prop_dropWhileSBB f xs = P.dropWhile f (P.pack xs) == P.pack (dropWhile f xs)

prop_spanSBB f xs = P.span f (P.pack xs) ==
    let (a,b) = span f xs in (P.pack a, P.pack b)

prop_breakSBB f xs = P.break f (P.pack xs) ==
    let (a,b) = break f xs in (P.pack a, P.pack b)

prop_breakspan_1BB xs c = P.break (== c) xs == P.span (/= c) xs

prop_linesSBB xs = C.lines (C.pack xs) == map C.pack (lines xs)

prop_unlinesSBB xss = C.unlines (map C.pack xss) == C.pack (unlines xss)

prop_wordsSBB xs =
    C.words (C.pack xs) == map C.pack (words xs)

prop_unwordsSBB xss = C.unwords (map C.pack xss) == C.pack (unwords xss)

prop_splitWithBB f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map P.length splits) == P.length xs - l2
  where splits = P.splitWith f xs
        l1 = length splits
        l2 = P.length (P.filter f xs)

prop_joinsplitBB c xs = P.join (P.pack [c]) (P.split c xs) == xs

-- prop_linessplitBB xs =
--     (not . C.null) xs ==>
--     C.lines' xs == C.split '\n' xs

prop_linessplit2BB xs =
    C.lines xs == C.split '\n' xs ++ (if C.last xs == '\n' then [C.empty] else [])

prop_splitsplitWithBB c xs = P.split c xs == P.splitWith (== c) xs

prop_bijectionBB  c = (P.w2c . P.c2w) c == id c
prop_bijectionBB' w = (P.c2w . P.w2c) w == id w

prop_packunpackBB  s = (P.unpack . P.pack) s == id s
prop_packunpackBB' s = (P.pack . P.unpack) s == id s

prop_eq1BB xs      = xs            == (P.unpack . P.pack $ xs)
prop_eq2BB xs      = xs == xs
prop_eq3BB xs ys   = (xs == ys) == (P.unpack xs == P.unpack ys)

prop_compare1BB xs  = (P.pack xs         `compare` P.pack xs) == EQ
prop_compare2BB xs c = (P.pack (xs++[c]) `compare` P.pack xs) == GT
prop_compare3BB xs c = (P.pack xs `compare` P.pack (xs++[c])) == LT

prop_compare4BB xs  = (not (null xs)) ==> (P.pack xs  `compare` P.empty) == GT
prop_compare5BB xs  = (not (null xs)) ==> (P.empty `compare` P.pack xs) == LT
prop_compare6BB xs ys= (not (null ys)) ==> (P.pack (xs++ys)  `compare` P.pack xs) == GT

prop_compare7BB x  y = x `compare` y == (C.singleton x `compare` C.singleton y)
prop_compare8BB xs ys = xs `compare` ys == (P.pack xs `compare` P.pack ys)

prop_consBB  c xs = P.unpack (P.cons c (P.pack xs)) == (c:xs)
prop_cons1BB xs   = 'X' : xs == C.unpack ('X' `C.cons` (C.pack xs))
prop_cons2BB xs c = c : xs == P.unpack (c `P.cons` (P.pack xs))
prop_cons3BB c    = C.unpack (C.singleton c) == (c:[])
prop_cons4BB c    = (c `P.cons` P.empty)  == P.pack (c:[])

prop_snoc1BB xs c = xs ++ [c] == P.unpack ((P.pack xs) `P.snoc` c)

prop_head1BB xs     = (not (null xs)) ==> head  xs  == (P.head . P.pack) xs
prop_head2BB xs    = (not (null xs)) ==> head xs   == (P.unsafeHead . P.pack) xs
prop_head3BB xs    = not (P.null xs) ==> P.head xs == head (P.unpack xs)

prop_tailBB xs     = (not (null xs)) ==> tail xs    == (P.unpack . P.tail . P.pack) xs
prop_tail1BB xs    = (not (null xs)) ==> tail xs    == (P.unpack . P.unsafeTail. P.pack) xs

prop_lastBB xs     = (not (null xs)) ==> last xs    == (P.last . P.pack) xs

prop_initBB xs     =
    (not (null xs)) ==>
    init xs    == (P.unpack . P.init . P.pack) xs

-- prop_null xs = (null xs) ==> null xs == (nullPS (pack xs))

prop_append1BB xs    = (xs ++ xs) == (P.unpack $ P.pack xs `P.append` P.pack xs)
prop_append2BB xs ys = (xs ++ ys) == (P.unpack $ P.pack xs `P.append` P.pack ys)
prop_append3BB xs ys = P.append xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_map1BB f xs   = P.map f (P.pack xs)    == P.pack (map f xs)
prop_map2BB f g xs = P.map f (P.map g xs) == P.map (f . g) xs
prop_map3BB f xs   = map f xs == (P.unpack . P.map f .  P.pack) xs
-- prop_mapBB' f xs   = P.map' f (P.pack xs) == P.pack (map f xs)

prop_filter1BB xs   = (filter (=='X') xs) == (C.unpack $ C.filter (=='X') (C.pack xs))
prop_filter2BB p xs = (filter p xs) == (P.unpack $ P.filter p (P.pack xs))

prop_findBB p xs = find p xs == P.find p (P.pack xs)

prop_find_findIndexBB p xs =
    P.find p xs == case P.findIndex p xs of
                                Just n -> Just (xs `P.unsafeIndex` n)
                                _      -> Nothing

prop_foldl1BB xs a = ((foldl (\x c -> if c == a then x else c:x) [] xs)) ==
                   (P.unpack $ P.foldl (\x c -> if c == a then x else c `P.cons` x) P.empty (P.pack xs)) 
prop_foldl2BB xs = P.foldl (\xs c -> c `P.cons` xs) P.empty (P.pack xs) == P.reverse (P.pack xs)

prop_foldr1BB xs a = ((foldr (\c x -> if c == a then x else c:x) [] xs)) ==
                (P.unpack $ P.foldr (\c x -> if c == a then x else c `P.cons` x)
                    P.empty (P.pack xs))

prop_foldr2BB xs = P.foldr (\c xs -> c `P.cons` xs) P.empty (P.pack xs) == (P.pack xs)

prop_foldl1_1BB xs =
    (not . P.null) xs ==>
    P.foldl1 (\x c -> if c > x then c else x)   xs ==
    P.foldl  (\x c -> if c > x then c else x) 0 xs

prop_foldl1_2BB xs =
    (not . P.null) xs ==>
    P.foldl1 const xs == P.head xs

prop_foldl1_3BB xs =
    (not . P.null) xs ==>
    P.foldl1 (flip const) xs == P.last xs

prop_foldr1_1BB xs =
    (not . P.null) xs ==>
    P.foldr1 (\c x -> if c > x then c else x)   xs ==
    P.foldr  (\c x -> if c > x then c else x) 0 xs

prop_foldr1_2BB xs =
    (not . P.null) xs ==>
    P.foldr1 (flip const) xs == P.last xs

prop_foldr1_3BB xs =
    (not . P.null) xs ==>
    P.foldr1 const xs == P.head xs

prop_takeWhileBB xs a = (takeWhile (/= a) xs) == (P.unpack . (P.takeWhile (/= a)) . P.pack) xs

prop_dropWhileBB xs a = (dropWhile (/= a) xs) == (P.unpack . (P.dropWhile (/= a)) . P.pack) xs

prop_takeBB xs = (take 10 xs) == (P.unpack . (P.take 10) . P.pack) xs

prop_dropBB xs = (drop 10 xs) == (P.unpack . (P.drop 10) . P.pack) xs

prop_splitAtBB i xs = -- collect (i >= 0 && i < length xs) $
    splitAt i xs ==
    let (x,y) = P.splitAt i (P.pack xs) in (P.unpack x, P.unpack y)

prop_spanBB xs a = (span (/=a) xs) == (let (x,y) = P.span (/=a) (P.pack xs)
                                     in (P.unpack x, P.unpack y))

prop_breakBB xs a = (break (/=a) xs) == (let (x,y) = P.break (/=a) (P.pack xs)
                                       in (P.unpack x, P.unpack y))

prop_reverse1BB xs = (reverse xs) == (P.unpack . P.reverse . P.pack) xs
prop_reverse2BB xs = P.reverse (P.pack xs) == P.pack (reverse xs)
prop_reverse3BB xs = reverse (P.unpack xs) == (P.unpack . P.reverse) xs

prop_elemBB xs a = (a `elem` xs) == (a `P.elem` (P.pack xs))

prop_notElemBB c xs = P.notElem c (P.pack xs) == notElem c xs

-- should try to stress it
prop_concat1BB xs = (concat [xs,xs]) == (P.unpack $ P.concat [P.pack xs, P.pack xs])
prop_concat2BB xs = (concat [xs,[]]) == (P.unpack $ P.concat [P.pack xs, P.pack []])
prop_concatBB xss = P.concat (map P.pack xss) == P.pack (concat xss)

prop_concatMapBB xs = C.concatMap C.singleton xs == (C.pack . concatMap (:[]) . C.unpack) xs

prop_anyBB xs a = (any (== a) xs) == (P.any (== a) (P.pack xs))
prop_allBB xs a = (all (== a) xs) == (P.all (== a) (P.pack xs))

prop_linesBB xs = (lines xs) == ((map C.unpack) . C.lines . C.pack) xs

prop_unlinesBB xs = (unlines.lines) xs == (C.unpack. C.unlines . C.lines .C.pack) xs

prop_wordsBB xs =
    (words xs) == ((map C.unpack) . C.words . C.pack) xs
-- prop_wordstokensBB xs = C.words xs == C.tokens isSpace xs

prop_unwordsBB xs =
    (C.pack.unwords.words) xs == (C.unwords . C.words .C.pack) xs

prop_groupBB xs   = group xs == (map P.unpack . P.group . P.pack) xs

prop_groupByBB  xs = groupBy (==) xs == (map P.unpack . P.groupBy (==) . P.pack) xs
prop_groupBy1BB xs = groupBy (/=) xs == (map P.unpack . P.groupBy (/=) . P.pack) xs

prop_joinBB xs ys = (concat . (intersperse ys) . lines) xs ==
               (C.unpack $ C.join (C.pack ys) (C.lines (C.pack xs)))

prop_elemIndex1BB xs   = (elemIndex 'X' xs) == (C.elemIndex 'X' (C.pack xs))
prop_elemIndex2BB xs c = (elemIndex c xs) == (C.elemIndex c (C.pack xs))

-- prop_lineIndices1BB xs = C.elemIndices '\n' xs == C.lineIndices xs

prop_countBB c xs = length (P.elemIndices c xs) == P.count c xs

prop_elemIndexEnd1BB c xs = (P.elemIndexEnd c (P.pack xs)) ==
                           (case P.elemIndex c (P.pack (reverse xs)) of
                                Nothing -> Nothing
                                Just i  -> Just (length xs -1 -i))

prop_elemIndexEnd2BB c xs = (P.elemIndexEnd c (P.pack xs)) ==
                           ((-) (length xs - 1) `fmap` P.elemIndex c (P.pack $ reverse xs))

prop_elemIndicesBB xs c = elemIndices c xs == P.elemIndices c (P.pack xs)

prop_findIndexBB xs a = (findIndex (==a) xs) == (P.findIndex (==a) (P.pack xs))

prop_findIndiciesBB xs c = (findIndices (==c) xs) == (P.findIndices (==c) (P.pack xs))

-- example properties from QuickCheck.Batch
prop_sort1BB xs = sort xs == (P.unpack . P.sort . P.pack) xs
prop_sort2BB xs = (not (null xs)) ==> (P.head . P.sort . P.pack $ xs) == minimum xs
prop_sort3BB xs = (not (null xs)) ==> (P.last . P.sort . P.pack $ xs) == maximum xs
prop_sort4BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.head . P.sort) (P.append (P.pack xs) (P.pack ys)) == min (minimum xs) (minimum ys)
prop_sort5BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.last . P.sort) (P.append (P.pack xs) (P.pack ys)) == max (maximum xs) (maximum ys)

prop_intersperseBB c xs = (intersperse c xs) == (P.unpack $ P.intersperse c (P.pack xs))

prop_transposeBB xs = (transpose xs) == ((map P.unpack) . P.transpose .  (map P.pack)) xs

prop_maximumBB xs = (not (null xs)) ==> (maximum xs) == (P.maximum ( P.pack xs ))
prop_minimumBB xs = (not (null xs)) ==> (minimum xs) == (P.minimum ( P.pack xs ))

-- prop_dropSpaceBB xs    = dropWhile isSpace xs == C.unpack (C.dropSpace (C.pack xs))
-- prop_dropSpaceEndBB xs = (C.reverse . (C.dropWhile isSpace) . C.reverse) (C.pack xs) ==
--                        (C.dropSpaceEnd (C.pack xs))

-- prop_breakSpaceBB xs =
--     (let (x,y) = C.breakSpace (C.pack xs)
--      in (C.unpack x, C.unpack y)) == (break isSpace xs)

prop_spanEndBB xs =
        (C.spanEnd (not . isSpace) (C.pack xs)) ==
        (let (x,y) = C.span (not.isSpace) (C.reverse (C.pack xs)) in (C.reverse y,C.reverse x))

prop_breakEndBB p xs = P.breakEnd (not.p) xs == P.spanEnd p xs

-- prop_breakCharBB c xs =
--         (break (==c) xs) ==
--         (let (x,y) = C.breakChar c (C.pack xs) in (C.unpack x, C.unpack y))

-- prop_spanCharBB c xs =
--         (break (/=c) xs) ==
--         (let (x,y) = C.spanChar c (C.pack xs) in (C.unpack x, C.unpack y))

-- prop_spanChar_1BB c xs =
--         (C.span (==c) xs) == C.spanChar c xs

-- prop_wordsBB' xs =
--     (C.unpack . C.unwords  . C.words' . C.pack) xs ==
--     (map (\c -> if isSpace c then ' ' else c) xs)

-- prop_linesBB' xs = (C.unpack . C.unlines' . C.lines' . C.pack) xs == (xs)

prop_unfoldrBB c n =
    (fst $ C.unfoldrN n fn c) == (C.pack $ take n $ unfoldr fn c)
    where
      fn x = Just (x, chr (ord x + 1))

prop_prefixBB xs ys = isPrefixOf xs ys == (P.pack xs `P.isPrefixOf` P.pack ys)
prop_suffixBB xs ys = isSuffixOf xs ys == (P.pack xs `P.isSuffixOf` P.pack ys)

prop_copyBB xs = let p = P.pack xs in P.copy p == p

prop_initsBB xs = inits xs == map P.unpack (P.inits (P.pack xs))

prop_tailsBB xs = tails xs == map P.unpack (P.tails (P.pack xs))

prop_findSubstringsBB s x l
    = C.findSubstrings (C.pack p) (C.pack s) == naive_findSubstrings p s
  where
    _ = l :: Int
    _ = x :: Int

    -- we look for some random substring of the test string
    p = take (model l) $ drop (model x) s

    -- naive reference implementation
    naive_findSubstrings :: String -> String -> [Int]
    naive_findSubstrings p s = [x | x <- [0..length s], p `isPrefixOf` drop x s]

prop_replicate1BB n c = P.unpack (P.replicate n c) == replicate n c
prop_replicate2BB n c = P.replicate n c == fst (P.unfoldrN n (\u -> Just (u,u)) c)

prop_replicate3BB c = P.unpack (P.replicate 0 c) == replicate 0 c

prop_readintBB n = (fst . fromJust . C.readInt . C.pack . show) n == (n :: Int)
prop_readintLL n = (fst . fromJust . D.readInt . D.pack . show) n == (n :: Int)

prop_readint2BB s =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in C.readInt (C.pack s') == Nothing

-- prop_filterChar1BB c xs = (filter (==c) xs) == ((C.unpack . C.filterChar c . C.pack) xs)
-- prop_filterChar2BB c xs = (C.filter (==c) (C.pack xs)) == (C.filterChar c (C.pack xs))
-- prop_filterChar3BB c xs = C.filterChar c xs == C.replicate (C.count c xs) c

-- prop_filterNotChar1BB c xs = (filter (/=c) xs) == ((C.unpack . C.filterNotChar c . C.pack) xs)
-- prop_filterNotChar2BB c xs = (C.filter (/=c) (C.pack xs)) == (C.filterNotChar c (C.pack xs))

-- prop_joinjoinpathBB xs ys c = C.joinWithChar c xs ys == C.join (C.singleton c) [xs,ys]

prop_zipBB  xs ys = zip xs ys == P.zip (P.pack xs) (P.pack ys)
prop_zip1BB xs ys = P.zip xs ys == zip (P.unpack xs) (P.unpack ys)

prop_zipWithBB xs ys = P.zipWith (,) xs ys == P.zip xs ys
-- prop_zipWith'BB xs ys = P.pack (P.zipWith (+) xs ys) == P.zipWith' (+) xs ys

prop_unzipBB x = let (xs,ys) = unzip x in (P.pack xs, P.pack ys) == P.unzip x

------------------------------------------------------------------------
-- The entry point

main = run tests

run :: [(String, Int -> IO ())] -> IO ()
run tests = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests

--
-- And now a list of all the properties to test.
--

tests = bb_tests ++ ll_tests

------------------------------------------------------------------------
-- extra ByteString properties

bb_tests =
    [    ("bijection",      mytest prop_bijectionBB)
    ,    ("bijection'",     mytest prop_bijectionBB')
    ,    ("pack/unpack",    mytest prop_packunpackBB)
    ,    ("unpack/pack",    mytest prop_packunpackBB')
    ,    ("eq 1",           mytest prop_eq1BB)
    ,    ("eq 2",           mytest prop_eq3BB)
    ,    ("eq 3",           mytest prop_eq3BB)
    ,    ("compare 1",      mytest prop_compare1BB)
    ,    ("compare 2",      mytest prop_compare2BB)
    ,    ("compare 3",      mytest prop_compare3BB)
    ,    ("compare 4",      mytest prop_compare4BB)
    ,    ("compare 5",      mytest prop_compare5BB)
    ,    ("compare 6",      mytest prop_compare6BB)
    ,    ("compare 7",      mytest prop_compare7BB)
    ,    ("compare 8",      mytest prop_compare8BB)
    ,    ("empty 1",        mytest prop_nil1BB)
    ,    ("empty 2",        mytest prop_nil2BB)
    ,    ("null",           mytest prop_nullBB)
    ,    ("length 1",       mytest prop_lengthBB)
    ,    ("length 2",       mytest prop_lengthSBB)
    ,    ("cons 1",         mytest prop_consBB)
    ,    ("cons 2",         mytest prop_cons1BB)
    ,    ("cons 3",         mytest prop_cons2BB)
    ,    ("cons 4",         mytest prop_cons3BB)
    ,    ("cons 5",         mytest prop_cons4BB)
    ,    ("snoc",           mytest prop_snoc1BB)
    ,    ("head 1",         mytest prop_head1BB)
    ,    ("head 2",         mytest prop_head2BB)
    ,    ("head 3",         mytest prop_head3BB)
    ,    ("tail",           mytest prop_tailBB)
    ,    ("tail 1",         mytest prop_tail1BB)
    ,    ("last",           mytest prop_lastBB)
    ,    ("init",           mytest prop_initBB)
    ,    ("append 1",       mytest prop_append1BB)
    ,    ("append 2",       mytest prop_append2BB)
    ,    ("append 3",       mytest prop_append3BB)
    ,    ("map 1",          mytest prop_map1BB)
    ,    ("map 2",          mytest prop_map2BB)
    ,    ("map 3",          mytest prop_map3BB)
    ,    ("filter1",        mytest prop_filter1BB)
    ,    ("filter2",        mytest prop_filter2BB)
    ,    ("map fusion",     mytest prop_mapfusionBB)
    ,    ("filter fusion",  mytest prop_filterfusionBB)
    ,    ("reverse 1",      mytest prop_reverse1BB)
    ,    ("reverse 2",      mytest prop_reverse2BB)
    ,    ("reverse 3",      mytest prop_reverse3BB)
    ,    ("foldl 1",        mytest prop_foldl1BB)
    ,    ("foldl 2",        mytest prop_foldl2BB)
    ,    ("foldr 1",        mytest prop_foldr1BB)
    ,    ("foldr 2",        mytest prop_foldr2BB)
    ,    ("foldl1 1",       mytest prop_foldl1_1BB)
    ,    ("foldl1 2",       mytest prop_foldl1_2BB)
    ,    ("foldl1 3",       mytest prop_foldl1_3BB)
    ,    ("foldr1 1",       mytest prop_foldr1_1BB)
    ,    ("foldr1 2",       mytest prop_foldr1_2BB)
    ,    ("foldr1 3",       mytest prop_foldr1_3BB)
    ,    ("scanl/foldl",    mytest prop_scanlfoldlBB)
    ,    ("all",            mytest prop_allBB)
    ,    ("any",            mytest prop_anyBB)
    ,    ("take",           mytest prop_takeBB)
    ,    ("drop",           mytest prop_dropBB)
    ,    ("takeWhile",      mytest prop_takeWhileBB)
    ,    ("dropWhile",      mytest prop_dropWhileBB)
    ,    ("splitAt",        mytest prop_splitAtBB)
    ,    ("span",           mytest prop_spanBB)
    ,    ("break",          mytest prop_breakBB)
    ,    ("elem",           mytest prop_elemBB)
    ,    ("notElem",        mytest prop_notElemBB)
    ,    ("concat 1",       mytest prop_concat1BB)
    ,    ("concat 2",       mytest prop_concat2BB)
    ,    ("concat 3",       mytest prop_concatBB)
    ,    ("lines",          mytest prop_linesBB)
    ,    ("unlines",        mytest prop_unlinesBB)
    ,    ("words",          mytest prop_wordsBB)
    ,    ("unwords",        mytest prop_unwordsBB)
    ,    ("group",          mytest prop_groupBB)
    ,    ("groupBy",        mytest prop_groupByBB)
    ,    ("groupBy 1",      mytest prop_groupBy1BB)
    ,    ("join",           mytest prop_joinBB)
    ,    ("elemIndex 1",    mytest prop_elemIndex1BB)
    ,    ("elemIndex 2",    mytest prop_elemIndex2BB)
    ,    ("findIndex",      mytest prop_findIndexBB)
    ,    ("findIndicies",   mytest prop_findIndiciesBB)
    ,    ("elemIndices",    mytest prop_elemIndicesBB)
    ,    ("find",           mytest prop_findBB)
    ,    ("find/findIndex", mytest prop_find_findIndexBB)
    ,    ("sort 1",         mytest prop_sort1BB)
    ,    ("sort 2",         mytest prop_sort2BB)
    ,    ("sort 3",         mytest prop_sort3BB)
    ,    ("sort 4",         mytest prop_sort4BB)
    ,    ("sort 5",         mytest prop_sort5BB)
    ,    ("intersperse",    mytest prop_intersperseBB)
    ,    ("maximum",        mytest prop_maximumBB)
    ,    ("minimum",        mytest prop_minimumBB)
--  ,    ("breakChar",      mytest prop_breakCharBB)
--  ,    ("spanChar 1",     mytest prop_spanCharBB)
--  ,    ("spanChar 2",     mytest prop_spanChar_1BB)
--  ,    ("breakSpace",     mytest prop_breakSpaceBB)
--  ,    ("dropSpace",      mytest prop_dropSpaceBB)
    ,    ("spanEnd",        mytest prop_spanEndBB)
    ,    ("breakEnd",       mytest prop_breakEndBB)
    ,    ("elemIndexEnd 1",mytest prop_elemIndexEnd1BB)
    ,    ("elemIndexEnd 2",mytest prop_elemIndexEnd2BB)
--  ,    ("words'",         mytest prop_wordsBB')
--  ,    ("lines'",         mytest prop_linesBB')
--  ,    ("dropSpaceEnd",   mytest prop_dropSpaceEndBB)
    ,    ("unfoldr",        mytest prop_unfoldrBB)
    ,    ("prefix",         mytest prop_prefixBB)
    ,    ("suffix",         mytest prop_suffixBB)
    ,    ("copy",           mytest prop_copyBB)
    ,    ("inits",          mytest prop_initsBB)
    ,    ("tails",          mytest prop_tailsBB)
    ,    ("findSubstrings ",mytest prop_findSubstringsBB)
    ,    ("replicate1",     mytest prop_replicate1BB)
    ,    ("replicate2",     mytest prop_replicate2BB)
    ,    ("replicate3",     mytest prop_replicate3BB)
    ,    ("readInt",        mytest prop_readintBB)
    ,    ("readInt 2",      mytest prop_readint2BB)
    ,    ("Lazy.readInt",   mytest prop_readintLL)
--  ,    ("filterChar1",    mytest prop_filterChar1BB)
--  ,    ("filterChar2",    mytest prop_filterChar2BB)
--  ,    ("filterChar3",    mytest prop_filterChar3BB)
--  ,    ("filterNotChar1", mytest prop_filterNotChar1BB)
--  ,    ("filterNotChar2", mytest prop_filterNotChar2BB)
    ,    ("tail",           mytest prop_tailSBB)
    ,    ("index",          mytest prop_indexBB)
    ,    ("unsafeIndex",    mytest prop_unsafeIndexBB)
--  ,    ("map'",           mytest prop_mapBB')
    ,    ("filter",         mytest prop_filterBB)
    ,    ("elem",           mytest prop_elemSBB)
    ,    ("take",           mytest prop_takeSBB)
    ,    ("drop",           mytest prop_dropSBB)
    ,    ("splitAt",        mytest prop_splitAtSBB)
    ,    ("foldl",          mytest prop_foldlBB)
    ,    ("foldr",          mytest prop_foldrBB)
    ,    ("takeWhile ",     mytest prop_takeWhileSBB)
    ,    ("dropWhile ",     mytest prop_dropWhileSBB)
    ,    ("span ",          mytest prop_spanSBB)
    ,    ("break ",         mytest prop_breakSBB)
    ,    ("breakspan",      mytest prop_breakspan_1BB)
    ,    ("lines ",         mytest prop_linesSBB)
    ,    ("unlines ",       mytest prop_unlinesSBB)
    ,    ("words ",         mytest prop_wordsSBB)
    ,    ("unwords ",       mytest prop_unwordsSBB)
--  ,    ("wordstokens",    mytest prop_wordstokensBB)
    ,    ("splitWith",      mytest prop_splitWithBB)
    ,    ("joinsplit",      mytest prop_joinsplitBB)
--  ,    ("lineIndices",    mytest prop_lineIndices1BB)
    ,    ("count",          mytest prop_countBB)
--  ,    ("linessplit",     mytest prop_linessplitBB)
    ,    ("splitsplitWith", mytest prop_splitsplitWithBB)
--  ,    ("joinjoinpath",   mytest prop_joinjoinpathBB)
    ,    ("zip",            mytest prop_zipBB)
    ,    ("zip1",           mytest prop_zip1BB)
    ,    ("zipWith",        mytest prop_zipWithBB)
--  ,    ("zipWith'",       mytest prop_zipWith'BB)
    ,    ("unzip",          mytest prop_unzipBB)
    ,    ("concatMap",      mytest prop_concatMapBB)
    ]


------------------------------------------------------------------------
-- Extra lazy properties

ll_tests =
    [("eq 1",               mytest prop_eq1)
    ,("eq 2",               mytest prop_eq2)
    ,("eq 3",               mytest prop_eq3)
    ,("eq refl",            mytest prop_eq_refl)
    ,("eq symm",            mytest prop_eq_symm)
    ,("compare 1",          mytest prop_compare1)
    ,("compare 2",          mytest prop_compare2)
    ,("compare 3",          mytest prop_compare3)
    ,("compare 4",          mytest prop_compare4)
    ,("compare 5",          mytest prop_compare5)
    ,("compare 6",          mytest prop_compare6)
    ,("compare 7",          mytest prop_compare7)
    ,("compare 8",          mytest prop_compare8)
    ,("empty 1",            mytest prop_empty1)
    ,("empty 2",            mytest prop_empty2)
    ,("pack/unpack",        mytest prop_packunpack)
    ,("unpack/pack",        mytest prop_unpackpack)
    ,("null",               mytest prop_null)
    ,("length 1",           mytest prop_length1)
    ,("length 2",           mytest prop_length2)
    ,("cons 1"    ,         mytest prop_cons1)
    ,("cons 2"    ,         mytest prop_cons2)
    ,("cons 3"    ,         mytest prop_cons3)
    ,("cons 4"    ,         mytest prop_cons4)
    ,("snoc"    ,           mytest prop_snoc1)
    ,("head/pack",          mytest prop_head)
    ,("head/unpack",        mytest prop_head1)
    ,("tail/pack",          mytest prop_tail)
    ,("tail/unpack",        mytest prop_tail1)
    ,("last",               mytest prop_last)
    ,("init",               mytest prop_init)
    ,("append 1",           mytest prop_append1)
    ,("append 2",           mytest prop_append2)
    ,("append 3",           mytest prop_append3)
    ,("map 1",              mytest prop_map1)
    ,("map 2",              mytest prop_map2)
    ,("map 3",              mytest prop_map3)
    ,("filter 1",           mytest prop_filter1)
    ,("filter 2",           mytest prop_filter2)
    ,("reverse",            mytest prop_reverse)
    ,("reverse1",           mytest prop_reverse1)
    ,("reverse2",           mytest prop_reverse2)
    ,("transpose",          mytest prop_transpose)
    ,("foldl",              mytest prop_foldl)
    ,("foldl/reverse",      mytest prop_foldl_1)
    ,("foldr",              mytest prop_foldr)
    ,("foldr/id",           mytest prop_foldr_1)
    ,("foldl1/foldl",       mytest prop_foldl1_1)
    ,("foldl1/head",        mytest prop_foldl1_2)
    ,("foldl1/tail",        mytest prop_foldl1_3)
    ,("foldr1/foldr",       mytest prop_foldr1_1)
    ,("foldr1/last",        mytest prop_foldr1_2)
    ,("foldr1/head",        mytest prop_foldr1_3)
    ,("concat 1",           mytest prop_concat1)
    ,("concat 2",           mytest prop_concat2)
    ,("concat/pack",        mytest prop_concat3)
    ,("any",                mytest prop_any)
    ,("all",                mytest prop_all)
    ,("maximum",            mytest prop_maximum)
    ,("minimum",            mytest prop_minimum)
    ,("replicate 1",        mytest prop_replicate1)
    ,("replicate 2",        mytest prop_replicate2)
    ,("take",               mytest prop_take1)
    ,("drop",               mytest prop_drop1)
    ,("splitAt",            mytest prop_drop1)
    ,("takeWhile",          mytest prop_takeWhile)
    ,("dropWhile",          mytest prop_dropWhile)
    ,("break",              mytest prop_break)
    ,("span",               mytest prop_span)
    ,("break/span",         mytest prop_breakspan)
--  ,("break/breakByte",    mytest prop_breakByte)
--  ,("span/spanByte",      mytest prop_spanByte)
    ,("split",              mytest prop_split)
    ,("splitWith",          mytest prop_splitWith)
    ,("join.split/id",      mytest prop_joinsplit)
--  ,("join/joinByte",      mytest prop_joinjoinByte)
    ,("group",              mytest prop_group)
--  ,("groupBy",            mytest prop_groupBy)
    ,("index",              mytest prop_index)
    ,("elemIndex",          mytest prop_elemIndex)
    ,("elemIndices",        mytest prop_elemIndices)
    ,("count/elemIndices",  mytest prop_count)
    ,("findIndex",          mytest prop_findIndex)
    ,("findIndices",        mytest prop_findIndicies)
    ,("find",               mytest prop_find)
    ,("find/findIndex",     mytest prop_find_findIndex)
    ,("elem",               mytest prop_elem)
    ,("notElem",            mytest prop_notElem)
    ,("elem/notElem",       mytest prop_elem_notelem)
--  ,("filterByte 1",       mytest prop_filterByte)
--  ,("filterByte 2",       mytest prop_filterByte2)
--  ,("filterNotByte 1",    mytest prop_filterNotByte)
--  ,("filterNotByte 2",    mytest prop_filterNotByte2)
    ,("isPrefixOf",         mytest prop_isPrefixOf)
    ,("concatMap",          mytest prop_concatMap)
    ]

