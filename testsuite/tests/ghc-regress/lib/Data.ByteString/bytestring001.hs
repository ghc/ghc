#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- Uses multi-param type classes
--

import Test.QuickCheck.Batch
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

import Data.ByteString.Fusion
import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base as L (LazyByteString(..))

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

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary           = do a <- arbitrary ; elements [Nothing, Just a]
  coarbitrary Nothing = variant 0
  coarbitrary _       = variant 1 -- ok?

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
-- ByteString.Lazy <=> ByteString
--

prop_concatBP       = L.concat      `eq1`  P.concat
prop_nullBP         = L.null        `eq1`  P.null
prop_reverseBP      = L.reverse     `eq1`  P.reverse
prop_transposeBP    = L.transpose   `eq1`  P.transpose
prop_groupBP        = L.group       `eq1`  P.group
prop_initsBP        = L.inits       `eq1`  P.inits
prop_tailsBP        = L.tails       `eq1`  P.tails
prop_allBP          = L.all         `eq2`  P.all
prop_anyBP          = L.any         `eq2`  P.any
prop_appendBP       = L.append      `eq2`  P.append
prop_breakBP        = L.break       `eq2`  P.break
-- prop_concatMapBP    = L.concatMap   `eq2`  P.concatMap
prop_consBP         = L.cons        `eq2`  P.cons
prop_countBP        = L.count       `eq2`  P.count
prop_dropBP         = L.drop        `eq2`  P.drop
prop_dropWhileBP    = L.dropWhile   `eq2`  P.dropWhile
prop_filterBP       = L.filter      `eq2`  P.filter
prop_findBP         = L.find        `eq2`  P.find
prop_findIndexBP    = L.findIndex   `eq2`  P.findIndex
prop_findIndicesBP  = L.findIndices `eq2`  P.findIndices
prop_isPrefixOfBP   = L.isPrefixOf  `eq2`  P.isPrefixOf
prop_mapBP          = L.map         `eq2`  P.map
prop_replicateBP    = L.replicate   `eq2`  P.replicate
prop_snocBP         = L.snoc        `eq2`  P.snoc
prop_spanBP         = L.span        `eq2`  P.span
prop_splitBP        = L.split       `eq2`  P.split
prop_splitAtBP      = L.splitAt     `eq2`  P.splitAt
prop_takeBP         = L.take        `eq2`  P.take
prop_takeWhileBP    = L.takeWhile   `eq2`  P.takeWhile
prop_elemBP         = L.elem        `eq2`  P.elem
prop_notElemBP      = L.notElem     `eq2`  P.notElem
prop_elemIndexBP    = L.elemIndex   `eq2`  P.elemIndex
prop_elemIndicesBP  = L.elemIndices `eq2`  P.elemIndices
prop_lengthBP       = L.length      `eq1`  (fromIntegral . P.length :: P.ByteString -> Int64)
prop_readIntBP      = D.readInt     `eq1`  C.readInt
prop_linesBP        = D.lines       `eq1`  C.lines

prop_headBP         = L.head        `eqnotnull1` P.head
prop_initBP         = L.init        `eqnotnull1` P.init
prop_lastBP         = L.last        `eqnotnull1` P.last
prop_maximumBP      = L.maximum     `eqnotnull1` P.maximum
prop_minimumBP      = L.minimum     `eqnotnull1` P.minimum
prop_tailBP         = L.tail        `eqnotnull1` P.tail
prop_foldl1BP       = L.foldl1      `eqnotnull2` P.foldl1
prop_foldl1BP'      = L.foldl1'     `eqnotnull2` P.foldl1'
prop_foldr1BP       = L.foldr1      `eqnotnull2` P.foldr1
prop_scanlBP        = L.scanl       `eqnotnull3` P.scanl

prop_eqBP        = eq2
    ((==) :: B -> B -> Bool)
    ((==) :: P -> P -> Bool)
prop_compareBP   = eq2
    ((compare) :: B -> B -> Ordering)
    ((compare) :: P -> P -> Ordering)
prop_foldlBP     = eq3
    (L.foldl     :: (X -> W -> X) -> X -> B -> X)
    (P.foldl     :: (X -> W -> X) -> X -> P -> X)
prop_foldlBP'    = eq3
    (L.foldl'    :: (X -> W -> X) -> X -> B -> X)
    (P.foldl'    :: (X -> W -> X) -> X -> P -> X)
prop_foldrBP     = eq3
    (L.foldr     :: (W -> X -> X) -> X -> B -> X)
    (P.foldr     :: (W -> X -> X) -> X -> P -> X)
prop_mapAccumLBP = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B -> (X, B))
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))

prop_unfoldrBP   = eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                     fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)

--
-- properties comparing ByteString.Lazy `eq1` List
--

prop_concatBL       = L.concat      `eq1` (concat    :: [[W]] -> [W])
prop_lengthBL       = L.length      `eq1` (length    :: [W] -> Int)
prop_nullBL         = L.null        `eq1` (null      :: [W] -> Bool)
prop_reverseBL      = L.reverse     `eq1` (reverse   :: [W] -> [W])
prop_transposeBL    = L.transpose   `eq1` (transpose :: [[W]] -> [[W]])
prop_groupBL        = L.group       `eq1` (group     :: [W] -> [[W]])
prop_initsBL        = L.inits       `eq1` (inits     :: [W] -> [[W]])
prop_tailsBL        = L.tails       `eq1` (tails     :: [W] -> [[W]])
prop_allBL          = L.all         `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyBL          = L.any         `eq2` (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendBL       = L.append      `eq2` ((++)      :: [W] -> [W] -> [W])
prop_breakBL        = L.break       `eq2` (break     :: (W -> Bool) -> [W] -> ([W],[W]))
-- prop_concatMapBL    = L.concatMap   `eq2` (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consBL         = L.cons        `eq2` ((:)       :: W -> [W] -> [W])
prop_dropBL         = L.drop        `eq2` (drop      :: Int -> [W] -> [W])
prop_dropWhileBL    = L.dropWhile   `eq2` (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterBL       = L.filter      `eq2` (filter    :: (W -> Bool ) -> [W] -> [W])
prop_findBL         = L.find        `eq2` (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndicesBL  = L.findIndices `eq2` (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_findIndexBL    = L.findIndex   `eq2` (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfBL   = L.isPrefixOf  `eq2` (isPrefixOf:: [W] -> [W] -> Bool)
prop_mapBL          = L.map         `eq2` (map       :: (W -> W) -> [W] -> [W])
prop_replicateBL    = L.replicate   `eq2` (replicate :: Int -> W -> [W])
prop_snocBL         = L.snoc        `eq2` ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanBL         = L.span        `eq2` (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtBL      = L.splitAt     `eq2` (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takeBL         = L.take        `eq2` (take      :: Int -> [W] -> [W])
prop_takeWhileBL    = L.takeWhile   `eq2` (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemBL         = L.elem        `eq2` (elem      :: W -> [W] -> Bool)
prop_notElemBL      = L.notElem     `eq2` (notElem   :: W -> [W] -> Bool)
prop_elemIndexBL    = L.elemIndex   `eq2` (elemIndex :: W -> [W] -> Maybe Int)
prop_elemIndicesBL  = L.elemIndices `eq2` (elemIndices:: W -> [W] -> [Int])
prop_linesBL        = D.lines       `eq1` (lines     :: String -> [String])

prop_foldl1BL       = L.foldl1  `eqnotnull2` (foldl1    :: (W -> W -> W) -> [W] -> W)
prop_foldl1BL'      = L.foldl1' `eqnotnull2` (foldl1'   :: (W -> W -> W) -> [W] -> W)
prop_foldr1BL       = L.foldr1  `eqnotnull2` (foldr1    :: (W -> W -> W) -> [W] -> W)
prop_headBL         = L.head    `eqnotnull1` (head      :: [W] -> W)
prop_initBL         = L.init    `eqnotnull1` (init      :: [W] -> [W])
prop_lastBL         = L.last    `eqnotnull1` (last      :: [W] -> W)
prop_maximumBL      = L.maximum `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumBL      = L.minimum `eqnotnull1` (minimum   :: [W] -> W)
prop_tailBL         = L.tail    `eqnotnull1` (tail      :: [W] -> [W])

prop_eqBL         = eq2
    ((==) :: B   -> B   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_compareBL    = eq2
    ((compare) :: B   -> B   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlBL      = eq3
    (L.foldl  :: (X -> W -> X) -> X -> B   -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W] -> X)
prop_foldlBL'     = eq3
    (L.foldl' :: (X -> W -> X) -> X -> B   -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W] -> X)
prop_foldrBL      = eq3
    (L.foldr  :: (W -> X -> X) -> X -> B   -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W] -> X)
prop_mapAccumLBL  = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_unfoldrBL = eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                  take n $
          unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

--
-- And finally, check correspondance between Data.ByteString and List
--

prop_lengthPL     = (fromIntegral.P.length :: P -> Int) `eq1` (length :: [W] -> Int)
prop_nullPL       = P.null      `eq1` (null      :: [W] -> Bool)
prop_reversePL    = P.reverse   `eq1` (reverse   :: [W] -> [W])
prop_transposePL  = P.transpose `eq1` (transpose :: [[W]] -> [[W]])
prop_groupPL      = P.group     `eq1` (group     :: [W] -> [[W]])
prop_initsPL      = P.inits     `eq1` (inits     :: [W] -> [[W]])
prop_tailsPL      = P.tails     `eq1` (tails     :: [W] -> [[W]])
prop_concatPL     = P.concat    `eq1` (concat    :: [[W]] -> [W])
prop_allPL        = P.all       `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyPL        = P.any       `eq2`    (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendPL     = P.append    `eq2`    ((++)      :: [W] -> [W] -> [W])
prop_breakPL      = P.break     `eq2`    (break     :: (W -> Bool) -> [W] -> ([W],[W]))
-- prop_concatMapPL  = P.concatMap `eq2`    (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consPL       = P.cons      `eq2`    ((:)       :: W -> [W] -> [W])
prop_dropPL       = P.drop      `eq2`    (drop      :: Int -> [W] -> [W])
prop_dropWhilePL  = P.dropWhile `eq2`    (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterPL     = P.filter    `eq2`    (filter    :: (W -> Bool ) -> [W] -> [W])
prop_findPL       = P.find      `eq2`    (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndexPL  = P.findIndex `eq2`    (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfPL = P.isPrefixOf`eq2`    (isPrefixOf:: [W] -> [W] -> Bool)
prop_mapPL        = P.map       `eq2`    (map       :: (W -> W) -> [W] -> [W])
prop_replicatePL  = P.replicate `eq2`    (replicate :: Int -> W -> [W])
prop_snocPL       = P.snoc      `eq2`    ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanPL       = P.span      `eq2`    (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtPL    = P.splitAt   `eq2`    (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takePL       = P.take      `eq2`    (take      :: Int -> [W] -> [W])
prop_takeWhilePL  = P.takeWhile `eq2`    (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemPL       = P.elem      `eq2`    (elem      :: W -> [W] -> Bool)
prop_notElemPL    = P.notElem   `eq2`    (notElem   :: W -> [W] -> Bool)
prop_elemIndexPL  = P.elemIndex `eq2`    (elemIndex :: W -> [W] -> Maybe Int)
prop_linesPL      = C.lines     `eq1`    (lines     :: String -> [String])
prop_findIndicesPL= P.findIndices`eq2`   (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_elemIndicesPL= P.elemIndices`eq2`   (elemIndices:: W -> [W] -> [Int])

prop_foldl1PL     = P.foldl1    `eqnotnull2` (foldl1   :: (W -> W -> W) -> [W] -> W)
prop_foldl1PL'    = P.foldl1'   `eqnotnull2` (foldl1' :: (W -> W -> W) -> [W] -> W)
prop_foldr1PL     = P.foldr1    `eqnotnull2` (foldr1 :: (W -> W -> W) -> [W] -> W)
prop_scanlPL      = P.scanl     `eqnotnull3` (scanl  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanl1PL     = P.scanl1    `eqnotnull2` (scanl1 :: (W -> W -> W) -> [W] -> [W])
prop_scanrPL      = P.scanr     `eqnotnull3` (scanr  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanr1PL     = P.scanr1    `eqnotnull2` (scanr1 :: (W -> W -> W) -> [W] -> [W])
prop_headPL       = P.head      `eqnotnull1` (head      :: [W] -> W)
prop_initPL       = P.init      `eqnotnull1` (init      :: [W] -> [W])
prop_lastPL       = P.last      `eqnotnull1` (last      :: [W] -> W)
prop_maximumPL    = P.maximum   `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumPL    = P.minimum   `eqnotnull1` (minimum   :: [W] -> W)
prop_tailPL       = P.tail      `eqnotnull1` (tail      :: [W] -> [W])

prop_eqPL      = eq2
    ((==) :: P   -> P   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_comparePL = eq2
    ((compare) :: P   -> P   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlPL   = eq3
    (P.foldl  :: (X -> W -> X) -> X -> P        -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldlPL'  = eq3
    (P.foldl' :: (X -> W -> X) -> X -> P        -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldrPL   = eq3
    (P.foldr  :: (W -> X -> X) -> X -> P        -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W]      -> X)
prop_mapAccumLPL= eq3
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_mapAccumRPL= eq3
    (P.mapAccumR :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_unfoldrPL = eq3
    ((\n f a ->      fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)
    ((\n f a ->   take n $
          unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

------------------------------------------------------------------------
--
-- And check fusion RULES.
--

prop_lazylooploop em1 em2 start1 start2 arr =
    loopL em2 start2 (loopArr (loopL em1 start1 arr))             ==
    loopSndAcc (loopL (em1 `fuseEFL` em2) (start1 :*: start2) arr)
 where
   _ = start1 :: Int
   _ = start2 :: Int

prop_looploop em1 em2 start1 start2 arr =
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) ==
    loopSndAcc (loopU (em1 `fuseEFL` em2) (start1 :*: start2) arr)
 where
   _ = start1 :: Int
   _ = start2 :: Int

------------------------------------------------------------------------

-- check associativity of sequence loops
prop_sequenceloops_assoc n m o x y z a1 a2 a3 xs =

    k ((f * g) * h) == k (f * (g * h))  -- associativity

    where
       (*) = sequenceLoops
       f = (sel n)      x a1
       g = (sel m)      y a2
       h = (sel o)      z a3

       _ = a1 :: Int; _ = a2 :: Int; _ = a3 :: Int
       k g = loopArr (loopWrapper g xs)

-- check wrapper elimination
prop_loop_loop_wrapper_elimination n m x y a1 a2 xs =
  loopWrapper g (loopArr (loopWrapper f xs)) ==
    loopSndAcc (loopWrapper (sequenceLoops f g) xs)
  where
       f = (sel n) x a1
       g = (sel m) y a2
       _ = a1 :: Int; _ = a2 :: Int

sel :: Bool
       -> (acc -> Word8 -> PairS acc (MaybeS Word8))
       -> acc
       -> Ptr Word8
       -> Ptr Word8
       -> Int
       -> IO (PairS (PairS acc Int) Int)
sel False = doDownLoop
sel True  = doUpLoop

------------------------------------------------------------------------
--
-- Test fusion forms
--

prop_up_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doUpLoop f2 acc2)) ==
  k (doUpLoop (f1 `fuseAccAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_down_down_loop_fusion f1 f2 acc1 acc2 xs =
    k (sequenceLoops (doDownLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseAccAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int ; _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
  k (doNoAccLoop (f1 `fuseNoAccNoAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int ; _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doUpLoop f2 acc2)) ==
  k (doUpLoop (f1 `fuseNoAccAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_up_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
  k (doUpLoop (f1 `fuseAccNoAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_noAcc_down_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseNoAccAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_down_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doDownLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
  k (doDownLoop (f1 `fuseAccNoAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_map_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doMapLoop (f1 `fuseMapMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doFilterLoop (f1 `fuseFilterFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseMapFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseFilterMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseMapNoAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseNoAccMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doUpLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseMapAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_up_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseAccMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_down_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseMapAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_down_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doDownLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseAccMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseFilterNoAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseNoAccFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doUpLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseFilterAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_up_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseAccFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_down_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseFilterAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_down_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doDownLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseAccFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

------------------------------------------------------------------------

prop_length_loop_fusion_1 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doUpLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doUpLoop f1 acc1) xs))
  where _ = acc1 :: Int

prop_length_loop_fusion_2 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doDownLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doDownLoop f1 acc1) xs))
  where _ = acc1 :: Int

prop_length_loop_fusion_3 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doMapLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doMapLoop f1 acc1) xs))
  where _ = acc1 :: Int

prop_length_loop_fusion_4 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doFilterLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doFilterLoop f1 acc1) xs))
  where _ = acc1 :: Int

------------------------------------------------------------------------
-- The entry point

main :: IO ()
main = myrun tests

myrun :: [(String, Int -> IO ())] -> IO ()
myrun tests = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests

--
-- And now a list of all the properties to test.
--

tests =  misc_tests
      ++ bl_tests
      ++ bp_tests
      ++ pl_tests
      ++ fusion_tests

misc_tests =
    [("invariant",          mytest prop_invariant)]

------------------------------------------------------------------------
-- ByteString.Lazy <=> List

bl_tests =
    [("all",         mytest prop_allBL)
    ,("any",         mytest prop_anyBL)
    ,("append",      mytest prop_appendBL)
    ,("compare",     mytest prop_compareBL)
    ,("concat",      mytest prop_concatBL)
    ,("cons",        mytest prop_consBL)
    ,("eq",          mytest prop_eqBL)
    ,("filter",      mytest prop_filterBL)
    ,("find",        mytest prop_findBL)
    ,("findIndex",   mytest prop_findIndexBL)
    ,("findIndices", mytest prop_findIndicesBL)
    ,("foldl",       mytest prop_foldlBL)
    ,("foldl'",      mytest prop_foldlBL')
    ,("foldl1",      mytest prop_foldl1BL)
    ,("foldl1'",     mytest prop_foldl1BL')
    ,("foldr",       mytest prop_foldrBL)
    ,("foldr1",      mytest prop_foldr1BL)
    ,("mapAccumL",   mytest prop_mapAccumLBL)
    ,("unfoldr",     mytest prop_unfoldrBL)
    ,("head",        mytest prop_headBL)
    ,("init",        mytest prop_initBL)
    ,("isPrefixOf",  mytest prop_isPrefixOfBL)
    ,("last",        mytest prop_lastBL)
    ,("length",      mytest prop_lengthBL)
    ,("map",         mytest prop_mapBL)
    ,("maximum",     mytest prop_maximumBL)
    ,("minimum",     mytest prop_minimumBL)
    ,("null",        mytest prop_nullBL)
    ,("reverse",     mytest prop_reverseBL)
    ,("snoc",        mytest prop_snocBL)
    ,("tail",        mytest prop_tailBL)
    ,("transpose",   mytest prop_transposeBL)
    ,("replicate",   mytest prop_replicateBL)
    ,("take",        mytest prop_takeBL)
    ,("drop",        mytest prop_dropBL)
    ,("splitAt",     mytest prop_splitAtBL)
    ,("takeWhile",   mytest prop_takeWhileBL)
    ,("dropWhile",   mytest prop_dropWhileBL)
    ,("break",       mytest prop_breakBL)
    ,("span",        mytest prop_spanBL)
    ,("group",       mytest prop_groupBL)
    ,("inits",       mytest prop_initsBL)
    ,("tails",       mytest prop_tailsBL)
    ,("elem",        mytest prop_elemBL)
    ,("notElem",     mytest prop_notElemBL)
    ,("lines",       mytest prop_linesBL)
    ,("elemIndex",   mytest prop_elemIndexBL)
    ,("elemIndices", mytest prop_elemIndicesBL)
--  ,("concatMap",   mytest prop_concatMapBL)
    ]

------------------------------------------------------------------------
-- ByteString.Lazy <=> ByteString

bp_tests =
    [("all",         mytest prop_allBP)
    ,("any",         mytest prop_anyBP)
    ,("append",      mytest prop_appendBP)
    ,("compare",     mytest prop_compareBP)
    ,("concat",      mytest prop_concatBP)
    ,("cons",        mytest prop_consBP)
    ,("eq",          mytest prop_eqBP)
    ,("filter",      mytest prop_filterBP)
    ,("find",        mytest prop_findBP)
    ,("findIndex",   mytest prop_findIndexBP)
    ,("findIndices", mytest prop_findIndicesBP)
    ,("foldl",       mytest prop_foldlBP)
    ,("foldl'",      mytest prop_foldlBP')
    ,("foldl1",      mytest prop_foldl1BP)
    ,("foldl1'",     mytest prop_foldl1BP')
    ,("foldr",       mytest prop_foldrBP)
    ,("foldr1",      mytest prop_foldr1BP)
    ,("mapAccumL",   mytest prop_mapAccumLBP)
    ,("unfoldr",     mytest prop_unfoldrBP)
    ,("head",        mytest prop_headBP)
    ,("init",        mytest prop_initBP)
    ,("isPrefixOf",  mytest prop_isPrefixOfBP)
    ,("last",        mytest prop_lastBP)
    ,("length",      mytest prop_lengthBP)
    ,("readInt",     mytest prop_readIntBP)
    ,("lines",       mytest prop_linesBP)
    ,("map",         mytest prop_mapBP)
    ,("maximum   ",  mytest prop_maximumBP)
    ,("minimum"   ,  mytest prop_minimumBP)
    ,("null",        mytest prop_nullBP)
    ,("reverse",     mytest prop_reverseBP)
    ,("snoc",        mytest prop_snocBP)
    ,("tail",        mytest prop_tailBP)
    ,("scanl",       mytest prop_scanlBP)
    ,("transpose",   mytest prop_transposeBP)
    ,("replicate",   mytest prop_replicateBP)
    ,("take",        mytest prop_takeBP)
    ,("drop",        mytest prop_dropBP)
    ,("splitAt",     mytest prop_splitAtBP)
    ,("takeWhile",   mytest prop_takeWhileBP)
    ,("dropWhile",   mytest prop_dropWhileBP)
    ,("break",       mytest prop_breakBP)
    ,("span",        mytest prop_spanBP)
    ,("split",       mytest prop_splitBP)
    ,("count",       mytest prop_countBP)
    ,("group",       mytest prop_groupBP)
    ,("inits",       mytest prop_initsBP)
    ,("tails",       mytest prop_tailsBP)
    ,("elem",        mytest prop_elemBP)
    ,("notElem",     mytest prop_notElemBP)
    ,("elemIndex",   mytest prop_elemIndexBP)
    ,("elemIndices", mytest prop_elemIndicesBP)
--  ,("concatMap",   mytest prop_concatMapBP)
    ]

------------------------------------------------------------------------
-- ByteString <=> List

pl_tests =
    [("all",         mytest prop_allPL)
    ,("any",         mytest prop_anyPL)
    ,("append",      mytest prop_appendPL)
    ,("compare",     mytest prop_comparePL)
    ,("concat",      mytest prop_concatPL)
    ,("cons",        mytest prop_consPL)
    ,("eq",          mytest prop_eqPL)
    ,("filter",      mytest prop_filterPL)
    ,("find",        mytest prop_findPL)
    ,("findIndex",   mytest prop_findIndexPL)
    ,("findIndices", mytest prop_findIndicesPL)
    ,("foldl",       mytest prop_foldlPL)
    ,("foldl'",      mytest prop_foldlPL')
    ,("foldl1",      mytest prop_foldl1PL)
    ,("foldl1'",     mytest prop_foldl1PL')
    ,("foldr1",      mytest prop_foldr1PL)
    ,("foldr",       mytest prop_foldrPL)
    ,("mapAccumL",   mytest prop_mapAccumLPL)
    ,("mapAccumR",   mytest prop_mapAccumRPL)
    ,("unfoldr",     mytest prop_unfoldrPL)
    ,("scanl",       mytest prop_scanlPL)
    ,("scanl1",      mytest prop_scanl1PL)
    ,("scanr",       mytest prop_scanrPL)
    ,("scanr1",      mytest prop_scanr1PL)
    ,("head",        mytest prop_headPL)
    ,("init",        mytest prop_initPL)
    ,("last",        mytest prop_lastPL)
    ,("maximum",     mytest prop_maximumPL)
    ,("minimum",     mytest prop_minimumPL)
    ,("tail",        mytest prop_tailPL)
    ,("isPrefixOf",  mytest prop_isPrefixOfPL)
    ,("length",      mytest prop_lengthPL)
    ,("map",         mytest prop_mapPL)
    ,("null",        mytest prop_nullPL)
    ,("reverse",     mytest prop_reversePL)
    ,("snoc",        mytest prop_snocPL)
    ,("transpose",   mytest prop_transposePL)
    ,("replicate",   mytest prop_replicatePL)
    ,("take",        mytest prop_takePL)
    ,("drop",        mytest prop_dropPL)
    ,("splitAt",     mytest prop_splitAtPL)
    ,("takeWhile",   mytest prop_takeWhilePL)
    ,("dropWhile",   mytest prop_dropWhilePL)
    ,("break",       mytest prop_breakPL)
    ,("span",        mytest prop_spanPL)
    ,("group",       mytest prop_groupPL)
    ,("inits",       mytest prop_initsPL)
    ,("tails",       mytest prop_tailsPL)
    ,("elem",        mytest prop_elemPL)
    ,("notElem",     mytest prop_notElemPL)
    ,("lines",       mytest prop_linesBL)
    ,("elemIndex",   mytest prop_elemIndexPL)
    ,("elemIndices", mytest prop_elemIndicesPL)
--  ,("concatMap",   mytest prop_concatMapPL)
    ]

------------------------------------------------------------------------
-- Fusion rules

fusion_tests =
-- v1 fusion
    [    ("lazy loop/loop fusion", mytest prop_lazylooploop)
    ,    ("loop/loop fusion",      mytest prop_looploop)

-- v2 fusion
    ,("loop/loop wrapper elim",       mytest prop_loop_loop_wrapper_elimination)
    ,("sequence association",         mytest prop_sequenceloops_assoc)

    ,("up/up         loop fusion",    mytest prop_up_up_loop_fusion)
    ,("down/down     loop fusion",    mytest prop_down_down_loop_fusion)
    ,("noAcc/noAcc   loop fusion",    mytest prop_noAcc_noAcc_loop_fusion)
    ,("noAcc/up      loop fusion",    mytest prop_noAcc_up_loop_fusion)
    ,("up/noAcc      loop fusion",    mytest prop_up_noAcc_loop_fusion)
    ,("noAcc/down    loop fusion",    mytest prop_noAcc_down_loop_fusion)
    ,("down/noAcc    loop fusion",    mytest prop_down_noAcc_loop_fusion)
    ,("map/map       loop fusion",    mytest prop_map_map_loop_fusion)
    ,("filter/filter loop fusion",    mytest prop_filter_filter_loop_fusion)
    ,("map/filter    loop fusion",    mytest prop_map_filter_loop_fusion)
    ,("filter/map    loop fusion",    mytest prop_filter_map_loop_fusion)
    ,("map/noAcc     loop fusion",    mytest prop_map_noAcc_loop_fusion)
    ,("noAcc/map     loop fusion",    mytest prop_noAcc_map_loop_fusion)
    ,("map/up        loop fusion",    mytest prop_map_up_loop_fusion)
    ,("up/map        loop fusion",    mytest prop_up_map_loop_fusion)
    ,("map/down      loop fusion",    mytest prop_map_down_fusion)
    ,("down/map      loop fusion",    mytest prop_down_map_loop_fusion)
    ,("filter/noAcc  loop fusion",    mytest prop_filter_noAcc_loop_fusion)
    ,("noAcc/filter  loop fusion",    mytest prop_noAcc_filter_loop_fusion)
    ,("filter/up     loop fusion",    mytest prop_filter_up_loop_fusion)
    ,("up/filter     loop fusion",    mytest prop_up_filter_loop_fusion)
    ,("filter/down   loop fusion",    mytest prop_filter_down_fusion)
    ,("down/filter   loop fusion",    mytest prop_down_filter_loop_fusion)

    ,("length/loop   fusion",          mytest prop_length_loop_fusion_1)
    ,("length/loop   fusion",          mytest prop_length_loop_fusion_2)
    ,("length/loop   fusion",          mytest prop_length_loop_fusion_3)
    ,("length/loop   fusion",          mytest prop_length_loop_fusion_4)
    ]


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

