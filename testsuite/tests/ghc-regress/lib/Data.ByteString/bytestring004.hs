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

main = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests

--
-- Test that, after loop fusion, our code behaves the same as the
-- unfused lazy or list models. Use -ddump-simpl to also check that
-- rules are firing for each case.
--
tests =                           -- 29/5/06, all tests are fusing:
    [("down/down     list", mytest prop_downdown_list)          -- checked
    ,("down/filter   list", mytest prop_downfilter_list)        -- checked
    ,("down/map      list", mytest prop_downmap_list)           -- checked
    ,("filter/down   lazy", mytest prop_filterdown_lazy)        -- checked
    ,("filter/down   list", mytest prop_filterdown_list)        -- checked
    ,("filter/filter lazy", mytest prop_filterfilter_lazy)      -- checked
    ,("filter/filter list", mytest prop_filterfilter_list)      -- checked
    ,("filter/map    lazy", mytest prop_filtermap_lazy)         -- checked
    ,("filter/map    list", mytest prop_filtermap_list)         -- checked
    ,("filter/up     lazy", mytest prop_filterup_lazy)          -- checked
    ,("filter/up     list", mytest prop_filterup_list)          -- checked
    ,("map/down      lazy", mytest prop_mapdown_lazy)           -- checked
    ,("map/down      list", mytest prop_mapdown_list)           -- checked
    ,("map/filter    lazy", mytest prop_mapfilter_lazy)         -- checked
    ,("map/filter    list", mytest prop_mapfilter_list)         -- checked
    ,("map/map       lazy", mytest prop_mapmap_lazy)            -- checked
    ,("map/map       list", mytest prop_mapmap_list)            -- checked
    ,("map/up        lazy", mytest prop_mapup_lazy)             -- checked
    ,("map/up        list", mytest prop_mapup_list)             -- checked
    ,("up/filter     lazy", mytest prop_upfilter_lazy)          -- checked
    ,("up/filter     list", mytest prop_upfilter_list)          -- checked
    ,("up/map        lazy", mytest prop_upmap_lazy)             -- checked
    ,("up/map        list", mytest prop_upmap_list)             -- checked
    ,("up/up         lazy", mytest prop_upup_lazy)              -- checked
    ,("up/up         list", mytest prop_upup_list)              -- checked
    ,("noacc/noacc   lazy", mytest prop_noacc_noacc_lazy)       -- checked
    ,("noacc/noacc   list", mytest prop_noacc_noacc_list)       -- checked
    ,("noacc/up      lazy", mytest prop_noacc_up_lazy)          -- checked
    ,("noacc/up      list", mytest prop_noacc_up_list)          -- checked
    ,("up/noacc      lazy", mytest prop_up_noacc_lazy)          -- checked
    ,("up/noacc      list", mytest prop_up_noacc_list)          -- checked
    ,("map/noacc     lazy", mytest prop_map_noacc_lazy)         -- checked
    ,("map/noacc     list", mytest prop_map_noacc_list)         -- checked
    ,("noacc/map     lazy", mytest prop_noacc_map_lazy)         -- checked
    ,("noacc/map     list", mytest prop_noacc_map_list)         -- checked
    ,("filter/noacc  lazy", mytest prop_filter_noacc_lazy)      -- checked
    ,("filter/noacc  list", mytest prop_filter_noacc_list)      -- checked
    ,("noacc/filter  lazy", mytest prop_noacc_filter_lazy)      -- checked
    ,("noacc/filter  list", mytest prop_noacc_filter_list)      -- checked
    ,("noacc/down    lazy", mytest prop_noacc_down_lazy)        -- checked
    ,("noacc/down    list", mytest prop_noacc_down_list)        -- checked
--  ,("down/noacc    lazy", mytest prop_down_noacc_lazy)        -- checked
    ,("down/noacc    list", mytest prop_down_noacc_list)        -- checked


    ,("length/loop   list", mytest prop_lengthloop_list)
--  ,("length/loop   lazy", mytest prop_lengthloop_lazy)
    ,("maximum/loop  list", mytest prop_maximumloop_list)
--  ,("maximum/loop  lazy", mytest prop_maximumloop_lazy)
    ,("minimum/loop  list", mytest prop_minimumloop_list)
--  ,("minimum/loop  lazy", mytest prop_minimumloop_lazy)

    ]

prop_upup_list = eq3
     (\f g  -> P.foldl f (0::Int) . P.scanl g (0::W))
     ((\f g ->   foldl f (0::Int) .   scanl g (0::W)) :: (X -> W -> X) -> (W -> W -> W) -> [W] -> X)

prop_upup_lazy = eq3
     (\f g  -> L.foldl f (0::X) . L.scanl g (0::W))
     (\f g  -> P.foldl f (0::X) . P.scanl g (0::W))

prop_mapmap_list = eq3
     (\f g  -> P.map f . P.map g)
     ((\f g ->   map f .   map g) :: (W -> W) -> (W -> W) -> [W] -> [W])

prop_mapmap_lazy = eq3
     (\f g  -> L.map f . L.map g)
     (\f g  -> P.map f . P.map g)

prop_filterfilter_list = eq3
     (\f g  -> P.filter f . P.filter g)
     ((\f g ->   filter f .   filter g) :: (W -> Bool) -> (W -> Bool) -> [W] -> [W])

prop_filterfilter_lazy = eq3
     (\f g  -> L.filter f . L.filter g)
     (\f g  -> P.filter f . P.filter g)

prop_mapfilter_list = eq3
     (\f g  -> P.filter f . P.map g)
     ((\f g ->   filter f .   map g) :: (W -> Bool) -> (W -> W) -> [W] -> [W])

prop_mapfilter_lazy = eq3
     (\f g  -> L.filter f . L.map g)
     (\f g  -> P.filter f . P.map g)

prop_filtermap_list = eq3
     (\f g  -> P.map f . P.filter g)
     ((\f g ->   map f .   filter g) :: (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_filtermap_lazy = eq3
     (\f g  -> L.map f . L.filter g)
     (\f g  -> P.map f . P.filter g)

prop_mapup_list = eq3
     (\f g  -> P.foldl g (0::W) . P.map f)
     ((\f g ->   foldl g (0::W) .   map f) :: (W -> W) -> (W -> W -> W) -> [W] -> W)

prop_mapup_lazy = eq3
     (\f g -> L.foldl g (0::W) . L.map f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldl g (0::W) . P.map f)

prop_upmap_list = eq3
     (\f g  -> P.map f . P.scanl g (0::W))
     ((\f g ->   map f .   scanl g (0::W)) :: (W -> W) -> (W -> W -> W) -> [W] -> [W])

prop_upmap_lazy = eq3
     (\f g -> L.map f . L.scanl g (0::W))
     (\f g -> P.map f . P.scanl g (0::W))

prop_filterup_list = eq3
     (\f g  -> P.foldl g (0::W) . P.filter f)
     ((\f g ->   foldl g (0::W) .   filter f) :: (W -> Bool) -> (W -> W -> W) -> [W] -> W)

prop_filterup_lazy = eq3
     (\f g -> L.foldl g (0::W) . L.filter f)
     (\f g -> P.foldl g (0::W) . P.filter f)

prop_upfilter_list = eq3
     (\f g  -> P.filter f . P.scanl g (0::W))
     ((\f g ->   filter f .   scanl g (0::W)) :: (W -> Bool) -> (W -> W -> W) -> [W] -> [W])

prop_upfilter_lazy = eq3
     (\f g -> L.filter f . L.scanl g (0::W))
     (\f g -> P.filter f . P.scanl g (0::W))

prop_downdown_list = eq3
     (\f g  -> P.foldr f (0::X) . P.scanr g (0::W))
     ((\f g ->   foldr f (0::X) .   scanr g (0::W)) :: (W -> X -> X) -> (W -> W -> W) -> [W] -> X)

{-
-- no lazy scanr yet
prop_downdown_lazy = eq3
     (\f g  -> L.foldr f (0::X) . L.scanr g (0::W))
     (\f g  -> P.foldr f (0::X) . P.scanr g (0::W))
-}

prop_mapdown_list = eq3
     (\f g  -> P.foldr g (0::W) . P.map f)
     ((\f g ->   foldr g (0::W) .   map f) :: (W -> W) -> (W -> W -> W) -> [W] -> W)

prop_mapdown_lazy = eq3
     (\f g -> L.foldr g (0::W) . L.map f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldr g (0::W) . P.map f)

prop_downmap_list = eq3
     (\f g  -> P.map f . P.scanr g (0::W))
     ((\f g ->   map f .   scanr g (0::W)) :: (W -> W) -> (W -> W -> W) -> [W] -> [W])

{-
prop_downmap_lazy = eq3
     (\f g -> L.map f . L.scanr g (0::W))
     (\f g -> P.map f . P.scanr g (0::W))
-}

prop_filterdown_list = eq3
     (\f g  -> P.foldr g (0::W) . P.filter f)
     ((\f g ->   foldr g (0::W) .   filter f) :: (W -> Bool) -> (W -> W -> W) -> [W] -> W)

prop_filterdown_lazy = eq3
     (\f g -> L.foldr g (0::W) . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldr g (0::W) . P.filter f)

prop_downfilter_list = eq3
     (\f g  -> P.filter f . P.scanr g (0::W))
     ((\f g ->   filter f .   scanr g (0::W)) :: (W -> Bool) -> (W -> W -> W) -> [W] -> [W])

{-
prop_downfilter_lazy = eq3
     (\f g -> L.filter f . L.scanr g (0::W))
     (\f g -> P.filter f . P.scanr g (0::W))
-}

prop_noacc_noacc_list = eq5
    (\f g h i -> (P.map f . P.filter g) . (P.map h . P.filter i))
    ((\f g h i -> (  map f .   filter g) . (  map h .   filter i))
        :: (W -> W) -> (W -> Bool) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_noacc_noacc_lazy = eq5
     (\f g h i -> (L.map f . L.filter g) . (L.map h . L.filter i))
     (\f g h i -> (P.map f . P.filter g) . (P.map h . P.filter i))

prop_noacc_up_list = eq4
    ( \g h i -> P.foldl g (0::W) . (P.map h . P.filter i))
    ((\g h i ->   foldl g (0::W) . (  map h .   filter i))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> W)

prop_noacc_up_lazy = eq4
    (\g h i -> L.foldl g (0::W) . (L.map h . L.filter i))
    (\g h i -> P.foldl g (0::W) . (P.map h . P.filter i))

prop_up_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.scanl g (0::W))
    ((\g h i -> (  map h .   filter i) .   scanl g (0::W))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_up_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.scanl g (0::W))
    (\g h i -> (P.map h . P.filter i) . P.scanl g (0::W))

prop_map_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.map g)
    ((\g h i -> (  map h .   filter i) .   map g)
        :: (W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_map_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.map g)
    (\g h i -> (P.map h . P.filter i) . P.map g)

prop_noacc_map_list = eq4
    ( \g h i -> P.map g . (P.map h . P.filter i))
    ((\g h i ->   map g . (  map h .   filter i))
        :: (W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_noacc_map_lazy = eq4
    (\g h i -> L.map g . (L.map h . L.filter i))
    (\g h i -> P.map g . (P.map h . P.filter i))

prop_filter_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.filter g)
    ((\g h i -> (  map h .   filter i) .   filter g)
        :: (W -> Bool) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_filter_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.filter g)
    (\g h i -> (P.map h . P.filter i) . P.filter g)

prop_noacc_filter_list = eq4
    ( \g h i -> P.filter g . (P.map h . P.filter i))
    ((\g h i ->   filter g . (  map h .   filter i))
        :: (W -> Bool) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_noacc_filter_lazy = eq4
    (\g h i -> L.filter g . (L.map h . L.filter i))
    (\g h i -> P.filter g . (P.map h . P.filter i))

prop_noacc_down_list = eq4
    ( \g h i -> P.foldr g (0::W) . (P.map h . P.filter i))
    ((\g h i ->   foldr g (0::W) . (  map h .   filter i))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> W)

prop_noacc_down_lazy = eq4
    (\g h i -> L.foldr g (0::W) . (L.map h . L.filter i))
    (\g h i -> P.foldr g (0::W) . (P.map h . P.filter i))

prop_down_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.scanr g (0::W))
    ((\g h i -> (  map h .   filter i) .   scanr g (0::W))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

{-
prop_down_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.scanl g (0::W))
    (\g h i -> (P.map h . P.filter i) . P.scanl g (0::W))
-}

------------------------------------------------------------------------

prop_lengthloop_list = eq2
     (\f  -> P.length . P.filter f)
     ((\f ->   length .   filter f) :: (W -> Bool) -> [W] -> X)

{-
prop_lengthloop_lazy = eq2
     (\f g -> L.length . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.length . P.filter f)
-}

prop_maximumloop_list = eqnotnull2
     (\f  -> P.maximum . P.map f)   -- so we don't get null strings
     ((\f ->   maximum .   map f) :: (W -> W) -> [W] -> W)

{-
prop_maximumloop_lazy = eq2
     (\f g -> L.maximum . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.maximum . P.filter f)
-}

prop_minimumloop_list = eqnotnull2
     (\f  -> P.minimum . P.map f)
     ((\f ->   minimum .   map f) :: (W -> W) -> [W] -> W)

{-
prop_minimumloop_lazy = eq2
     (\f g -> L.minimum . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.minimum . P.filter f)
-}

