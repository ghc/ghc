{-# LANGUAGE ScopedTypeVariables, DatatypeContexts #-}

module Main where

{- 
 - This is a test framework for Arrays, using QuickCheck 
 -
 -}

import qualified Data.Array as Array
import Data.List
import Control.Monad ( liftM2, liftM3, liftM4 )
import System.Random


import Data.Ix
import Data.List( (\\) )

infixl 9  !, //
infixr 0 ==>
infix  1 `classify`

prop_array = 
    forAll genBounds       $ \ (b :: (Int,Int))     ->
    forAll (genIVPs b 10)     $ \ (vs :: [(Int,Int)]) ->
    Array.array b vs
	 `same_arr`
    array b vs
prop_listArray = 
    forAll genBounds       $ \ (b :: (Int,Int))     ->
    forAll (vector (length [fst b..snd b]))
	                   $ \ (vs :: [Bool]) ->
    Array.listArray b vs == Array.array b (zipWith (\ a b -> (a,b))
				                   (Array.range b) vs)

prop_indices = 
    forAll genBounds       $ \ (b :: (Int,Int))     ->
    forAll (genIVPs b 10)     $ \ (vs :: [(Int,Int)]) ->
    let arr = Array.array b vs
    in Array.indices arr == ((Array.range . Array.bounds) arr)

prop_elems = 
    forAll genBounds       $ \ (b :: (Int,Int))     ->
    forAll (genIVPs b 10)     $ \ (vs :: [(Int,Int)]) ->
    let arr = Array.array b vs
    in Array.elems arr == [arr Array.! i | i <- Array.indices arr]

prop_assocs = 
    forAll genBounds       $ \ (b :: (Int,Int))     ->
    forAll (genIVPs b 10)     $ \ (vs :: [(Int,Int)]) ->
    let arr = Array.array b vs
    in Array.assocs arr == [(i, arr Array.! i) | i <- Array.indices arr]

prop_slashslash = 
    forAll genBounds       $ \ (b :: (Int,Int))     ->
    forAll (genIVPs b 10)     $ \ (vs :: [(Int,Int)])  ->
    let arr = Array.array b vs
        us = []
    in arr Array.// us == Array.array (Array.bounds arr)
                          ([(i,arr Array.! i) 
			    | i <- Array.indices arr \\ [i | (i,_) <- us]]
                             ++ us)
prop_accum = 
    forAll genBounds          $ \ (b :: (Int,Int))    ->
    forAll (genIVPs b 10)     $ \ (vs :: [(Int,Int)]) ->

    forAll (genIVPs b 10)     $ \ (us :: [(Int,Int)]) ->
    forAll (choose (0,length us))
	                   $ \ n ->
    let us' = take n us in
    forAll arbitrary       $ \ (fn :: Int -> Int -> Int) ->
    let arr = Array.array b vs
    in Array.accum fn arr us' 
        == foldl (\a (i,v) -> a Array.// [(i,fn (a Array.! i) v)]) arr us'

prop_accumArray = 
    forAll arbitrary          $ \ (f :: Int -> Int -> Int) ->
    forAll arbitrary          $ \ (z :: Int) ->
    forAll genBounds          $ \ (b :: (Int,Int))    ->
    forAll (genIVPs b 10)     $ \ (vs :: [(Int,Int)]) ->
    Array.accumArray f z b vs == Array.accum f 
                (Array.array b [(i,z) | i <- Array.range b]) vs


same_arr :: (Eq b) => Array.Array Int b -> Array Int b -> Bool
same_arr a1 a2 = a == c && b == d
		 && all (\ n -> (a1 Array.! n) == (a2 ! n)) [a..b]
    where (a,b) = Array.bounds a1 :: (Int,Int)
          (c,d) = bounds a2 :: (Int,Int)

genBounds :: Gen (Int,Int)
genBounds = do m <- choose (0,20)
	       n <- choose (minBound,maxBound-m) 
	       return (n,n+m-1)

genIVP :: Arbitrary a => (Int,Int) -> Gen (Int,a)
genIVP b = do { i <- choose b
	      ; v <- arbitrary
              ; return (i,v) 
	      }

genIVPs :: Arbitrary a => (Int,Int) -> Int -> Gen [(Int,a)]
genIVPs b@(low,high) s
  = do { let is = [low..high]
       ; vs <- vector (length is)
       ; shuffle s (zip is vs)
       }

prop_id = forAll genBounds $ \ (b :: (Int,Int)) ->
          forAll (genIVPs b 10) $ \ (ivps :: [(Int,Int)])  ->
          label (show (ivps :: [(Int,Int)])) True

-- rift takes a list, split it (using an Int argument),
-- and then rifts together the split lists into one.
-- Think: rifting a pack of cards.
rift :: Int -> [a] -> [a]
rift n xs = comb (drop n xs) (take n xs) 
   where
      comb (a:as) (b:bs) = a : b : comb as bs
      comb (a:as) []     = a : as
      comb []     (b:bs) = b : bs
      comb []     []     = []


-- suffle makes n random rifts. Typically after
-- log n rifts, the list is in a pretty random order.
-- (where n is the number of elements in the list) 

shuffle :: Int -> [a] -> Gen [a]
shuffle 0 m = return m
shuffle n m = do { r <- choose (1,length m)
                 ; shuffle (n-1) (rift r m)
		 }
prop_shuffle = 
    forAll (shuffle 10 [1..10::Int]) $ \ lst ->
    label (show lst) True

------------------------------------------------------------------------------

main = do test prop_array
	  test prop_listArray
	  test prop_indices
	  test prop_elems
	  test prop_assocs
	  test prop_slashslash
	  test prop_accum
	  test prop_accumArray


instance Show (a -> b) where { show _ = "<FN>" }

------------------------------------------------------------------------------

data (Ix a) => Array a b = MkArray (a,a) (a -> b) deriving ()

array       :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b ivs =
    if and [inRange b i | (i,_) <- ivs]
        then MkArray b
                     (\j -> case [v | (i,v) <- ivs, i == j] of
                            [v]   -> v
                            []    -> error "Array.!: \
                                           \undefined array element"
                            _     -> error "Array.!: \
                                           \multiply defined array element")
        else error "Array.array: out-of-range array association"

listArray             :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs        =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

(!)                   :: (Ix a) => Array a b -> a -> b
(!) (MkArray _ f)     =  f

bounds                :: (Ix a) => Array a b -> (a,a)
bounds (MkArray b _)  =  b

indices               :: (Ix a) => Array a b -> [a]
indices               =  range . bounds

elems                 :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]

assocs                :: (Ix a) => Array a b -> [(a,b)]
assocs a              =  [(i, a!i) | i <- indices a]

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
a // us               =  array (bounds a)
                            ([(i,a!i) | i <- indices a \\ [i | (i,_) <- us]]
                             ++ us)

accum                 :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)]
                                   -> Array a b
accum f               =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

accumArray            :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)]
                                   -> Array a b
accumArray f z b      =  accum f (array b [(i,z) | i <- range b])

ixmap                 :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c
                                         -> Array a c
ixmap b f a           = array b [(i, a ! f i) | i <- range b]

instance  (Ix a)          => Functor (Array a) where
    fmap fn (MkArray b f) =  MkArray b (fn . f) 

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'             =  assocs a == assocs a'

instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <=  a'            =  assocs a <=  assocs a'

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
                    showString "array " .
                    shows (bounds a) . showChar ' ' .
                    shows (assocs a)                  )

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
           (\r -> [(array b as, u) | ("array",s) <- lex r,
                                     (b,t)       <- reads s,
                                     (as,u)      <- reads t   ])
--------------------------------------------------------------------

-- QuickCheck v.0.2
-- DRAFT implementation; last update 000104.
-- Koen Claessen, John Hughes.
-- This file represents work in progress, and might change at a later date.


--------------------------------------------------------------------
-- Generator

newtype Gen a
  = Gen (Int -> StdGen -> a)

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

resize :: Int -> Gen a -> Gen a
resize n (Gen m) = Gen (\_ r -> m n r)

rand :: Gen StdGen
rand = Gen (\n r -> r)

promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> \a -> let Gen m = f a in m n r)

variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v+1)))
 where
  rands r0 = r1 : rands r2 where (r1, r2) = split r0

generate :: Int -> StdGen -> Gen a -> a
generate n rnd (Gen m) = m size rnd'
 where
  (size, rnd') = randomR (0, n) rnd

instance Functor Gen where
  fmap f m = m >>= return . f

instance Monad Gen where
  return a    = Gen (\n r -> a)
  Gen m >>= k =
    Gen (\n r0 -> let (r1,r2) = split r0
                      Gen m'  = k (m n r1)
                   in m' n r2)

-- derived

--choose :: Random a => (a, a) -> Gen a
choose bounds = ((fst . randomR bounds) `fmap` rand)

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | i <- [1..n] ]

oneof :: [Gen a] -> Gen a
oneof gens = elements gens >>= id

frequency :: [(Int, Gen a)] -> Gen a
frequency xs = choose (1, tot) >>= (`pick` xs)
 where
  tot = sum (map fst xs)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs

-- general monadic

two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

four :: Monad m => m a -> m (a, a, a, a)
four m = liftM4 (,,,) m m m m

--------------------------------------------------------------------
-- Arbitrary

class Arbitrary a where
  arbitrary   :: Gen a
  coarbitrary :: a -> Gen b -> Gen b

instance Arbitrary () where
  arbitrary     = return ()
  coarbitrary _ = variant 0

instance Arbitrary Bool where
  arbitrary     = elements [True, False]
  coarbitrary b = if b then variant 0 else variant 1

instance Arbitrary Int where
  arbitrary     = sized $ \n -> choose (-n,n)
  coarbitrary n = variant (if n >= 0 then 2*n else 2*(-n) + 1)

instance Arbitrary Integer where
  arbitrary     = sized $ \n -> choose (-fromIntegral n,fromIntegral n)
  coarbitrary n = variant (fromInteger (if n >= 0 then 2*n else 2*(-n) + 1))

instance Arbitrary Float where
  arbitrary     = liftM3 fraction arbitrary arbitrary arbitrary 
  coarbitrary x = coarbitrary (decodeFloat x)

instance Arbitrary Double where
  arbitrary     = liftM3 fraction arbitrary arbitrary arbitrary 
  coarbitrary x = coarbitrary (decodeFloat x)

fraction a b c = fromInteger a + (fromInteger b / (abs (fromInteger c) + 1))

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary          = liftM2 (,) arbitrary arbitrary
  coarbitrary (a, b) = coarbitrary a . coarbitrary b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c) where
  arbitrary             = liftM3 (,,) arbitrary arbitrary arbitrary
  coarbitrary (a, b, c) = coarbitrary a . coarbitrary b . coarbitrary c

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (a, b, c, d)
 where
  arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary
  coarbitrary (a, b, c, d) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d

instance Arbitrary a => Arbitrary [a] where
  arbitrary          = sized (\n -> choose (0,n) >>= vector)
  coarbitrary []     = variant 0
  coarbitrary (a:as) = coarbitrary a . variant 1 . coarbitrary as

instance (Arbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary         = promote (`coarbitrary` arbitrary)
  coarbitrary f gen = arbitrary >>= ((`coarbitrary` gen) . f)

--------------------------------------------------------------------
-- Testable

data Result
  = Result { ok :: Maybe Bool, stamp :: [String], arguments :: [String] }

nothing :: Result
nothing = Result{ ok = Nothing, stamp = [], arguments = [] }

newtype Property
  = Prop (Gen Result)

result :: Result -> Property
result res = Prop (return res)

evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen = property a

class Testable a where
  property :: a -> Property

instance Testable () where
  property _ = result nothing

instance Testable Bool where
  property b = result (nothing{ ok = Just b })

instance Testable Result where
  property res = result res

instance Testable Property where
  property prop = prop

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
  do a   <- gen
     res <- evaluate (body a)
     return (argument a res)
 where
  argument a res = res{ arguments = show a : arguments res }

(==>) :: Testable a => Bool -> a -> Property
True  ==> a = property a
False ==> a = property ()

label :: Testable a => String -> a -> Property
label s a = Prop (add `fmap` evaluate a)
 where
  add res = res{ stamp = s : stamp res }

classify :: Testable a => Bool -> String -> a -> Property
classify True  name = label name
classify False _    = property

trivial :: Testable a => Bool -> a -> Property
trivial = (`classify` "trivial")

collect :: (Show a, Testable b) => a -> b -> Property
collect v = label (show v)

--------------------------------------------------------------------
-- Testing

data Config = Config
  { configMaxTest :: Int
  , configMaxFail :: Int
  , configSize    :: Int -> Int
  , configEvery   :: Int -> [String] -> String
  }

quick :: Config
quick = Config
  { configMaxTest = 100
  , configMaxFail = 1000
  , configSize    = (+ 3) . (`div` 2)
  , configEvery   = \n args -> let s = show n in s ++ ","
  }
         
verbose :: Config
verbose = quick
  { configEvery = \n args -> show n ++ ":\n" ++ unlines args
  }

test, quickCheck, verboseCheck :: Testable a => a -> IO ()
test         = check quick
quickCheck   = check quick
verboseCheck = check verbose
         
check :: Testable a => Config -> a -> IO ()
check config a =
  do rnd <- newStdGen
     tests config (evaluate a) rnd 0 0 []

tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO () 
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK, passed" ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (configEvery config ntest (arguments result))
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
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

--------------------------------------------------------------------
-- the end.

{-
instance Observable StdGen where { observer = observeBase }

instance Observable a => Observable (Gen a) where 
  observer (Gen a) = send "Gen" (return (Gen) << a)
			   
-}