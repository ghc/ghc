-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.QuickCheck
-- Copyright   :  (c) Koen Claessen, John Hughes 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck v.0.2
-- DRAFT implementation; last update 000104.
-- Koen Claessen, John Hughes.
-- This file represents work in progress, and might change at a later date.
--
-----------------------------------------------------------------------------

module Debug.QuickCheck
  -- testing functions
  ( quickCheck    -- :: prop -> IO ()
  , verboseCheck  -- :: prop -> IO ()
  , test          -- :: prop -> IO ()  -- = quickCheck
  
  , Config(..)    -- :: *
  , check         -- :: Config -> prop -> IO ()
 
  -- property combinators
  , forAll        -- :: Gen a -> (a -> prop) -> prop
  , (==>)         -- :: Bool -> prop -> prop
  
  -- gathering test-case information
  , label         -- :: String         -> prop -> prop
  , collect       -- :: Show a => a    -> prop -> prop
  , classify      -- :: Bool -> String -> prop -> prop
  , trivial       -- :: Bool           -> prop -> prop
  
  -- generator combinators
  , Gen           -- :: * -> * ; Functor, Monad
  
  , elements      -- :: [a] -> Gen a
  , two           -- :: Gen a -> Gen (a,a)
  , three         -- :: Gen a -> Gen (a,a,a)
  , four          -- :: Gen a -> Gen (a,a,a,a)
  
  , sized         -- :: (Int -> Gen a) -> Gen a
  , resize        -- :: Int -> Gen a -> Gen a
  , choose        -- :: Random a => (a, a) -> Gen a
  , oneof         -- :: [Gen a] -> Gen a
  , frequency     -- :: [(Int, Gen a)] -> Gen a
  
  , vector        -- :: Arbitrary a => Int -> Gen [a]

  -- default generators
  , Arbitrary(..) -- :: class
  , rand          -- :: Gen StdGen
  , promote       -- :: (a -> Gen b) -> Gen (a -> b)
  , variant       -- :: Int -> Gen a -> Gen a

  -- testable
  , Testable(..)  -- :: class
  , Property      -- :: *

  -- For writing your own driver
  , Result(..)	 -- :: data
  , generate	 -- :: Int -> StdGen -> Gen a -> a
  , evaluate     -- :: Testable a => a -> Gen Result
  )
 where

import Prelude

import System.Random
import Data.List( group, sort, intersperse )
import Control.Monad( liftM2, liftM3, liftM4 )

infixr 0 ==>
infix  1 `classify`

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

choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

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
  , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
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
