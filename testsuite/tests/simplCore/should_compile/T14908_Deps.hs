module T14908_Deps
  ( Reader
  , runReader
  , asks
  , Arbitrary(..)
  , Gen
  , sized
  , choose
  , Args(..)
  , stdArgs
  , quickCheckWith
  ) where

import System.CPUTime (getCPUTime)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure x = Reader (\_ -> x)
  Reader f <*> Reader g = Reader (\r -> f r (g r))

instance Monad (Reader r) where
  return = pure
  Reader g >>= f = Reader (\r -> runReader (f (g r)) r)

asks :: (r -> a) -> Reader r a
asks f = Reader f

newtype Gen a = Gen { runGen :: Int -> Int -> (a, Int) }

instance Functor Gen where
  fmap f (Gen g) = Gen $ \n s ->
    let (x, s') = g n s
    in (f x, s')

instance Applicative Gen where
  pure x = Gen (\_ s -> (x, s))
  Gen f <*> Gen g = Gen $ \n s ->
    let (h, s1) = f n s
        (x, s2) = g n s1
    in (h x, s2)

instance Monad Gen where
  return = pure
  Gen g >>= f = Gen $ \n s ->
    let (x, s1) = g n s
    in runGen (f x) n s1

class Arbitrary a where
  arbitrary :: Gen a

sized :: (Int -> Gen a) -> Gen a
sized f = Gen $ \n s -> runGen (f n) n s

choose :: (Int, Int) -> Gen Int
choose (lo, hi) = Gen $ \_ s ->
  let s' = nextSeed s
      width = hi - lo + 1
      offset = if width <= 0 then 0 else abs s' `mod` width
  in (lo + offset, s')

data Args = Args
  { maxSize :: Int
  , maxSuccess :: Int
  }

stdArgs :: Args
stdArgs = Args
  { maxSize = 100
  , maxSuccess = 100
  }

quickCheckWith :: (Arbitrary a, Show a) => Args -> (a -> Bool) -> IO ()
quickCheckWith args prop = do
  seed0 <- seedFromCPUTime
  loop 0 seed0
  where
    total = max 0 (maxSuccess args)
    maxN = max 1 (maxSize args)

    loop n seed
      | n >= total = putStrLn ("+++ OK, passed " ++ show total ++ " tests.")
      | otherwise =
          let size = (n `mod` maxN) + 1
              (x, seed') = runGen arbitrary size seed
          in if prop x
               then loop (n + 1) seed'
               else putStrLn ("*** Failed! Falsified with input: " ++ show x)

seedFromCPUTime :: IO Int
seedFromCPUTime = do
  t <- getCPUTime
  return (fromInteger (t `mod` 2147483647))

nextSeed :: Int -> Int
nextSeed x =
  fromInteger ((1103515245 * toInteger x + 12345) `mod` 2147483647)
