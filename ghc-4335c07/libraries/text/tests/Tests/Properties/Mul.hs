{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Properties.Mul (tests) where

import Control.Applicative ((<$>), pure)
import Control.Exception as E (SomeException, catch, evaluate)
import Data.Int (Int32, Int64)
import Data.Text.Internal (mul, mul32, mul64)
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((.&.))

mulRef :: (Integral a, Bounded a) => a -> a -> Maybe a
mulRef a b
  | ab < bot || ab > top = Nothing
  | otherwise            = Just (fromIntegral ab)
  where ab  = fromIntegral a * fromIntegral b
        top = fromIntegral (maxBound `asTypeOf` a) :: Integer
        bot = fromIntegral (minBound `asTypeOf` a) :: Integer

eval :: (a -> b -> c) -> a -> b -> Maybe c
eval f a b = unsafePerformIO $
  (Just <$> evaluate (f a b)) `E.catch` (\(_::SomeException) -> pure Nothing)

t_mul32 :: Int32 -> Int32 -> Property
t_mul32 a b = mulRef a b === eval mul32 a b

t_mul64 :: Int64 -> Int64 -> Property
t_mul64 a b = mulRef a b === eval mul64 a b

t_mul :: Int -> Int -> Property
t_mul a b = mulRef a b === eval mul a b

tests :: [Test]
tests = [
   testProperty "t_mul" t_mul
 , testProperty "t_mul32" t_mul32
 , testProperty "t_mul64" t_mul64
 ]
