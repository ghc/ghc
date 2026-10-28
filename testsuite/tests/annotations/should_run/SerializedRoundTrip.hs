{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
-- | Round-trip values through serializeWithData/deserializeWithData and
-- report the size of their encoding. See Note [Serialized encoding] in
-- GHC.Serialized.
module Main where

import GHC.Serialized
import Data.Data
import Data.Int
import Data.Word
import Data.Ratio ((%))
import qualified Data.ByteString.Short as Short
import Control.Monad (unless)
import System.Exit

data T = A | B Int | C { c1 :: Integer, c2 :: [T], c3 :: Maybe Char } | D Double Rational
  deriving (Data, Show, Eq)

check :: forall a. (Data a, Show a, Eq a) => a -> IO ()
check x = do
  let bs = serializeWithData x
      y  = deserializeWithData bs :: a
  putStrLn (show (Short.length bs) ++ " bytes: " ++ take 50 (show x))
  unless (x == y) $ do
    putStrLn ("MISMATCH: " ++ show y ++ " bytes=" ++ show (Short.unpack bs))
    exitFailure

main :: IO ()
main = do
  -- Int: small values take one LEB128 byte, 64 and -65 need two
  check (0 :: Int)
  check (42 :: Int)
  check (63 :: Int)
  check (64 :: Int)
  check (-1 :: Int)
  check (-64 :: Int)
  check (-65 :: Int)
  check (minBound :: Int)
  check (maxBound :: Int)
  check (minBound :: Int8)
  check (maxBound :: Word64)
  -- Integer
  check (2^(100 :: Int) :: Integer)
  check (-(2^(100 :: Int)) :: Integer)
  check (-(2^(63 :: Int)) :: Integer)
  -- Rational, Double, Float
  check (3.25 :: Double)
  check (-3.25 :: Double)
  check (0 :: Double)
  check (1/3 :: Double)
  check (1.5 :: Float)
  check (-7 % 22 :: Rational)
  -- Char at the LEB128 boundaries
  check 'x'
  check '\0'
  check '\x7f'
  check '\x80'
  check '\x3fff'
  check '\x4000'
  check (maxBound :: Char)
  -- Algebraic types
  check "helloworld"
  check "h\233llo w\246rld \x1F600"
  check (Just True)
  check (Nothing :: Maybe Int)
  check ([1..100] :: [Int])
  check (Left 7 :: Either Int String)
  check (Right "abc" :: Either Int String)
  check [A, B (-5), C 12345678901234567890 [A, B 1] (Just 'q'), D 2.5 (3 % 7)]
  check ((), (1 :: Int, 'c'), [[True, False]])
  putStrLn "all round trips OK"
