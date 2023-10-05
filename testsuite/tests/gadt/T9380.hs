{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Kind (Type)
import Foreign
import Unsafe.Coerce

data M = A | B deriving (Show, Eq)

newtype S (a :: M) = S Int

data SomeS = forall a . SomeS (S a)

data V0 :: M -> Type where
  V0A :: Int -> V0 A
  V0B :: Double -> V0 B

data V1 :: M -> Type where
  V1A :: Int -> V1 A
  V1B :: Double -> V1 B
  V1a :: () -> V1 a

viewV0 :: S a -> V0 a
viewV0 (S i)
  | even i = unsafeCoerce $ V0A 1
  | otherwise = unsafeCoerce $ V0B 2

viewV1 :: S a -> V1 a
viewV1 (S i)
  | even i = unsafeCoerce $ V1A 1
  | otherwise = unsafeCoerce $ V1B 2


typeOf :: S a -> M
typeOf (S i) = if even i then A else B

cast :: M -> SomeS -> S a
cast ty (SomeS s@(S i))
  | ty == typeOf s = S i
  | otherwise = error "cast"

test0 :: IO ()
test0 =
  let s = cast A (SomeS (S 0))
  in case viewV0 s of
       V0A{} -> putStrLn "test0 - A"
       V0B{} -> putStrLn "test0 - B"

test1 :: IO ()
test1 =
  let s = cast A (SomeS (S 2)) :: S A
  in case viewV0 s of
      V0A{} -> putStrLn "test1 - A"

test2 :: IO ()
test2 =
  let s = cast A (SomeS (S 4))
  in case viewV1 s of
      V1A{} -> putStrLn "test2 - A"
      V1B{} -> putStrLn "test2 - B"
      V1a{} -> putStrLn "test2 - O_o"

main = do
  test0 -- no output at all
  test1 -- A
  test2 -- O_o
