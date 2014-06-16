{-# LANGUAGE TemplateHaskell #-}

module Ann01 where

{-# ANN module (1 :: Int) #-}
{-# ANN module (1 :: Integer) #-}
{-# ANN module (1 :: Double) #-}
{-# ANN module $([| 1 :: Int |]) #-}
{-# ANN module "Hello" #-}
{-# ANN module (Just (1 :: Int)) #-}
{-# ANN module [1 :: Int, 2, 3] #-}
{-# ANN module ([1..10] :: [Integer]) #-}
{-# ANN module ''Foo #-}
{-# ANN module (-1 :: Int) #-}

{-# ANN type Foo (1 :: Int) #-}
{-# ANN type Foo (1 :: Integer) #-}
{-# ANN type Foo (1 :: Double) #-}
{-# ANN type Foo $([| 1 :: Int |]) #-}
{-# ANN type Foo "Hello" #-}
{-# ANN type Foo (Just (1 :: Int)) #-}
{-# ANN type Foo [1 :: Int, 2, 3] #-}
{-# ANN type Foo ([1..10] :: [Integer]) #-}
{-# ANN type Foo ''Foo #-}
{-# ANN type Foo (-1 :: Int) #-}
data Foo = Bar Int

{-# ANN f (1 :: Int) #-}
{-# ANN f (1 :: Integer) #-}
{-# ANN f (1 :: Double) #-}
{-# ANN f $([| 1 :: Int |]) #-}
{-# ANN f "Hello" #-}
{-# ANN f (Just (1 :: Int)) #-}
{-# ANN f [1 :: Int, 2, 3] #-}
{-# ANN f ([1..10] :: [Integer]) #-}
{-# ANN f 'f #-}
{-# ANN f (-1 :: Int) #-}
f x = x
