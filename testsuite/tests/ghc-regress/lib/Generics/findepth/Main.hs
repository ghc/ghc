{-# OPTIONS -fglasgow-exts #-}

{-

This example illustrate test-set generation,
namely all terms of a given depth are generated.

-}

module Main where
import Data.Generics
import LittleLanguage


-- Generate all terms of a given depth
findepth :: forall a. Data a => Int -> [a]
findepth 0 = []
findepth d = result

 where

  -- Helper for type disambiguation
  result = concat $ map recurse shapes

  -- Possible top-level shapes
  shapes = map fromConstr consOf

  -- Recurse into subterms
  recurse = gmapM (const (findepth (d-1)))

  -- Retrieve all constructors of the requested type
  consOf   = algTypeCons
           $ dataTypeOf 
           $ head result


-- For silly tests
data T0 = T0 T1 T2 T3 deriving (Show, Typeable, Data)
data T1 = T1a | T1b   deriving (Show, Typeable, Data)
data T2 = T2a | T2b   deriving (Show, Typeable, Data)
data T3 = T3a | T3b   deriving (Show, Typeable, Data)

main = print $ (   findepth 0 :: [Id]
               , ( findepth 1 :: [Id]
               , ( findepth 2 :: [Id]
               , ( findepth 2 :: [T0]
               , ( findepth 3 :: [Prog]
               )))))

