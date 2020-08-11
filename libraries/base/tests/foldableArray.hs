{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Main where
import Prelude hiding (foldr, foldl, foldl', foldr1, foldl1, length, null, sum,
                       product, all, any, and, or)
import Data.Foldable
import Control.Exception
import Data.Array
import Data.Foldable
import Data.Typeable
import Data.Either
import Control.Applicative
import Control.DeepSeq
#if __GLASGOW_HASKELL__ < 709
import qualified Data.List as L
#else
import qualified GHC.List as L
#endif

data BadElementException = BadFirst | BadLast deriving (Show, Eq)
instance Exception BadElementException

newtype ForceDefault f a = ForceDefault (f a)
instance Foldable f => Foldable (ForceDefault f) where
  foldMap f (ForceDefault c) = foldMap f c

goodLists, badFronts, badBacks :: [[Integer]]
goodLists = [[0..n] | n <- [(-1)..5]]
badFronts = map (throw BadFirst :) goodLists
badBacks  = map (++ [throw BadLast]) goodLists
doubleBads = map (\l -> throw BadFirst : l ++ [throw BadLast]) goodLists
lists =
        goodLists
        ++ badFronts
        ++ badBacks
        ++ doubleBads

makeArray xs = array (1::Int, length xs) (zip [1..] xs)

arrays = map makeArray lists
goodArrays = map makeArray goodLists


strictCons x y = x + 10*y
rightLazyCons x y = x
leftLazyCons x y = y

conses :: [Integer -> Integer -> Integer]
conses = [(+), strictCons, rightLazyCons, leftLazyCons]

runOneRight :: forall f . Foldable f =>
                             (forall a b . (a -> b -> b) -> b -> f a -> b) ->
                             (Integer -> Integer -> Integer) -> f Integer ->
                             IO (Either BadElementException Integer)
runOneRight fol f container = try (evaluate (fol f 12 container))

runOne1 :: forall f . Foldable f => (forall a . (a -> a -> a) -> f a -> a) ->
                              (Integer -> Integer -> Integer) -> f Integer ->
                              IO (Either BadElementException Integer)
runOne1 fol f container = try (evaluate (fol f container))

runOneLeft :: forall f . Foldable f =>
                             (forall a b . (b -> a -> b) -> b -> f a -> b) ->
                              (Integer -> Integer -> Integer) -> f Integer ->
                              IO (Either BadElementException Integer)
runOneLeft fol f container = try (evaluate (fol f 13 container))

runWithAllRight :: forall f . Foldable f =>
                          (forall a b . (a -> b -> b) -> b -> f a -> b) ->
                          [f Integer] -> IO [Either BadElementException Integer]
runWithAllRight fol containers =
       mapM (uncurry (runOneRight fol)) [(f,c) | f <- conses, c <- containers]

runWithAll1 :: forall f . Foldable f =>
                        (forall a . (a -> a -> a) -> f a -> a) ->
                        [f Integer] -> IO [Either BadElementException Integer]
runWithAll1 fol containers =
          mapM (uncurry (runOne1 fol)) [(f,c) | f <- conses, c <- containers]

runWithAllLeft :: forall f . Foldable f =>
                          (forall a b . (b -> a -> b) -> b -> f a -> b) ->
                          [f Integer] -> IO [Either BadElementException Integer]
runWithAllLeft fol containers = mapM (uncurry (runOneLeft fol))
                              [(f,c) | f <- map flip conses, c <- containers]

testWithAllRight :: forall f . Foldable f =>
                 (forall a b . (a -> b -> b) -> b -> f a -> b) ->
                  (forall a b . (a -> b -> b) -> b -> ForceDefault f a -> b) ->
                   [f Integer] -> IO Bool
testWithAllRight fol1 fol2 containers = (==) <$>
       runWithAllRight fol1 containers <*>
           runWithAllRight fol2 (map ForceDefault containers)

testWithAllLeft :: forall f . Foldable f =>
                   (forall a b . (b -> a -> b) -> b -> f a -> b) ->
                   (forall a b . (b -> a -> b) -> b -> ForceDefault f a -> b) ->
                       [f Integer] -> IO Bool
testWithAllLeft fol1 fol2 containers = (==) <$>
      runWithAllLeft fol1 containers <*>
         runWithAllLeft fol2 (map ForceDefault containers)


testWithAll1 :: forall f . Foldable f =>
                        (forall a . (a -> a -> a) -> f a -> a) ->
                        (forall a . (a -> a -> a) -> ForceDefault f a -> a) ->
                                               [f Integer] -> IO Bool
testWithAll1 fol1 fol2 containers =
  (==) <$> runWithAll1 fol1 containers
            <*> runWithAll1 fol2 (map ForceDefault containers)

checkup f g cs = map f cs == map g (map ForceDefault cs)

main = do
         testWithAllRight foldr foldr arrays >>= print
         testWithAllRight foldr' foldr' arrays >>= print
         testWithAllLeft foldl foldl arrays >>= print
         testWithAllLeft foldl' foldl' arrays >>= print
         testWithAll1 foldl1 foldl1 (filter (not . null) arrays) >>= print
         testWithAll1 foldr1 foldr1 (filter (not . null) arrays) >>= print
         -- we won't bother with the fancy laziness tests for the rest
         print $ checkup length length goodArrays
         print $ checkup sum sum goodArrays
         print $ checkup product product goodArrays
         print $ checkup maximum maximum $ filter (not . null) goodArrays
         print $ checkup minimum minimum $ filter (not . null) goodArrays
         print $ checkup toList toList goodArrays
         print $ checkup null null arrays
