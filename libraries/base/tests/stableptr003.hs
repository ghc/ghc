module Main where

import Control.Monad
import System.Mem.StableName
import Control.Exception

main = do
  mapM_ evaluate list
  stable_list1 <- mapM makeStableName list
  stable_list2 <- mapM makeStableName list
  unless (stable_list1 == stable_list2) $ do
    let l1 = map hashStableName stable_list1
    let l2 = map hashStableName stable_list2
    print $ zip l1 l2

list = [1..10000] :: [Integer]
