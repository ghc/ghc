{-# OPTIONS_GHC -fno-full-laziness #-}

module Main where

import Control.Monad (unless)
import Data.Time.Clock
import System.IO

data AppState = AppState [Int]

cycleState :: [Int] -> [Int]
cycleState w =  filter (check w) w

check :: [Int] -> Int -> Bool
check world pos = pos `elem` world

initialSet :: [Int]
initialSet = [1]

main :: IO ()
main = appLoop 24 (AppState initialSet)

appLoop :: Int -> AppState -> IO ()
appLoop n s
  | n == 0 = return ()
  | otherwise = do let AppState state = s
                   print state
                   appLoop (n-1) $ AppState (cycleState state)

