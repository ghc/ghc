
module Main (main) where

import Data.List

main :: IO ()
main = do xs <- readFile "testlog"
          let ls = lines xs
              tests = breakTests ls
              unexpectedTests = filter (any ("unexpected" `isInfixOf`)) tests
          putStr $ unlines $ concat unexpectedTests

breakTests :: [String] -> [[String]]
breakTests xs = splitStarting ("=====> " `isPrefixOf`)
                -- Ignore lines telling us that we're running a .T file:
              $ filter (not . ("====> Running " `isPrefixOf`)) xs

splitStarting :: (a -> Bool) -> [a] -> [[a]]
splitStarting f xs0 = case break f xs0 of
                      (_, intro : xs) -> ss intro xs
                      _ -> error "No data"
    where ss intro xs = case break f xs of
                        (this, intro' : rest) ->
                            (intro : this) : ss intro' rest
                        (this, []) -> [intro : this]
