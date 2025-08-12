module Main where

import GHC.Types.Unique

main :: IO ()
main = do
  let allTags = [minBound .. maxBound] :: [UniqueTag]
  let failures = [(tag, uniqueTag tag, charToUniqueTag (uniqueTag tag))
                 | tag <- allTags
                 , charToUniqueTag (uniqueTag tag) /= tag]

  if null failures
    then putStrLn "PASS: uniqueTag and charToUniqueTag are inverses"
    else do
      putStrLn "FAIL: The following tags failed the inverse property:"
      mapM_ print failures
