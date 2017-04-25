{-# LANGUAGE TypeApplications #-}
import Data.Ord

main :: IO ()
main = do
  print ((<) 10 20)
  print ((<) @(Down _) 10 20)
