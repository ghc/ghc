module Main (main) where
import Data.Tuple (Solo (..))

main = do
  print $ Solo (3 :: Int)
  print $ Solo (Just "")
  print $ Just (Solo "")
  print (read (show (Solo (3 :: Int))) :: Solo Int)
  print (read "Just Solo { getSolo = 5 }" :: Maybe (Solo Int))
