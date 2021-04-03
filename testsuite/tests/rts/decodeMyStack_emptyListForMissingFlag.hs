module Main where

import GHC.Stack.CloneStack
import System.IO.Unsafe

returnFrame :: Int -> [StackEntry]
returnFrame i = case ( unsafePerformIO $ do
                         stack <- cloneMyStack
                         stackEntries <- decode stack
                         pure (i, stackEntries)
                     ) of
  (1, stackEntries) -> stackEntries
  _ -> []

main :: IO ()
main = do
  assertEqual (returnFrame 1) []
  return ()

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y =
  if x == y
    then return ()
    else error $ "assertEqual: " ++ show x ++ " /= " ++ show y
