module Main where

import GHC.Stack.CloneStack
import System.IO.Unsafe

getDeepStack :: Int -> (Int, [StackEntry])
getDeepStack deepness = case getDeepStackCase deepness of
  [] -> (0, [])
  s -> (deepness, s)
  where
    getDeepStackCase :: Int -> [StackEntry]
    getDeepStackCase 0 =
      unsafePerformIO $
        ( do
            stack <- cloneMyStack
            GHC.Stack.CloneStack.decode stack
        )
    getDeepStackCase n = snd $ getDeepStack $ n - 1

main :: IO ()
main = do
  let (_, stackEntries) = getDeepStack 10
  mapM_ (putStrLn . show) stackEntries
