module Main where

import GHC.Stack.CloneStack
import GHC.Internal.ClosureTypes
import System.IO.Unsafe
import Control.Monad

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

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y =
  if x == y
    then return ()
    else error $ "assertEqual: " ++ show x ++ " /= " ++ show y

main :: IO ()
main = do
  let (_, stack) = getDeepStack 1000

  assertEqual (length stack) 1003
  assertEqual
    (stack !! 0)
    StackEntry
      { functionName = "assertEqual",
        moduleName = "Main",
        srcLoc = "decodeMyStack_underflowFrames.hs:24:11",
        closureType = STACK
      }
  assertEqual
    (stack !! 1)
    StackEntry
      { functionName = "main.(...)",
        moduleName = "Main",
        srcLoc = "decodeMyStack_underflowFrames.hs:30:20-36",
        closureType = STACK
      }
  forM_
    [2 .. 1001]
    ( \i ->
        assertEqual
          (stack !! i)
          StackEntry
            { functionName = "getDeepStack.getDeepStackCase",
              moduleName = "Main",
              srcLoc = "decodeMyStack_underflowFrames.hs:20:26-28",
              closureType = STACK
            }
    )
  assertEqual
    (stack !! 1002)
    StackEntry
      { functionName = "getDeepStack.getDeepStackCase",
        moduleName = "Main",
        srcLoc = "decodeMyStack_underflowFrames.hs:15:7-21",
        closureType = STACK
      }
