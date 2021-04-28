module Main where

import GHC.Stack.CloneStack
import System.IO.Unsafe

getDecodedStack :: Int -> (Int, [StackEntry])
getDecodedStack i = case ( unsafePerformIO $ do
                             stack <- cloneMyStack
                             stackEntries <- decode stack
                             pure (i, stackEntries)
                         ) of
  (1, stackEntries) -> (1, stackEntries)
  (i, _) -> (i, [])

returnFrame :: Int -> [StackEntry]
returnFrame i = case getDecodedStack i of
  (1, stackEntries) -> stackEntries
  _ -> []

main :: IO ()
main = do
  assertEqual
    (returnFrame 1)
    [ StackEntry
        { functionName = "getDecodedStack",
          moduleName = "Main",
          srcLoc = "decodeMyStack.hs:7:28-42",
          closureType = 53
        },
      StackEntry
        { functionName = "returnFrame",
          moduleName = "Main",
          srcLoc = "decodeMyStack.hs:(16,1)-(18,9)",
          closureType = 53
        },
      StackEntry
        { functionName = "assertEqual",
          moduleName = "Main",
          srcLoc = "decodeMyStack.hs:47:11",
          closureType = 53
        }
    ]
  return ()

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y =
  if x == y
    then return ()
    else error $ "assertEqual: " ++ show x ++ " /= " ++ show y
