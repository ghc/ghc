module B where

import Foreign.StablePtr

id2 :: Int
id2 = 2

createHeapObjectB :: IO (StablePtr [Int])
createHeapObjectB = do
  newStablePtr [2+id2]

freeHeapObjectB :: StablePtr [Int] -> IO ()
freeHeapObjectB obj = freeStablePtr obj

foreign export ccall createHeapObjectB :: IO (StablePtr [Int])
foreign export ccall freeHeapObjectB   :: StablePtr [Int] -> IO ()
