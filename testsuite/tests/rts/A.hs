module A where

import Foreign.StablePtr

id1 :: Int
id1 = 1

createHeapObjectA :: IO (StablePtr [Int])
createHeapObjectA = do
  newStablePtr [2+id1]

freeHeapObjectA :: StablePtr [Int] -> IO ()
freeHeapObjectA obj = freeStablePtr obj

foreign export ccall createHeapObjectA :: IO (StablePtr [Int])
foreign export ccall freeHeapObjectA   :: StablePtr [Int] -> IO ()
