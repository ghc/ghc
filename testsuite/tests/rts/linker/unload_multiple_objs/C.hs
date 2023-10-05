module C where

import Foreign.StablePtr

id3 :: Int
id3 = 3

createHeapObjectC :: IO (StablePtr [Int])
createHeapObjectC = do
  newStablePtr [2+id3]

freeHeapObjectC :: StablePtr [Int] -> IO ()
freeHeapObjectC obj = freeStablePtr obj

foreign export ccall createHeapObjectC :: IO (StablePtr [Int])
foreign export ccall freeHeapObjectC   :: StablePtr [Int] -> IO ()
