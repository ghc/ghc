module D where

import Foreign.StablePtr

id4 :: Int
id4 = 4

createHeapObjectD :: IO (StablePtr [Int])
createHeapObjectD = do
  newStablePtr [2+id4]

freeHeapObjectD :: StablePtr [Int] -> IO ()
freeHeapObjectD obj = freeStablePtr obj

foreign export ccall createHeapObjectD :: IO (StablePtr [Int])
foreign export ccall freeHeapObjectD   :: StablePtr [Int] -> IO ()
