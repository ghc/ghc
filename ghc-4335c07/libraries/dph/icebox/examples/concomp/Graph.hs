module Graph
where

import Data.Array.Parallel.Unlifted

import System.IO
import Foreign

data Graph = Graph { nodeCount :: Int
                   , edgeCount :: Int
                   , edges     :: UArr (Int :*: Int)
                   }
  deriving(Read,Show)

hPutGraph :: Handle -> Graph -> IO ()
hPutGraph h (Graph { nodeCount = n, edgeCount = e, edges = edges })
  = alloca $ \iptr ->
    do
      poke iptr n
      hPutBuf h iptr (sizeOf n)
      poke iptr e
      hPutBuf h iptr (sizeOf e)
      hPutU h edges

hGetGraph :: Handle -> IO Graph
hGetGraph h
  = alloca $ \iptr ->
    do
      hGetBuf h iptr (sizeOf (undefined :: Int))
      n <- peek iptr
      hGetBuf h iptr (sizeOf (undefined :: Int))
      e <- peek iptr
      edges <- hGetU h
      return $ Graph { nodeCount = n, edgeCount = e, edges = edges }

storeGraph :: FilePath -> Graph -> IO ()
storeGraph file g = withBinaryFile file WriteMode (`hPutGraph` g)

loadGraph :: FilePath -> IO Graph 
loadGraph file = withBinaryFile file ReadMode hGetGraph

