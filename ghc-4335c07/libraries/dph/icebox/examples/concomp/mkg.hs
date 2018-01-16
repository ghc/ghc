import Data.Array.ST
import Data.Array
import System.Random
import System.IO
import System.Exit
import System.Environment

import Data.Array.Parallel.Unlifted
import Graph

randomG :: RandomGen g => g -> Int -> Int -> Graph
randomG g n e = Graph n e ues
  where
    aes = runSTArray (do
            arr <- newArray (0,n-1) []
            fill arr (randomRs (0,n-1) g) e
          )

    fill arr _ 0        = return arr
    fill arr (m:n:rs) e =
      let lo = min m n
          hi = max m n
      in
      do
        ns <- readArray arr lo
        if lo == hi || hi `elem` ns
          then fill arr rs e
          else do
                 writeArray arr lo (hi : ns)
                 fill arr rs (e-1)


    ues = toU $ concat [map (m :*:) ns | (m,ns) <- assocs aes]

main = do
         args       <- getArgs
         (n,e,file) <- parseArgs args
         g          <- newStdGen
         storeGraph file $ randomG g n e
  where
    parseArgs [nodes,edges,file] =
      do
        n <- parseInt nodes
        e <- parseInt edges
        return (n,e,file)
    parseArgs _ = do
                    hPutStrLn stderr "Invalid arguments"
                    exitFailure

    parseInt s = case reads s of
                   ((n,_) : _) -> return n
                   _           -> do
                                    hPutStrLn stderr $ "Invalid argument " ++ s
                                    exitFailure

