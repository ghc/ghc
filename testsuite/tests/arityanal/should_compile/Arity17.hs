module Bug (downsweep) where

import GHC.Utils.Misc ( filterOut )
import qualified Data.Map.Strict as M ( Map, elems )
import qualified Data.Map as Map ( fromListWith )

type DownsweepCache = M.Map Int Int

downsweep :: [Int] -> IO DownsweepCache
downsweep rootSummariesOk = do
    let root_map = mkRootMap rootSummariesOk
    checkDuplicates root_map
    return root_map
  where
    checkDuplicates :: DownsweepCache -> IO ()
    checkDuplicates root_map = multiRootsErr dup_roots
       where
         dup_roots = filterOut (>2) (M.elems root_map)

mkRootMap
  :: [Int]
  -> DownsweepCache
mkRootMap summaries = Map.fromListWith const
  [ (s, s) | s <- summaries ]

multiRootsErr :: [a] -> IO ()
multiRootsErr [] = pure ()
