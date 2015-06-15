{-# LANGUAGE ParallelListComp,
             TransformListComp,
             RecordWildCards #-}
--             MonadComprehensions,

module ListComprehensions where



import GHC.Exts
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (sortBy)

-- Let’s look at a simple, normal list comprehension to start:

parallelListComp :: [Int]
parallelListComp = [ x + y * z
                   | x <- [0..10]
                   | y <- [10..20]
                   | z <- [20..30]
                   ]

oldest :: [Int] -> [String]
oldest tbl = [ "str"
             | n <- tbl
             , then id
             ]
