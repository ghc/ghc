{-# LANGUAGE ParallelListComp,
             TransformListComp,
             RecordWildCards #-}
module Test10312 where
-- From
-- https://ocharles.org.uk/blog/guest-posts/2014-12-07-list-comprehensions.html

import GHC.Exts
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (sortBy)

-- Let’s look at a simple, normal list comprehension to start:

regularListComp :: [Int]
regularListComp = [ x + y * z
                  | x <- [0..10]
                  , y <- [10..20]
                  , z <- [20..30]
                  ]

parallelListComp :: [Int]
parallelListComp = [ x + y * z
                   | x <- [0..10]
                   | y <- [10..20]
                   | z <- [20..30]
                   ]

-- fibs :: [Int]
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs :: [Int]
fibs = 0 : 1 : [ x + y
               | x <- fibs
               | y <- tail fibs
               ]

fiblikes :: [Int]
fiblikes = 0 : 1 : [ x + y + z
                   | x <- fibs
                   | y <- tail fibs
                   | z <- tail (tail fibs)
                   ]

-- TransformListComp
data Character = Character
  { firstName :: String
  , lastName :: String
  , birthYear :: Int
  } deriving (Show, Eq)

friends :: [Character]
friends = [ Character "Phoebe" "Buffay" 1963
          , Character "Chandler" "Bing" 1969
          , Character "Rachel" "Green" 1969
          , Character "Joey" "Tribbiani" 1967
          , Character "Monica" "Geller" 1964
          , Character "Ross" "Geller" 1966
          ]

oldest :: Int -> [Character] -> [String]
oldest k tbl = [ firstName ++ " " ++ lastName
               | Character{..} <- tbl
               , then sortWith by birthYear
               , then take k
               ]

groupByLargest :: Ord b => (a -> b) -> [a] -> [[a]]
groupByLargest f = sortBy (comparing (negate . length)) . groupWith f

bestBirthYears :: [Character] -> [(Int, [String])]
bestBirthYears tbl = [ (the birthYear, firstName)
                     | Character{..} <- tbl
                     , then group by birthYear using groupByLargest
                     ]

uniq_fs = [ (n, the p, the d') | (n, Fixity p d) <- fs
                                   , let d' = ppDir d
                                   , then group by Down (p,d') using groupWith ]
