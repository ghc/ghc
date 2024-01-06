-- See commentary in #17932

module T17932 where

flags (Options f x)
  = reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse x)))))))
  `seq` f

data X = X String Bool Bool Bool Bool

data Options = Options !X [Int]
