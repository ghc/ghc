{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Generator.RangeSwitch
  ( rangeCases,
    RangeTree (..),
  )
where

data Case a = Case
  { caseMin :: Int
  , caseMax :: Int
  , caseConstant :: Either a [a]
  }
  deriving stock (Show)

ranges :: (Enum a, Eq a, Show a) => [a] -> [(Int, Int, a)]
ranges = \case
  [] -> []
  (x : xs) -> reverse (go 0 0 x [] xs)
  where
    go mi ma v rs = \case
      [] -> (mi, ma, v) : rs
      (x : xs)
        | x == v -> go mi (ma + 1) v rs xs
        | otherwise -> go (ma + 1) (ma + 1) x ((mi, ma, v) : rs) xs

cases :: Int -> [a] -> [(Int, Int, a)] -> [Case a]
cases max_rep all_cats = go
  where
    go = \case
      [] -> []
      (r@(mi, ma, v) : rs)
        | rangeSize r > max_rep -> Case mi ma (Left v) : go rs
        | otherwise -> go_lookup mi ma (Left v) rs

    go_lookup rmi rma mv = \case
      [] -> [Case rmi rma mv]
      (r@(mi, ma, v) : rs)
        | rangeSize r > max_rep -> Case rmi rma mv : Case mi ma (Left v) : go rs
        | otherwise -> go_lookup rmi ma (Right (take (ma - rmi + 1) (drop rmi all_cats))) rs

    rangeSize :: (Num a) => (a, a, c) -> a
    rangeSize (mi, ma, _) = ma - mi + 1

rangeCases :: (Enum a, Eq a, Show a) => Int -> [a] -> RangeTree (Either a [a])
rangeCases max_char_length cats = buildRangeTree $ cases max_char_length cats (ranges cats)

data RangeTree a
  = Leaf Int Int a
  | Node Int Int (RangeTree a) (RangeTree a)
  deriving stock (Show)

buildRangeTree :: [Case a] -> RangeTree (Either a [a])
buildRangeTree [(Case start end cat)] = Leaf start end cat
buildRangeTree ranges' =
  let mid = length ranges' `div` 2
      (leftRanges, rightRanges) = splitAt mid ranges'
      (Case startL _ _) = head leftRanges
      (Case _ endR _) = last rightRanges
  in Node startL endR (buildRangeTree leftRanges) (buildRangeTree rightRanges)
