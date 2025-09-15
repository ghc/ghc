import Data.Int
import Data.Word

main = do
  print [5 `div` (minBound+k::Int)    | k <- [0 .. 10]]
  print [5 `div` (minBound+k::Int8)   | k <- [0 .. 10]]
  print [5 `div` (minBound+k::Int16)  | k <- [0 .. 10]]
  print [5 `div` (minBound+k::Int32)  | k <- [0 .. 10]]
  print [5 `div` (minBound+k::Int64)  | k <- [0 .. 10]]
  print [5 `quot` (minBound+k::Int)    | k <- [0 .. 10]]
  print [5 `quot` (minBound+k::Int8)   | k <- [0 .. 10]]
  print [5 `quot` (minBound+k::Int16)  | k <- [0 .. 10]]
  print [5 `quot` (minBound+k::Int32)  | k <- [0 .. 10]]
  print [5 `quot` (minBound+k::Int64)  | k <- [0 .. 10]]
