{-# LANGUAGE Haskell2010 #-}
-- | This tests the table markup
module Table
  ( tableWithHeader
  , tableWithoutHeader
  , fancyTable
  ) where

-- | Table with header.
--
-- +------+--------------+------------------------------------------+
-- | code | message      | description                              |
-- +======+==============+==========================================+
-- | 200  |   @OK@       | operation successful                     |
-- +------+--------------+------------------------------------------+
-- | 204  | @No Content@ | operation successful, no body returned   |
-- +------+--------------+------------------------------------------+
tableWithHeader :: a -> a
tableWithHeader a = a

-- | Table without header.
--
-- +------+--------------+------------------------------------------+
-- | 200  |   @OK@       | operation successful                     |
-- +------+--------------+------------------------------------------+
-- | 204  | @No Content@ | operation successful, no body returned   |
-- +------+--------------+------------------------------------------+
-- | 404  | @Not Found@  | resource not found                       |
-- +------+--------------+------------------------------------------+
tableWithoutHeader :: a -> a
tableWithoutHeader a = a

-- | Fancy table.
--
-- +------------------------+------------+----------+----------+
-- | Header row, column 1   | Header 2   | Header 3 | Header 4 |
-- | (header rows optional) |            |          |          |
-- +========================+============+==========+==========+
-- | body row 1, column 1   | column 2   | column 3 | column 4 |
-- +------------------------+------------+----------+----------+
-- | 'tableWithHeader'      | Cells may span columns.          |
-- +------------------------+------------+---------------------+
-- | body row 3             | Cells may  | \[                  |
-- +------------------------+ span rows. | f(n) = \sum_{i=1}   |
-- | body row 4             |            | \]                  |
-- +------------------------+------------+---------------------+
fancyTable :: a -> a
fancyTable x = x
