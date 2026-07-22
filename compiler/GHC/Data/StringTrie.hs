module GHC.Data.StringTrie
  ( StringTrie(..)
  , empty
  , insert
  , fromList
  , fromListWithSparseness
  , lookup
  ) where

import GHC.Prelude hiding (lookup)

import Data.Array (Array, accumArray, assocs)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Lazy as M

-- | A trie mapping strings to values of type @a@.
--
-- See #25763 and Note [Optimising the processing of command-line arguments]
-- in "GHC.Driver.Session".
data StringTrie a
    = -- | A value and other children under the same path.
      --
      -- The 'IntMap' can be empty.
      STNode a (M.IntMap (StringTrie a))
      -- | There is no value at this path, but there are children under it
      --
      -- The 'IntMap' must be non-empty, so 'STEmpty' remains the only way to
      -- represent a trie with no values.
    | STFork (M.IntMap (StringTrie a))
      -- | A trie with no values
    | STEmpty
  deriving (Eq, Show)

-- | There are no key-value pairs in this trie.
empty :: StringTrie a
empty = STEmpty

-- | Inserts a key-value pair into the trie. When a value for the key is already
-- present, the new value replaces the old one.
insert :: forall a. String -> a -> StringTrie a -> StringTrie a
insert k0 v = go k0
  where
    go :: String -> StringTrie a -> StringTrie a
    go (x:xs) STEmpty = STFork $ M.singleton (fromEnum x) (go xs STEmpty)
    go "" STEmpty = STNode v M.empty
    go "" (STFork children) = STNode v children
    go (x:xs) (STFork children) =
        STFork (M.insert (fromEnum x) child children)
      where
        child = go xs (fromMaybe empty $ M.lookup (fromEnum x) children)

    go "" (STNode _ children) = STNode v children
    go (x:xs) (STNode vs children) =
        STNode vs (M.insert (fromEnum x) child children)
      where
        child = go xs (fromMaybe empty $ M.lookup (fromEnum x) children)

-- | Returns the values associated with prefixes of a query string as well as
-- the non-matching suffix. The matches are returned ordered by suffix length.
--
-- Examples:
--
-- > lookup "abc" (fromList [("ab", 2), ("abc", 3), ("b", 4)]) == [(3, ""), (2, "c")]
-- > lookup "abc" (fromList [("ab", 2), ("ab", 3)]) == [(3, "c")]
-- > lookup "ba" (fromList [("ab", 2), ("abc", 3)]) == []
--
-- More formally:
--
-- @(v, suffix)@ is in @lookup queryString t@ iff there is a string @k@ such that
--     * @(k, v)@ is in @t@, and
--     * @k ++ suffix == queryString@.
lookup
    :: String
    -> StringTrie a
    -> [(a, String)]
lookup = go []
  where
    go :: [(a, String)] -> String -> StringTrie a -> [(a, String)]
    --
    -- In @lookup queryString t@ and @go acc unmatchedSuffix t'@,
    --
    -- unmatchedSuffix is a suffix of queryString, and t' is the subtrie
    -- of t at path @queryString - unmatchedSuffix@.
    --
    -- (v, suffix) is in acc iff there is a k such that
    --    * (k, v) is in t, and
    --    * @k ++ suffix == queryString@, and
    --    * @unmatchedSuffix /= suffix@.
    go acc [] (STNode v  _) = (v, "") : acc
    go acc [] (STFork _) = acc
    go acc _ STEmpty = acc
    go acc unmatchedSuffix@(c:rest) (STNode v children) =
      let acc' = (v, unmatchedSuffix) : acc
       in maybe acc' (go acc' rest) (M.lookup (fromEnum c) children)
    go acc (c:rest) (STFork children) =
      maybe acc (go acc rest) (M.lookup (fromEnum c) children)

-- | Constructs a trie from a list of key-value pairs.
--
-- Later entries in the list will overwrite earlier ones with the same key.
--
-- Equivalent to @foldl (\t (k, v) -> insert k v t) empty@, but lazier: the
-- subtrie for each first character is only built when demanded.
fromList :: [(String, a)] -> StringTrie a
fromList = fromListWithSparseness 32

-- | Like 'fromList', but allows the caller to specify a threshold for when to use
-- an array to sort the keys.
fromListWithSparseness :: Int -> [(String, a)] -> StringTrie a
fromListWithSparseness sparseness = go
  where
    go [] = STEmpty
    go xs =
      let revxs = reverse xs
          valuesWithEmptyKeys = filter (null . fst) revxs
          children = M.map go (headBuckets sparseness revxs)
       in case valuesWithEmptyKeys of
            (_, v) : _ -> STNode v children
            _          -> STFork children

    -- Group the entries with a non-empty key by their first character (in reverse order).
    --
    -- Discards entries with an empty key. Uses an array to sort the entries, unless the
    -- entries are too sparse.
    headBuckets :: Int -> [(String, a)] -> M.IntMap [(String, a)]
    headBuckets sparseness kvs
        -- Entries are too sparse if they are too few or if they span too large a range
        -- of characters.
      | null (drop sparseness kvs) || any non_ascii kvs
      = M.fromListWith (++) [ (fromEnum c, [(cs, v)]) | (c:cs, v) <- kvs ]
      | otherwise
      = M.fromDistinctAscList
           [ (i, group) | (i, group) <- assocs (bucketsArr kvs), not (null group) ]

    -- @(k, v)@ is in @bucketsArr kvs ! i@ iff @(tail k, v)@ is in @kvs@ and
    -- @fromEnum (head k) == i@.
    --
    -- Note: Discard entries with an empty key.
    bucketsArr :: [(String, a)] -> Array Int [(String, a)]
    bucketsArr kvs =
      accumArray
        (flip (:))
        []
        (0, 0x7f)
        [ (fromEnum c, (cs, v)) | (c:cs, v) <- kvs ]

    non_ascii (c:_, _) = fromEnum c > 0x7f
    non_ascii ([],  _) = False
