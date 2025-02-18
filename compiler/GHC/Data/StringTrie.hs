module GHC.Data.StringTrie
  ( StringTrie
  , empty
  , insert
  , fromList
  , lookup
  , lookup'
  ) where

import GHC.Prelude hiding (lookup)

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

data StringTrie a
    = STNode !(Maybe a) !(M.Map Char (StringTrie a))

empty :: StringTrie a
empty = STNode Nothing M.empty

insert :: String -> a -> StringTrie a -> StringTrie a
insert k0 v = go k0
  where
    --go :: String -> StringTrie a -> StringTrie a
    go [] (STNode _ children) = STNode (Just v) children
    go (x:xs) (STNode end children) =
        STNode end (M.insert x child children)
      where
        child = go xs (fromMaybe empty $ M.lookup x children)

lookup :: String -> StringTrie a -> Maybe (a, String)
lookup = lookup' (\_ _ -> True)

-- | Returns the value associated with the longest match prefix as well
-- as the non-matching suffix.
lookup'
    :: (a -> String -> Bool)
       -- ^ a predicate on the value and non-matching suffix
       -- filtering acceptable matches
    -> String
       -- ^ key to llookup
    -> StringTrie a
    -> Maybe (a, String)
lookup' pred = go Nothing
  where
    --go :: Maybe (a, String) -> String -> StringTrie a -> Maybe (a, String)
    go _prev []       (STNode (Just v) _)   = Just (v, "")
    go prev  []       (STNode Nothing  _)   = prev
    go prev  match@(c:rest) (STNode end children) =
      case M.lookup c children of
        Nothing    -> prev'
        Just child -> go prev' rest child
      where
        prev' = case end of
                  Just v | pred v match -> Just (v, match)
                  _ -> prev

fromList :: [(String, a)] -> StringTrie a
fromList = foldl' (\trie (k,v) -> insert k v trie) empty
