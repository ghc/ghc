{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

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

type Result a = (# (# #) | (# a, String #) #)

pattern URNothing :: Result a
pattern URNothing = (# (# #) | #)

pattern URJust :: a -> String -> Result a
pattern URJust x rest = (# | (# x, rest #) #)

{-# COMPLETE URNothing, URJust #-}

-- | Returns the value associated with the longest match prefix as well
-- as the non-matching suffix. Accepts a predicate to further filter matches.
lookup'
    :: forall a.
       (a -> String -> Bool)
       -- ^ a predicate on the value and non-matching suffix
       -- filtering acceptable matches
    -> String
       -- ^ key to lookup
    -> StringTrie a
    -> Maybe (a, String)
lookup' pred = \str trie ->
    case go URNothing str trie of
      URNothing -> Nothing
      URJust v rest -> Just (v, rest)
  where
    go :: Result a -> String -> StringTrie a -> Result a
    go _prev []       (STNode (Just v) _)   = URJust v ""
    go prev  []       (STNode Nothing  _)   = prev
    go prev  match@(c:rest) (STNode end children) =
      case M.lookup c children of
        Nothing    -> prev'
        Just child -> go prev' rest child
      where
        prev' :: Result a
        prev' = case end of
                  Just v | pred v match -> URJust v match
                  _ -> prev


fromList :: [(String, a)] -> StringTrie a
fromList = foldl' (\trie (k,v) -> insert k v trie) empty
