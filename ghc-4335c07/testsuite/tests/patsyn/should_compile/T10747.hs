{-# LANGUAGE PatternSynonyms #-}

module T10747 where

pattern head `Cons` tail = head : tail
