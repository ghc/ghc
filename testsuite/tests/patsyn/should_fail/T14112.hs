{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module T14112 where

pattern MyJust1 a = Just !a
