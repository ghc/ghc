{-# LANGUAGE PatternSynonyms #-}

module T9023 where

pattern P a b = Just (a, b)
foo P{} = True
