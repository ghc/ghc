{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module T11977 where

-- A pattern synonym for a /function/

pattern F :: b -> Char -> b
pattern F a <- (($ 'a') -> a)
