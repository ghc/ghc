{-# LANGUAGE PatternSynonyms #-}
module T10897a where
pattern Single :: a -> a
pattern Single x = x
