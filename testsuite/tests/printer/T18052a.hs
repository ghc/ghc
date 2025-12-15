{-# LANGUAGE PatternSynonyms #-}
module T18052a where

(+++) = (++)
pattern x :||: y = (x,y)
type (^^^) = Either
data (&&&)
