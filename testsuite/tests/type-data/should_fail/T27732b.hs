{-# LANGUAGE TypeData, LazyFieldAnnotations #-}
module T27732b where

type data T a where
     Cons :: ~a -> T a
