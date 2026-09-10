{-# LANGUAGE TypeData, LazyFieldAnnotations #-}
module T27732a where

type data T a = Cons ~a
