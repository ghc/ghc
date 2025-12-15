{-# LANGUAGE TypeFamilies #-}

module T11699 where

type family F a where F (f a) = f a
