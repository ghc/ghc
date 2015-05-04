{-# LANGUAGE TypeFamilies #-}

module T9840 where

import T9840a

type family X :: * -> * where

type family F (a :: * -> *) where

foo :: G (F X) -> G (F X)
foo x = x
