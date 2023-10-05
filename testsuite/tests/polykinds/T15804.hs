{-# Language PolyKinds #-}

module T15804 where

data T :: (a :: k) -> *
