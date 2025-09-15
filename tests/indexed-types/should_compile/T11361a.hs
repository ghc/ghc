{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T11361a where

class C a where
  type F (a :: k) :: *
  type F (x :: *) = x    -- Not right!
