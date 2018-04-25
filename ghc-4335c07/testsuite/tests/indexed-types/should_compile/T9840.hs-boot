{-# LANGUAGE TypeFamilies #-}

module T9840 where

-- X is an abstract type family (it might be empty or not, though it
-- will turn out to be empty when we check the hs file)
type family X :: * -> * where ..

-- F is known to be empty in the hs-boot file
type family F (a :: * -> *) where
