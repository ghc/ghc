{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}

module T24955a () where

-- Listing multiple classes which use different assumed strategies.
-- The suggested fixes should list:
--   deriving stock (Show, Read)
--   deriving newtype (Eq, Ord)
newtype N = N Int
  deriving (Show, Eq, Read, Ord)
