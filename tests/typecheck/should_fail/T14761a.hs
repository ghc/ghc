{-# LANGUAGE TypeOperators, GADTs #-}

-- Enable -Werror to fail in case we get this warning:
--
--   UNPACK pragma lacks '!' on the first argument of ‘A’
--
-- In this test case we expect to get this warning and fail,
-- see T14761c for the opposite.
{-# OPTIONS -Werror #-}

module T14761a where

data A = A { a :: {-# UNPACK #-} Maybe Int }

data x && y = Pair x y

data B = B { b :: {-# UNPACK #-} Maybe Int && [] Char && Int }

data G where
  MkG2 :: {-# UNPACK #-} Maybe Int && [] Char && Int -> G
