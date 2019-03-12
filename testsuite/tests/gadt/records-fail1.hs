{-# LANGUAGE GADTs #-}

-- Tests record syntax for GADTs

-- Record syntax in GADTs has been deprecated since July 2009
-- see commit 432b9c9322181a3644083e3c19b7e240d90659e7 by simonpj:
-- "New syntax for GADT-style record declarations, and associated refactoring"
-- and #3306

-- It's been removed in August 2015
-- see Phab D1118

-- test should result in a parse error

module ShouldFail where

data T a where
  T1 { x :: a, y :: b } :: T (a,b)
  T4 { x :: Int } :: T [a]
