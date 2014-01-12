{-# LANGUAGE GADTs #-}

-- Test for Trac #1396
-- Panics in GHC 6.6.1

module ShouldCompile where

data Right provides final where
  RightNull :: Right final final
  RightCons :: b -> Right a final -> Right (b -> a) final

collapse_right :: right -> Right right final -> final
--collapse_right f (RightNull)     = f
collapse_right f (RightCons b r) = collapse_right (f b) r
