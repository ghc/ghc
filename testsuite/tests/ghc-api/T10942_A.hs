{-

A normal comment, to check if we can still pick up the CPP directive after it.

-}
-- Check that we can parse a file with leading comments

-- ^ haddock
-- * haddock
-- | haddock
-- $ haddock
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module T10942 where

main = return ()
