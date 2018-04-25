{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-trustworthy-safe #-} -- temp broken by 452d6aa95

-- | This module is marked trustworthy but should be inferable as -XSafe.
-- Warning enabled through `-W`.
module TrustworthySafe03 where

g :: Int
g = 1

