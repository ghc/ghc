{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -W -fno-warn-trustworthy-safe #-}

-- | This module is marked trustworthy but should be inferable as -XSafe.
-- Warning enabled through `-W` but then disabled with `-fno-warn...`.
module TrustworthySafe04 where

g :: Int
g = 1

