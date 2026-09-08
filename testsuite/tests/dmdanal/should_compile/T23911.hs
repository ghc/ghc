module T23911 where

import GHC.Exts( noinline )

-- We expect foo and bar to get the same
-- strictness signature, despite the 'noinline'

{-# NOINLINE foo #-}
foo x y = (noinline const) x y

{-# NOINLINE bar #-}
bar x y = const x y


{-# NOINLINE sfoo #-}
sfoo x y = (noinline (+)) x y :: Int

{-# NOINLINE sbar #-}
sbar x y = x + y :: Int
