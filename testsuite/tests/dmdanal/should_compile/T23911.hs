module T23911 where

import GHC.Exts( noinline )

{-# NOINLINE foo #-}
foo x y = (noinline const) x y

{-# NOINLINE bar #-}
bar x y = const x y


{-# NOINLINE sfoo #-}
sfoo x y = (noinline (+)) x y :: Int

{-# NOINLINE sbar #-}
sbar x y = x + y :: Int
