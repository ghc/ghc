module MultiCaretFallback where

-- A single diagnostic with several related locations, where some can be
-- rendered as carets and some cannot. The third binding is moved to a
-- phantom file via a LINE pragma; that source is unreadable, so it falls
-- through to the "At:" list while the readable bindings get carets.
-- The phantom file is named to sort before this file, so that the phantom
-- span is a related location rather than the primary (the primary is the
-- last occurrence in source order).
-- Run with -fdiagnostics-show-caret to override the testsuite default.

f :: Int -> Int
f x =
  let a = 1
      a = 2
{-# LINE 50 "APhantom.hs" #-}
      a = 3
  in a
