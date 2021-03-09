module Main where

import T13292a

-- main :: IO ()
main = someFunc

-- This one is compiled with -fdefer-type-errors, and
-- annoyingly reports the ill-typed twice. It is awkward
-- to prevent this, and it's very much a corner case,
-- so I'm accepting it. See Note [Dealing with main]
