#!/usr/bin/env whatever

{-# LANGUAGE CPP #-}

module Main where

-- Compiling a program with CPP that also has a hash bang should work (#6132).
-- Before ghc-7.8, it failed with:
--
-- error: invalid preprocessing directive #!
-- #!/usr/bin/env runghc

#if 1
main = return ()
#endif
