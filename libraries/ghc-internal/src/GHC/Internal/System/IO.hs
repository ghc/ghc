{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.System.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The standard IO library.
--
-----------------------------------------------------------------------------

module GHC.Internal.System.IO (putStrLn, print) where

import GHC.Internal.Base (String)
import GHC.Internal.IO (IO)
import GHC.Internal.IO.Handle.Text (hPutStrLn)
import GHC.Internal.IO.StdHandles (stdout)
import GHC.Internal.Show (Show, show)

-- | The same as 'putStr', but adds a newline character.
--
-- This operation may fail with the same errors, and has the same issues with concurrency, as 'hPutStr'!
putStrLn        :: String -> IO ()
putStrLn s      =  hPutStrLn stdout s

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- 'print' is implemented as @'putStrLn' '.' 'show'@
--
-- This operation may fail with the same errors, and has the same issues with concurrency, as 'hPutStr'!
--
-- ==== __Examples__
--
-- >>> print [1, 2, 3]
-- [1,2,3]
--
-- Be careful when using 'print' for outputting strings,
-- as this will invoke 'show' and cause strings to be printed
-- with quotation marks and non-ascii symbols escaped.
--
-- >>> print "λ :D"
-- "\995 :D"
--
-- A program to print the first 8 integers and their
-- powers of 2 could be written as:
--
-- >>> print [(n, 2^n) | n <- [0..8]]
-- [(0,1),(1,2),(2,4),(3,8),(4,16),(5,32),(6,64),(7,128),(8,256)]
print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)
