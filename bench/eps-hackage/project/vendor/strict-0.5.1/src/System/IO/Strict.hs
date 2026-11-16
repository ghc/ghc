{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Strict
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  dons@galois.com
-- Stability   :  stable
-- Portability :  portable
--
-- The standard IO input functions using strict IO.
--
-----------------------------------------------------------------------------

module System.IO.Strict (

    -- * Strict Handle IO
    hGetContents,              -- :: Handle -> IO [Char]

    -- * Strict String IO wrappers
    getContents,               -- :: IO String
    readFile,                  -- :: FilePath -> IO String
    interact                   -- :: (String -> String) -> IO ()

  ) where

import Prelude ( String, (>>=), seq, return, (.), (=<<), FilePath, length)
import System.IO (IO)
import qualified System.IO as IO

-- -----------------------------------------------------------------------------
-- Strict hGetContents

-- | Computation 'hGetContents' @hdl@ returns the list of characters
-- corresponding to the unread portion of the channel or file managed
-- by @hdl@, which is immediate closed.
--
-- Items are read strictly from the input Handle.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.

hGetContents    :: IO.Handle -> IO.IO String
hGetContents h  = IO.hGetContents h >>= \s -> length s `seq` return s

-- -----------------------------------------------------------------------------
-- Standard IO

-- | The 'getContents' operation returns all user input as a single string,
-- which is read stirctly (same as 'hGetContents' 'stdin').

getContents     :: IO String
getContents     =  hGetContents IO.stdin
{-# INLINE getContents #-}

-- | The 'interact' function takes a function of type @String->String@
-- as its argument.  The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string is
-- output on the standard output device.

interact        ::  (String -> String) -> IO ()
interact f      =   IO.putStr . f =<< getContents
{-# INLINE interact #-}

-- | The 'readFile' function reads a file and
-- returns the contents of the file as a string.
-- The file is read strictly, as with 'getContents'.

readFile        :: FilePath -> IO String
readFile name   =  IO.openFile name IO.ReadMode >>= hGetContents
{-# INLINE readFile #-}
