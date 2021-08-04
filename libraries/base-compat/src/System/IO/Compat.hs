{-# LANGUAGE CPP, NoImplicitPrelude #-}
module System.IO.Compat (
  module Base
, getContents'
, hGetContents'
, readFile'
) where

import System.IO as Base

#if !(MIN_VERSION_base(4,15,0))
import Prelude.Compat

-- | The 'getContents'' operation returns all user input as a single string,
-- which is fully read before being returned
-- (same as 'hGetContents'' 'stdin').
--
-- /Since: 4.15.0.0/

getContents'    :: IO String
getContents'    =  hGetContents' stdin

-- | The 'readFile'' function reads a file and
-- returns the contents of the file as a string.
-- The file is fully read before being returned, as with 'getContents''.
--
-- /Since: 4.15.0.0/

readFile'       :: FilePath -> IO String
readFile' name  =  openFile name ReadMode >>= hGetContents'

-- | The 'hGetContents'' operation reads all input on the given handle
-- before returning it as a 'String' and closing the handle.
--
-- /Since: 4.15.0.0/

hGetContents'   :: Handle -> IO String
hGetContents' h =  hGetContents h >>= \s -> length s `seq` return s
 -- NB: The actual implementation of hGetContents' in `base` uses a lot of
 -- low-level code from GHC.IO.Handle.Text. What's worse, a lot of this
 -- low-level code isn't exported, so we'd have to reimplement large chunks
 -- of it in base-compat if we wanted to backport it. For now, I've opted for
 -- the simpler approach of simply defining hGetContents' in terms of
 -- hGetContents, which is the approach that the `extra` and `strict` libraries
 -- use. (Indeed, the code above is taken from `strict`.)
#endif
