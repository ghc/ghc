-----------------------------------------------------------------------------
-- 
-- Module      :  System.Environment
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Environment.hs,v 1.1 2001/06/28 14:15:04 simonmar Exp $
--
-- Miscellaneous information about the system environment.
--
-----------------------------------------------------------------------------

module System.Environment
    ( 
    , getArgs	    -- :: IO [String]
    , getProgName   -- :: IO String
    , getEnv        -- :: String -> IO String
  ) where

import Prelude

import Foreign
import Foreign.C

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
#endif

-- ---------------------------------------------------------------------------
-- getArgs, getProgName, getEnv

-- Computation `getArgs' returns a list of the program's command
-- line arguments (not including the program name).

getArgs :: IO [String]
getArgs = do
  argv <- peek prog_argv_label
  argc <- peek prog_argc_label
  peekArray (fromIntegral argc - 1) (advancePtr argv 1) >>= mapM peekCString

foreign label "prog_argv" prog_argv_label :: Ptr (Ptr (Ptr CChar))
foreign label "prog_argc" prog_argc_label :: Ptr CInt

-- Computation `getProgName' returns the name of the program
-- as it was invoked.

getProgName :: IO String
getProgName = do
  argv <- peek prog_argv_label
  unpackProgName argv

unpackProgName	:: Ptr (Ptr CChar) -> IO String   -- argv[0]
unpackProgName argv = do 
  s <- peekElemOff argv 0 >>= peekCString
  return (de_slash "" s)
  where
    -- re-start accumulating at every '/'
    de_slash :: String -> String -> String
    de_slash  acc []	   = reverse acc
    de_slash _acc ('/':xs) = de_slash []      xs
    de_slash  acc (x:xs)   = de_slash (x:acc) xs

-- Computation `getEnv var' returns the value
-- of the environment variable {\em var}.  

-- This computation may fail with
--    NoSuchThing: The environment variable does not exist.

getEnv :: String -> IO String
getEnv name =
    withUnsafeCString name $ \s -> do
      litstring <- c_getenv s
      if litstring /= nullPtr
	then peekCString litstring
        else ioException (IOError Nothing NoSuchThing "getEnv"
			  "no environment variable" (Just name))

foreign import ccall "getenv" unsafe 
   c_getenv :: UnsafeCString -> IO (Ptr CChar)
