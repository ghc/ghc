{-# OPTIONS -cpp -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- $Id: KludgedSystem.hs,v 1.2 2001/02/13 15:53:10 qrczak Exp $

-- system that works feasibly under Windows (i.e. passes the command line to sh,
-- because system() under Windows doesn't look at SHELL, and always uses CMD.EXE)

module KludgedSystem (system, defaultCompiler) where

#include "../../includes/config.h"

#ifndef mingw32_TARGET_OS

import System (system)

defaultCompiler :: String
defaultCompiler = "gcc"

#else

import qualified System
import System    (ExitCode)
import IO        (bracket_)
import Directory (removeFile)

system :: String -> IO ExitCode
system cmd = do
    pid <- getProcessID
    let tmp = "/tmp/sh"++show pid
    writeFile tmp (cmd++"\n")
    bracket_ (return tmp) removeFile $ System.system ("sh - "++tmp)

foreign import "_getpid" unsafe getProcessID :: IO Int

defaultCompiler :: String
defaultCompiler = "gcc -mno-cygwin"

#endif /* mingw32_TARGET_OS */
