{-# OPTIONS -cpp -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- $Id: KludgedSystem.hs,v 1.1 2001/08/30 17:22:13 rrt Exp $

-- system that works feasibly under Windows (i.e. passes the command line to sh,
-- because system() under Windows doesn't look at SHELL, and always uses CMD.EXE)

module KludgedSystem (system) where

#include "../../ghc/includes/config.h"

#ifndef mingw32_TARGET_OS

import System (system)

#else

import qualified System
import System    (ExitCode)
import IO        (bracket_)
import Directory (removeFile)

system :: String -> IO ExitCode
system cmd = do
    pid <- getProcessID
    let tmp = "sh"++show pid
    writeFile tmp (cmd++"\n")
    bracket_ (return tmp) removeFile $ System.system ("sh - "++tmp)

foreign import "_getpid" unsafe getProcessID :: IO Int

#endif /* mingw32_TARGET_OS */
