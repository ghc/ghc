-----------------------------------------------------------------------------
-- 
-- Module      :  Text.Regex.Posix
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (only on platforms that provide POSIX regexps)
--
-- $Id: Posix.hsc,v 1.5 2002/02/14 14:08:50 simonmar Exp $
--
-- Interface to the POSIX regular expression library.
-- ToDo: should have an interface using PackedStrings.
--
-----------------------------------------------------------------------------

module Text.Regex.Posix (
	Regex,	 	-- abstract

	regcomp, 	-- :: String -> Int -> IO Regex

	regexec, 	-- :: Regex		     -- pattern
	         	-- -> String		     -- string to match
	         	-- -> IO (Maybe (String,     -- everything before match
	         	-- 	 	 String,     -- matched portion
	         	--		 String,     -- everything after match
	         	-- 	 	 [String]))  -- subexpression matches

	regExtended,	-- (flag to regcomp) use extended regex syntax
	regIgnoreCase,	-- (flag to regcomp) ignore case when matching
	regNewline	-- (flag to regcomp) '.' doesn't match newline
  ) where

#include "regex.h"

import Prelude

import Foreign
import Foreign.C

newtype Regex = Regex (ForeignPtr CRegex)

-- -----------------------------------------------------------------------------
-- regcomp

regcomp :: String -> Int -> IO Regex
regcomp pattern flags = do
  regex_ptr <- mallocBytes (#const sizeof(regex_t))
  regex_fptr <- newForeignPtr regex_ptr (regfree regex_ptr)
  r <- withCString pattern $ \cstr ->
    	 withForeignPtr regex_fptr $ \p ->
           c_regcomp p cstr (fromIntegral flags)
  if (r == 0)
     then return (Regex regex_fptr)
     else error "Text.Regex.Posix.regcomp: error in pattern" -- ToDo

regfree :: Ptr CRegex -> IO ()
regfree p_regex = do
  c_regfree p_regex
  free p_regex

-- -----------------------------------------------------------------------------
-- regexec

regexec :: Regex			-- pattern
	-> String			-- string to match
	-> IO (Maybe (String,		-- everything before match
		      String,		-- matched portion
		      String,		-- everything after match
		      [String])) 	-- subexpression matches

regexec (Regex regex_fptr) str = do
  withCString str $ \cstr -> do
    withForeignPtr regex_fptr $ \regex_ptr -> do
      nsub <- (#peek regex_t, re_nsub) regex_ptr
      let nsub_int = fromIntegral (nsub :: CSize)
      allocaBytes ((1 + nsub_int) * (#const sizeof(regmatch_t))) $ \p_match -> do
		-- add one because index zero covers the whole match
        r <- c_regexec regex_ptr cstr (1 + nsub) p_match 0{-no flags for now-}

        if (r /= 0) then return Nothing else do 

        (before,match,after) <- matched_parts str p_match

        sub_strs <- 
	  mapM (unpack str) $ take nsub_int $ tail $
	     iterate (`plusPtr` (#const sizeof(regmatch_t))) p_match

        return (Just (before, match, after, sub_strs))

matched_parts :: String -> Ptr CRegMatch -> IO (String, String, String)
matched_parts string p_match = do
  start <- (#peek regmatch_t, rm_so) p_match :: IO CInt
  end   <- (#peek regmatch_t, rm_eo) p_match :: IO CInt
  let s = fromIntegral start; e = fromIntegral end
  return ( take (s-1) string, 
	   take (e-s) (drop s string),
	   drop e string )  

unpack :: String -> Ptr CRegMatch -> IO (String)
unpack string p_match = do
  start <- (#peek regmatch_t, rm_so) p_match :: IO CInt
  end   <- (#peek regmatch_t, rm_eo) p_match :: IO CInt
  -- the subexpression may not have matched at all, perhaps because it
  -- was optional.  In this case, the offsets are set to -1.
  if (start == -1) then return "" else do
  return (take (fromIntegral (end-start)) (drop (fromIntegral start) string))

-- -----------------------------------------------------------------------------
-- The POSIX regex C interface

-- Flags for regexec
#enum Int,, \
	REG_NOTBOL, \
	REG_NOTEOL \

-- Return values from regexec
#enum Int,, \
	REG_NOMATCH
--	REG_ESPACE

-- Flags for regcomp
#enum Int,, \
	REG_EXTENDED, \
	regIgnoreCase = REG_ICASE, \
	REG_NOSUB, \
	REG_NEWLINE

-- Error codes from regcomp
#enum Int,, \
	REG_BADBR, \
	REG_BADPAT, \
	REG_BADRPT, \
	REG_ECOLLATE, \
	REG_ECTYPE, \
	REG_EESCAPE, \
	REG_ESUBREG, \
	REG_EBRACK, \
	REG_EPAREN, \
	REG_EBRACE, \
	REG_ERANGE, \
	REG_ESPACE

type CRegex    = ()
type CRegMatch = ()

foreign import ccall unsafe "regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt

foreign import ccall  unsafe "regfree"
  c_regfree :: Ptr CRegex -> IO ()

foreign import ccall unsafe "regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
	    -> Ptr CRegMatch -> CInt -> IO CInt
