-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Posix
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface to the POSIX regular expression library.
--
-----------------------------------------------------------------------------

-- ToDo: should have an interface using PackedStrings.
#ifndef __NHC__
#include "HsBaseConfig.h"
#else
#define HAVE_REGEX_H 1
#define HAVE_REGCOMP 1
#endif

module Text.Regex.Posix (
	-- * The @Regex@ type
	Regex,	 	-- abstract

	-- * Compiling a regular expression
	regcomp, 	-- :: String -> Int -> IO Regex

	-- ** Flags for regcomp
	regExtended,	-- (flag to regcomp) use extended regex syntax
	regIgnoreCase,	-- (flag to regcomp) ignore case when matching
	regNewline,	-- (flag to regcomp) '.' doesn't match newline

	-- * Matching a regular expression
	regexec, 	-- :: Regex		     -- pattern
	         	-- -> String		     -- string to match
	         	-- -> IO (Maybe (String,     -- everything before match
	         	-- 	 	 String,     -- matched portion
	         	--		 String,     -- everything after match
	         	-- 	 	 [String]))  -- subexpression matches

  ) where

import Prelude

import Foreign
import Foreign.C

type CRegex    = ()

-- | A compiled regular expression
newtype Regex = Regex (ForeignPtr CRegex)


-- The C-library backend
#include <sys/types.h>

#if HAVE_REGEX_H && HAVE_REGCOMP
#include "regex.h"
#else
#include "regex/regex.h"

-- CFILES stuff is Hugs only
{-# CFILES cbits/regex/reallocf.c #-}
{-# CFILES cbits/regex/regcomp.c #-}
{-# CFILES cbits/regex/regerror.c #-}
{-# CFILES cbits/regex/regexec.c #-}
{-# CFILES cbits/regex/regfree.c #-}
#endif

-- -----------------------------------------------------------------------------
-- regcomp

-- | Compiles a regular expression
regcomp
  :: String  	-- ^ The regular expression to compile
  -> Int    	-- ^ Flags (summed together)
  -> IO Regex  	-- ^ Returns: the compiled regular expression
regcomp pattern flags = do
  regex_fptr <- mallocForeignPtrBytes (#const sizeof(regex_t))
  r <- withCString pattern $ \cstr ->
    	 withForeignPtr regex_fptr $ \p ->
           c_regcomp p cstr (fromIntegral flags)
  if (r == 0)
     then do addForeignPtrFinalizer ptr_regfree regex_fptr
	     return (Regex regex_fptr)
     else error "Text.Regex.Posix.regcomp: error in pattern" -- ToDo

-- -----------------------------------------------------------------------------
-- regexec

-- | Matches a regular expression against a string
regexec :: Regex			-- ^ Compiled regular expression
	-> String			-- ^ String to match against
	-> IO (Maybe (String, String, String, [String]))
	 	-- ^ Returns: 'Nothing' if the regex did not match the
		-- string, or:
		--
		-- @
		--   'Just' (everything before match,
		--         matched portion,
		--         everything after match,
		--         subexpression matches)
		-- @

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
  start <- (#peek regmatch_t, rm_so) p_match :: IO (#type regoff_t)
  end   <- (#peek regmatch_t, rm_eo) p_match :: IO (#type regoff_t)
  let s = fromIntegral start; e = fromIntegral end
  return ( take s string, 
	   take (e-s) (drop s string),
	   drop e string )  

unpack :: String -> Ptr CRegMatch -> IO (String)
unpack string p_match = do
  start <- (#peek regmatch_t, rm_so) p_match :: IO (#type regoff_t)
  end   <- (#peek regmatch_t, rm_eo) p_match :: IO (#type regoff_t)
  -- the subexpression may not have matched at all, perhaps because it
  -- was optional.  In this case, the offsets are set to -1.
  if (start == -1) then return "" else do
    return (take (fromIntegral (end-start)) (drop (fromIntegral start) string))

-- -----------------------------------------------------------------------------
-- The POSIX regex C interface

-- Flags for regexec
#enum Int,, \
	REG_NOTBOL, \
	REG_NOTEOL

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

type CRegMatch = ()

-- GHC and Hugs get the appropriate include file from the OPTIONS
-- pragma generated by hsc2hs from the above #include.
-- Implementations following the FFI spec have to specify it in the
-- foreign import, which is awkward because some systems provide
-- regex.h and the rest of the regex library, but otherwise we
-- need to use our own copy, regex/regex.h.

#if __GLASGOW_HASKELL__ || __HUGS__
foreign import ccall unsafe "regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt

foreign import ccall  unsafe "&regfree"
  ptr_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
	    -> Ptr CRegMatch -> CInt -> IO CInt
#elif HAVE_REGEX_H && HAVE_REGCOMP
foreign import ccall unsafe "regex.h regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt

foreign import ccall  unsafe "regex.h &regfree"
  ptr_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regex.h regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
	    -> Ptr CRegMatch -> CInt -> IO CInt
#else
foreign import ccall unsafe "regex/regex.h regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt

foreign import ccall  unsafe "regex/regex.h &regfree"
  ptr_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regex/regex.h regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
	    -> Ptr CRegMatch -> CInt -> IO CInt
#endif
