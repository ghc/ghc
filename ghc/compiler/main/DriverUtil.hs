-----------------------------------------------------------------------------
-- $Id: DriverUtil.hs,v 1.24 2001/06/14 12:50:06 simonpj Exp $
--
-- Utils for the driver
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module DriverUtil where

#include "../includes/config.h"
#include "HsVersions.h"

import Util
import Panic

import IOExts
import Exception
import Dynamic
import RegexString

import Directory	( getDirectoryContents )
import IO
import List
import Char
import Monad


-----------------------------------------------------------------------------
-- Errors

-----------------------------------------------------------------------------
-- Reading OPTIONS pragmas

getOptionsFromSource 
	:: String		-- input file
	-> IO [String]		-- options, if any
getOptionsFromSource file
  = do h <- openFile file ReadMode
       catchJust ioErrors (look h)
	  (\e -> if isEOFError e then return [] else ioError e)
  where
	look h = do
	    l <- hGetLine h
	    case () of
		() | null l -> look h
		   | prefixMatch "#" l -> look h
		   | prefixMatch "{-# LINE" l -> look h   -- -}
		   | Just (opts:_) <- matchRegex optionRegex l
			-> do rest <- look h
                              return (words opts ++ rest)
		   | otherwise -> return []

optionRegex = mkRegex "\\{-#[ \t]+OPTIONS[ \t]+(.*)#-\\}"   -- -}

-----------------------------------------------------------------------------
-- A version of getDirectoryContents that is non-fatal if the
-- directory doesn't exist.

softGetDirectoryContents d
   = IO.catch (getDirectoryContents d)
	  (\_ -> do hPutStr stderr 
		          ("WARNING: error while reading directory " ++ d)
		    return []
	  )

-----------------------------------------------------------------------------
-- Utils

unknownFlagErr :: String -> a
unknownFlagErr f = throwDyn (UsageError ("unrecognised flag: " ++ f))

my_partition :: (a -> Maybe b) -> [a] -> ([(a,b)],[a])
my_partition _ [] = ([],[])
my_partition p (a:as)
  = let (bs,cs) = my_partition p as in
    case p a of
	Nothing -> (bs,a:cs)
	Just b  -> ((a,b):bs,cs)

my_prefix_match :: String -> String -> Maybe String
my_prefix_match []    rest = Just rest
my_prefix_match (_:_) []   = Nothing
my_prefix_match (p:pat) (r:rest)
  | p == r    = my_prefix_match pat rest
  | otherwise = Nothing

later = flip finally

handleDyn :: Typeable ex => (ex -> IO a) -> IO a -> IO a
handleDyn = flip catchDyn

handle :: (Exception -> IO a) -> IO a -> IO a
#if __GLASGOW_HASKELL__ < 501
handle = flip Exception.catchAllIO
#else
handle h f = f `Exception.catch` \e -> case e of
    ExitException _ -> throw e
    _               -> h e
#endif

split :: Char -> String -> [String]
split c s = case rest of
		[]     -> [chunk] 
		_:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

add :: IORef [a] -> a -> IO ()
add var x = do
  xs <- readIORef var
  writeIORef var (x:xs)

addNoDups :: Eq a => IORef [a] -> a -> IO ()
addNoDups var x = do
  xs <- readIORef var
  unless (x `elem` xs) $ writeIORef var (x:xs)

------------------------------------------------------
--		Filename manipulation
------------------------------------------------------
		
type Suffix = String

splitFilename :: String -> (String,Suffix)
splitFilename f = split_longest_prefix f '.'

getFileSuffix :: String -> Suffix
getFileSuffix f = drop_longest_prefix f '.'

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", ".ext")
splitFilename3 :: String -> (String,String,Suffix)
splitFilename3 str
   = let (dir, rest) = split_longest_prefix str '/'
	 (name, ext) = splitFilename rest
	 real_dir | null dir  = "."
		  | otherwise = dir
     in  (real_dir, name, ext)

remove_suffix :: Char -> String -> Suffix
remove_suffix c s
  | null pre  = reverse suf
  | otherwise = reverse pre
  where (suf,pre) = break (==c) (reverse s)

drop_longest_prefix :: String -> Char -> String
drop_longest_prefix s c = reverse suf
  where (suf,_pre) = break (==c) (reverse s)

take_longest_prefix :: String -> Char -> String
take_longest_prefix s c = reverse pre
  where (_suf,pre) = break (==c) (reverse s)

-- split a string at the last occurence of 'c', returning the two
-- parts of the string with the 'c' removed.  If the string contains
-- no 'c's, the entire string is returned in the second component.
split_longest_prefix :: String -> Char -> (String,String)
split_longest_prefix s c
  = case pre of
	[]      -> ([], reverse suf)
	(_:pre) -> (reverse pre, reverse suf)
  where (suf,pre) = break (==c) (reverse s)

newsuf :: String -> Suffix -> String
newsuf suf s = remove_suffix '.' s ++ suf

-- getdir strips the filename off the input string, returning the directory.
getdir :: String -> String
getdir s = if null dir then "." else init dir
  where dir = take_longest_prefix s '/'

newdir :: String -> String -> String
newdir dir s = dir ++ '/':drop_longest_prefix s '/'

remove_spaces :: String -> String
remove_spaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace


