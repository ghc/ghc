-----------------------------------------------------------------------------
-- $Id: DriverUtil.hs,v 1.19 2001/03/08 09:50:18 simonmar Exp $
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

import IO
import System
import List
import Char
import Monad

-----------------------------------------------------------------------------
-- Errors

GLOBAL_VAR(v_Path_usage,  "",  String)

long_usage = do
  usage_path <- readIORef v_Path_usage
  usage <- readFile usage_path
  dump usage
  exitWith ExitSuccess
  where
     dump "" = return ()
     dump ('$':'$':s) = hPutStr stderr progName >> dump s
     dump (c:s) = hPutChar stderr c >> dump s

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
my_prefix_match [] rest = Just rest
my_prefix_match (_:_) [] = Nothing
my_prefix_match (p:pat) (r:rest)
  | p == r    = my_prefix_match pat rest
  | otherwise = Nothing

later = flip finally

handleDyn :: Typeable ex => (ex -> IO a) -> IO a -> IO a
handleDyn = flip catchDyn

handle :: (Exception -> IO a) -> IO a -> IO a
handle = flip Exception.catchAllIO

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

splitFilename :: String -> (String,String)
splitFilename f = split_longest_prefix f '.'

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", ".ext")
splitFilename3 :: String -> (String,String,String)
splitFilename3 str
   = let (dir, rest) = split_longest_prefix str '/'
	 (name, ext) = splitFilename rest
	 real_dir | null dir  = "."
		  | otherwise = dir
     in  (real_dir, name, ext)

remove_suffix :: Char -> String -> String
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

newsuf :: String -> String -> String
newsuf suf s = remove_suffix '.' s ++ suf

-- getdir strips the filename off the input string, returning the directory.
getdir :: String -> String
getdir s = if null dir then "." else init dir
  where dir = take_longest_prefix s '/'

newdir :: String -> String -> String
newdir dir s = dir ++ '/':drop_longest_prefix s '/'

remove_spaces :: String -> String
remove_spaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

