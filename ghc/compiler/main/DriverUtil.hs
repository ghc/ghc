-----------------------------------------------------------------------------
-- $Id: DriverUtil.hs,v 1.31 2002/02/27 16:24:00 simonmar Exp $
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
import Config		( cLeadingUnderscore )

import IOExts
import Exception
import Dynamic

import Directory	( getDirectoryContents )
import IO
import List
import Char
import Monad

-----------------------------------------------------------------------------
-- Reading OPTIONS pragmas

getOptionsFromSource 
	:: String		-- input file
	-> IO [String]		-- options, if any
getOptionsFromSource file
  = do h <- openFile file ReadMode
       catchJust ioErrors (look h `finally` hClose h)
	  (\e -> if isEOFError e then return [] else ioError e)
  where
	look h = do
	    l' <- hGetLine h
	    let l = remove_spaces l'
	    case () of
		() | null l -> look h
		   | prefixMatch "#" l -> look h
		   | prefixMatch "{-# LINE" l -> look h   -- -}
		   | Just opts <- matchOptions l
			-> do rest <- look h
                              return (words opts ++ rest)
		   | otherwise -> return []

matchOptions s
  | Just s1 <- my_prefix_match "{-#" s,
    Just s2 <- my_prefix_match "OPTIONS" (remove_spaces s1),
    Just s3 <- my_prefix_match "}-#" (reverse s2)
  = Just (reverse s3)
  | otherwise
  = Nothing

-----------------------------------------------------------------------------
-- A version of getDirectoryContents that is non-fatal if the
-- directory doesn't exist.

softGetDirectoryContents d
   = IO.catch (getDirectoryContents d)
	  (\_ -> do hPutStrLn stderr 
		          ("WARNING: error while reading directory " ++ d)
		    return []
	  )

-----------------------------------------------------------------------------
-- Prefixing underscore to linker-level names
prefixUnderscore :: String -> String
prefixUnderscore
 | cLeadingUnderscore == "YES" = ('_':)
 | otherwise                   = id

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
splitFilename f = split_longest_prefix f (=='.')

getFileSuffix :: String -> Suffix
getFileSuffix f = drop_longest_prefix f (=='.')

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", ".ext")
splitFilename3 :: String -> (String,String,Suffix)
splitFilename3 str
   = let (dir, rest) = split_longest_prefix str isPathSeparator
	 (name, ext) = splitFilename rest
	 real_dir | null dir  = "."
		  | otherwise = dir
     in  (real_dir, name, ext)

remove_suffix :: Char -> String -> Suffix
remove_suffix c s
  | null pre  = reverse suf
  | otherwise = reverse pre
  where (suf,pre) = break (==c) (reverse s)

drop_longest_prefix :: String -> (Char -> Bool) -> String
drop_longest_prefix s pred = reverse suf
  where (suf,_pre) = break pred (reverse s)

take_longest_prefix :: String -> (Char -> Bool) -> String
take_longest_prefix s pred = reverse pre
  where (_suf,pre) = break pred (reverse s)

-- split a string at the last character where 'pred' is True,
-- returning a pair of strings. The first component holds the string
-- up (but not including) the last character for which 'pred' returned
-- True, the second whatever comes after (but also not including the
-- last character).
--
-- If 'pred' returns False for all characters in the string, the original
-- string is returned in the second component (and the first one is just
-- empty).
split_longest_prefix :: String -> (Char -> Bool) -> (String,String)
split_longest_prefix s pred
  = case pre of
	[]      -> ([], reverse suf)
	(_:pre) -> (reverse pre, reverse suf)
  where (suf,pre) = break pred (reverse s)

newsuf :: String -> Suffix -> String
newsuf suf s = remove_suffix '.' s ++ suf

-- getdir strips the filename off the input string, returning the directory.
getdir :: String -> String
getdir s = if null dir then "." else init dir
  where dir = take_longest_prefix s isPathSeparator

newdir :: String -> String -> String
newdir dir s = dir ++ '/':drop_longest_prefix s isPathSeparator

remove_spaces :: String -> String
remove_spaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

escapeSpaces :: String -> String
escapeSpaces = foldr (\c s -> if isSpace c then '\\':c:s else c:s) ""

isPathSeparator :: Char -> Bool
isPathSeparator ch =
#ifdef mingw32_TARGET_OS
  ch == '/' || ch == '\\'
#else
  ch == '/'
#endif
