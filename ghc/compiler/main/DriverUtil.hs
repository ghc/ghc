-----------------------------------------------------------------------------
-- $Id: DriverUtil.hs,v 1.2 2000/10/11 14:08:52 simonmar Exp $
--
-- Utils for the driver
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module DriverUtil where

#include "HsVersions.h"

import Config
import Util

import IOExts
import Exception
import Dynamic
import RegexString

import IO
import System
import Directory
import List
import Char
import Monad

-----------------------------------------------------------------------------
-- Errors

short_usage = "Usage: For basic information, try the `--help' option."
   
GLOBAL_VAR(path_usage,  "",  String)

long_usage = do
  usage_path <- readIORef path_usage
  usage <- readFile usage_path
  dump usage
  exitWith ExitSuccess
  where
     dump "" = return ()
     dump ('$':'$':s) = hPutStr stderr get_prog_name >> dump s
     dump (c:s) = hPutChar stderr c >> dump s

version_str = cProjectVersion

data BarfKind
  = PhaseFailed String ExitCode
  | Interrupted
  | UsageError String			-- prints the short usage msg after the error
  | OtherError String			-- just prints the error message
  deriving Eq

GLOBAL_VAR(prog_name, "ghc", String)

get_prog_name = unsafePerformIO (readIORef prog_name) -- urk!

instance Show BarfKind where
  showsPrec _ e = showString get_prog_name . showString ": " . showBarf e

showBarf (UsageError str) = showString str . showChar '\n' . showString short_usage
showBarf (OtherError str) = showString str
showBarf (PhaseFailed phase code) = 
	showString phase . showString " failed, code = " . shows code
showBarf (Interrupted) = showString "interrupted"

unknownFlagErr f = throwDyn (UsageError ("unrecognised flag: " ++ f))

barfKindTc = mkTyCon "BarfKind"
instance Typeable BarfKind where
  typeOf _ = mkAppTy barfKindTc []

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
			-> return (words opts)
		   | otherwise -> return []

optionRegex = mkRegex "\\{-#[ \t]+OPTIONS[ \t]+(.*)#-\\}"   -- -}

-----------------------------------------------------------------------------
-- Utils

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

booter_version
 = case "\ 
	\ __GLASGOW_HASKELL__" of
    ' ':n:ns -> n:'.':ns
    ' ':m    -> m

