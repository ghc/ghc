{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory.Internals
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  hidden
-- Portability :  portable
--
-- System-independent pathname manipulations.
--
-----------------------------------------------------------------------------

-- #hide
module System.Directory.Internals (
        joinFileName,
	joinFileExt,
	parseSearchPath,
	pathParents,
	exeExtension,
  ) where

#if __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase (FilePath)
#endif
import Data.List

-- | The 'joinFileName' function is the opposite of 'splitFileName'. 
-- It joins directory and file names to form a complete file path.
--
-- The general rule is:
--
-- > dir `joinFileName` basename == path
-- >   where
-- >     (dir,basename) = splitFileName path
--
-- There might be an exceptions to the rule but in any case the
-- reconstructed path will refer to the same object (file or directory).
-- An example exception is that on Windows some slashes might be converted
-- to backslashes.
joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir++fname
  | otherwise                  = dir++pathSeparator:fname

-- | The 'joinFileExt' function is the opposite of 'splitFileExt'.
-- It joins a file name and an extension to form a complete file path.
--
-- The general rule is:
--
-- > filename `joinFileExt` ext == path
-- >   where
-- >     (filename,ext) = splitFileExt path
joinFileExt :: String -> String -> FilePath
joinFileExt path ""  = path
joinFileExt path ext = path ++ '.':ext

-- | Gets this path and all its parents.
-- The function is useful in case if you want to create 
-- some file but you aren\'t sure whether all directories 
-- in the path exist or if you want to search upward for some file.
-- 
-- Some examples:
--
-- \[Posix\]
--
-- >  pathParents "/"          == ["/"]
-- >  pathParents "/dir1"      == ["/", "/dir1"]
-- >  pathParents "/dir1/dir2" == ["/", "/dir1", "/dir1/dir2"]
-- >  pathParents "dir1"       == [".", "dir1"]
-- >  pathParents "dir1/dir2"  == [".", "dir1", "dir1/dir2"]
--
-- \[Windows\]
--
-- >  pathParents "c:"             == ["c:."]
-- >  pathParents "c:\\"           == ["c:\\"]
-- >  pathParents "c:\\dir1"       == ["c:\\", "c:\\dir1"]
-- >  pathParents "c:\\dir1\\dir2" == ["c:\\", "c:\\dir1", "c:\\dir1\\dir2"]
-- >  pathParents "c:dir1"         == ["c:.","c:dir1"]
-- >  pathParents "dir1\\dir2"     == [".", "dir1", "dir1\\dir2"]
--
-- Note that if the file is relative then the current directory (\".\") 
-- will be explicitly listed.
pathParents :: FilePath -> [FilePath]
pathParents p =
    root'' : map ((++) root') (dropEmptyPath $ inits path')
    where
#ifdef mingw32_HOST_OS
       (root,path) = case break (== ':') p of
          (path,    "") -> ("",path)
          (root,_:path) -> (root++":",path)
#else
       (root,path) = ("",p)
#endif
       (root',root'',path') = case path of
         (c:path) | isPathSeparator c -> (root++[pathSeparator],root++[pathSeparator],path)
         _                            -> (root                 ,root++"."            ,path)

       dropEmptyPath ("":paths) = paths
       dropEmptyPath paths      = paths

       inits :: String -> [String]
       inits [] =  [""]
       inits cs = 
         case pre of
           "."  -> inits suf
           ".." -> map (joinFileName pre) (dropEmptyPath $ inits suf)
           _    -> "" : map (joinFileName pre) (inits suf)
         where
           (pre,suf) = case break isPathSeparator cs of
              (pre,"")    -> (pre, "")
              (pre,_:suf) -> (pre,suf)

--------------------------------------------------------------
-- * Search path
--------------------------------------------------------------

-- | The function splits the given string to substrings
-- using the 'searchPathSeparator'.
parseSearchPath :: String -> [FilePath]
parseSearchPath path = split searchPathSeparator path
  where
    split :: Char -> String -> [String]
    split c s =
      case rest of
        []      -> [chunk] 
        _:rest' -> chunk : split c rest'
      where
        (chunk, rest) = break (==c) s

--------------------------------------------------------------
-- * Separators
--------------------------------------------------------------

-- | Checks whether the character is a valid path separator for the host
-- platform. The valid character is a 'pathSeparator' but since the Windows
-- operating system also accepts a slash (\"\/\") since DOS 2, the function
-- checks for it on this platform, too.
isPathSeparator :: Char -> Bool
isPathSeparator ch = ch == pathSeparator || ch == '/'

-- | Provides a platform-specific character used to separate directory levels in
-- a path string that reflects a hierarchical file system organization. The
-- separator is a slash (@\"\/\"@) on Unix and Macintosh, and a backslash
-- (@\"\\\"@) on the Windows operating system.
pathSeparator :: Char
#ifdef mingw32_HOST_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif

-- ToDo: This should be determined via autoconf (PATH_SEPARATOR)
-- | A platform-specific character used to separate search path strings in
-- environment variables. The separator is a colon (@\":\"@) on Unix and
-- Macintosh, and a semicolon (@\";\"@) on the Windows operating system.
searchPathSeparator :: Char
#ifdef mingw32_HOST_OS
searchPathSeparator = ';'
#else
searchPathSeparator = ':'
#endif

-- ToDo: This should be determined via autoconf (AC_EXEEXT)
-- | Extension for executable files
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: String
#ifdef mingw32_HOST_OS
exeExtension = "exe"
#else
exeExtension = ""
#endif

