{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- Package management tool
-----------------------------------------------------------------------------

module Main where

import Package

#if __GLASGOW_HASKELL__ >= 504
import System.Console.GetOpt
import Text.PrettyPrint
import qualified Control.Exception as Exception
#else
import GetOpt
import Pretty
import qualified Exception
#endif

import Monad
import Directory
import System	( getEnv, getArgs, getProgName,
		  system, exitWith,
		  ExitCode(..)
		)
import IO
import List ( isPrefixOf, isSuffixOf )

import ParsePkgConfLite

#include "../../includes/config.h"

#ifdef mingw32_HOST_OS
import Foreign

#if __GLASGOW_HASKELL__ >= 504
import Foreign.C.String
#else
import CString
#endif
#endif

main = do
  args <- getArgs

  case getOpt Permute flags args of
	(cli,_,[]) | DumpHelp `elem` cli -> do
	   prog <- getProgramName
	   bye (usageInfo (usageHeader prog) flags)
	(cli,_,[]) | DumpVersion `elem` cli ->
	   bye copyright
	(cli@(_:_),[],[]) ->
	   runit cli
	(_,_,errors) -> do
	   prog <- getProgramName
	   die (concat errors ++ usageInfo (usageHeader prog) flags)

data Flag 
  = Config FilePath
  | Input FilePath
  | List
  | ListLocal
  | Add Bool {- True => replace existing info -}
  | Remove String | Show String 
  | Field String | AutoGHCiLibs | Force
  | DefinedName String String
  | DumpHelp
  | DumpVersion
  deriving (Eq)

isAction (Config _)     = False
isAction (Field _)      = False
isAction (Input _)      = False
isAction (AutoGHCiLibs) = False
isAction (Force)	= False
isAction DefinedName{}  = False
isAction _              = True

copyright :: String
copyright = "GHC package manager version " ++ version ++ "\n"

-- hackery to convice cpp to splice GHC_PKG_VERSION into a string
version :: String
version = tail "\ 
   \ GHC_PKG_VERSION"

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [OPTION...]\n"

flags = [
  Option ['f'] ["config-file"] (ReqArg Config "FILE")
	"use the specified package config file",
  Option ['l'] ["list-packages"] (NoArg List)
 	"list packages in all config files",
  Option ['L'] ["list-local-packages"] (NoArg ListLocal)
 	"list packages in the specified config file",
  Option ['a'] ["add-package"] (NoArg (Add False))
 	"add a new package",
  Option ['u'] ["update-package"] (NoArg (Add True))
 	"update package with new configuration",
  Option ['i'] ["input-file"] (ReqArg Input "FILE")
	"read new package info from specified file",
  Option ['s'] ["show-package"] (ReqArg Show "NAME")
 	"show the configuration for package NAME",
  Option [] ["field"] (ReqArg Field "FIELD")
 	"(with --show-package) Show field FIELD only",
  Option [] ["force"] (NoArg Force)
 	"ignore missing directories/libraries",
  Option ['r'] ["remove-package"] (ReqArg Remove "NAME")
 	"remove an installed package",
  Option ['g'] ["auto-ghci-libs"] (NoArg AutoGHCiLibs)
	"automatically build libs for GHCi (with -a)",
  Option ['D'] ["define-name"] (ReqArg toDefined "NAME=VALUE")
  	"define NAME as VALUE",
   Option ['?'] ["help"] (NoArg DumpHelp)
	"display this help and exit",
   Option ['V'] ["version"] (NoArg DumpVersion)
	"output version information and exit"
  ]
 where
  toDefined str = 
    case break (=='=') str of
      (nm,[]) -> DefinedName nm []
      (nm,_:val) -> DefinedName nm val

runit clis = do
  let err_msg = "missing -f option, location of package.conf unknown\n"
  conf_filenames <- 
     case [ f | Config f <- clis ] of
        fs@(_:_) -> return (reverse fs) -- NOTE reverse
	[] -> do mb_dir <- getExecDir "/bin/ghc-pkg.exe"
		 case mb_dir of
			Nothing  -> die err_msg
			Just dir -> return [dir ++ "/package.conf"]

  let toField "import_dirs"     = return import_dirs
      toField "source_dirs"     = return source_dirs
      toField "library_dirs"    = return library_dirs
      toField "hs_libraries"    = return hs_libraries
      toField "extra_libraries" = return extra_libraries
      toField "include_dirs"    = return include_dirs
      toField "c_includes"      = return c_includes
      toField "package_deps"    = return package_deps
      toField "extra_ghc_opts"  = return extra_ghc_opts
      toField "extra_cc_opts"   = return extra_cc_opts
      toField "extra_ld_opts"   = return extra_ld_opts  
      toField "framework_dirs"  = return framework_dirs  
      toField "extra_frameworks"= return extra_frameworks  
      toField s			= die ("unknown field: `" ++ s ++ "'\n")

  fields <- mapM toField [ f | Field f <- clis ]

  let read_parse_conf filename = do
	  str <- readFile filename
	  let packages = parsePackageConfig str
	  eval_catch packages
	    (\_ -> die (filename ++ ": parse error in package config file\n"))

  pkg_confs <- mapM read_parse_conf conf_filenames

  let conf_filename = head conf_filenames
	-- this is the file we're going to update: the last one specified
	-- on the command-line.

  let auto_ghci_libs = any isAuto clis 
	 where isAuto AutoGHCiLibs = True; isAuto _ = False
      input_file = head ([ f | (Input f) <- clis] ++ ["-"])

      force = Force `elem` clis
      
      defines = [ (nm,val) | DefinedName nm val <- clis ]

  case [ c | c <- clis, isAction c ] of
    [ List ]      -> listPackages pkg_confs conf_filenames
    [ ListLocal ] -> listPackages [head pkg_confs] [""]
    [ Add upd ]  -> addPackage pkg_confs defines 
    			       conf_filename input_file
			       auto_ghci_libs upd force
    [ Remove p ] -> removePackage pkg_confs conf_filename p
    [ Show p ]   -> showPackage pkg_confs conf_filename p fields
    _            -> do prog <- getProgramName
		       die (usageInfo (usageHeader prog) flags)


listPackages :: [[PackageConfig]] -> [FilePath] -> IO ()
listPackages pkg_confs conf_filenames = do
  zipWithM_ show_pkgconf pkg_confs conf_filenames
  where show_pkgconf pkg_conf filename =
	  hPutStrLn stdout (render $
		if null filename 
			then packages	
			else text (filename ++ ":") $$ nest 4 packages
		)
	   where packages = fsep (punctuate comma (map (text . name) pkg_conf))

showPackage :: [[PackageConfig]]
	    -> FilePath
	    -> String
	    -> [PackageConfig -> [String]]
	    -> IO ()
showPackage pkg_confs filename pkg_name fields =
  case [ p | pkgs <- pkg_confs, p <- pkgs, name p == pkg_name ] of
    []    -> die ("can't find package `" ++ pkg_name ++ "'\n")
    [pkg] | null fields -> hPutStrLn stdout (render (dumpPkgGuts pkg))
	  | otherwise   -> hPutStrLn stdout (render (vcat 
				(map (vcat . map text) (map ($ pkg) fields))))
    _     -> die "showPackage: internal error\n"

addPackage :: [[PackageConfig]] -> [(String, String)] 
	   -> FilePath -> FilePath
 	   -> Bool -> Bool -> Bool -> IO ()
addPackage pkg_confs defines 
	   filename inputFile
	   auto_ghci_libs updatePkg force = do
  checkConfigAccess filename
  s <-
    case inputFile of
      "-" -> do
	hPutStr stdout "Reading package info from stdin... "
        getContents
      f   -> do
        hPutStr stdout ("Reading package info from " ++ show f)
	readFile f
  let new_pkg = parseOnePackageConfig s
  eval_catch new_pkg (\_ -> die "parse error in package info\n")
  hPutStrLn stdout "done."
  hPutStr stdout "Expanding embedded variables... "
  new_exp_pkg <- expandEnvVars new_pkg defines force
  hPutStrLn stdout "done."
  new_details <- validatePackageConfig new_exp_pkg pkg_confs 
			auto_ghci_libs updatePkg force
  savePackageConfig filename
  maybeRestoreOldConfig filename $
    writeNewConfig filename new_details

removePackage :: [[PackageConfig]] -> FilePath -> String -> IO ()
removePackage (packages : _) filename pkgName = do  
  checkConfigAccess filename
  when (pkgName `notElem` map name packages)
       (die (filename ++ ": package `" ++ pkgName ++ "' not found\n"))
  savePackageConfig filename
  maybeRestoreOldConfig filename $
    writeNewConfig filename (filter ((/= pkgName) . name) packages)

checkConfigAccess :: FilePath -> IO ()
checkConfigAccess filename = do
  access <- getPermissions filename
  when (not (writable access))
      (die (filename ++ ": you don't have permission to modify this file\n"))

maybeRestoreOldConfig :: FilePath -> IO () -> IO ()
maybeRestoreOldConfig filename io
  = my_catch io (\e -> do
        hPutStr stdout "\nWARNING: an error was encountered while the new \n\ 
        	       \configuration was being written.  Attempting to \n\ 
        	       \restore the old configuration... "
	renameFile (filename ++ ".old")  filename
        hPutStrLn stdout "done."
	my_throw e
    )

writeNewConfig :: FilePath -> [PackageConfig] -> IO ()
writeNewConfig filename packages = do
  hPutStr stdout "Writing new package config file... "
  h <- openFile filename WriteMode
  hPutStrLn h (dumpPackages packages)
  hClose h
  hPutStrLn stdout "done."

savePackageConfig :: FilePath -> IO ()
savePackageConfig filename = do
  hPutStr stdout "Saving old package config file... "
    -- mv rather than cp because we've already done an hGetContents
    -- on this file so we won't be able to open it for writing
    -- unless we move the old one out of the way...
  let oldFile = filename ++ ".old"
  doesExist <- doesFileExist oldFile  `catch` (\ _ -> return False)
  when doesExist (removeFile oldFile `catch` (const $ return ()))
  catch (renameFile filename oldFile)
  	(\ err -> do
		hPutStrLn stderr (unwords [ "Unable to rename "
					  , show filename
					  , " to "
					  , show oldFile
					  ])
		ioError err)
  hPutStrLn stdout "done."

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

validatePackageConfig :: PackageConfig 
		      -> [[PackageConfig]]
		      -> Bool
		      -> Bool
		      -> Bool
		      -> IO [PackageConfig]
validatePackageConfig pkg pkg_confs@(pkgs:_) auto_ghci_libs updatePkg force = do
  when (not updatePkg && (name pkg `elem` map name pkgs))
       (die ("package `" ++ name pkg ++ "' is already installed\n"))
  mapM_	(checkDep pkg_confs force) (package_deps pkg)
  mapM_	(checkDir force) (import_dirs pkg)
  mapM_	(checkDir force) (source_dirs pkg)
  mapM_	(checkDir force) (library_dirs pkg)
  mapM_	(checkDir force) (include_dirs pkg)
  mapM_ (checkHSLib (library_dirs pkg) auto_ghci_libs force) (hs_libraries pkg)
  -- ToDo: check these somehow?
  --	extra_libraries :: [String],
  --	c_includes      :: [String],
  let existing_pkgs
       | updatePkg = filter ((/=(name pkg)).name) pkgs  
       | otherwise = pkgs
  return (existing_pkgs ++ [pkg])

checkDir force d
 | "$libdir" `isPrefixOf` d = return ()
	-- can't check this, because we don't know what $libdir is
 | otherwise = do
   there <- doesDirectoryExist d
   when (not there)
       (dieOrForce force ("`" ++ d ++ "' doesn't exist or isn't a directory\n"))

checkDep :: [[PackageConfig]] -> Bool -> String -> IO ()
checkDep pkgs force n
  | n `elem` pkg_names = return ()
  | otherwise = dieOrForce force ("dependency `" ++ n ++ "' doesn't exist\n")
  where
    pkg_names = concat (map (map name) pkgs)

checkHSLib :: [String] -> Bool -> Bool -> String -> IO ()
checkHSLib dirs auto_ghci_libs force lib = do
  let batch_lib_file = "lib" ++ lib ++ ".a"
  bs <- mapM (doesLibExistIn batch_lib_file) dirs
  case [ dir | (exists,dir) <- zip bs dirs, exists ] of
	[] -> dieOrForce force ("cannot find `" ++ batch_lib_file ++
				 "' on library path") 
	(dir:_) -> checkGHCiLib dirs dir batch_lib_file lib auto_ghci_libs

doesLibExistIn lib d
 | "$libdir" `isPrefixOf` d = return True
 | otherwise                = doesFileExist (d ++ '/':lib)

checkGHCiLib :: [String] -> String -> String -> String -> Bool -> IO ()
checkGHCiLib dirs batch_lib_dir batch_lib_file lib auto_build = do
  let ghci_lib_file = lib ++ ".o"
      ghci_lib_path = batch_lib_dir ++ '/':ghci_lib_file
  bs <- mapM (\d -> doesFileExist (d ++ '/':ghci_lib_file)) dirs
  case [ dir | (exists,dir) <- zip bs dirs, exists ] of
        [] | auto_build -> 
		autoBuildGHCiLib batch_lib_dir batch_lib_file ghci_lib_file
	   | otherwise  -> 
		hPutStrLn stderr ("warning: can't find GHCi lib `"
					 ++ ghci_lib_file ++ "'")
   	(dir:_) -> return ()

-- automatically build the GHCi version of a batch lib, 
-- using ld --whole-archive.

autoBuildGHCiLib dir batch_file ghci_file = do
  let ghci_lib_file  = dir ++ '/':ghci_file
      batch_lib_file = dir ++ '/':batch_file
  hPutStr stderr ("building GHCi library `" ++ ghci_lib_file ++ "'...")
#ifdef darwin_TARGET_OS
  system("ld -r -x -o " ++ ghci_lib_file ++ 
	 " -all_load " ++ batch_lib_file)
#else
  system("ld -r -x -o " ++ ghci_lib_file ++ 
	 " --whole-archive " ++ batch_lib_file)
#endif
  hPutStrLn stderr (" done.")

-----------------------------------------------------------------------------
expandEnvVars :: PackageConfig -> [(String, String)] -> Bool -> IO PackageConfig
expandEnvVars pkg defines force = do
   -- permit _all_ strings to contain ${..} environment variable references,
   -- arguably too flexible.
  nm       <- expandString (name pkg)
  imp_dirs <- expandStrings (import_dirs pkg) 
  src_dirs <- expandStrings (source_dirs pkg) 
  lib_dirs <- expandStrings (library_dirs pkg) 
  hs_libs  <- expandStrings (hs_libraries pkg)
  ex_libs  <- expandStrings (extra_libraries pkg)
  inc_dirs <- expandStrings (include_dirs pkg)
  c_incs   <- expandStrings (c_includes pkg)
  p_deps   <- expandStrings (package_deps pkg)
  e_g_opts <- expandStrings (extra_ghc_opts pkg)
  e_c_opts <- expandStrings (extra_cc_opts pkg)
  e_l_opts <- expandStrings (extra_ld_opts pkg)
  f_dirs   <- expandStrings (framework_dirs pkg)
  e_frames <- expandStrings (extra_frameworks pkg)
  return (pkg { name            = nm
  	      , import_dirs     = imp_dirs
	      , source_dirs     = src_dirs
	      , library_dirs    = lib_dirs
	      , hs_libraries    = hs_libs
	      , extra_libraries = ex_libs
	      , include_dirs    = inc_dirs
	      , c_includes      = c_incs
	      , package_deps    = p_deps
	      , extra_ghc_opts  = e_g_opts
	      , extra_cc_opts   = e_c_opts
	      , extra_ld_opts   = e_l_opts
	      , framework_dirs  = f_dirs
	      , extra_frameworks= e_frames
	      })
  where
   expandStrings vs = do
     xs <- mapM expandString vs
       -- Flatten the elements of the expanded list; this is
       -- to permit substitutions for list-valued variables. e.g.,
       --     package_deps["${deps}"] where env var (say) 'deps'
       -- is "base,haskell98,network"
     return (concat (map (wordsBy (==',')) xs))
   
    -- Just for fun, keep this in the IO monad.
   expandString :: String -> IO String
   expandString str =
     case break (=='$') str of
       (xs, _:'{':rs) ->
         case span (/='}') rs of
	   (nm,_:remainder) -> do
	      nm'  <- lookupEnvVar nm
	      str' <- expandString remainder
	      return (xs ++ nm' ++ str')
	   _ -> return str -- no closing '}'
       _ -> return str	   

   lookupEnvVar nm = 
     case lookup nm defines of
       Just x | not (null x) -> return x
       _      -> 
	catch (System.getEnv nm)
	   (\ _ -> do dieOrForce force ("Unable to expand variable " ++ 
					show nm)
		      return "")

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsBy p s'' where (w,s'') = break p s'

-----------------------------------------------------------------------------

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = do { hFlush stdout ; hPutStr stderr s; exitWith (ExitFailure 1) }

dieOrForce :: Bool -> String -> IO ()
dieOrForce force s 
  | force     = do hFlush stdout; hPutStrLn stderr (s ++ " (ignoring)")
  | otherwise = die (s ++ "\n")

-----------------------------------------------------------------------------
-- Exceptions

#ifndef __GLASGOW_HASKELL__

eval_catch a h = a `seq` return ()
my_catch = IO.catch
my_throw = IO.fail

#else /* GHC */

my_throw = Exception.throw
#if __GLASGOW_HASKELL__ > 408
eval_catch = Exception.catch . Exception.evaluate
my_catch = Exception.catch
#else
eval_catch = Exception.catchAll
my_catch = Exception.catchAllIO
#endif

#endif

-----------------------------------------
--	Cut and pasted from ghc/compiler/SysTools

#if defined(mingw32_HOST_OS)
subst a b ls = map (\ x -> if x == a then b else x) ls
unDosifyPath xs = subst '\\' '/' xs

getExecDir :: String -> IO (Maybe String)
-- (getExecDir cmd) returns the directory in which the current
--	  	    executable, which should be called 'cmd', is running
-- So if the full path is /a/b/c/d/e, and you pass "d/e" as cmd,
-- you'll get "/a/b/c" back as the result
getExecDir cmd
  = allocaArray len $ \buf -> do
	ret <- getModuleFileName nullPtr buf len
	if ret == 0 then return Nothing
	            else do s <- peekCString buf
			    return (Just (reverse (drop (length cmd) 
							(reverse (unDosifyPath s)))))
  where
    len = 2048::Int -- Plenty, PATH_MAX is 512 under Win32.

foreign import stdcall "GetModuleFileNameA" unsafe 
  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32
#else
getExecDir :: String -> IO (Maybe String) 
getExecDir s = do return Nothing
#endif
