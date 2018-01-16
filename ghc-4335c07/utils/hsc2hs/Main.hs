{-# LANGUAGE CPP #-}

------------------------------------------------------------------------
-- Program for converting .hsc files to .hs files, by converting the
-- file into a C program which is run to generate the Haskell source.
-- Certain items known only to the C compiler can then be used in
-- the Haskell module; for example #defined constants, byte offsets
-- within structures, etc.
--
-- See the documentation in the Users' Guide for more details.

import Control.Monad            ( liftM, forM_ )
import Data.List                ( isSuffixOf )
import System.Console.GetOpt

-- If we ware building the hsc2hs
-- binary for binary distribution
-- in the GHC tree.  Obtain
-- the path to the @$topdir/lib@
-- folder, and try to locate the
-- @template-hsc.h@ there.
--
-- XXX: Note this does not work
--      on windows due to for
--      symlinks. See Trac #14483.

#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C.String
#endif
import System.Directory         ( doesFileExist, findExecutable )
import System.Environment       ( getProgName, getArgs )
import System.Exit              ( ExitCode(..), exitWith )
import System.FilePath          ( normalise, splitFileName, splitExtension )
import System.IO

#ifdef BUILD_NHC
import System.Directory         ( getCurrentDirectory )
#else
import Paths_hsc2hs as Main     ( getDataFileName )
#endif
#if defined(IN_GHC_TREE)
import System.Environment       ( getExecutablePath )
import System.FilePath          ( takeDirectory, (</>) )
#endif

import Common
import CrossCodegen
import DirectCodegen
import Flags
import HSCParser

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

#ifdef BUILD_NHC
getDataFileName s = do here <- getCurrentDirectory
                       return (here++"/"++s)
#endif

versionString :: String
versionString = "hsc2hs version " ++ CURRENT_PACKAGE_VERSION ++ "\n"

main :: IO ()
main = do
    prog <- getProgramName
    let header = "Usage: "++prog++" [OPTIONS] INPUT.hsc [...]\n"
        usage = usageInfo header options
    args <- getArgs
    let (fs, files, errs) = getOpt Permute options args
    let mode = foldl (.) id fs emptyMode
    case mode of
        Help     -> bye usage
        Version  -> bye versionString
        UseConfig config ->
            case (files, errs) of
            ((_:_), []) -> processFiles config files usage
            (_,     _ ) -> die (concat errs ++ usage)

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` "-bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

processFiles :: ConfigM Maybe -> [FilePath] -> String -> IO ()
processFiles configM files usage = do
    mb_libdir <- getLibDir

    (template, extraFlags) <- findTemplate usage mb_libdir configM
    compiler <- findCompiler mb_libdir configM
    let linker = case cmLinker configM of
                 Nothing -> compiler
                 Just l -> l
        config = Config {
                     cmTemplate    = Id template,
                     cmCompiler    = Id compiler,
                     cmLinker      = Id linker,
                     cKeepFiles    = cKeepFiles configM,
                     cNoCompile    = cNoCompile configM,
                     cCrossCompile = cCrossCompile configM,
                     cCrossSafe    = cCrossSafe configM,
                     cColumn       = cColumn configM,
                     cVerbose      = cVerbose configM,
                     cFlags        = cFlags configM ++ extraFlags
                 }

    let outputter = if cCrossCompile config then outputCross else outputDirect

    forM_ files (\name -> do
        (outName, outDir, outBase) <- case [f | Output f <- cFlags config] of
             [] -> if not (null ext) && last ext == 'c'
                      then return (dir++base++init ext,  dir, base)
                      else
                         if ext == ".hs"
                            then return (dir++base++"_out.hs", dir, base)
                            else return (dir++base++".hs",     dir, base)
                   where
                    (dir,  file) = splitFileName  name
                    (base, ext)  = splitExtension file
             [f] -> let
                 (dir,  file) = splitFileName  f
                 (base, _)    = splitExtension file
                 in return (f, dir, base)
             _ -> onlyOne "output file"
        let file_name = normalise name
        toks <- parseFile file_name
        outputter config outName outDir outBase file_name toks)

findTemplate :: String -> Maybe FilePath -> ConfigM Maybe
             -> IO (FilePath, [Flag])
findTemplate usage mb_libdir config
 = -- If there's no template specified on the commandline, try to locate it
   case cmTemplate config of
   Just t ->
       return (t, [])
   Nothing -> do
     -- If there is no Template flag explicitly specified, try
     -- to find one. We first look near the executable.  This only
     -- works on Win32 or Hugs (getExecDir). If this finds a template
     -- file then it's certainly the one we want, even if hsc2hs isn't
     -- installed where we told Cabal it would be installed.
     --
     -- Next we try the location we told Cabal about.
     --
     -- If IN_GHC_TREE is defined (-fin-ghc-tree), we also try to locate
     -- the template in the `baseDir`, as provided by the `ghc-boot`
     -- library. Note that this is a hack to work around only partial
     -- relocatable support in cabal, and is here to allow the hsc2hs
     -- built and shipped with ghc to be relocatable with the ghc
     -- binary distribution it ships with.
     --
     -- If neither of the above work, then hopefully we're on Unix and
     -- there's a wrapper script which specifies an explicit template flag.
     mb_templ1 <-
       case mb_libdir of
       Nothing   -> return Nothing
       Just path -> do
       -- Euch, this is horrible. Unfortunately
       -- Paths_hsc2hs isn't too useful for a
       -- relocatable binary, though.
         let
             templ1 = path ++ "/template-hsc.h"
             incl = path ++ "/include/"
         exists1 <- doesFileExist templ1
         if exists1
            then return $ Just (templ1, CompFlag ("-I" ++ incl))
            else return Nothing
     mb_templ2 <- case mb_templ1 of
         Just (templ1, incl) ->
             return $ Just (templ1, [incl])
         Nothing -> do
             templ2 <- getDataFileName "template-hsc.h"
             exists2 <- doesFileExist templ2
             if exists2
                then return $ Just (templ2, [])
                else return Nothing
     case mb_templ2 of
         Just x -> return x
#if defined(IN_GHC_TREE)
         Nothing -> do
             -- XXX: this will *not* work on windows for symlinks, until `getExecutablePath` in `base` is
             --      fixed. The alternative would be to bring the whole logic from the SysTools module in here
             --      which is rather excessive. See Trac #14483.
             let getBaseDir = Just . (\p -> p </> "lib") . takeDirectory . takeDirectory <$> getExecutablePath
             mb_templ3 <- fmap (</> "template-hsc.h") <$> getBaseDir
             mb_exists3 <- mapM doesFileExist mb_templ3
             case (mb_templ3, mb_exists3) of
                 (Just templ3, Just True) -> return (templ3, [])
                 _ -> die ("No template specified, and template-hsc.h not located.\n\n" ++ usage)
#else
         Nothing -> die ("No template specified, and template-hsc.h not located.\n\n" ++ usage)
#endif

findCompiler :: Maybe FilePath -> ConfigM Maybe -> IO FilePath
findCompiler mb_libdir config
 = case cmCompiler config of
   Just c -> return c
   Nothing ->
       do let search_path = do
                  mb_path <- findExecutable default_compiler
                  case mb_path of
                      Nothing ->
                          die ("Can't find "++default_compiler++"\n")
                      Just path -> return path
              -- if this hsc2hs is part of a GHC installation on
              -- Windows, then we should use the mingw gcc that
              -- comes with GHC (#3929)
              inplaceGccs = case mb_libdir of
                            Nothing -> []
                            Just d  -> [d ++ "/../mingw/bin/gcc.exe"]
              search [] = search_path
              search (x : xs) = do b <- doesFileExist x
                                   if b then return x else search xs
          search inplaceGccs

parseFile :: String -> IO [Token]
parseFile name
  = do h <- openBinaryFile name ReadMode
       -- use binary mode so we pass through UTF-8, see GHC ticket #3837
       -- But then on Windows we end up turning things like
       --     #let alignment t = e^M
       -- into
       --     #define hsc_alignment(t ) printf ( e^M);
       -- which gcc doesn't like, so strip out any ^M characters.
       s <- hGetContents h
       let s' = filter ('\r' /=) s
       case runParser parser name s' of
         Success _ _ _ toks -> return toks
         Failure (SourcePos name' line col) msg ->
           die (name'++":"++show line++":"++show col++": "++msg++"\n")

getLibDir :: IO (Maybe String)
getLibDir = fmap (fmap (++ "/lib")) $ getExecDir "/bin/hsc2hs.exe"

-- (getExecDir cmd) returns the directory in which the current
--                  executable, which should be called 'cmd', is running
-- So if the full path is /a/b/c/d/e, and you pass "d/e" as cmd,
-- you'll get "/a/b/c" back as the result
getExecDir :: String -> IO (Maybe String)
getExecDir cmd =
    getExecPath >>= maybe (return Nothing) removeCmdSuffix
    where initN n = reverse . drop n . reverse
          removeCmdSuffix = return . Just . initN (length cmd) . normalise

getExecPath :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
getExecPath = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> fmap Just $ peekCWString buf
            | otherwise  -> try_size (size * 2)

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getExecPath = return Nothing
#endif
