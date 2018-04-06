{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

{-
  gen-dll is a replacement for dll-split which aims to solve a simple problem
  during the building of stage2. The issue is that the PE image format only has
  a 16-bit field for the symbol count. This means we can't have more than 2^16-1
  symbols in a single PE file. See Trac #5987.

  gen-dll solves this issue by partitioning the object files in such a way that
  a single dll never has more than the allowed amount of symbols. The general
  workflow of gen-dll is:

  1) use nm -g to dump the symbols defined in each object file, from this dump
     we collect three key pieces information:
     a) the object file the symbol belongs to
     b) the symbol's kind (e.g data or function)
     c) the symbol name.

  2) If the amount of symbols is lower than the maximum, we're done and we'll
     just link the entire list of symbols and move on.

     If however we have too many symbols we'll partition the symbols using a
     per object file granularity. This is because we can't split the content of
     an object file. An oc belongs to one and only one image file.

  3) Once we have the partitioning, we sub partition these into two groups for
     each partition:
     a) data
     b) function

     The reason for this is that data exports are directly accessed, whereas
     functions generally go through a trampoline. The trampolines are there to
     allow for extra functionality such as delay loading (if requested) and to
     cover for memory model changes due to linking all the object code in on
     PE image.

     Data is usually accessed direct, so we don't want the trampoline otherwise
        extern int foo;
     would point to executable code instead of data.

  4) Once we have everything correctly tagged, the partitions are dumped into a
     module definition file (def). Each file is named <dll-name>-pt<num>.<ext>
     which is also the partitioning scheme used for all other files including
     the resulting dlls.

     From the .def file we use libtool or genlib (when available) to generate
     an import library. In this case we generate a GNU style import library
     See Note [BFD import library].

     These import libraries are used to break the cyclic dependencies that may
     exist between the symbols due to the random partitioning. e.g. A may
     require B, but A and B can be in different dlls. With the import libraries
     we promise A that at runtime it'll have B, and vice versa. The Windows
     runtime linker and loader will take care of breaking this cycle at runtime.

  5) Once we have an import library for each partition, we start linking the
     final dlls. if e.g. we have 3 dlls, linking dll 1 means passing import
     libraries 2 and 3 as an argument to the linking of dll 1. This allows it
     to find all symbols since PE image files can't have dangling symbols.

  6) After creating the dlls the final step is to create one top level import
     library that is named after the original dll that we were supposed to link.

     To continue the 3 split example. say we were supposed to make libfoo.dll,
     instead we created libfoo-pt1.dll, libfoo-pt2.dll and libfoo-pt3.dll.
     Obviously using -lfoo would no longer locate the dlls.

     This is solved by using import libraries again. GNU style import libraries
     are just plain AR archives where each object file essentially contains
     only 1 symbol and the dll in which to find this symbol.

     A proper linker processes all the object files in this AR file (lld, ld and
     ghci do this.) and so while genlib and libtool don't allow you to create
     import libraries with multiple dll pointers, it is trivial to do.

     We use ar to merge together the import libraries into a large complete one.
     e.g. libfoo-pt1.dll.a, libfoo-pt2.dll.a and libfoo-pt3.dll.a are merged
     into libfoo.dll.a. The name isn't coincidental. On Windows you don't link
     directly against a dll, instead you link against an import library that
     then tells you how to get to the dll functions.

     In this case by creating a correctly named merged import library we solve
     the -lfoo problem.

     In the end we end up with libfoo-pt1.dll, libfoo-pt2.dll and libfoo-pt3.dll
     along with libfoo.dll.a. To the rest of the pipeline the split is
     completely transparent as -lfoo will just continue to work, and the linker
     is responsible for populating the IAT (Import Address Table) with the
     actual dlls we need.

  This scheme is fully scalable and will not need manual maintenance or
  intervention like dll-split needed. If we ever do switch to compiling using
  Microsoft compilers, we need to use a custom tool to modify the PE import
  libraries lib.exe creates. This is slightly more work but for now we can just
  rely on the GNU import libraries.

  If supported by the stage1 compiler, we'll create dll's which can be used as
  SxS assemblies, but in order for us to do so, we have to give GHC some extra
  information such as the stable abi name for the dll and the version of the
  dll being created. This is purely a deployment thing and does not really
  affect the workings of this tool.
-}
module Main(main) where

import Control.Arrow ((***))
import Control.Monad (when, forM_)
import Control.Exception (bracket)

import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, nub, sort, (\\))
import qualified Data.Map as M (Map(), alter, empty, toList)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.Directory (findFilesWith, getCurrentDirectory)
import System.FilePath (takeBaseName, takeDirectory, dropExtension, (<.>)
                       ,takeFileName)
import System.IO (hClose, hGetContents, withFile, IOMode(..), hPutStrLn, openFile)
import System.Process (proc, createProcess_, StdStream (..), CreateProcess(..)
                      ,waitForProcess)

import Foreign.C.Types (CInt(..), )
import Foreign.C.String (withCWString, peekCWString, CWString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (alloca)

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

-- Setup some standard program names.
nm :: FilePath
nm = NM_TOOL_BIN

libexe :: FilePath
libexe = LIB_TOOL_BIN

genlib :: FilePath
genlib = GENLIB_TOOL_BIN

ar :: FilePath
ar = AR_TOOL_BIN

-- Technically speaking the limit for the amount of symbols you can have in a
-- dll is 2^16-1, however Microsoft's lib.exe for some reason refuses to link
-- up to this amount. The reason is likely that it adds some extra symbols in
-- the generated dll, such as dllmain etc. So we reserve some space in the
-- symbol table to accomodate this. This number is just purely randomly chosen.
#define SYMBOL_PADDING 10

usage :: IO ()
usage = putStrLn $ unlines [ " -= Split a dll if required and perform the linking =- "
                           , ""
                           , " Usage: gen-dll <action>"
                           , ""
                           , " Where <action> is one of:"
                           , "     link     perform a real link of dll, "
                           , "              arguments: dir distdir way flags libs objs out link_cmd delay name version"
                           ]

main :: IO ()
main = do
  args <- getArgs
  if null args
     then usage
     else case (head args) of
             "link" -> let (dir:distdir:way:extra_flags:extra_libs:objs:output:
                            command:delayed:abi_name:abi_version:_) = tail args
                       in process_dll_link dir distdir way extra_flags extra_libs
                                           objs output command delayed abi_name
                                           abi_version
             _      -> usage

type Symbol = String
type Symbols = [Symbol]
type SymbolType = Char
data Obj
  = Obj { objName  :: String
        , objCount :: Int
        , objItems :: [(SymbolType, Symbol)]
        }
    deriving Show
type Objs = [Obj]

-- | Create the final DLL by using the provided arguments
--   This also creates the resulting special import library.
process_dll_link :: String -- ^ dir
                 -> String -- ^ distdir
                 -> String -- ^ way
                 -> String -- ^ extra flags
                 -> String -- ^ extra libraries to link
                 -> String -- ^ object files to link
                 -> String -- ^ output filename
                 -> String -- ^ link command
                 -> String -- ^ create delay load import libs
                 -> String -- ^ SxS Name
                 -> String -- ^ SxS version
                 -> IO ()
process_dll_link _dir _distdir _way extra_flags extra_libs objs_files output
                 link_cmd delay_imp sxs_name sxs_version
  = do let base = dropExtension output
       -- We need to know how many symbols came from other static archives
       -- So take the total number of symbols and remove those we know came
       -- from the object files. Use this to lower the max amount of symbols.
       --
       -- This granularity is the best we can do without --print-map like info.
       raw_exports <- execProg nm Nothing ["-g", "--defined-only", objs_files]
       putStrLn $ "Processing symbols.."

       let objs    = collectObjs raw_exports
           num_sym = foldr (\a b -> b + objCount a) 0 objs
           exports = base <.> "lst"

       putStrLn $ "Number of symbols in object files for " ++ output ++ ": " ++ show num_sym

       _ <- withFile exports WriteMode $ \hExports ->
             mapM_ (hPutStrLn hExports . unlines . map snd . objItems) objs

#if defined(GEN_SXS)
       -- Side-by-Side assembly generation flags for GHC. Pass these along so the DLLs
       -- get the proper manifests generated.
       let sxs_opts = [ "-fgen-sxs-assembly"
                      , "-dylib-abi-name"
                      , show sxs_name
                      , "-dylib-abi-version"
                      , show sxs_version
                      ]
#else
       let sxs_opts = []
#endif

       -- Now check that the DLL doesn't have too many symbols. See trac #5987.
       case num_sym > dll_max_symbols of
         False -> do putStrLn $ "DLL " ++ output ++ " OK, no need to split."
                     let defFile    = base <.> "def"
                         dll_import = base <.> "dll.a"

                     build_import_lib base (takeFileName output) defFile objs

                     _ <- execProg link_cmd Nothing
                              $ concat [[objs_files
                                        ,extra_libs
                                        ,extra_flags
                                        ]
                                       ,sxs_opts
                                       ,["-fno-shared-implib"
                                        ,"-optl-Wl,--retain-symbols-file=" ++ exports
                                        ,"-o"
                                        ,output
                                        ]
                                       ]

                     build_delay_import_lib defFile dll_import delay_imp

         True -> do putStrLn $ "Too many symbols for a single DLL " ++ output
                    putStrLn "We'll have to split the dll..."
                    putStrLn $  "OK, we only have space for "
                             ++ show dll_max_symbols
                             ++ " symbols from object files when building "
                             ++ output

                    -- First split the dlls up by whole object files
                    -- To do this, we iterate over all object file and
                    -- generate a the partitions based on allowing a
                    -- maximum of $DLL_MAX_SYMBOLS in one DLL.
                    let spl_objs   = groupObjs objs
                        n_spl_objs = length spl_objs
                        base'      = base ++ "-pt"

                    mapM_ (\(n, _) -> putStrLn $ ">> DLL split at " ++ show n ++ " symbols.") spl_objs
                    putStrLn $ "OK, based on the amount of symbols we'll split the DLL into " ++ show n_spl_objs ++ " pieces."

                    -- Start off by creating the import libraries to break the
                    -- mutual dependency chain.
                    forM_ (zip [(1::Int)..] spl_objs) $ \(i, (n, o)) ->
                      do putStrLn $ "Processing file " ++ show i   ++ " of "
                                 ++ show n_spl_objs    ++ " with " ++ show n
                                 ++ " symbols."
                         let base_pt = base' ++ show i
                             file    = base_pt <.> "def"
                             dll     = base_pt <.> "dll"
                             lst     = base_pt <.> "lst"

                         _ <- withFile lst WriteMode $ \hExports ->
                               mapM_ (hPutStrLn hExports . unlines . map snd . objItems) o

                         build_import_lib base_pt (takeFileName dll) file o

                    -- Now create the actual DLLs by using the import libraries
                    -- to break the mutual recursion.
                    forM_ (zip [1..] spl_objs) $ \(i, (n, _)) ->
                      do putStrLn $ "Creating DLL " ++ show i   ++ " of "
                                 ++ show n_spl_objs    ++ " with " ++ show n
                                 ++ " symbols."
                         let base_pt = base' ++ show i
                             file    = base_pt <.> "def"
                             dll     = base_pt <.> "dll"
                             lst     = base_pt <.> "lst"
                             imp_lib = base_pt <.> "dll.a"
                             indexes = [1..(length spl_objs)]\\[i]
                             libs    = map (\ix -> (base' ++ show ix) <.> "dll.a") indexes

                         _ <- execProg link_cmd Nothing
                                  $ concat [[objs_files
                                            ,extra_libs
                                            ,extra_flags
                                            ,file
                                            ]
                                           ,libs
                                           ,sxs_opts
                                           ,["-fno-shared-implib"
                                            ,"-optl-Wl,--retain-symbols-file=" ++ lst
                                            ,"-o"
                                            ,dll
                                            ]
                                           ]

                         -- build_delay_import_lib file imp_lib delay_imp
                         putStrLn $ "Created " ++ dll ++ "."

                    -- And finally, merge the individual import libraries into
                    -- one with the name of the original library we were
                    -- supposed to make. This means that nothing has to really
                    -- know how we split up the DLLs, for everything else it'so
                    -- as if it's still one large assembly.
                    create_merged_archive base base' (length spl_objs)


collectObjs :: [String] -> Objs
collectObjs = map snd . M.toList . foldr collectObjs' M.empty

collectObjs' :: String -> M.Map String Obj -> M.Map String Obj
collectObjs' []  m   = m
collectObjs' str_in m
  = let clean        = dropWhile isSpace
        str          = clean str_in
        (file, rest) = ((takeWhile (/=':') . clean) *** clean) $
                         break isSpace str
        (typ , sym ) = (id *** clean) $ break isSpace rest
        obj          = Obj { objName  = file
                           , objCount = 1
                           , objItems = [(head typ, sym)]
                           }
        upd value
          = if length typ /= 1
               then value
               else Just $ maybe obj
                                 (\o -> o { objCount = objCount o + 1
                                          , objItems = (head typ, sym) : objItems o
                                          })
                                 value
    in M.alter upd file m

-- Split a list of objects into globals and functions
splitObjs :: Objs -> (Symbols, Symbols)
splitObjs []     = ([], [])
splitObjs (y:ys) = group_ (objItems y) (splitObjs ys)
  where globals = "DdGgrRSsbBC"
        group_ :: [(Char, Symbol)] -> (Symbols, Symbols) -> (Symbols, Symbols)
        group_ []     x                             = x
        group_ (x:xs) (g, f) | fst x `elem` globals = group_ xs (snd x:g, f)
                             |     otherwise        = group_ xs (g, snd x:f)

-- Determine how to split the objects up.
groupObjs :: Objs -> [(Int, Objs)]
groupObjs = binObjs 0 []
 where binObjs :: Int -> Objs -> Objs -> [(Int, Objs)]
       binObjs n l []     = [(n, l)]
       binObjs n l (o:os)
         = let nx = objCount o
               n' = n + nx
           in if n' > dll_max_symbols
                 then (n, l) : binObjs 0 [] os
                 else binObjs n' (o:l) os

-- Maximum number of symbols to allow into
-- one DLL. This is the split factor used.
dll_max_symbols :: Int
dll_max_symbols = 65535 - SYMBOL_PADDING -- Some padding for required symbols.

isTrue :: String -> Bool
isTrue s = let s' = map toLower s
           in case () of
               () | s' == "yes" -> True
                  | s' == "no"  -> False
                  | otherwise   -> error $ "Expected yes/no but got '" ++ s ++ "'"

foreign import WINDOWS_CCONV unsafe "Shellapi.h CommandLineToArgvW"
     c_CommandLineToArgvW :: CWString -> Ptr CInt -> IO (Ptr CWString)

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
    localFree :: Ptr a -> IO (Ptr a)

mkArgs :: String -> IO [String]
mkArgs []  = return []
mkArgs arg =
  do withCWString arg $ \c_arg -> do
       alloca $ \c_size -> do
         res <- c_CommandLineToArgvW c_arg c_size
         size <- peek c_size
         args <- peekArray (fromIntegral size) res
         values <- mapM peekCWString args
         _ <- localFree res
         return values

execProg :: String -> Maybe FilePath -> [String] -> IO [String]
execProg prog m_stdin args =
  do args' <- fmap concat $ mapM mkArgs args
     prog' <- mkArgs prog
     let full@(c_prog:c_args) = prog' ++ args'
     -- print the commands we're executing for debugging and transparency
     putStrLn $ unwords $ full ++ [maybe "" ("< " ++) m_stdin]
     cwdir <- getCurrentDirectory
     let cp = (proc c_prog c_args)
              { std_out = CreatePipe, cwd = Just cwdir }
     cp' <- case m_stdin of
              Nothing   -> return cp
              Just path -> do h <- openFile path ReadMode
                              return cp{ std_in = UseHandle h}
     bracket
       (createProcess_ ("execProg: " ++ prog)  cp')
       (\(_, Just hout, _, ph) -> do
         hClose hout
         code <- waitForProcess ph
         case std_in cp' of
           UseHandle h -> hClose h
           _           -> return ()
         case code of
           ExitFailure _ -> exitWith code
           ExitSuccess   -> return ())
       (\(_, Just hout, _, _) -> do
         results <- hGetContents hout
         length results `seq` return $ lines results)

-- | Mingw-w64's genlib.exe is generally a few order of magnitudes faster than
-- libtool which is BFD based. So we prefer it, but it's not standard so
-- support both. We're talking a difference of 45 minutes in build time here.
execLibTool :: String -> String -> IO [String]
execLibTool input_def output_lib =
  do if HAS_GENLIB
        then execProg genlib Nothing [input_def, "-o", output_lib]
        else execProg libexe Nothing ["-d", input_def, "-l", output_lib]

-- Builds a delay import lib at the very end which is used to
-- be able to delay the picking of a DLL on Windows.
-- This function is called always and decided internally
-- what to do.
build_delay_import_lib :: String -- ^ input def file
                       -> String -- ^ ouput import delayed import lib
                       -> String -- ^ flag to indicate if delay import
                                 --   lib should be created
                       -> IO ()
build_delay_import_lib input_def output_lib create_delayed
  = when (isTrue create_delayed) $
       execLibTool input_def output_lib >> return ()

-- Build a normal import library from the object file definitions
build_import_lib :: FilePath -> FilePath -> FilePath -> Objs -> IO ()
build_import_lib base dll_name defFile objs
  = do -- Create a def file hiding symbols not in original object files
       -- because --export-all is re-exporting things from static libs
       -- we need to separate out data from functions. So first create two temporaries
       let (globals, functions) = splitObjs objs

       -- This split is important because for DATA entries the compiler should not generate
       -- a trampoline since CONTS DATA is directly referenced and not executed. This is not very
       -- important for mingw-w64 which would generate both the trampoline and direct referecne
       -- by default, but for libtool is it and even for mingw-w64 we can trim the output.
       _ <- withFile defFile WriteMode $ \hDef -> do
              hPutStrLn hDef $ unlines $ ["LIBRARY " ++ show dll_name
                                         ,"EXPORTS"
                                         ]
              mapM_ (\v -> hPutStrLn hDef $ "    " ++ show v ++ " DATA") globals
              mapM_ (\v -> hPutStrLn hDef $ "    " ++ show v           ) functions

       let dll_import = base <.> "dll.a"
       _ <- execLibTool defFile dll_import
       return ()

-- Do some cleanup and create merged lib.
-- Because we have no split the DLL we need
-- to provide a way for the linker to know about the split
-- DLL. Also the compile was supposed to produce a DLL
-- foo.dll and import library foo.lib. However we've actually
-- produced foo-pt1.dll, foo-pt2.dll etc. What we don't want is to have
-- To somehow convey back to the compiler that we split the DLL in x pieces
-- as this would require a lot of changes.
--
-- Instead we produce a merged import library which contains the union of
-- all the import libraries produced. This works because import libraries contain
-- only .idata section which point to the right dlls. So LD will do the right thing.
-- And this means we don't have to do any special handling for the rest of the pipeline.
create_merged_archive :: FilePath -> String -> Int -> IO ()
create_merged_archive base prefix count
  = do let ar_script = base <.> "mri"
           imp_lib   = base <.> "dll.a"
           imp_libs  = map (\i -> prefix ++ show i <.> "dll.a") [1..count]
       let script = [ "create " ++ imp_lib    ] ++
                    map ("addlib " ++) imp_libs ++
                    [ "save", "end" ]
       writeFile ar_script (unlines script)
       _ <- execProg ar (Just ar_script) ["-M"]
       return ()
