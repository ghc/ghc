module GHC.Toolchain.Program
    ( Program(..)
    , shProgram
    , _prgPath
    , _prgFlags
    , addFlagIfNew
      -- * Running programs
    , runProgram
    , callProgram
    , readProgram
    , readProgramStdout
      -- * Finding 'Program's
    , ProgOpt(..)
    , emptyProgOpt
    , programFromOpt
    , _poPath
    , _poFlags
    , findProgram
     -- * Compiler programs
    , compile
    , supportsTarget
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (intercalate, isPrefixOf)
import Data.Maybe
import System.FilePath
import System.Directory
import System.Exit
import System.Process hiding (env)

import GHC.Platform.ArchOS
import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils

data Program = Program { prgPath :: FilePath
                       , prgFlags :: [String]
                       }
    deriving (Read, Eq, Ord)

shProgram :: Program
shProgram = Program "sh" []

instance Show Program where
  -- Normalise filepaths before showing to aid with diffing the target files.
  show (Program p f) = unwords
    [ "Program { prgPath = ", show (normalise p), ", prgFlags =", show f , "}"]

_prgPath :: Lens Program FilePath
_prgPath = Lens prgPath (\x o -> o {prgPath = x})

_prgFlags :: Lens Program [String]
_prgFlags = Lens prgFlags (\x o -> o {prgFlags = x})

-- | Prepends a flag to a program's flags if the flag is not in the existing flags.
addFlagIfNew :: String -> Program -> Program
addFlagIfNew flag prog@(Program path flags)
  = if flag `elem` flags
       then prog
       else Program path (flags ++ [flag])

runProgram :: Program -> [String] -> M ExitCode
runProgram prog args = do
    logExecute prog args
    let cp = (proc (prgPath prog) (prgFlags prog ++ args))
            { std_out = CreatePipe
            -- , std_err = CreatePipe
            }
    (code, _stdout, _stderr) <- liftIO $ readCreateProcessWithExitCode cp ""
    return code

callProgram :: Program -> [String] -> M ()
callProgram prog args = do
    code <- runProgram prog args
    case code of
      ExitSuccess -> return ()
      ExitFailure n -> throwEs (err n)
  where
    cmdline = [prgPath prog] ++ prgFlags prog ++ args
    err n =
        [ "Command failed: " ++ unwords cmdline
        , "Exited with code " ++ show n
        ]

-- | Runs a program with a list of arguments and returns the exit code and the
-- stdout and stderr output
readProgram :: Program -> [String] -> M (ExitCode, String, String)
readProgram prog args = do
    logExecute prog args
    liftIO $ readProcessWithExitCode (prgPath prog) (prgFlags prog ++ args) ""

-- | Runs a program with a list of arguments and returns the stdout output,
-- ignoring the exit code.
readProgramStdout :: Program -> [String] -> M String
readProgramStdout prog args = do
    logExecute prog args
    (_code, stdout, _stderr) <- liftIO $ readProcessWithExitCode (prgPath prog) (prgFlags prog ++ args) ""
    -- Ignores the exit code!
    return stdout

logExecute :: Program -> [String] -> M ()
logExecute prog args =
    logDebug $ "Execute: " ++ intercalate " " ([prgPath prog] ++ prgFlags prog ++ args)

-- | Program specifier from the command-line.
data ProgOpt = ProgOpt { poPath :: Maybe String
                       -- ^ Refers to the path to an executable, or simply the
                       -- executable name.
                       , poFlags :: Maybe [String]
                       }

_poPath :: Lens ProgOpt (Maybe FilePath)
_poPath = Lens poPath (\x o -> o {poPath=x})

_poFlags :: Lens ProgOpt (Maybe [String])
_poFlags = Lens poFlags (\x o -> o {poFlags=x})

emptyProgOpt :: ProgOpt
emptyProgOpt = ProgOpt Nothing Nothing

-- | Make a @'Program'@ from user specified program options (@'ProgOpt'@),
-- defaulting to the given path and flags if unspecified in the @'ProgOpt'@.
programFromOpt :: ProgOpt
               -> FilePath -- ^ Program path to default to
               -> [String] -- ^ Program flags to default to
               -> Program
programFromOpt userSpec path flags = Program { prgPath = fromMaybe path (poPath userSpec), prgFlags = fromMaybe flags (poFlags userSpec) }

-- | Tries to find the user specified program by path or tries to look for one
-- in the given list of candidates.
--
-- If the 'ProgOpt' program flags are unspecified the program will have an empty list of flags.
findProgram :: String
            -> ProgOpt     -- ^ path provided by user
            -> [FilePath]  -- ^ candidate names
            -> M Program
findProgram description userSpec candidates
  | Just path <- poPath userSpec = do
      let err =
            [ "Failed to find " ++ description ++ "."
            , "Looked for user-specified program '" ++ path ++ "' in the system search path."
            ]
      toProgram <$> find_it path <|> throwEs err

  | otherwise = do
      env <- getEnv
      let prefixedCandidates =
              case targetPrefix env of
                Just prefix -> map (prefix++) candidates
                Nothing     -> []
          candidates' = prefixedCandidates ++ candidates
          err =
            [ "Failed to find " ++ description ++ "."
            , "Looked for one of " ++ show candidates' ++ " in the system search path."
            ]
      toProgram <$> oneOf' err (map find_it candidates') <|> throwEs err
  where
      toProgram path = Program { prgPath = path, prgFlags = fromMaybe [] (poFlags userSpec) }

      find_it name = do
          r <- liftIO $ findExecutable name
          case r of
            Nothing -> throwE $ name ++ " not found in search path"
            -- Use the given `prgPath` or candidate name rather than the
            -- absolute path returned by `findExecutable`.
            Just _x -> return name

-------------------- Compiling utilities --------------------

-- | Compile a program with a given compiler.
--
-- The compiler must
-- * Take the program path as a positional argument
-- * Accept @-o@ to specify output path
compile
    :: FilePath  -- ^ input extension
    -> [String]  -- ^ extra flags
    -> Lens compiler Program
    -> compiler
    -> FilePath  -- ^ output path
    -> String    -- ^ source
    -> M ()
compile ext extraFlags lens c outPath program = do
    let srcPath = outPath <.> ext
    writeFile srcPath program
    callProgram (view lens c) $ extraFlags ++ ["-o", outPath, srcPath]
    expectFileExists outPath "compiler produced no output"

-- Note [Don't pass --target to emscripten toolchain]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Emscripten's CC wrapper is a bit wonky in that it accepts the `--target`
-- flag when used as a linker yet rejects it as a compiler (e.g. with `-c`).
-- This is exacerbated by the fact that Cabal currently in some cases
-- combines (and therefore conflates) link and compilation flags.
--
-- Ultimately this should be fixed in Cabal but in the meantime we work around it
-- by handling this toolchain specifically in the various
-- "supports --target" checks in `configure` and `ghc-toolchain`.
--
-- Fixes #23744.

-- | Does compiler program support the @--target=<triple>@ option? If so, we should
-- pass it whenever possible to avoid ambiguity and potential compile-time
-- errors (e.g. see #20162).
supportsTarget :: ArchOS
               -> Lens compiler Program
               -> (compiler -> M ()) -- ^ Action to check if compiler with @--target@ flag works
               -> String             -- ^ The LLVM target to use if @cc@ supports @--target@
               -> compiler           -- ^ The compiler to check @--target@ support for
               -> M compiler         -- ^ Return compiler with @--target@ flag if supported
supportsTarget archOs lens checkWorks llvmTarget c
    -- See Note [Don't pass --target to emscripten toolchain].
  | ArchJavaScript <- archOS_arch archOs
  = return c

    -- No reason to check if the options already contain a --target flag
  | any ("--target=" `isPrefixOf`) (view (lens % _prgFlags) c)
  = return c

  | otherwise
  = let c' = over (lens % _prgFlags) (("--target="++llvmTarget):) c
     in (c' <$ checkWorks (over (lens % _prgFlags) ("-Werror":) c')) <|> return c

