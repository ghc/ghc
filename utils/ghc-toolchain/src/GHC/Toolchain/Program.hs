module GHC.Toolchain.Program
    ( Program(..)
    , _prgPath
    , _prgFlags
      -- * Running programs
    , runProgram
    , callProgram
    , readProgram
      -- * Finding 'Program's
    , ProgOpt(..)
    , emptyProgOpt
    , _poPath
    , _poFlags
    , findProgram
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (intercalate)
import System.Directory
import System.Exit
import System.Process hiding (env)

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils

data Program = Program { prgPath :: FilePath
                       , prgFlags :: [String]
                       }
    deriving (Show, Read)

_prgPath :: Lens Program FilePath
_prgPath = Lens prgPath (\x o -> o {prgPath = x})

_prgFlags :: Lens Program [String]
_prgFlags = Lens prgFlags (\x o -> o {prgFlags = x})

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
      ExitFailure n -> throwE (err n)
  where
    cmdline = [prgPath prog] ++ prgFlags prog ++ args
    err n = unlines
        [ "Command failed: " ++ unwords cmdline
        , "Exited with code " ++ show n
        ]

readProgram :: Program -> [String] -> M String
readProgram prog args = do
    logExecute prog args
    liftIO $ readProcess (prgPath prog) (prgFlags prog ++ args) ""

logExecute :: Program -> [String] -> M ()
logExecute prog args =
    logDebug $ "Execute: " ++ intercalate " " ([prgPath prog] ++ prgFlags prog ++ args)

-- | Program specifier from the command-line.
data ProgOpt = ProgOpt { poPath :: Maybe FilePath
                       , poFlags :: [String]
                       }

_poPath :: Lens ProgOpt (Maybe FilePath)
_poPath = Lens poPath (\x o -> o {poPath=x})

_poFlags :: Lens ProgOpt [String]
_poFlags = Lens poFlags (\x o -> o {poFlags=x})

emptyProgOpt :: ProgOpt
emptyProgOpt = ProgOpt Nothing []

findProgram :: String
            -> ProgOpt     -- ^ path provided by user
            -> [FilePath]  -- ^ candidate names
            -> M Program
findProgram description userSpec candidates
  | Just path <- poPath userSpec = do
      let err = unlines
            [ "Failed to find " ++ description ++ "."
            , "Looked for user-specified program '" ++ path ++ "' in the system search path."
            ]
      toProgram <$> find_it path <|> throwE err

  | otherwise = do
      env <- getEnv
      let prefixedCandidates =
              case targetPrefix env of
                Just prefix -> map (prefix++) candidates
                Nothing     -> []
          candidates' = prefixedCandidates ++ candidates
          err = unlines
            [ "Failed to find " ++ description ++ "."
            , "Looked for one of " ++ show candidates' ++ " in the system search path."
            ]
      toProgram <$> oneOf err (map find_it candidates') <|> throwE err
  where
      toProgram path = Program { prgPath = path, prgFlags = poFlags userSpec }

      find_it name = do
          r <- liftIO $ findExecutable name
          case r of
            Nothing -> throwE $ name ++ " not found in search path"
            Just x -> return x
