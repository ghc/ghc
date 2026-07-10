{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, ScopedTypeVariables, NamedFieldPuns #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable, RecordWildCards #-}


-- | This module provides functions for calling command line programs, primarily
--   'command' and 'cmd'. As a simple example:
--
-- @
-- 'command' [] \"gcc\" [\"-c\",myfile]
-- @
--
--   The functions from this module are now available directly from "Development.Shake".
--   You should only need to import this module if you are using the 'cmd' function in the 'IO' monad.
module Development.Shake.Command(
    command, command_, cmd, cmd_, unit, CmdArgument(..), CmdArguments(..), IsCmdArgument(..), (:->),
    Stdout(..), StdoutTrim(..), Stderr(..), Stdouterr(..), Exit(..), Process(..), CmdTime(..), CmdLine(..), FSATrace(..),
    CmdResult, CmdString, CmdOption(..),
    addPath, addEnv,
    ) where

import Data.Tuple.Extra
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Exception.Extra
import Data.Char
import Data.Either.Extra
import Data.Foldable (toList)
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty)
import qualified Data.HashSet as Set
import Data.Maybe
import Data.Data
import Data.Semigroup
import System.Directory
import qualified System.IO.Extra as IO
import System.Environment
import System.Exit
import System.IO.Extra hiding (withTempFile, withTempDir)
import System.Process
import System.Info.Extra
import System.Time.Extra
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.UTF8 as UTF8
import General.Extra
import General.Process
import Prelude

import Development.Shake.Internal.CmdOption
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Types hiding (Result)
import Development.Shake.FilePath
import Development.Shake.Internal.Options
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Derived

---------------------------------------------------------------------
-- ACTUAL EXECUTION

-- | /Deprecated:/ Use 'AddPath'. This function will be removed in a future version.
--
--   Add a prefix and suffix to the @$PATH@ environment variable. For example:
--
-- @
-- opt <- 'addPath' [\"\/usr\/special\"] []
-- 'cmd' opt \"userbinary --version\"
-- @
--
--   Would prepend @\/usr\/special@ to the current @$PATH@, and the command would pick
--   @\/usr\/special\/userbinary@, if it exists. To add other variables see 'addEnv'.
addPath :: MonadIO m => [String] -> [String] -> m CmdOption
addPath pre post = do
    args <- liftIO getEnvironment
    let (path,other) = partition ((== "PATH") . (if isWindows then upper else id) . fst) args
    pure $ Env $
        [("PATH",intercalate [searchPathSeparator] $ pre ++ post) | null path] ++
        [(a,intercalate [searchPathSeparator] $ pre ++ [b | b /= ""] ++ post) | (a,b) <- path] ++
        other

-- | /Deprecated:/ Use 'AddEnv'. This function will be removed in a future version.
--
--   Add a single variable to the environment. For example:
--
-- @
-- opt <- 'addEnv' [(\"CFLAGS\",\"-O2\")]
-- 'cmd' opt \"gcc -c main.c\"
-- @
--
--   Would add the environment variable @$CFLAGS@ with value @-O2@. If the variable @$CFLAGS@
--   was already defined it would be overwritten. If you wish to modify @$PATH@ see 'addPath'.
addEnv :: MonadIO m => [(String, String)] -> m CmdOption
addEnv extra = do
    args <- liftIO getEnvironment
    pure $ Env $ extra ++ filter (\(a,_) -> a `notElem` map fst extra) args


data Str = Str String | BS BS.ByteString | LBS LBS.ByteString | Unit deriving (Eq,Show)

strTrim :: Str -> Str
strTrim (Str x) = Str $ trim x
strTrim (BS x) = BS $ fst $ BS.spanEnd isSpace $ BS.dropWhile isSpace x
strTrim (LBS x) = LBS $ trimEnd $ LBS.dropWhile isSpace x
    where
        trimEnd x = case LBS.uncons x of
            Just (c, x2) | isSpace c -> trimEnd x2
            _ -> x
strTrim Unit = Unit


data Result
    = ResultStdout Str
    | ResultStderr Str
    | ResultStdouterr Str
    | ResultCode ExitCode
    | ResultTime Double
    | ResultLine String
    | ResultProcess PID
    | ResultFSATrace [FSATrace FilePath]
    | ResultFSATraceBS [FSATrace BS.ByteString]
      deriving (Eq,Show)

data PID = PID0 | PID ProcessHandle
instance Eq PID where _ == _ = True
instance Show PID where show PID0 = "PID0"; show _ = "PID"

data Params = Params
    {funcName :: String
    ,opts :: [CmdOption]
    ,results :: [Result]
    ,prog :: String
    ,args :: [String]
    } deriving Show

class MonadIO m => MonadTempDir m where
    runWithTempDir :: (FilePath -> m a) -> m a
    runWithTempFile :: (FilePath -> m a) -> m a
instance MonadTempDir IO where
    runWithTempDir = IO.withTempDir
    runWithTempFile = IO.withTempFile
instance MonadTempDir Action where
    runWithTempDir = withTempDir
    runWithTempFile = withTempFile

---------------------------------------------------------------------
-- DEAL WITH Shell

removeOptionShell
    :: MonadTempDir m
    => Params -- ^ Given the parameter
    -> (Params -> m a) -- ^ Call with the revised params, program name and command line
    -> m a
removeOptionShell params@Params{..} call
    | Shell `elem` opts = do
        -- put our UserCommand first, as the last one wins, and ours is lowest priority
        let userCmdline = unwords $ prog : args
        params <- pure params{opts = UserCommand userCmdline : filter (/= Shell) opts}

        prog <- liftIO $ if isFSATrace params then copyFSABinary prog else pure prog
        let realCmdline = unwords $ prog : args
        if not isWindows then
            call params{prog = "/bin/sh", args = ["-c",realCmdline]}
        else
            -- On Windows the Haskell behaviour isn't that clean and is very fragile, so we try and do better.
            runWithTempDir $ \dir -> do
                let file = dir </> "s.bat"
                writeFile' file realCmdline
                call params{prog = "cmd.exe", args = ["/d/q/c",file]}
    | otherwise = call params


---------------------------------------------------------------------
-- DEAL WITH FSATrace

isFSATrace :: Params -> Bool
isFSATrace Params{..} = any isResultFSATrace  results || any isFSAOptions opts

-- Mac disables tracing on system binaries, so we copy them over, yurk
copyFSABinary :: FilePath -> IO FilePath
copyFSABinary prog
    | not isMac = pure prog
    | otherwise = do
        progFull <- findExecutable prog
        case progFull of
            Just x | any (`isPrefixOf` x) ["/bin/","/usr/","/sbin/"] -> do
                -- The file is one of the ones we can't trace, so we make a copy of it in $TMP and run that
                -- We deliberately don't clean up this directory, since otherwise we spend all our time copying binaries over
                tmpdir <- getTemporaryDirectory
                let fake = tmpdir </> "fsatrace-fakes" ++ x -- x is absolute, so must use ++
                unlessM (doesFileExist fake) $ do
                    createDirectoryRecursive $ takeDirectory fake
                    copyFile x fake
                pure fake
            _ -> pure prog

removeOptionFSATrace
    :: MonadTempDir m
    => Params -- ^ Given the parameter
    -> (Params -> m [Result]) -- ^ Call with the revised params, program name and command line
    -> m [Result]
removeOptionFSATrace params@Params{..} call
    | not $ isFSATrace params = call params
    | ResultProcess PID0 `elem` results =
        -- This is a bad state to get into, you could technically just ignore the tracing, but that's a bit dangerous
        liftIO $ errorIO "Asyncronous process execution combined with FSATrace is not support"
    | otherwise = runWithTempFile $ \file -> do
        liftIO $ writeFile file "" -- ensures even if we fail before fsatrace opens the file, we can still read it
        params <- liftIO $ fsaParams file params
        res <- call params{opts = UserCommand (showCommandForUser2 prog args) : filter (not . isFSAOptions) opts}
        fsaResBS <- liftIO $ parseFSA <$> BS.readFile file
        let fsaRes = map (fmap UTF8.toString) fsaResBS
        pure $ flip map res $ \case
            ResultFSATrace [] -> ResultFSATrace fsaRes
            ResultFSATraceBS [] -> ResultFSATraceBS fsaResBS
            x -> x
    where
        fsaFlags = lastDef "rwmdqt" [x | FSAOptions x <- opts]

        fsaParams file Params{..} = do
            prog <- copyFSABinary prog
            pure params{prog = "fsatrace", args = fsaFlags : file : "--" : prog : args }


isFSAOptions FSAOptions{} = True
isFSAOptions _ = False

isResultFSATrace ResultFSATrace{} = True
isResultFSATrace ResultFSATraceBS{} = True
isResultFSATrace _ = False

addFSAOptions :: String -> [CmdOption] -> [CmdOption]
addFSAOptions x opts | any isFSAOptions opts = map f opts
    where f (FSAOptions y) = FSAOptions $ nubOrd $ y ++ x
          f x = x
addFSAOptions x opts = FSAOptions x : opts


-- | The results produced by @fsatrace@. All files will be absolute paths.
--   You can get the results for a 'cmd' by requesting a value of type
--   @['FSATrace']@.
data FSATrace a
    = -- | Writing to a file
      FSAWrite a
    | -- | Reading from a file
      FSARead a
    | -- | Deleting a file
      FSADelete a
    | -- | Moving, arguments destination, then source
      FSAMove a a
    | -- | Querying\/stat on a file
      FSAQuery a
    | -- | Touching a file
      FSATouch a
      deriving (Show,Eq,Ord,Data,Typeable,Functor)


-- | Parse the 'FSATrace' entries, ignoring anything you don't understand.
parseFSA :: BS.ByteString -> [FSATrace BS.ByteString]
parseFSA = mapMaybe (f . dropR) . BS.lines
    where
        -- deal with CRLF on Windows
        dropR x = case BS.unsnoc x of
            Just (x, '\r') -> x
            _ -> x

        f x
            | Just (k, x) <- BS.uncons x
            , Just ('|', x) <- BS.uncons x =
                case k of
                    'w' -> Just $ FSAWrite x
                    'r' -> Just $ FSARead  x
                    'd' -> Just $ FSADelete x
                    'm' | (xs, ys) <- BS.break (== '|') x, Just ('|',ys) <- BS.uncons ys ->
                        Just $ FSAMove xs ys
                    'q' -> Just $ FSAQuery x
                    't' -> Just $ FSATouch x
                    _ -> Nothing
            | otherwise = Nothing


---------------------------------------------------------------------
-- ACTION EXPLICIT OPERATION

-- | Given explicit operations, apply the Action ones, like skip/trace/track/autodep
commandExplicitAction :: Partial => Params -> Action [Result]
commandExplicitAction oparams = do
    ShakeOptions{shakeCommandOptions,shakeRunCommands,shakeLint,shakeLintInside} <- getShakeOptions
    params@Params{..}<- pure $ oparams{opts = shakeCommandOptions ++ opts oparams}

    let skipper act = if null results && not shakeRunCommands then pure [] else act

    let verboser act = do
            let cwd = listToMaybe $ reverse [x | Cwd x <- opts]
            putVerbose $
                maybe "" (\x -> "cd " ++ x ++ "; ") cwd ++
                lastDef (showCommandForUser2 prog args) [x | UserCommand x <- opts]
            verb <- getVerbosity
            -- run quietly to suppress the tracer (don't want to print twice)
            (if verb >= Verbose then quietly else id) act

    let tracer act = do
            -- note: use the oparams - find a good tracing before munging it for shell stuff
            let msg = lastDef (defaultTraced oparams) [x | Traced x <- opts]
            if msg == "" then liftIO act else traced msg act

    let async = ResultProcess PID0 `elem` results
    let tracker act
            | AutoDeps `elem` opts = if async then liftIO $ errorIO "Can't use AutoDeps and asyncronous execution" else autodeps act
            | shakeLint == Just LintFSATrace && not async = fsalint act
            | otherwise = act params

        autodeps act = do
            ResultFSATrace pxs : res <- act params{opts = addFSAOptions "rwm" opts, results = ResultFSATrace [] : results}
            let written = Set.fromList $ [x | FSAMove x _ <- pxs] ++ [x | FSAWrite x <- pxs]
            -- If something both reads and writes to a file, it isn't eligible to be an autodeps
            xs <- liftIO $ filterM doesFileExist [x | FSARead x <- pxs, not $ x `Set.member` written]
            cwd <- liftIO getCurrentDirectory
            temp <- fixPaths cwd xs
            unsafeAllowApply $ need temp
            pure res

        fixPaths cwd xs = liftIO $ do
            xs<- pure $ map toStandard xs
            xs<- pure $ filter (\x -> any (`isPrefixOf` x) shakeLintInside) xs
            mapM (\x -> fromMaybe x <$> makeRelativeEx cwd x) xs

        fsalint act = do
            ResultFSATrace xs : res <- act params{opts = addFSAOptions "rwm" opts, results = ResultFSATrace [] : results}
            let reader (FSARead x) = Just x; reader _ = Nothing
                writer (FSAWrite x) = Just x; writer (FSAMove x _) = Just x; writer _ = Nothing
                existing f = liftIO . filterM doesFileExist . nubOrd . mapMaybe f
            cwd <- liftIO getCurrentDirectory
            trackRead  =<< fixPaths cwd =<< existing reader xs
            trackWrite =<< fixPaths cwd =<< existing writer xs
            pure res

    skipper $ tracker $ \params -> verboser $ tracer $ commandExplicitIO params


defaultTraced :: Params -> String
defaultTraced Params{..} = takeBaseName $ if Shell `elem` opts then fst (word1 prog) else prog


---------------------------------------------------------------------
-- IO EXPLICIT OPERATION

-- | Given a very explicit set of CmdOption, translate them to a General.Process structure
commandExplicitIO :: Partial => Params -> IO [Result]
commandExplicitIO params = removeOptionShell params $ \params -> removeOptionFSATrace params $ \Params{..} -> do
    let (grabStdout, grabStderr) = both or $ unzip $ flip map results $ \case
            ResultStdout{} -> (True, False)
            ResultStderr{} -> (False, True)
            ResultStdouterr{} -> (True, True)
            _ -> (False, False)

    optEnv <- resolveEnv opts
    let optCwd = mergeCwd [x | Cwd x <- opts]
    let optStdin = flip mapMaybe opts $ \case
            Stdin x -> Just $ SrcString x
            StdinBS x -> Just $ SrcBytes x
            FileStdin x -> Just $ SrcFile x
            InheritStdin -> Just SrcInherit
            _ -> Nothing
    let optBinary = BinaryPipes `elem` opts
    let optAsync = ResultProcess PID0 `elem` results
    let optTimeout = listToMaybe $ reverse [x | Timeout x <- opts]
    let optWithStdout = lastDef False [x | WithStdout x <- opts]
    let optWithStderr = lastDef True [x | WithStderr x <- opts]
    let optFileStdout = [x | FileStdout x <- opts]
    let optFileStderr = [x | FileStderr x <- opts]
    let optEchoStdout = lastDef (not grabStdout && null optFileStdout) [x | EchoStdout x <- opts]
    let optEchoStderr = lastDef (not grabStderr && null optFileStderr) [x | EchoStderr x <- opts]
    let optRealCommand = showCommandForUser2 prog args
    let optUserCommand = lastDef optRealCommand [x | UserCommand x <- opts]
    let optCloseFds = CloseFileHandles `elem` opts
    let optProcessGroup = NoProcessGroup `notElem` opts

    let bufLBS f = do (a,b) <- buf $ LBS LBS.empty; pure (a, (\(LBS x) -> f x) <$> b)
        buf Str{} | optBinary = bufLBS (Str . LBS.unpack)
        buf Str{} = do x <- newBuffer; pure ([DestString x | not optAsync], Str . concat <$> readBuffer x)
        buf LBS{} = do x <- newBuffer; pure ([DestBytes x | not optAsync], LBS . LBS.fromChunks <$> readBuffer x)
        buf BS {} = bufLBS (BS . BS.concat . LBS.toChunks)
        buf Unit  = pure ([], pure Unit)
    (dStdout, dStderr, resultBuild) :: ([[Destination]], [[Destination]], [Double -> ProcessHandle -> ExitCode -> IO Result]) <-
        fmap unzip3 $ forM results $ \case
            ResultCode _ -> pure ([], [], \_ _ ex -> pure $ ResultCode ex)
            ResultTime _ -> pure ([], [], \dur _ _ -> pure $ ResultTime dur)
            ResultLine _ -> pure ([], [], \_ _ _ -> pure $ ResultLine optUserCommand)
            ResultProcess _ -> pure ([], [], \_ pid _ -> pure $ ResultProcess $ PID pid)
            ResultStdout    s -> do (a,b) <- buf s; pure (a , [], \_ _ _ -> fmap ResultStdout b)
            ResultStderr    s -> do (a,b) <- buf s; pure ([], a , \_ _ _ -> fmap ResultStderr b)
            ResultStdouterr s -> do (a,b) <- buf s; pure (a , a , \_ _ _ -> fmap ResultStdouterr b)
            ResultFSATrace _ -> pure ([], [], \_ _ _ -> pure $ ResultFSATrace []) -- filled in elsewhere
            ResultFSATraceBS _ -> pure ([], [], \_ _ _ -> pure $ ResultFSATraceBS []) -- filled in elsewhere

    exceptionBuffer <- newBuffer
    po <- resolvePath ProcessOpts
        {poCommand = RawCommand prog args
        ,poCwd = optCwd, poEnv = optEnv, poTimeout = optTimeout
        ,poStdin = [SrcBytes LBS.empty | optBinary && not (null optStdin)] ++ optStdin
        ,poStdout = [DestEcho | optEchoStdout] ++ map DestFile optFileStdout ++ [DestString exceptionBuffer | optWithStdout && not optAsync] ++ concat dStdout
        ,poStderr = [DestEcho | optEchoStderr] ++ map DestFile optFileStderr ++ [DestString exceptionBuffer | optWithStderr && not optAsync] ++ concat dStderr
        ,poAsync = optAsync
        ,poCloseFds = optCloseFds
        ,poGroup = optProcessGroup
        }
    (dur,(pid,exit)) <- duration $ process po
    if exit == ExitSuccess || ResultCode ExitSuccess `elem` results then
        mapM (\f -> f dur pid exit) resultBuild
     else do
        exceptionBuffer <- readBuffer exceptionBuffer
        let captured = ["Stderr" | optWithStderr] ++ ["Stdout" | optWithStdout]
        cwd <- case optCwd of
            Nothing -> pure ""
            Just v -> do
                v <- canonicalizePath v `catchIO` const (pure v)
                pure $ "Current directory: " ++ v ++ "\n"
        liftIO $ errorIO $
            "Development.Shake." ++ funcName ++ ", system command failed\n" ++
            "Command line: " ++ optRealCommand ++ "\n" ++
            (if optRealCommand /= optUserCommand then "Original command line: " ++ optUserCommand ++ "\n" else "") ++
            cwd ++
            "Exit code: " ++ show (case exit of ExitFailure i -> i; _ -> 0) ++ "\n" ++
            if null captured then "Stderr not captured because WithStderr False was used\n"
            else if null exceptionBuffer then intercalate " and " captured ++ " " ++ (if length captured == 1 then "was" else "were") ++ " empty"
            else intercalate " and " captured ++ ":\n" ++ unlines (dropWhile null $ lines $ concat exceptionBuffer)


mergeCwd :: [FilePath] -> Maybe FilePath
mergeCwd [] = Nothing
mergeCwd xs = Just $ foldl1 (</>) xs

-- | Apply all environment operations, to produce a new environment to use.
resolveEnv :: [CmdOption] -> IO (Maybe [(String, String)])
resolveEnv opts
    | null env, null addEnv, null addPath, null remEnv = pure Nothing
    | otherwise = Just . unique . tweakPath . (++ addEnv) . filter (flip notElem remEnv . fst) <$>
                  if null env then getEnvironment else pure (concat env)
    where
        env = [x | Env x <- opts]
        addEnv = [(x,y) | AddEnv x y <- opts]
        remEnv = [x | RemEnv x <- opts]
        addPath = [(x,y) | AddPath x y <- opts]

        newPath mid = intercalate [searchPathSeparator] $
            concat (reverse $ map fst addPath) ++ [mid | mid /= ""] ++ concatMap snd addPath
        isPath x = (if isWindows then upper else id) x == "PATH"
        tweakPath xs | not $ any (isPath . fst) xs = ("PATH", newPath "") : xs
                     | otherwise = map (\(a,b) -> (a, if isPath a then newPath b else b)) xs

        unique = reverse . nubOrdOn (if isWindows then upper . fst else fst) . reverse


-- | If the user specifies a custom $PATH, and not Shell, then try and resolve their prog ourselves.
--   Tricky, because on Windows it doesn't look in the $PATH first.
resolvePath :: ProcessOpts -> IO ProcessOpts
resolvePath po
    | Just e <- poEnv po
    , Just (_, path) <- find ((==) "PATH" . (if isWindows then upper else id) . fst) e
    , RawCommand prog args <- poCommand po
    = do
    let progExe = if prog == prog -<.> exe then prog else prog <.> exe
    -- use unsafeInterleaveIO to allow laziness to skip the queries we don't use
    pathOld <- unsafeInterleaveIO $ fromMaybe "" <$> lookupEnv "PATH"
    old <- unsafeInterleaveIO $ findExecutable prog
    new <- unsafeInterleaveIO $ findExecutableWith (splitSearchPath path) progExe
    old2 <- unsafeInterleaveIO $ findExecutableWith (splitSearchPath pathOld) progExe

    switch<- pure $ case () of
        _ | path == pathOld -> False -- The state I can see hasn't changed
          | Nothing <- new -> False -- I have nothing to offer
          | Nothing <- old -> True -- I failed last time, so this must be an improvement
          | Just old <- old, Just new <- new, equalFilePath old new -> False -- no different
          | Just old <- old, Just old2 <- old2, equalFilePath old old2 -> True -- I could predict last time
          | otherwise -> False
    pure $ case new of
        Just new | switch -> po{poCommand = RawCommand new args}
        _ -> po
resolvePath po = pure po


-- | Given a list of directories, and a file name, return the complete path if you can find it.
--   Like findExecutable, but with a custom PATH.
findExecutableWith :: [FilePath] -> String -> IO (Maybe FilePath)
findExecutableWith path x = flip firstJustM (map (</> x) path) $ \s ->
    ifM (doesFileExist s) (pure $ Just s) (pure Nothing)


---------------------------------------------------------------------
-- FIXED ARGUMENT WRAPPER

-- | Collect the @stdout@ of the process.
--   If used, the @stdout@ will not be echoed to the terminal, unless you include 'EchoStdout'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
--
--   Note that most programs end their output with a trailing newline, so calling
--   @ghc --numeric-version@ will result in 'Stdout' of @\"6.8.3\\n\"@. If you want to automatically
--   trim the resulting string, see 'StdoutTrim'.
newtype Stdout a = Stdout {fromStdout :: a}

-- | Like 'Stdout' but remove all leading and trailing whitespaces.
newtype StdoutTrim a = StdoutTrim {fromStdoutTrim :: a}

-- | Collect the @stderr@ of the process.
--   If used, the @stderr@ will not be echoed to the terminal, unless you include 'EchoStderr'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
newtype Stderr a = Stderr {fromStderr :: a}

-- | Collect the @stdout@ and @stderr@ of the process.
--   If used, the @stderr@ and @stdout@ will not be echoed to the terminal, unless you include 'EchoStdout' and 'EchoStderr'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
newtype Stdouterr a = Stdouterr {fromStdouterr :: a}

-- | Collect the 'ExitCode' of the process.
--   If you do not collect the exit code, any 'ExitFailure' will cause an exception.
newtype Exit = Exit {fromExit :: ExitCode}

-- | Collect the 'ProcessHandle' of the process.
--   If you do collect the process handle, the command will run asyncronously and the call to 'cmd' \/ 'command'
--   will return as soon as the process is spawned. Any 'Stdout' \/ 'Stderr' captures will return empty strings.
newtype Process = Process {fromProcess :: ProcessHandle}

-- | Collect the time taken to execute the process. Can be used in conjunction with 'CmdLine' to
--   write helper functions that print out the time of a result.
--
-- @
-- timer :: ('CmdResult' r, MonadIO m) => (forall r . 'CmdResult' r => m r) -> m r
-- timer act = do
--     ('CmdTime' t, 'CmdLine' x, r) <- act
--     liftIO $ putStrLn $ \"Command \" ++ x ++ \" took \" ++ show t ++ \" seconds\"
--     pure r
--
-- run :: IO ()
-- run = timer $ 'cmd' \"ghc --version\"
-- @
newtype CmdTime = CmdTime {fromCmdTime :: Double}

-- | Collect the command line used for the process. This command line will be approximate -
--   suitable for user diagnostics, but not for direct execution.
newtype CmdLine = CmdLine {fromCmdLine :: String}

-- | The allowable 'String'-like values that can be captured.
class CmdString a where cmdString :: (Str, Str -> a)
instance CmdString () where cmdString = (Unit, \Unit -> ())
instance CmdString String where cmdString = (Str "", \(Str x) -> x)
instance CmdString BS.ByteString where cmdString = (BS BS.empty, \(BS x) -> x)
instance CmdString LBS.ByteString where cmdString = (LBS LBS.empty, \(LBS x) -> x)


class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)


-- | A class for specifying what results you want to collect from a process.
--   Values are formed of 'Stdout', 'Stderr', 'Exit' and tuples of those.
class CmdResult a where
    -- Return a list of results (with the right type but dummy data)
    -- and a function to transform a populated set of results into a value
    cmdResult :: ([Result], [Result] -> a)

instance CmdResult Exit where
    cmdResult = ([ResultCode ExitSuccess], \[ResultCode x] -> Exit x)

instance CmdResult ExitCode where
    cmdResult = ([ResultCode ExitSuccess], \[ResultCode x] -> x)

instance CmdResult Process where
    cmdResult = ([ResultProcess PID0], \[ResultProcess (PID x)] -> Process x)

instance CmdResult ProcessHandle where
    cmdResult = ([ResultProcess PID0], \[ResultProcess (PID x)] -> x)

instance CmdResult CmdLine where
    cmdResult = ([ResultLine ""], \[ResultLine x] -> CmdLine x)

instance CmdResult CmdTime where
    cmdResult = ([ResultTime 0], \[ResultTime x] -> CmdTime x)

instance CmdResult [FSATrace FilePath] where
    cmdResult = ([ResultFSATrace []], \[ResultFSATrace x] -> x)

instance CmdResult [FSATrace BS.ByteString] where
    cmdResult = ([ResultFSATraceBS []], \[ResultFSATraceBS x] -> x)

instance CmdString a => CmdResult (Stdout a) where
    cmdResult = let (a,b) = cmdString in ([ResultStdout a], \[ResultStdout x] -> Stdout $ b x)

instance CmdString a => CmdResult (StdoutTrim a) where
    cmdResult = let (a,b) = cmdString in ([ResultStdout a], \[ResultStdout x] -> StdoutTrim $ b $ strTrim x)

instance CmdString a => CmdResult (Stderr a) where
    cmdResult = let (a,b) = cmdString in ([ResultStderr a], \[ResultStderr x] -> Stderr $ b x)

instance CmdString a => CmdResult (Stdouterr a) where
    cmdResult = let (a,b) = cmdString in ([ResultStdouterr a], \[ResultStdouterr x] -> Stdouterr $ b x)

instance CmdResult () where
    cmdResult = ([], \[] -> ())

instance (CmdResult x1, CmdResult x2) => CmdResult (x1,x2) where
    cmdResult = (a1++a2, \rs -> let (r1,r2) = splitAt (length a1) rs in (b1 r1, b2 r2))
        where (a1,b1) = cmdResult
              (a2,b2) = cmdResult

cmdResultWith :: forall b c. CmdResult b => (b -> c) -> ([Result], [Result] -> c)
cmdResultWith f = second (f .) cmdResult

instance (CmdResult x1, CmdResult x2, CmdResult x3) => CmdResult (x1,x2,x3) where
    cmdResult = cmdResultWith $ \(a,(b,c)) -> (a,b,c)

instance (CmdResult x1, CmdResult x2, CmdResult x3, CmdResult x4) => CmdResult (x1,x2,x3,x4) where
    cmdResult = cmdResultWith $ \(a,(b,c,d)) -> (a,b,c,d)

instance (CmdResult x1, CmdResult x2, CmdResult x3, CmdResult x4, CmdResult x5) => CmdResult (x1,x2,x3,x4,x5) where
    cmdResult = cmdResultWith $ \(a,(b,c,d,e)) -> (a,b,c,d,e)


-- | Execute a system command. Before running 'command' make sure you 'Development.Shake.need' any files
--   that are used by the command.
--
--   This function takes a list of options (often just @[]@, see 'CmdOption' for the available
--   options), the name of the executable (either a full name, or a program on the @$PATH@) and
--   a list of arguments. The result is often @()@, but can be a tuple containg any of 'Stdout',
--   'Stderr' and 'Exit'. Some examples:
--
-- @
-- 'command_' [] \"gcc\" [\"-c\",\"myfile.c\"]                          -- compile a file, throwing an exception on failure
-- 'Exit' c <- 'command' [] \"gcc\" [\"-c\",myfile]                     -- run a command, recording the exit code
-- ('Exit' c, 'Stderr' err) <- 'command' [] \"gcc\" [\"-c\",\"myfile.c\"]   -- run a command, recording the exit code and error output
-- 'Stdout' out <- 'command' [] \"gcc\" [\"-MM\",\"myfile.c\"]            -- run a command, recording the output
-- 'command_' ['Cwd' \"generated\"] \"gcc\" [\"-c\",myfile]               -- run a command in a directory
-- @
--
--   Unless you retrieve the 'ExitCode' using 'Exit', any 'ExitFailure' will throw an error, including
--   the 'Stderr' in the exception message. If you capture the 'Stdout' or 'Stderr', that stream will not be echoed to the console,
--   unless you use the option 'EchoStdout' or 'EchoStderr'.
--
--   If you use 'command' inside a @do@ block and do not use the result, you may get a compile-time error about being
--   unable to deduce 'CmdResult'. To avoid this error, use 'command_'.
--
--   By default the @stderr@ stream will be captured for use in error messages, and also echoed. To only echo
--   pass @'WithStderr' 'False'@, which causes no streams to be captured by Shake, and certain programs (e.g. @gcc@)
--   to detect they are running in a terminal.
command :: (Partial, CmdResult r) => [CmdOption] -> String -> [String] -> Action r
command opts x xs = withFrozenCallStack $ b <$> commandExplicitAction (Params "command" opts a x xs)
    where (a,b) = cmdResult

-- | A version of 'command' where you do not require any results, used to avoid errors about being unable
--   to deduce 'CmdResult'.
command_ :: Partial => [CmdOption] -> String -> [String] -> Action ()
command_ opts x xs = withFrozenCallStack $ void $ commandExplicitAction (Params "command_" opts [] x xs)


---------------------------------------------------------------------
-- VARIABLE ARGUMENT WRAPPER

-- | A type annotation, equivalent to the first argument, but in variable argument contexts,
--   gives a clue as to what return type is expected (not actually enforced).
type a :-> t = a


-- | Build or execute a system command. Before using 'cmd' to run a command, make sure you 'Development.Shake.need' any files
--   that are used by the command.
--
-- * @String@ arguments are treated as a list of whitespace separated arguments.
--
-- * @[String]@ arguments are treated as a list of literal arguments.
--
-- * 'CmdOption' arguments are used as options.
--
-- * 'CmdArgument' arguments, which can be built by 'cmd' itself, are spliced into the containing command.
--
--   Typically only string literals should be passed as @String@ arguments. When using variables
--   prefer @[myvar]@ so that if @myvar@ contains spaces they are properly escaped.
--
--   As some examples, here are some calls, and the resulting command string:
--
-- @
-- 'cmd_' \"git log --pretty=\" \"oneline\"           -- git log --pretty= oneline
-- 'cmd_' \"git log --pretty=\" [\"oneline\"]         -- git log --pretty= oneline
-- 'cmd_' \"git log\" (\"--pretty=\" ++ \"oneline\")    -- git log --pretty=oneline
-- 'cmd_' \"git log\" (\"--pretty=\" ++ \"one line\")   -- git log --pretty=one line
-- 'cmd_' \"git log\" [\"--pretty=\" ++ \"one line\"]   -- git log "--pretty=one line"
-- @
--
--   More examples, including return values, see this translation of the examples given for the 'command' function:
--
-- @
-- 'cmd_' \"gcc -c myfile.c\"                                       -- compile a file, throwing an exception on failure
-- 'Exit' c <- 'cmd' \"gcc -c\" [myfile]                              -- run a command, recording the exit code
-- ('Exit' c, 'Stderr' err) <- 'cmd' \"gcc -c myfile.c\"                -- run a command, recording the exit code and error output
-- 'Stdout' out <- 'cmd' \"gcc -MM myfile.c\"                         -- run a command, recording the output
-- 'cmd' ('Cwd' \"generated\") \"gcc -c\" [myfile] :: 'Action' ()         -- run a command in a directory
--
-- let gccCommand = 'cmd' \"gcc -c\" :: 'CmdArgument'                 -- build a sub-command. 'cmd' can return 'CmdArgument' values as well as execute commands
-- cmd ('Cwd' \"generated\") gccCommand [myfile]                 -- splice that command into a greater command
-- @
--
--   If you use 'cmd' inside a @do@ block and do not use the result, you may get a compile-time error about being
--   unable to deduce 'CmdResult'. To avoid this error, use 'cmd_'. If you enable @OverloadedStrings@ or @OverloadedLists@
--   you may have to give type signatures to the arguments, or use the more constrained 'command' instead.
--
--   The 'cmd' function can also be run in the 'IO' monad, but then 'Traced' is ignored and command lines are not echoed.
--   As an example:
--
-- @
-- 'cmd' ('Cwd' \"generated\") 'Shell' \"gcc -c myfile.c\" :: IO ()
-- @
cmd :: (Partial, CmdArguments args) => args :-> Action r
cmd = withFrozenCallStack $ cmdArguments mempty

-- | See 'cmd'. Same as 'cmd' except with a unit result.
-- 'cmd' is to 'cmd_' as 'command' is to 'command_'.
cmd_ :: (Partial, CmdArguments args, Unit args) => args :-> Action ()
cmd_ = withFrozenCallStack cmd

-- | The arguments to 'cmd' - see 'cmd' for examples and semantics.
newtype CmdArgument = CmdArgument [Either CmdOption String]
  deriving (Eq, Semigroup, Monoid, Show)

-- | The arguments to 'cmd' - see 'cmd' for examples and semantics.
class CmdArguments t where
    -- | Arguments to cmd
    cmdArguments :: Partial => CmdArgument -> t
instance (IsCmdArgument a, CmdArguments r) => CmdArguments (a -> r) where
    cmdArguments xs x = cmdArguments $ xs `mappend` toCmdArgument x
instance CmdResult r => CmdArguments (Action r) where
    cmdArguments (CmdArgument x) = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in b <$> commandExplicitAction (Params "cmd" opts a x xs)
        _ -> error "Error, no executable or arguments given to Development.Shake.cmd"
instance CmdResult r => CmdArguments (IO r) where
    cmdArguments (CmdArgument x) = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in b <$> commandExplicitIO (Params "cmd" opts a x xs)
        _ -> error "Error, no executable or arguments given to Development.Shake.cmd"
instance CmdArguments CmdArgument where
    cmdArguments = id

-- | Class to convert an a  to a CmdArgument
class IsCmdArgument a where
    -- | Conversion to a CmdArgument
    toCmdArgument :: a -> CmdArgument
instance IsCmdArgument () where toCmdArgument = mempty
instance IsCmdArgument String where toCmdArgument = CmdArgument . map Right . words
instance IsCmdArgument [String] where toCmdArgument = CmdArgument . map Right
instance IsCmdArgument (NonEmpty String) where toCmdArgument = toCmdArgument . toList
instance IsCmdArgument CmdOption where toCmdArgument = CmdArgument . pure . Left
instance IsCmdArgument [CmdOption] where toCmdArgument = CmdArgument . map Left
instance IsCmdArgument CmdArgument where toCmdArgument = id
instance IsCmdArgument a => IsCmdArgument (Maybe a) where toCmdArgument = maybe mempty toCmdArgument


---------------------------------------------------------------------
-- UTILITIES

-- A better version of showCommandForUser, which doesn't escape so much on Windows
showCommandForUser2 :: FilePath -> [String] -> String
showCommandForUser2 cmd args = unwords $ map (\x -> if safe x then x else showCommandForUser x []) $ cmd : args
    where
        safe xs = xs /= "" && not (any bad xs)
        bad x = isSpace x || (x == '\\' && not isWindows) || x `elem` ("\"\'" :: String)
