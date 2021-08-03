{-# LANGUAGE ScopedTypeVariables, MultiWayIf, TupleSections #-}
{-
  The GHCJS-specific parts of the frontend (ghcjs program)

  Our main frontend is copied from GHC, Compiler.Program
 -}

module Compiler.GhcjsProgram where

import           GHC
import           GhcMonad
import           DynFlags
import           ErrUtils (fatalErrorMsg'')
import           Panic (handleGhcException)
import           Exception
import           Packages (initPackages)
import Prelude

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.ByteString as B
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import           Distribution.System (buildOS, OS(..), buildPlatform)
import           Distribution.Verbosity (deafening, intToVerbosity)
import           Distribution.Simple.BuildPaths (exeExtension)
import           Distribution.Simple.Utils (installExecutableFile, installDirectoryContents)
import           Distribution.Simple.Program (runProgramInvocation, simpleProgramInvocation)

import           Options.Applicative

import           System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

import           Compiler.Compat
import           Compiler.GhcjsPlatform
import           Compiler.Info
import           Compiler.Settings
import           Compiler.Utils
import qualified Compiler.Platform as Platform

-- fixme, make frontend independent of backend
import qualified Gen2.Object      as Object
import qualified Gen2.ClosureInfo as Gen2
import qualified Gen2.Shim        as Gen2
import qualified Gen2.Rts         as Gen2
import qualified Gen2.TH          as Gen2

getGhcjsSettings :: [Located String] -> IO ([Located String], GhcjsSettings)
getGhcjsSettings args =
  case p of
    Failure failure -> do
      let (msg, code) = renderFailure failure "ghcjs"
      hPutStrLn stderr msg
      exitWith code
    Success gs1 -> do
      gs2 <- envSettings
      return (args', gs1 <> gs2)
    CompletionInvoked _ -> exitWith (ExitFailure 1)
  where
    (ga,args') = partArgs args -- partition (\a -> any (`isPrefixOf` unLoc a) as) args
    p = execParserPure (prefs mempty) optParser' ga
    partArgs :: [Located String] -> ([String], [Located String])
    partArgs [] = ([],[])
    partArgs (x:xs)
      | unLoc x `elem` ghcjsFlags =
       let (g,o) = partArgs xs in (('-':unLoc x):g,o)
    partArgs (x1:x2:xs) | unLoc x1 `elem` ghcjsOpts =
       let (g,o) = partArgs xs in (('-':unLoc x1++"="++unLoc x2):g,o)
    partArgs (x:xs) =
       let (g,o) = partArgs xs in (g,x:o)

    ghcjsFlags = [ "-native-executables"
                 , "-native-too"
                 , "-build-runner"
                 , "-no-js-executables"
                 , "-only-out"
                 , "-no-rts"
                 , "-no-stats"
                 , "-dedupe"
                 ]
    ghcjsOpts = [ "-strip-program"
                , "-log-commandline"
                , "-with-ghc"
                , "-generate-base"
                , "-use-base"
                , "-link-js-lib"
                , "-js-lib-outputdir"
                , "-js-lib-src"
                ]
    envSettings = GhcjsSettings <$> getEnvOpt "GHCJS_NATIVE_EXECUTABLES"
                                <*> getEnvOpt "GHCJS_NATIVE_TOO"
                                <*> pure False
                                <*> pure False
                                <*> pure Nothing
                                <*> getEnvMay "GHCJS_LOG_COMMANDLINE_NAME"
                                <*> getEnvMay "GHCJS_WITH_GHC"
                                <*> pure False
                                <*> pure False
                                <*> pure False
                                <*> pure Nothing
                                <*> pure NoBase
                                <*> pure Nothing
                                <*> pure Nothing
                                <*> pure []
                                <*> pure False

optParser' :: ParserInfo GhcjsSettings
optParser' = info (helper <*> optParser) fullDesc

optParser :: Parser GhcjsSettings
optParser = GhcjsSettings
            <$> switch ( long "native-executables" )
            <*> switch ( long "native-too" )
            <*> switch ( long "build-runner" )
            <*> switch ( long "no-js-executables" )
            <*> optStr ( long "strip-program" )
            <*> optStr ( long "log-commandline" )
            <*> optStr ( long "with-ghc" )
            <*> switch ( long "only-out" )
            <*> switch ( long "no-rts" )
            <*> switch ( long "no-stats" )
            <*> optStr ( long "generate-base" )
            <*> (maybe NoBase BaseFile <$> optStr ( long "use-base" ))
            <*> optStr ( long "link-js-lib" )
            <*> optStr ( long "js-lib-outputdir" )
            <*> strings ( long "js-lib-src" )
            <*> switch ( long "dedupe" )

strings :: Mod OptionFields String -> Parser [String]
strings = many <$> option str

optStr :: Mod OptionFields String -> Parser (Maybe String)
optStr = optional . option str

printVersion :: IO ()
printVersion = putStrLn $
  "The Glorious Glasgow Haskell Compilation System for JavaScript, version " ++
     getCompilerVersion ++ " (GHC " ++ getGhcCompilerVersion ++ ")"

printNumericVersion :: IO ()
printNumericVersion = putStrLn getCompilerVersion

printRts :: DynFlags -> IO ()
printRts dflags = TL.putStrLn (Gen2.rtsText dflags $ Gen2.dfCgSettings dflags) >> exitSuccess

printDeps :: FilePath -> IO ()
printDeps = Object.readDepsFile >=> TL.putStrLn . Object.showDeps

printObj :: FilePath -> IO ()
printObj = Object.readObjectFile >=> TL.putStrLn . Object.showObject

runJsProgram :: Maybe String -> [String] -> IO ()
runJsProgram Nothing _ = error noTopDirErrorMsg
runJsProgram (Just topDir) args
  | (_:script:scriptArgs) <- dropWhile (/="--run") args = do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      hSetBuffering stderr NoBuffering
      node <- T.strip <$> T.readFile (topDir </> "node")
      ph <- runProcess (T.unpack node) (script:scriptArgs) Nothing Nothing Nothing Nothing Nothing
      exitWith =<< waitForProcess ph
  | otherwise = error "usage: ghcjs --run [script] [arguments]"

-- | when booting GHCJS, we pretend to have the Cabal lib installed
--   call GHC to compile our Setup.hs
bootstrapFallback :: IO ()
bootstrapFallback = do
    ghc <- fmap (fromMaybe "ghc") $ getEnvMay "GHCJS_WITH_GHC"
    as  <- ghcArgs <$> getArgs
    e   <- rawSystem ghc $ as -- run without GHCJS library prefix arg
    case (e, getOutput as) of
      (ExitSuccess, Just o) ->
        createDirectoryIfMissing False (o <.> "jsexe")
      _ -> return ()
    exitWith e
    where
      ignoreArg a  = "-B" `isPrefixOf` a || a == "-build-runner"
      ghcArgs args = filter (not . ignoreArg) args ++ ["-threaded"]
      getOutput []         = Nothing
      getOutput ("-o":x:_) = Just x
      getOutput (_:xs)     = getOutput xs

installExecutable :: DynFlags -> GhcjsSettings -> [String] -> IO ()
installExecutable dflags settings srcs =
    case (srcs, outputFile dflags) of
        ([from], Just to) -> do
          let v = fromMaybe deafening . intToVerbosity $ verbosity dflags
          nativeExists <- doesFileExist $ from <.> exeExtension buildPlatform
          when nativeExists $ do
            installExecutableFile v (from <.> exeExtension buildPlatform) (to <.> exeExtension buildPlatform)
            let stripFlags = if buildOS == OSX then ["-x"] else []
            case gsStripProgram settings of
                Just strip -> runProgramInvocation v . simpleProgramInvocation strip $
                                stripFlags ++ [to <.> exeExtension buildPlatform]
                Nothing -> return ()
          jsExists <- doesDirectoryExist $ from <.> jsexeExtension
          when jsExists $ installDirectoryContents v (from <.> jsexeExtension) (to <.> jsexeExtension)
          unless (nativeExists || jsExists) $ do
            hPutStrLn stderr $ "No executable found to install at " ++ from
            exitFailure
        _ -> do
            hPutStrLn stderr "Usage: ghcjs --install-executable <from> -o <to>"
            exitFailure

{-
  Generate lib.js and lib1.js for the latest version of all installed
  packages

  fixme: make this variant-aware?
 -}

generateLib :: GhcjsSettings -> Ghc ()
generateLib _settings = do
  dflags1 <- getSessionDynFlags
  liftIO $ do
    (dflags2, pkgs0) <- initPackages dflags1
    let pkgs = mapMaybe (\p -> fmap (T.pack (getInstalledPackageName dflags2 p),)
                                    (getInstalledPackageVersion dflags2 p))
                        pkgs0
        base = getDataDir (getLibDir dflags2) </> "shims"
        pkgs' :: [(T.Text, Version)]
        pkgs' = M.toList $ M.fromListWith max pkgs
    (beforeFiles, afterFiles) <- Gen2.collectShims base pkgs'
    B.writeFile "lib.js"  . mconcat =<< mapM B.readFile beforeFiles
    B.writeFile "lib1.js" . mconcat =<< mapM B.readFile afterFiles
    putStrLn "generated lib.js and lib1.js for:"
    mapM_ (\(p,v) -> putStrLn $ "    " ++ T.unpack p ++
      if isEmptyVersion v then "" else ("-" ++ T.unpack (showVersion v))) pkgs'

setGhcjsSuffixes :: Bool     -- oneshot option, -c
                 -> DynFlags
                 -> DynFlags
setGhcjsSuffixes oneshot df = df
    { objectSuf     = mkGhcjsSuf (objectSuf df)
    , dynObjectSuf  = mkGhcjsSuf (dynObjectSuf df)
    , hiSuf         = mkGhcjsSuf (hiSuf df)
    , dynHiSuf      = mkGhcjsSuf (dynHiSuf df)
    , outputFile    = fmap mkGhcjsOutput (outputFile df)
    , dynOutputFile = fmap mkGhcjsOutput (dynOutputFile df)
    , outputHi      = fmap mkGhcjsOutput (outputHi df)
    , ghcLink       = if oneshot then NoLink else ghcLink df
    }


-- | make sure we don't show panic messages with the "report GHC bug" text, since
--   those are probably our fault.
ghcjsErrorHandler :: (ExceptionMonad m, MonadIO m)
                    => FatalMessager -> FlushOut -> m a -> m a
ghcjsErrorHandler fm (FlushOut flushOut) inner =
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  ghandle (\exception -> liftIO $ do
           flushOut
           case fromException exception of
                -- an IO exception probably isn't our fault, so don't panic
                Just (ioe :: IOException) ->
                  fatalErrorMsg'' fm (show ioe)
                _ -> case fromException exception of
                     Just UserInterrupt ->
                         -- Important to let this one propagate out so our
                         -- calling process knows we were interrupted by ^C
                         liftIO $ throwIO UserInterrupt
                     Just StackOverflow ->
                         fatalErrorMsg'' fm "stack overflow: use +RTS -K<size> to increase it"
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> liftIO $ throwIO ex
                          _ -> fatalErrorMsg'' fm (ghcjsShowException exception)
           exitWith (ExitFailure 1)
         ) $

  -- error messages propagated as exceptions
  handleGhcException
            (\ge -> liftIO $ do
                flushOut
                case ge of
                     Signal _ -> exitWith (ExitFailure 1)
                     _ -> do fatalErrorMsg'' fm (ghcjsShowException $ toException ge)
                             exitWith (ExitFailure 1)
            ) $
  inner

ghcjsShowException :: SomeException -> String
ghcjsShowException e = case fromException e of
  Just (PprPanic s _) -> ghcjsShowException $ toException (Panic (s ++ "\n<<details unavailable>>"))
  Just (Panic s) -> "panic! (the 'impossible' happened)\n"
                ++ "  (GHCJS version " ++ getCompilerVersion ++ ", GHC version " ++ getGhcCompilerVersion ++ "):\n\t"
                ++ s ++ "\n\n"
                ++ "Please report this as a GHCJS bug:  https://github.com/ghcjs/ghcjs/issues\n"
  Just (PprSorry s _) -> ghcjsShowException $ toException (Sorry (s ++ "\n<<details unavailable>>"))
  Just (Sorry s) -> "sorry! (unimplemented feature or known bug)\n"
                  ++ "  (GHCJS version " ++ getCompilerVersion ++ ", GHC version " ++ getGhcCompilerVersion ++ "):\n\t"
                  ++ s ++ "\n"
  _ -> show e

ghcjsCleanupHandler :: (ExceptionMonad m, MonadIO m)
                    => DynFlags -> GhcjsEnv -> m a -> m a
ghcjsCleanupHandler dflags env inner =
      defaultCleanupHandler dflags inner `gfinally`
          (liftIO $ Gen2.finishTHAll env)

runGhcjsSession :: Maybe FilePath  -- ^ Directory with library files,
                   -- like GHC's -B argument
                -> GhcjsSettings
                -> Ghc b           -- ^ Action to perform
                -> IO b
runGhcjsSession mbMinusB settings m = runGhc mbMinusB $ do
    setupSessionForGhcjs settings
    m

-- | modifies a Ghc session to use the GHCJS target
setupSessionForGhcjs :: GhcjsSettings
                     -> Ghc ()
setupSessionForGhcjs ghcjsSettings = do
  dflags <- getSessionDynFlags
  let base = getLibDir dflags
  jsEnv <- liftIO newGhcjsEnv
  _ <- setSessionDynFlags
       $ setGhcjsPlatform ghcjsSettings jsEnv [] base
       -- $ updateWays $ addWay' (WayCustom "js")
       -- $ setGhcjsSuffixes False 
       
         dflags
  pure ()

{-|
  get the command line arguments for GHCJS by adding the ones specified in the
  ghcjs.exe.options file on Windows, since we cannot run wrapper scripts there

  also handles some location information queries for Setup.hs and ghcjs-boot,
  that would otherwise fail due to missing boot libraries or a wrapper interfering
 -}

getWrappedArgs :: IO ([String], Bool, Bool)
getWrappedArgs = do
   booting  <- getEnvOpt "GHCJS_BOOTING"        -- do not check that we're booted
   booting1 <- getEnvOpt "GHCJS_BOOTING_STAGE1" -- enable GHC fallback
   as <- getArgs
   if | "--ghcjs-setup-print"   `elem` as -> printBootInfo as >> exitSuccess
      | "--ghcjs-booting-print" `elem` as -> getArgs >>= printBootInfo >> exitSuccess
      | otherwise                         -> do
         fas <- getArgs
         return (fas, booting, booting1)

printBootInfo :: [String] -> IO ()
printBootInfo v
  | "--print-topdir"         `elem` v = putStrLn t
  | "--print-libdir"         `elem` v = putStrLn t
  | "--print-global-db"      `elem` v = putStrLn (getGlobalPackageDB t)
  | "--print-user-db-dir"    `elem` v = putStrLn . fromMaybe "<none>" =<< getUserPackageDir
  | "--numeric-ghc-version"  `elem` v = putStrLn getGhcCompilerVersion
  | "--print-rts-profiled"   `elem` v = print rtsIsProfiled
  | otherwise                         = error "no --ghcjs-setup-print or --ghcjs-booting-print options found"
  where
    t = fromMaybe (error noTopDirErrorMsg) (getArgsTopDir v)


noTopDirErrorMsg :: String
noTopDirErrorMsg = "Cannot determine library directory.\n\nGHCJS requires a -B argument to specify the library directory. " ++
  if Platform.isWindows
  then
                   "On Windows, GHCJS reads the `ghcjs.exe.options' and `ghcjs-[version].exe.options' files from the " ++
                   "program directory for extra command line arguments."
  else
                   "Usually this argument is provided added by a shell script wrapper. Verify that you are not accidentally " ++
                   "invoking the executable directly."

getArgsTopDir :: [String] -> Maybe String
getArgsTopDir xs
  | null minusB_args = Nothing
  | otherwise        = Just (drop 2 $ last minusB_args)
  where
    minusB_args = filter ("-B" `isPrefixOf`) xs

buildJsLibrary :: DynFlags -> [FilePath] -> [FilePath] -> [FilePath] -> IO ()
buildJsLibrary _dflags srcs js_objs objs = do
  print srcs
  print js_objs
  print objs
  exitFailure
