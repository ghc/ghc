{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Test.Type(
    sleep, sleepFileTime, sleepFileTimeCalibrate,
    testBuildArgs, testBuild, testSimple, testNone,
    shakeRoot,
    defaultTest, hasTracker, notCI, notWindowsCI, notMacCI,
    copyDirectoryChanged, copyFileChangedIO,
    assertWithin,
    assertBool, assertBoolIO, assertException, assertExceptionAfter,
    assertContents, assertContentsUnordered, assertContentsWords, assertContentsInfix,
    assertExists, assertMissing,
    assertTimings,
    (===),
    (&?%>),
    Pat(PatWildcard), pat,
    BinarySentinel(..), RandomType(..),
    ) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Forward
import Development.Shake.Internal.FileName
import General.Extra
import Development.Shake.Internal.FileInfo
import Development.Shake.FilePath
import Development.Shake.Internal.Paths

import Control.Exception.Extra
import Control.Monad.Extra
import Data.List.Extra
import Text.Read(readMaybe)
import Data.Maybe
import Data.Either
import Data.Typeable
import System.Directory.Extra as IO
import System.Environment
import System.Random
import General.GetOpt
import System.IO.Extra as IO
import System.Time.Extra
import System.Info.Extra


testBuildArgs
    :: (([String] -> IO ()) -> IO ()) -- ^ The test driver
    -> [OptDescr (Either String a)] -- ^ Arguments the test can accept
    -> ([a] -> Rules ()) -- ^ The Shake script under test
    -> IO () -- ^ Sleep function, driven by passing @--sleep@
    -> IO ()
testBuildArgs f opts g = shakenEx False opts f
    (\os args -> if null args then g os else want args >> withoutActions (g os))

testBuild
    :: (([String] -> IO ()) -> IO ()) -- ^ The test driver
    -> Rules () -- ^ The Shake script under test
    -> IO () -- ^ Sleep function, driven by passing @--sleep@
    -> IO ()
testBuild f g = testBuildArgs f [] (const g)

testSimple :: IO () -> IO () -> IO ()
testSimple act = testBuild (const act) (pure ())

testNone :: IO () -> IO ()
testNone _ = pure ()

shakenEx
    :: Bool
    -> [OptDescr (Either String a)]
    -> (([String] -> IO ()) -> IO ())
    -> ([a] -> [String] -> Rules ())
    -> IO ()
    -> IO ()
shakenEx reenter options test rules sleeper = do
    initDataDirectory

    name:args <- getArgs
    putStrLn $ "## BUILD " ++ unwords (name:args)
    let forward = "--forward" `elem` args
    args <- pure $ delete "--forward" args
    let out = "output/" ++ name ++ "/"
    let change = if not reenter then withCurrentDirectory out else id
    let clean = do
            now <- getCurrentDirectory
            when (takeBaseName now /= name) $
                fail $ "Clean went horribly wrong! Dangerous deleting: " ++ show now
            withCurrentDirectory (now </> "..") $ do
                removePathForcibly now
                createDirectoryRecursive now
    unless reenter $ createDirectoryRecursive out
    case args of
        "test":_ -> do
            putStrLn $ "## TESTING " ++ name
            change $ test (\args -> withArgs (name:args) $ shakenEx True options test rules sleeper)
            putStrLn $ "## FINISHED TESTING " ++ name

        "clean":args -> do
            when (args /= []) $ fail "Unexpected additional arguments to 'clean'"
            change clean

        "perturb":args -> forever $ do
            del <- removeFilesRandom out
            threads <- randomRIO (1,4)
            putStrLn $ "## TESTING PERTURBATION (" ++ show del ++ " files, " ++ show threads ++ " threads)"
            shake shakeOptions{shakeFiles=out, shakeThreads=threads, shakeVerbosity=Error} $ rules [] args

        args -> change $ do
            t <- tracker
            opts <- pure shakeOptions{shakeFiles = "."}
            cwd <- getCurrentDirectory
            opts <- pure $ if forward then forwardOptions opts{shakeLintInside=[""]} else opts
                {shakeLint = Just t
                ,shakeLintInside = [cwd </> ".." </> ".."]
                ,shakeLintIgnore = [".cabal-sandbox/**",".stack-work/**","../../.stack-work/**"]}
            withArgs args $ do
                let optionsBuiltin = optionsEnumDesc
                        [(Clean, "Clean before building.")
                        ,(Sleep, "Pause before executing.")
                        ,(UsePredicate, "Use &?> in preference to &%>")]
                shakeArgsOptionsWith opts (optionsBuiltin `mergeOptDescr` options) $ \so extra files -> do
                    let (extra1, extra2) = partitionEithers extra
                    when (Clean `elem` extra1) clean
                    when (Sleep `elem` extra1) sleeper
                    so <- pure $ if UsePredicate `notElem` extra1 then so else
                        so{shakeExtra = addShakeExtra UsePredicateYes $ shakeExtra so}
                    if "clean" `elem` files then
                        clean >> pure Nothing
                    else pure $ Just $ (,) so $ do
                        -- if you have passed sleep, suppress the "no actions" warning
                        when (Sleep `elem` extra1) $ action $ pure ()
                        rules extra2 files

data Flags
    = Clean -- ^ Clean all the files before starting
    | Sleep -- ^ Call 'sleepFileTimeCalibrate' before starting
    | UsePredicate -- ^ Use &?> in preference to &%>
      deriving (Eq,Show)

data UsePredicateYes = UsePredicateYes deriving Typeable

(&?%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
deps &?%> act = do
    so :: Maybe UsePredicateYes <- getShakeExtraRules
    if isJust so
        then (\x -> if x `elem` deps then Just deps else Nothing) &?> act
        else deps &%> act

-- A way to get back to the source files after you get directory changed
shakeRoot :: FilePath
shakeRoot = "../.."

tracker :: IO Lint
tracker = do
    fsatrace <- findExecutable $ "fsatrace" <.> exe
    -- Tracking on a Mac is pretty unreliable
    pure $ if not isMac && isJust fsatrace then LintFSATrace else LintBasic

-- Tests that don't currently work on CI
notCI :: IO () -> IO ()
notCI act = do
    b <- lookupEnv "CI"
    when (isNothing b) act

-- Tests that don't currently work on Windows CI
notWindowsCI :: IO () -> IO ()
notWindowsCI = if isWindows then notCI else id

-- Tests that don't currently work on Mac CI
notMacCI :: IO () -> IO ()
notMacCI = if isMac then notCI else id

hasTracker :: IO Bool
hasTracker = do
    t <- tracker
    pure $ t == LintFSATrace

assertFail :: String -> IO a
assertFail msg = error $ "ASSERTION FAILED: " ++ msg

assertBool :: Bool -> String -> IO ()
assertBool b msg = unless b $ assertFail msg

assertBoolIO :: IO Bool -> String -> IO ()
assertBoolIO b msg = do b <- b; assertBool b msg

infix 4 ===

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = assertBool (a == b) $ "failed in ===\nLHS: " ++ show a ++ "\nRHS: " ++ show b


assertExists :: FilePath -> IO ()
assertExists file = do
    b <- IO.doesFileExist file
    assertBool b $ "File was expected to exist, but is missing: " ++ file

assertMissing :: FilePath -> IO ()
assertMissing file = do
    b <- IO.doesFileExist file
    assertBool (not b) $ "File was expected to be missing, but exists: " ++ file

assertWithin :: Seconds -> IO a -> IO a
assertWithin n act = do
    t <- timeout n act
    case t of
        Nothing -> assertFail $ "Expected to complete within " ++ show n ++ " seconds, but did not"
        Just v -> pure v

assertContents :: FilePath -> String -> IO ()
assertContents file want = do
    got <- IO.readFile' file
    assertBool (want == got) $ "File contents are wrong: " ++ file ++ "\nWANT: " ++ want ++ "\nGOT: " ++ got

assertContentsInfix :: FilePath -> String -> IO ()
assertContentsInfix file want = do
    got <- IO.readFile' file
    assertBool (want `isInfixOf` got) $ "File contents are wrong: " ++ file ++ "\nWANT (infix): " ++ want ++ "\nGOT: " ++ got

assertContentsOn :: (String -> String) -> FilePath -> String -> IO ()
assertContentsOn f file want = do
    got <- IO.readFile' file
    assertBool (f want == f got) $ "File contents are wrong: " ++ file ++ "\nWANT: " ++ want ++ "\nGOT: " ++ got ++
                                   "\nWANT (transformed): " ++ f want ++ "\nGOT (transformed): " ++ f got

assertContentsWords :: FilePath -> String -> IO ()
assertContentsWords = assertContentsOn (unwords . words)

assertContentsUnordered :: FilePath -> [String] -> IO ()
assertContentsUnordered file xs = assertContentsOn (unlines . sort . lines) file (unlines xs)

assertExceptionAfter :: (String -> String) -> [String] -> IO a -> IO ()
assertExceptionAfter tweak parts act = do
    res <- try_ act
    case res of
        Left err -> let s = tweak $ show err in forM_ parts $ \p ->
            assertBool (p `isInfixOf` s) $ "Incorrect exception, missing part:\nGOT: " ++ s ++ "\nWANTED: " ++ p
        Right _ -> error $ "Expected an exception containing " ++ show parts ++ ", but succeeded"

assertException :: [String] -> IO a -> IO ()
assertException = assertExceptionAfter id

assertTimings :: ([String] -> IO ()) -> [(String, Seconds)] -> IO ()
assertTimings build expect = do
    build ["--report=report.json","--no-build"]
    src <- IO.readFile' "report.json"
    let f ('[':'\"':xs)
            | (name,_:',':xs) <- break (== '\"') xs
            , num <- takeWhile (`notElem` ",]") xs
            , Just num <- readMaybe num
            = (name, num :: Double)
        f x = error $ "Failed to parse JSON output in assertTimings, " ++ show x
    let got = [f x | x <- map drop1 $ lines src, x /= ""]
    forM_ expect $ \(name, val) ->
        case lookup name got of
            Nothing -> assertFail $ "Couldn't find key " ++ show name ++ " in profiling output"
            Just v -> assertBool (v >= val && v < (val + 1)) $ "Unexpected value, got " ++ show v ++ ", hoping for " ++ show val ++ " (+ 1 sec)"


defaultTest :: ([String] -> IO ()) -> IO ()
defaultTest build = do
    build ["--abbrev=output=$OUT","-j3","--report"]
    build ["--no-build","--report=-"]
    build []


-- | Sleep long enough for the modification time resolution to catch up
sleepFileTime :: IO ()
sleepFileTime = sleep 1


sleepFileTimeCalibrate :: FilePath -> IO (IO ())
sleepFileTimeCalibrate file = do
    createDirectoryRecursive $ takeDirectory file
    -- with 10 measurements can get a bit slow, see #451
    -- if it rounds to a second then 1st will be a fraction, but 2nd will be full second
    mtimes <- forM [1..2] $ \i -> fmap fst $ duration $ do
        writeFile file $ show i
        let time = fmap (fst . fromMaybe (error "File missing during sleepFileTimeCalibrate")) $
                        getFileInfo False $ fileNameFromString file
        t1 <- time
        flip loopM 0 $ \j -> do
            writeFile file $ show (i,j)
            t2 <- time
            pure $ if t1 == t2 then Left $ j+1 else Right ()
    putStrLn $ "Longest file modification time lag was " ++ show (ceiling (maximum' mtimes * 1000)) ++ "ms"
    pure $ sleep $ min 1 $ maximum' mtimes * 2


removeFilesRandom :: FilePath -> IO Int
removeFilesRandom x = do
    files <- getDirectoryContentsRecursive x
    n <- randomRIO (0,length files)
    rs <- replicateM (length files) (randomIO :: IO Double)
    mapM_ (removeFile . snd) $ sort $ zip rs files
    pure n


getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    xs <- IO.getDirectoryContents dir
    (dirs,files) <- partitionM IO.doesDirectoryExist [dir </> x | x <- xs, not $ "." `isPrefixOf` x]
    rest <- concatMapM getDirectoryContentsRecursive dirs
    pure $ files++rest


copyDirectoryChanged :: FilePath -> FilePath -> IO ()
copyDirectoryChanged old new = do
    xs <- getDirectoryContentsRecursive old
    forM_ xs $ \from -> do
        let to = new </> drop (length $ addTrailingPathSeparator old) from
        createDirectoryRecursive $ takeDirectory to
        copyFileChangedIO from to


copyFileChangedIO :: FilePath -> FilePath -> IO ()
copyFileChangedIO old new =
    unlessM (liftIO $ IO.doesFileExist new &&^ IO.fileEq old new) $
        copyFile old new

-- The operators %> ?> &*> &?> |?> |*> all have an isomorphism
data Pat = PatWildcard | PatPredicate | PatOrWildcard | PatAndWildcard | PatAndPredicate
    deriving (Read, Show, Enum, Bounded)

pat :: Pat -> FilePattern -> (FilePath -> Action ()) -> Rules ()
pat PatWildcard p act = p %> act
pat PatPredicate p act = (p ?==) ?> act
pat PatOrWildcard p act = [p] |%> act
pat PatAndWildcard p act =
    -- single wildcard shortcircuits, so we use multiple to avoid that
    -- and thus have to fake writing an extra file
    [p, p ++ "'"] &%> \[x,x'] -> do act x; writeFile' x' ""
pat PatAndPredicate p act = (\x -> if p ?== x then Just [x] else Nothing) &?> \[x] -> act x


---------------------------------------------------------------------
-- TEST MATERIAL
-- Some errors require multiple modules to replicate (e.g. #506), so put that here

newtype BinarySentinel a = BinarySentinel ()
    deriving (Eq,Show,NFData,Typeable,Hashable)

instance forall a . Typeable a => Binary (BinarySentinel a) where
    put (BinarySentinel ()) = put $ show (typeRep (Proxy :: Proxy a))
    get = do
        x <- get
        let want = show (typeRep (Proxy :: Proxy a))
        if x == want then pure $ BinarySentinel () else
            error $ "BinarySentinel failed, got " ++ show x ++ " but wanted " ++ show want

newtype RandomType = RandomType (BinarySentinel ())
    deriving (Eq,Show,NFData,Typeable,Hashable,Binary)
