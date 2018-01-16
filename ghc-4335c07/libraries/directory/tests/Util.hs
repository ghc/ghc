{-# LANGUAGE BangPatterns #-}
-- | A rudimentary testing framework
module Util where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import System.FilePath ((</>), normalise)
import qualified Data.List as List

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = do
  x <- readIORef r
  let !x' = f x in writeIORef r x'

tryAny :: IO a -> IO (Either SomeException a)
tryAny action = do
  result <- newEmptyMVar
  mask $ \ unmask -> do
    thread <- forkIO (try (unmask action) >>= putMVar result)
    unmask (readMVar result) `onException` killThread thread

timeLimit :: Double -> IO a -> IO a
timeLimit time action = do
  result <- timeout (round (1000000 * time)) action
  case result of
    Nothing -> throwIO (userError "timed out")
    Just x  -> return x

data TestEnv =
  TestEnv
  { testCounter  :: IORef Int
  , testSilent   :: Bool
  , testKeepDirs :: Bool
  , testArgs     :: [(String, String)]
  }

printInfo :: TestEnv -> [String] -> IO ()
printInfo TestEnv{testSilent = True}  _   = return ()
printInfo TestEnv{testSilent = False} msg = do
  putStrLn (List.intercalate ": " msg)
  hFlush stdout

printErr :: [String] -> IO ()
printErr msg = do
  hPutStrLn stderr ("*** " <> List.intercalate ": " msg)
  hFlush stderr

printFailure :: TestEnv -> [String] -> IO ()
printFailure TestEnv{testCounter = n} msg = do
  modifyIORef' n (+ 1)
  printErr msg

check :: TestEnv -> Bool -> [String] -> [String] -> [String] -> IO ()
check t True  prefix msg _   = printInfo t (prefix <> msg)
check t False prefix _   msg = printFailure t (prefix <> msg)

checkEither :: TestEnv -> [String] -> Either [String] [String] -> IO ()
checkEither t prefix (Right msg) = printInfo t (prefix <> msg)
checkEither t prefix (Left  msg) = printFailure t (prefix <> msg)

showContext :: Show a => String -> Integer -> a -> String
showContext file line context =
  file <> ":" <> show line <>
  case show context of
    "()" -> ""
    s    -> ":" <> s

inform :: TestEnv -> String -> Integer -> String -> IO ()
inform t file line msg =
  printInfo t [showContext file line (), msg]

expect :: Show a => TestEnv -> String -> Integer -> a -> Bool -> IO ()
expect t file line context x =
  check t x
  [showContext file line context]
  ["True"]
  ["False, but True was expected"]

expectEq :: (Eq a, Show a, Show b) =>
            TestEnv -> String -> Integer -> b -> a -> a -> IO ()
expectEq t file line context x y =
  check t (x == y)
  [showContext file line context]
  [show x <> " equals "     <> show y]
  [show x <> " is not equal to " <> show y]

expectNe :: (Eq a, Show a, Show b) =>
            TestEnv -> String -> Integer -> b -> a -> a -> IO ()
expectNe t file line context x y =
  check t (x /= y)
  [showContext file line context]
  [show x <> " is not equal to " <> show y]
  [show x <> " equals "     <> show y]

expectNear :: (Num a, Ord a, Show a, Show b) =>
              TestEnv -> String -> Integer -> b -> a -> a -> a -> IO ()
expectNear t file line context x y diff =
  check t (abs (x - y) <= diff)
  [showContext file line context]
  [show x <> " is near "     <> show y]
  [show x <> " is not near " <> show y]

expectNearTime :: Show a =>
                  TestEnv -> String -> Integer -> a ->
                  UTCTime -> UTCTime -> NominalDiffTime -> IO ()
expectNearTime t file line context x y diff =
  check t (abs (diffUTCTime x y) <= diff)
  [showContext file line context]
  [show x <> " is near "     <> show y]
  [show x <> " is not near " <> show y]

expectIOErrorType :: Show a =>
                     TestEnv -> String -> Integer -> a
                  -> (IOError -> Bool) -> IO b -> IO ()
expectIOErrorType t file line context which action = do
  result <- try action
  checkEither t [showContext file line context] $ case result of
    Left  e | which e   -> Right ["got expected exception (" <> show e <> ")"]
            | otherwise -> Left  ["got wrong exception: ", show e]
    Right _             -> Left  ["did not throw an exception"]

-- | Traverse the directory tree in preorder.
preprocessPathRecursive :: (FilePath -> IO ()) -> FilePath -> IO ()
preprocessPathRecursive f path = do
  dirExists <- doesDirectoryExist path
  if dirExists
    then do
      isLink <- pathIsSymbolicLink path
      f path
      when (not isLink) $ do
        names <- listDirectory path
        traverse_ (preprocessPathRecursive f) ((path </>) <$> names)
    else do
      f path

withNewDirectory :: Bool -> FilePath -> IO a -> IO a
withNewDirectory keep dir action = do
  dir' <- makeAbsolute dir
  bracket_ (createDirectoryIfMissing True dir') (cleanup dir') action
  where cleanup dir' | keep      = return ()
                     | otherwise = removePathForcibly dir'

isolateWorkingDirectory :: Bool -> FilePath -> IO a -> IO a
isolateWorkingDirectory keep dir action = do
  when (normalise dir `List.elem` [".", "./"]) $
    throwIO (userError ("isolateWorkingDirectory cannot be used " <>
                        "with current directory"))
  dir' <- makeAbsolute dir
  removePathForcibly dir'
  withNewDirectory keep dir' $
    withCurrentDirectory dir' $
      action

run :: TestEnv -> String -> (TestEnv -> IO ()) -> IO ()
run t name action = do
  result <- tryAny (action t)
  case result of
    Left  e  -> check t False [name] [] ["exception", show e]
    Right () -> return ()

isolatedRun :: TestEnv -> String -> (TestEnv -> IO ()) -> IO ()
isolatedRun t@TestEnv{testKeepDirs = keep} name =
  run t name .
  (isolateWorkingDirectory keep ("dist/test-" <> name <> ".tmp") .)

tryRead :: Read a => String -> Maybe a
tryRead s =
  case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

getArg :: (String -> Maybe a) -> TestEnv -> String -> String -> a -> a
getArg parse TestEnv{testArgs = args} testname name defaultValue =
  fromMaybe defaultValue (List.lookup (prefix <> name) args >>= parse)
  where prefix | testname == "" = ""
               | otherwise      = testname <> "."

readArg :: Read a => TestEnv -> String -> String -> a -> a
readArg = getArg tryRead

readBool :: String -> Maybe Bool
readBool s = Just $
  case toLower <$> s of
    'y' : _ -> True
    't' : _ -> True
    _       -> False

parseArgs :: [String] -> [(String, String)]
parseArgs = List.reverse . (second (List.drop 1) . List.span (/= '=') <$>)

testMain :: (TestEnv -> IO ()) -> IO ()
testMain action = do
  args <- parseArgs <$> getArgs
  counter <- newIORef 0
  let t = TestEnv
          { testCounter  = counter
          , testSilent   = getArg readBool t "" "silent" False
          , testKeepDirs = getArg readBool t "" "keep-dirs" False
          , testArgs     = args
          }
  action t
  n <- readIORef (counter)
  unless (n == 0) $ do
    putStrLn ("[" <> show n <> " failures]")
    hFlush stdout
    exitFailure
