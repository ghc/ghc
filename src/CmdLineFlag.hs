module CmdLineFlag (
    putCmdLineFlags, cmdFlags, cmdConfigure, Configure (..), cmdFlavour,
    Flavour (..), cmdProgressInfo, ProgressInfo (..), cmdSplitObjects
    ) where

import Data.List.Extra
import System.Console.GetOpt

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Command line flags
data ProgressInfo = None | Brief | Normal | Unicorn deriving (Eq, Show)
data Configure    = SkipConfigure | RunConfigure String deriving (Eq, Show)
data Flavour      = Default | Quick deriving (Eq, Show)

-- | 'CmdLineFlag.Untracked' is a collection of flags that can be passed via the
-- command line. These flags are not tracked, that is they do not force any
-- build rules to be rurun.
data Untracked = Untracked
    { configure    :: Configure
    , flavour      :: Flavour
    , progressInfo :: ProgressInfo
    , splitObjects :: Bool }
    deriving (Eq, Show)

-- | Default values for 'CmdLineFlag.Untracked'.
defaultUntracked :: Untracked
defaultUntracked = Untracked
    { configure    = SkipConfigure
    , flavour      = Default
    , progressInfo = Normal
    , splitObjects = False }

readConfigure :: Maybe String -> Either String (Untracked -> Untracked)
readConfigure ms =
    maybe (Left "Cannot parse configure") (Right . set) (go $ lower <$> ms)
  where
    go :: Maybe String -> Maybe Configure
    go (Just args) = Just $ RunConfigure args
    go Nothing     = Just $ RunConfigure ""
    set :: Configure -> Untracked -> Untracked
    set flag flags = flags { configure = flag }

readFlavour :: Maybe String -> Either String (Untracked -> Untracked)
readFlavour ms =
    maybe (Left "Cannot parse flavour") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe Flavour
    go "default" = Just Default
    go "quick"   = Just Quick
    go _         = Nothing
    set :: Flavour -> Untracked -> Untracked
    set flag flags = flags { flavour = flag }

readProgressInfo :: Maybe String -> Either String (Untracked -> Untracked)
readProgressInfo ms =
    maybe (Left "Cannot parse progress-info") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe ProgressInfo
    go "none"    = Just None
    go "brief"   = Just Brief
    go "normal"  = Just Normal
    go "unicorn" = Just Unicorn
    go _         = Nothing
    set :: ProgressInfo -> Untracked -> Untracked
    set flag flags = flags { progressInfo = flag }

readSplitObjects :: Either String (Untracked -> Untracked)
readSplitObjects = Right $ \flags -> flags { splitObjects = True }

cmdFlags :: [OptDescr (Either String (Untracked -> Untracked))]
cmdFlags =
    [ Option [] ["configure"] (OptArg readConfigure "ARGS")
      "Run configure with ARGS (also run boot if necessary)."
    , Option [] ["flavour"] (OptArg readFlavour "FLAVOUR")
      "Build flavour (Default or Quick)."
    , Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal, or Unicorn)."
    , Option [] ["split-objects"] (NoArg readSplitObjects)
      "Generate split objects (requires a full clean rebuild)." ]

-- TODO: Avoid unsafePerformIO by using shakeExtra (awaiting Shake's release)
{-# NOINLINE cmdLineFlags #-}
cmdLineFlags :: IORef Untracked
cmdLineFlags = unsafePerformIO $ newIORef defaultUntracked

putCmdLineFlags :: [Untracked -> Untracked] -> IO ()
putCmdLineFlags flags = modifyIORef cmdLineFlags (\f -> foldl (flip id) f flags)

-- TODO: Avoid unsafePerformIO by using shakeExtra (awaiting Shake's release)
{-# NOINLINE getCmdLineFlags #-}
getCmdLineFlags :: Untracked
getCmdLineFlags = unsafePerformIO $ readIORef cmdLineFlags

cmdConfigure :: Configure
cmdConfigure = configure getCmdLineFlags

cmdFlavour :: Flavour
cmdFlavour = flavour getCmdLineFlags

cmdProgressInfo :: ProgressInfo
cmdProgressInfo = progressInfo getCmdLineFlags

cmdSplitObjects :: Bool
cmdSplitObjects = splitObjects getCmdLineFlags
