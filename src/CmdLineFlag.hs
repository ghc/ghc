module CmdLineFlag (
    putCmdLineFlags, cmdFlags, cmdBuildHaddock, cmdFlavour, Flavour (..),
    cmdProgressInfo, ProgressInfo (..), cmdSkipConfigure, cmdSplitObjects
    ) where

import Data.List.Extra
import System.Console.GetOpt

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Command line flags
data ProgressInfo = None | Brief | Normal | Unicorn deriving (Eq, Show)
data Flavour      = Default | Quick deriving (Eq, Show)

-- | 'CmdLineFlag.Untracked' is a collection of flags that can be passed via the
-- command line. These flags are not tracked, that is they do not force any
-- build rules to be rurun.
data Untracked = Untracked
    { buildHaddock  :: Bool
    , flavour       :: Flavour
    , progressInfo  :: ProgressInfo
    , skipConfigure :: Bool
    , splitObjects  :: Bool }
    deriving (Eq, Show)

-- | Default values for 'CmdLineFlag.Untracked'.
defaultUntracked :: Untracked
defaultUntracked = Untracked
    { buildHaddock  = False
    , flavour       = Default
    , progressInfo  = Normal
    , skipConfigure = False
    , splitObjects  = False }

readBuildHaddock :: Either String (Untracked -> Untracked)
readBuildHaddock = Right $ \flags -> flags { buildHaddock = True }

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

readSkipConfigure :: Either String (Untracked -> Untracked)
readSkipConfigure = Right $ \flags -> flags { skipConfigure = True }

readSplitObjects :: Either String (Untracked -> Untracked)
readSplitObjects = Right $ \flags -> flags { splitObjects = True }

cmdFlags :: [OptDescr (Either String (Untracked -> Untracked))]
cmdFlags =
    [ Option [] ["flavour"] (OptArg readFlavour "FLAVOUR")
      "Build flavour (Default or Quick)."
    , Option [] ["haddock"] (NoArg readBuildHaddock)
      "Generate Haddock documentation."
    , Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal, or Unicorn)."
    , Option [] ["skip-configure"] (NoArg readSkipConfigure)
      "Skip the boot and configure scripts (if you want to run them manually)."
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

cmdBuildHaddock :: Bool
cmdBuildHaddock = buildHaddock getCmdLineFlags

cmdFlavour :: Flavour
cmdFlavour = flavour getCmdLineFlags

cmdProgressInfo :: ProgressInfo
cmdProgressInfo = progressInfo getCmdLineFlags

cmdSplitObjects :: Bool
cmdSplitObjects = splitObjects getCmdLineFlags

cmdSkipConfigure :: Bool
cmdSkipConfigure = skipConfigure getCmdLineFlags
