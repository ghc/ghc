module CmdLineFlag (
    putCmdLineFlags, cmdFlags, cmdBuildHaddock, cmdFlavour, cmdIntegerSimple,
    cmdProgressColour, cmdProgressInfo, ProgressInfo (..), cmdSkipConfigure,
    cmdSplitObjects
    ) where

import Data.IORef
import Data.List.Extra
import Hadrian.Utilities
import System.Console.GetOpt
import System.IO.Unsafe

-- | 'CmdLineFlag.Untracked' is a collection of flags that can be passed via the
-- command line. These flags are not tracked, that is they do not force any
-- build rules to be rurun.
data Untracked = Untracked
    { buildHaddock   :: Bool
    , flavour        :: Maybe String
    , integerSimple  :: Bool
    , progressColour :: UseColour
    , progressInfo   :: ProgressInfo
    , skipConfigure  :: Bool
    , splitObjects   :: Bool }
    deriving (Eq, Show)

data ProgressInfo = None | Brief | Normal | Unicorn deriving (Eq, Show)

-- | Default values for 'CmdLineFlag.Untracked'.
defaultUntracked :: Untracked
defaultUntracked = Untracked
    { buildHaddock   = False
    , flavour        = Nothing
    , integerSimple  = False
    , progressColour = Auto
    , progressInfo   = Normal
    , skipConfigure  = False
    , splitObjects   = False }

readBuildHaddock :: Either String (Untracked -> Untracked)
readBuildHaddock = Right $ \flags -> flags { buildHaddock = True }

readFlavour :: Maybe String -> Either String (Untracked -> Untracked)
readFlavour ms = Right $ \flags -> flags { flavour = lower <$> ms }

readIntegerSimple :: Either String (Untracked -> Untracked)
readIntegerSimple = Right $ \flags -> flags { integerSimple = True }

readProgressColour :: Maybe String -> Either String (Untracked -> Untracked)
readProgressColour ms =
    maybe (Left "Cannot parse progress-colour") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe UseColour
    go "never"   = Just Never
    go "auto"    = Just Auto
    go "always"  = Just Always
    go _         = Nothing
    set :: UseColour -> Untracked -> Untracked
    set flag flags = flags { progressColour = flag }

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
      "Build flavour (Default, Devel1, Devel2, Perf, Prof, Quick or Quickest)."
    , Option [] ["haddock"] (NoArg readBuildHaddock)
      "Generate Haddock documentation."
    , Option [] ["integer-simple"] (NoArg readIntegerSimple)
      "Build GHC with integer-simple library."
    , Option [] ["progress-colour"] (OptArg readProgressColour "MODE")
      "Use colours in progress info (Never, Auto or Always)."
    , Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal or Unicorn)."
    , Option [] ["skip-configure"] (NoArg readSkipConfigure)
      "Skip the boot and configure scripts (if you want to run them manually)."
    , Option [] ["split-objects"] (NoArg readSplitObjects)
      "Generate split objects (requires a full clean rebuild)." ]

-- TODO: Avoid unsafePerformIO by using shakeExtra.
{-# NOINLINE cmdLineFlags #-}
cmdLineFlags :: IORef Untracked
cmdLineFlags = unsafePerformIO $ newIORef defaultUntracked

putCmdLineFlags :: [Untracked -> Untracked] -> IO ()
putCmdLineFlags flags = modifyIORef cmdLineFlags (\f -> foldl (flip id) f flags)

-- TODO: Avoid unsafePerformIO by using shakeExtra.
{-# NOINLINE getCmdLineFlags #-}
getCmdLineFlags :: Untracked
getCmdLineFlags = unsafePerformIO $ readIORef cmdLineFlags

cmdBuildHaddock :: Bool
cmdBuildHaddock = buildHaddock getCmdLineFlags

cmdFlavour :: Maybe String
cmdFlavour = flavour getCmdLineFlags

cmdIntegerSimple :: Bool
cmdIntegerSimple = integerSimple getCmdLineFlags

cmdProgressColour :: UseColour
cmdProgressColour = progressColour getCmdLineFlags

cmdProgressInfo :: ProgressInfo
cmdProgressInfo = progressInfo getCmdLineFlags

cmdSplitObjects :: Bool
cmdSplitObjects = splitObjects getCmdLineFlags

cmdSkipConfigure :: Bool
cmdSkipConfigure = skipConfigure getCmdLineFlags
