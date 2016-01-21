module CmdLineFlag (
    putCmdLineFlags, flags, cmdProgressInfo, ProgressInfo (..), cmdSplitObjects,
    Configure (..), cmdConfigure
    ) where

import Data.List.Extra
import System.Console.GetOpt

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Command line flags
data ProgressInfo = None | Brief | Normal | Unicorn deriving (Eq, Show)
data Configure    = SkipConfigure | RunConfigure String deriving (Eq, Show)

-- | 'CmdLineFlag.Untracked' is a collection of flags that can be passed via the
-- command line. These flags are not tracked, that is they do not force any
-- build rules to be rurun.
data Untracked = Untracked
    { progressInfo :: ProgressInfo
    , splitObjects :: Bool
    , configure    :: Configure }
    deriving (Eq, Show)

-- | Default values for 'CmdLineFlag.Untracked'.
defaultUntracked :: Untracked
defaultUntracked = Untracked
    { progressInfo = Normal
    , splitObjects = False
    , configure    = SkipConfigure }

readProgressInfo :: Maybe String -> Either String (Untracked -> Untracked)
readProgressInfo ms =
    maybe (Left "Cannot parse progressInfo") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe ProgressInfo
    go "none"    = Just None
    go "brief"   = Just Brief
    go "normal"  = Just Normal
    go "unicorn" = Just Unicorn
    go _         = Nothing -- Left "no parse"
    set :: ProgressInfo -> Untracked -> Untracked
    set flag flags = flags { progressInfo = flag }

readConfigure :: Maybe String -> Either String (Untracked -> Untracked)
readConfigure ms =
    maybe (Left "Cannot parse configure") (Right . set) (go $ lower <$> ms)
  where
    go :: Maybe String -> Maybe Configure
    go (Just args) = Just $ RunConfigure args
    go Nothing     = Just $ RunConfigure ""
    set :: Configure -> Untracked -> Untracked
    set flag flags = flags { configure = flag }

readSplitObjects :: Either String (Untracked -> Untracked)
readSplitObjects = Right $ \flags -> flags { splitObjects = True }

flags :: [OptDescr (Either String (Untracked -> Untracked))]
flags = [ Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
          "Progress info style (None, Brief, Normal, or Unicorn)."
        , Option [] ["split-objects"] (NoArg readSplitObjects)
          "Generate split objects (requires a full clean rebuild)."
        , Option [] ["configure"] (OptArg readConfigure "ARGS")
          "Run boot and configure scripts (passing ARGS to the latter)." ]

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

cmdProgressInfo :: ProgressInfo
cmdProgressInfo = progressInfo getCmdLineFlags

cmdSplitObjects :: Bool
cmdSplitObjects = splitObjects getCmdLineFlags

cmdConfigure :: Configure
cmdConfigure = configure getCmdLineFlags
