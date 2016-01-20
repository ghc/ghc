module CmdLineFlag (
    putCmdLineFlags, flags, cmdProgressInfo, ProgressInfo (..), cmdSplitObjects
    ) where

import Base
import Data.Char (toLower)
import System.Console.GetOpt

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Command line flags
data ProgressInfo = None | Brief | Normal | Unicorn deriving (Eq, Show)

-- | 'CmdLineFlag.Untracked' is a collection of flags that can be passed via the
-- command line. These flags are not tracked, that is they do not force any
-- build rules to be rurun.
data Untracked = Untracked
    { progressInfo :: ProgressInfo
    , splitObjects :: Bool }
    deriving (Eq, Show)

-- | Default values for 'CmdLineFlag.Untracked'.
defaultUntracked :: Untracked
defaultUntracked = Untracked
    { progressInfo = Normal
    , splitObjects = False }

readProgressInfo :: Maybe String -> Either String (Untracked -> Untracked)
readProgressInfo ms =
    maybe (Left "no parse") (Right . mkClosure) (go =<< fmap (map toLower) ms)
  where
    go :: String -> Maybe ProgressInfo
    go "none"    = Just None
    go "brief"   = Just Brief
    go "normal"  = Just Normal
    go "unicorn" = Just Unicorn
    go _         = Nothing -- Left "no parse"
    mkClosure :: ProgressInfo -> Untracked -> Untracked
    mkClosure flag flags = flags { progressInfo = flag }

readSplitObjects :: Either String (Untracked -> Untracked)
readSplitObjects = Right $ \flags -> flags { splitObjects = True }

flags :: [OptDescr (Either String (Untracked -> Untracked))]
flags = [ Option [] ["progress-info"] (OptArg readProgressInfo "")
          "Progress Info Style (None, Brief, Normal, or Unicorn)"
        , Option [] ["split-objects"] (NoArg readSplitObjects)
          "Generate split objects (requires a full clean rebuild)." ]

-- TODO: Get rid of unsafePerformIO by using shakeExtra.
{-# NOINLINE cmdLineFlags #-}
cmdLineFlags :: IORef Untracked
cmdLineFlags = unsafePerformIO $ newIORef defaultUntracked

putCmdLineFlags :: [Untracked -> Untracked] -> IO ()
putCmdLineFlags flags = modifyIORef cmdLineFlags (\f -> foldl (flip id) f flags)

getCmdLineFlags :: Action Untracked
getCmdLineFlags = liftIO $ readIORef cmdLineFlags

cmdProgressInfo :: Action ProgressInfo
cmdProgressInfo = progressInfo <$> getCmdLineFlags

cmdSplitObjects :: Action Bool
cmdSplitObjects = splitObjects <$> getCmdLineFlags
