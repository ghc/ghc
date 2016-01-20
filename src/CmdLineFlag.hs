module CmdLineFlag (
    putCmdLineFlags, flags, cmdProgressInfo, ProgressInfo (..)
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
    { progressInfo :: ProgressInfo }
    deriving (Eq, Show)

-- | Default values for 'CmdLineFlag.Untracked'.
defaultUntracked :: Untracked
defaultUntracked = Untracked
    { progressInfo = Normal }

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
    mkClosure flag opts = opts { progressInfo = flag }

flags :: [OptDescr (Either String (Untracked -> Untracked))]
flags = [ Option [] ["progress-info"] (OptArg readProgressInfo "")
          "Progress Info Style (None, Brief, Normal, or Unicorn)" ]

-- TODO: Get rid of unsafePerformIO by using shakeExtra.
{-# NOINLINE cmdLineFlags #-}
cmdLineFlags :: IORef Untracked
cmdLineFlags = unsafePerformIO $ newIORef defaultUntracked

putCmdLineFlags :: [Untracked -> Untracked] -> IO ()
putCmdLineFlags opts = modifyIORef cmdLineFlags (\o -> foldl (flip id) o opts)

getCmdLineFlags :: Action Untracked
getCmdLineFlags = liftIO $ readIORef cmdLineFlags

cmdProgressInfo :: Action ProgressInfo
cmdProgressInfo = progressInfo <$> getCmdLineFlags
