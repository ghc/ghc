module Oracles.Config.CmdLineFlag (putOptions, buildInfo, flags, BuildInfoFlag(..)) where

import Data.Char (toLower)
import System.Console.GetOpt

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

-- Flags

data BuildInfoFlag = None | Brief | Normal | Unicorn deriving (Eq, Show)

data CmdLineOptions = CmdLineOptions {
    flagBuildInfo :: BuildInfoFlag
} deriving (Eq, Show)

defaultCmdLineOptions :: CmdLineOptions
defaultCmdLineOptions = CmdLineOptions {
    flagBuildInfo = Normal
}

readBuildInfoFlag :: Maybe String -> Either String (CmdLineOptions -> CmdLineOptions)
readBuildInfoFlag ms =
    maybe (Left "no parse") (Right . mkClosure)
        (go =<< fmap (map toLower) ms)
  where
    go :: String -> Maybe BuildInfoFlag
    go "none"    = Just None
    go "brief"   = Just Brief
    go "normal"  = Just Normal
    go "unicorn" = Just Unicorn
    go _         = Nothing -- Left "no parse"
    mkClosure :: BuildInfoFlag -> CmdLineOptions -> CmdLineOptions
    mkClosure flag opts = opts { flagBuildInfo = flag }

flags :: [OptDescr (Either String (CmdLineOptions -> CmdLineOptions))]
flags = [Option [] ["progress-info"] (OptArg readBuildInfoFlag "") "Build Info Style (None, Brief, Normal, or Unicorn)"]

-- IO -- We use IO here instead of Oracles, as Oracles form part of shakes cache
-- hence, changing command line arguments, would cause a full rebuild.  And we
-- likely do *not* want to rebuild everything if only the @--build-info@ flag
-- was changed.
{-# NOINLINE cmdLineOpts #-}
cmdLineOpts :: IORef CmdLineOptions
cmdLineOpts = unsafePerformIO $ newIORef defaultCmdLineOptions

putOptions :: [CmdLineOptions -> CmdLineOptions] -> IO ()
putOptions opts = modifyIORef cmdLineOpts (\o -> foldl (flip id) o opts)

{-# NOINLINE getOptions #-}
getOptions :: CmdLineOptions
getOptions = unsafePerformIO $ readIORef cmdLineOpts

buildInfo :: BuildInfoFlag
buildInfo = flagBuildInfo getOptions
