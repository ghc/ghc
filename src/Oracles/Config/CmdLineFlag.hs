{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.Config.CmdLineFlag (buildInfo, cmdLineOracle, flags, BuildInfoFlag(..)) where

import GHC.Generics (Generic)

import Development.Shake hiding (Normal)
import Development.Shake.Classes
import Data.Char (toLower)
import System.Console.GetOpt

-- Flags

data BuildInfoFlag = Normal | Brief | Pony | Dot | None deriving (Eq, Show, Generic)

instance Hashable BuildInfoFlag
instance NFData BuildInfoFlag
instance Binary BuildInfoFlag

data CmdLineOptions = CmdLineOptions {
    flagBuildInfo :: BuildInfoFlag
} deriving (Eq, Show, Generic)

defaultCmdLineOptions :: CmdLineOptions
defaultCmdLineOptions = CmdLineOptions {
    flagBuildInfo = Normal
}

instance Hashable CmdLineOptions
instance NFData CmdLineOptions
instance Binary CmdLineOptions

readBuildInfoFlag :: Maybe String -> Either String (CmdLineOptions -> CmdLineOptions)
readBuildInfoFlag ms =
    maybe (Left "no parse") (Right . mkClosure)
        (go =<< fmap (map toLower) ms)
  where
    go :: String -> Maybe BuildInfoFlag
    go "normal" = Just Normal
    go "brief"  = Just Brief
    go "pony"   = Just Pony
    go "dot"    = Just Dot
    go "none"   = Just None
    go _        = Nothing -- Left "no parse"
    mkClosure :: BuildInfoFlag -> CmdLineOptions -> CmdLineOptions
    mkClosure flag opts = opts { flagBuildInfo = flag }

flags :: [OptDescr (Either String (CmdLineOptions -> CmdLineOptions))]
flags = [Option [] ["build-info"] (OptArg readBuildInfoFlag "") "Build Info Style (Normal, Brief, Pony, Dot, or None)"]

-- Oracles

newtype CmdLineFlags = CmdLineFlags ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

buildInfo :: Action BuildInfoFlag
buildInfo = do
    opts <- askOracle $ CmdLineFlags ()
    return $ flagBuildInfo opts

cmdLineOracle :: [CmdLineOptions -> CmdLineOptions] -> Rules ()
cmdLineOracle opts = do
    cache <- newCache $ \_ -> return $ foldl (flip id) defaultCmdLineOptions opts
    _ <- addOracle $ \CmdLineFlags{} -> cache ()
    return ()
