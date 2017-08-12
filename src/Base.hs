module Base (
    -- * General utilities
    module Control.Applicative,
    module Control.Monad.Extra,
    module Data.Bifunctor,
    module Data.Function,
    module Data.List.Extra,
    module Data.Maybe,
    module Data.Semigroup,

    -- * Shake
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.FilePath,

    -- * Paths
    configPath, configFile, sourcePath,

    -- * Miscellaneous utilities
    unifyPath, quote, (-/-), putColoured
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bifunctor
import Data.Function
import Data.List.Extra
import Data.Maybe
import Data.Semigroup
import Development.Shake hiding (parallel, unit, (*>), Normal)
import Development.Shake.Classes
import Development.Shake.FilePath
import Hadrian.Utilities
import System.Console.ANSI
import System.IO
import System.Info

import CmdLineFlag

-- TODO: reexport Stage, etc.?

-- | Hadrian lives in 'hadrianPath' directory of the GHC tree.
hadrianPath :: FilePath
hadrianPath = "hadrian"

-- TODO: Move this to build directory?
configPath :: FilePath
configPath = hadrianPath -/- "cfg"

configFile :: FilePath
configFile = configPath -/- "system.config"

-- | Path to source files of the build system, e.g. this file is located at
-- sourcePath -/- "Base.hs". We use this to `need` some of the source files.
sourcePath :: FilePath
sourcePath = hadrianPath -/- "src"

-- | A more colourful version of Shake's 'putNormal'.
putColoured :: ColorIntensity -> Color -> String -> Action ()
putColoured intensity colour msg = do
    c <- useColour
    when c . liftIO $ setSGR [SetColor Foreground intensity colour]
    putNormal msg
    when c . liftIO $ do
        setSGR []
        hFlush stdout

useColour :: Action Bool
useColour = case cmdProgressColour of
    Never  -> return False
    Always -> return True
    Auto   -> do
        supported <- liftIO $ hSupportsANSI stdout
        -- An ugly hack to always try to print colours when on mingw and cygwin.
        let windows = any (`isPrefixOf` os) ["mingw", "cygwin"]
        return $ windows || supported
