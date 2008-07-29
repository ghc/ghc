
module Main (main) where

import Control.Monad
import Data.Maybe
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
import System.Environment
import System.FilePath

import qualified Distribution.Make   as Make
import qualified Distribution.Simple as Simple

setupProg :: FilePath
setupProg = "./Setup"

main :: IO ()
main = do
    unprocessedArgs <- getArgs
    let verbosity = verbose
    case unprocessedArgs of
        ghc : packageConf : args ->
            doit verbosity ghc packageConf args
        _ -> die "Bad args"

doit :: Verbosity -> FilePath -> FilePath -> [String] -> IO ()
doit verbosity ghc packageConf args = do
    exists <- doesFileExist setupProg
    if exists then rawSystemExit verbosity setupProg args
              else do
        gpdFile <- defaultPackageDesc verbosity
        gpd <- readPackageDescription verbosity gpdFile
        let pd = packageDescription gpd
        case buildType pd of
            Just Simple    -> Simple.defaultMainArgs                     args
            Just Make      -> Make.defaultMainArgs                       args
            Just Configure -> defaultMainWithHooksArgs autoconfUserHooks args
            _ | packageName pd == PackageName "Cabal" ->
                              -- Cabal is special...*sigh*
                              Simple.defaultMainArgs                     args
              | otherwise  -> runSetup verbosity ghc packageConf args

runSetup :: Verbosity -> FilePath -> FilePath -> [String] -> IO ()
runSetup verbosity ghc packageConf args = do
    -- Don't bother building Setup if we are cleaning. If we need to
    -- build Setup in order to build, and Setup isn't built already,
    -- then there shouldn't be anything to clean anyway.
    unless cleaning $
        rawSystemExit verbosity ghc ["-package-conf", packageConf,
                                     "--make", "Setup", "-o", "Setup"]
    rawSystemExit verbosity "./Setup" args
  where cleaning = case args of
                   "clean" : _ -> True
                   _ -> False

