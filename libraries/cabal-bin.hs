
module Cabal (main) where

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
    let verbosity = verbose
    exists <- doesFileExist setupProg
    args <- getArgs
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
              | otherwise  -> die "Don't know what to do!"

