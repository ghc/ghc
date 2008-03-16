-- Returns exitcode 0 if the given package is buildable or is a boot package,
-- and 1 otherwise.

module Main (main) where

import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do args <- getArgs
          case args of
              [package] ->
                  doit package
              _ ->
                  error "Syntax: ifBuildable <package>"

doit :: String -> IO ()
doit package
 = do setCurrentDirectory package
      unbuildable <- doesFileExist "unbuildable"
      if not unbuildable
         then exitWith ExitSuccess
         else do mustBeBuildables <- getMustBeBuildablePackages
                 if package `elem` mustBeBuildables
                     then exitWith ExitSuccess
                     else do hPutStrLn stderr "Warning: Package is unbuildable"
                             exitWith (ExitFailure 1)

getMustBeBuildablePackages :: IO [String]
getMustBeBuildablePackages
 = do xs <- readFile "../boot-packages"
      return $ filter ("editline" /=) $ lines xs
