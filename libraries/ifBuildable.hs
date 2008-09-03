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
              [packagesFile, package] ->
                  doit packagesFile package
              _ ->
                  error "Syntax: ifBuildable <packages-file> <package>"

doit :: FilePath -> String -> IO ()
doit packagesFile package
 = do setCurrentDirectory package
      unbuildable <- doesFileExist "unbuildable"
      if not unbuildable
         then exitWith ExitSuccess
         else do mustBeBuildables <- getMustBeBuildables packagesFile
                 if package `elem` mustBeBuildables
                     then exitWith ExitSuccess
                     else do hPutStrLn stderr "Warning: Package is unbuildable"
                             exitWith (ExitFailure 1)

getMustBeBuildables :: FilePath -> IO [String]
getMustBeBuildables packagesFile
 = do xs <- readFile packagesFile
      let nonCommentLines = filter (("#" /=) . take 1) $ lines xs
          requiredLines = filter ((3 == ) . length) $ map words nonCommentLines
          requiredLibraries = [ x | 'l':'i':'b':'r':'a':'r':'i':'e':'s':'/':x
                                    <- map head requiredLines ]
      return $ filter ("editline" /=) requiredLibraries

