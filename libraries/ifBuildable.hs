
module Main (main) where

import Control.Monad
import System.Cmd
import System.Directory
import System.Environment
import System.Exit

main :: IO ()
main = do args <- getArgs
          case args of
              [] ->
                  error "No package or command given"
              [_] ->
                  error "No command given"
              package : prog : progArgs ->
                  do setCurrentDirectory package
                     unbuildable <- doesFileExist "unbuildable"
                     if unbuildable
                        then do mustBeBuildables <- getMustBeBuildablePackages
                                when (package `elem` mustBeBuildables)
                                     (error (package ++ " is unbuildable"))
                        else do ec <- rawSystem prog progArgs
                                exitWith ec

getMustBeBuildablePackages :: IO [String]
getMustBeBuildablePackages
 = do xs <- readFile "../core-packages"
      return $ filter ("readline" /=) $ lines xs

