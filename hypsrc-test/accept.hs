#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}


import System.Directory
import System.FilePath
import System.Environment

import Utils


main :: IO ()
main = do
    args <- getArgs
    files <- filter isHtmlFile <$> getDirectoryContents outDir'
    let files' = if args == ["--all"] || args == ["-a"]
        then files
        else filter ((`elem` args) . takeBaseName) files
    mapM_ copy files'
  where
    isHtmlFile = (== ".html") . takeExtension


copy :: FilePath -> IO ()
copy file = do
    content <- stripLocalReferences <$> readFile (outDir' </> file)
    writeFile (refDir' </> file) content
