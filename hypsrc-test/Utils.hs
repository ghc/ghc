{-# LANGUAGE CPP #-}


module Utils
    ( baseDir, rootDir
    , srcDir, refDir, outDir, refDir', outDir'
    , haddockPath
    , stripLocalAnchors, stripLocalLinks, stripLocalReferences
    ) where


import Data.List

import System.FilePath


baseDir, rootDir :: FilePath
baseDir = takeDirectory __FILE__
rootDir = baseDir </> ".."

srcDir, refDir, outDir, refDir', outDir' :: FilePath
srcDir = baseDir </> "src"
refDir = baseDir </> "ref"
outDir = baseDir </> "out"
refDir' = refDir </> "src"
outDir' = outDir </> "src"

haddockPath :: FilePath
haddockPath = rootDir </> "dist" </> "build" </> "haddock" </> "haddock"


replaceBetween :: Eq a => [a] -> a -> [a] -> [a] -> [a]
replaceBetween _ _ _ [] = []
replaceBetween pref end val html@(x:xs') = case stripPrefix pref html of
    Just strip -> pref ++ val ++ (replaceBetween' . dropWhile (/= end)) strip
    Nothing -> x:(replaceBetween' xs')
  where
    replaceBetween' = replaceBetween pref end val

stripLocalAnchors :: String -> String
stripLocalAnchors = replaceBetween "<a name=\"local-" '\"' "0"

stripLocalLinks :: String -> String
stripLocalLinks = replaceBetween "<a href=\"#local-" '\"' "0"

stripLocalReferences :: String -> String
stripLocalReferences = stripLocalLinks . stripLocalAnchors
