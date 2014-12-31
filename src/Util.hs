module Util (
    module Data.Char,
    replaceIf, replaceEq,
    postProcessPackageData
    ) where

import Base
import Data.Char

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf p to = map (\from -> if p from then to else from) 

replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from = replaceIf (== from)

-- Prepare a given 'packaga-data.mk' file for parsing by readConfigFile:
-- 1) Drop lines containing '$'
-- 2) Replace '/' and '\' with '_' before '='
postProcessPackageData :: FilePath -> Action ()
postProcessPackageData file = do
    pkgData <- (filter ('$' `notElem`) . lines) <$> liftIO (readFile file)
    length pkgData `seq` writeFileLines file $ map processLine pkgData
      where
        processLine line = replaceIf isPathSeparator '_' prefix ++ suffix
          where
            (prefix, suffix) = break (== '=') line
