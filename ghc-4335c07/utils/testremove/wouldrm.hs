
module Main (main) where

import System.Environment

data CleanWhat = CleanFile FilePath
               | CleanRec  FilePath
    deriving (Read, Show)

main :: IO ()
main = do args <- getArgs
          ls <- case args of
                "CLEAN_FILES" : files -> return $ map CleanFile files
                "CLEAN_REC"   : dirs  -> return $ map CleanRec dirs
                _ -> error "Bad args"
          appendFile "would-be-cleaned" $ unlines $ map show ls
