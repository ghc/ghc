{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception
import Documentation.Haddock (haddock)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = getArgs >>= expandResponse >>= haddock


-- | Arguments which look like '@foo' will be replaced with the
-- contents of file @foo@. The contents will be passed through 'words'
-- and blanks filtered out first.
--
-- We quit if the file is not found or reading somehow fails.
expandResponse :: [String] -> IO [String]
expandResponse = fmap concat . mapM expand
  where
    expand :: String -> IO [String]
    expand ('@':f) = readFileExc f >>= return . filter (not . null) . words
    expand x = return [x]

    readFileExc f =
      readFile f `catch` \(e :: IOException) -> do
        hPutStrLn stderr $ "Error while expanding response file: " ++ show e
        exitFailure
