
module Main (main) where

import Control.Exception
import Prelude hiding (catch)
import System.Directory

main :: IO ()
main = do doit ""
          doit "/no/such/file"

doit :: FilePath -> IO ()
doit fp = do fp' <- canonicalizePath fp
             print (fp, mangle fp')
    `catch` \e -> putStrLn ("Exception: " ++ show (e :: IOException))
  where -- On Windows, "/no/such/file" -> "C:\\no\\such\\file", so
        -- we remove the drive letter so as to get consistent output
        mangle (_ : ':' : xs) = "drive:" ++ xs
        mangle xs = xs

