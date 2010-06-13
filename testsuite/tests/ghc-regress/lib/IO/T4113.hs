
module Main (main) where

import Control.Exception
import Prelude hiding (catch)
import System.Directory

main :: IO ()
main = do doit ""
          doit "/no/such/file"

doit :: FilePath -> IO ()
doit fp = do fp' <- canonicalizePath fp
             print (fp, fp')
    `catch` \e -> putStrLn ("Exception: " ++ show (e :: IOException))
