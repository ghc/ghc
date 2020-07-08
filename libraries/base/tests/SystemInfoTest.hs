{-# LANGUAGE CPP #-}
module Main where

import           Data.Version (showVersion)
import           System.Info  (fullCompilerVersion)

main :: IO ()
main = if textualVersion == macroVersion
       then putStrLn "Match"
       else putStrLn $ "[!]" <> textualVersion <> "should be equal to " <> macroVersion
  where
    macroVersion   = __GLASGOW_HASKELL_FULL_VERSION__
    textualVersion = showVersion fullCompilerVersion
