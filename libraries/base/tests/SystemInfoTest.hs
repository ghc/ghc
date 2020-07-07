{-# LANGUAGE CPP #-}
module SystemInfoTest where

import           System.Info (fullCompilerVersion)

main :: IO ()
main = if textualVersion == majMinRev
       then "Match"
       else "[!]" <> textualVersion <> "should be equal to " <> majMinRev
  where
    macroVersion = __GLASGOW_HASKELL_FULL_VERSION__
    textualVersion = show fullCompilerVersion
    majMinRev = reverse $ tail $ dropWhile (/= '.') $ reverse macroVersion
