-- | A test program to check that ghc has got all of its extensions registered
--
module Main where

import Language.Haskell.Extension
import Distribution.Text
import Distribution.Simple.Utils
import Distribution.Verbosity

import Data.List ((\\))
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Environment
import System.Exit

-- | A list of GHC extensions that are deliberately not registered,
-- e.g. due to being experimental and not ready for public consumption
--
exceptions = map readExtension []

checkProblems :: [Extension] -> [String]
checkProblems implemented =

  let unregistered  =
        [ ext | ext <- implemented          -- extensions that ghc knows about 
              , not (registered ext)        -- but that are not registered
              , ext `notElem` exceptions ]  -- except for the exceptions

      -- check if someone has forgotten to update the exceptions list...

      -- exceptions that are not implemented
      badExceptions  = exceptions \\ implemented
      
      -- exceptions that are now registered
      badExceptions' = filter registered exceptions
      
   in catMaybes
      [ check unregistered $ unlines
          [ "The following extensions are known to GHC but are not in the "
          , "extension registry in Language.Haskell.Extension."
          , "  " ++ intercalate "\n  " (map display unregistered)
          , "If these extensions are ready for public consumption then they "
          , "should be registered. If they are still experimental and you "
          , "think they are not ready to be registered then please add them "
          , "to the exceptions list in this test program along with an "
          , "explanation."
          ]
      , check badExceptions $ unlines
          [ "Error in the extension exception list. The following extensions"
          , "are listed as exceptions but are not even implemented by GHC:"
          , "  " ++ intercalate "\n  " (map display badExceptions)
          , "Please fix this test program by correcting the list of"
          , "exceptions."
          ]
      , check badExceptions' $ unlines
          [ "Error in the extension exception list. The following extensions"
          , "are listed as exceptions to registration but they are in fact"
          , "now registered in Language.Haskell.Extension:"
          , "  " ++ intercalate "\n  " (map display badExceptions')
          , "Please fix this test program by correcting the list of"
          , "exceptions."
          ]
      ]
  where
   registered (UnknownExtension _) = False
   registered _                    = True

   check [] _ = Nothing  
   check _  i = Just i


main = topHandler $ do
  [ghcPath] <- getArgs
  exts      <- getExtensions ghcPath
  let problems = checkProblems exts
  putStrLn (intercalate "\n" problems)
  if null problems
    then exitSuccess
    else exitFailure

getExtensions :: FilePath -> IO [Extension]
getExtensions ghcPath =
        map readExtension . lines
    <$> rawSystemStdout normal ghcPath ["--supported-languages"]

readExtension :: String -> Extension
readExtension str = handleNoParse $ do
    -- GHC defines extensions in a positive way, Cabal defines them
    -- relative to H98 so we try parsing ("No" ++ extName) first
    ext <- simpleParse ("No" ++ str)
    case ext of
      UnknownExtension _ -> simpleParse str
      _                  -> return ext
  where
    handleNoParse :: Maybe Extension -> Extension
    handleNoParse = fromMaybe (error $ "unparsable extension " ++ show str)
