
module Main (main) where

import Data.List
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.PreProcess
import Distribution.Setup
import Distribution.Simple.LocalBuildInfo
import System.Environment

main :: IO ()
main = do args <- getArgs
          let (ghcArgs, args') = extractGhcArgs args
          let hooks = defaultUserHooks {
                  buildHook = add_ghc_options ghcArgs
                            $ buildHook defaultUserHooks }
          withArgs args' $ defaultMainWithHooks hooks

extractGhcArgs :: [String] -> ([String], [String])
extractGhcArgs args
 = let f [] = ([], [])
       f (x:xs) = case f xs of
                      (ghcArgs, otherArgs) ->
                          case removePrefix "--ghc-option=" x of
                              Just ghcArg ->
                                  (ghcArg:ghcArgs, otherArgs)
                              Nothing ->
                                  (ghcArgs, x:otherArgs)
   in f args

removePrefix :: String -> String -> Maybe String
removePrefix "" ys = Just ys
removePrefix (x:xs) (y:ys)
 | x == y = removePrefix xs ys
 | otherwise = Nothing

type Hook a = PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> a
           -> IO ()

add_ghc_options :: [String] -> Hook a -> Hook a
add_ghc_options args f pd lbi muhs x
 = do let lib' = case library pd of
                     Just lib ->
                         let bi = libBuildInfo lib
                             opts = options bi ++ [(GHC, args)]
                             bi' = bi { options = opts }
                         in lib { libBuildInfo = bi' }
                     Nothing -> error "Expected a library"
          pd' = pd { library = Just lib' }
      f pd' lbi muhs x

