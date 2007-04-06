
{-
We need to do some ugly hacks here as base mix of portable and
unportable stuff, as well as home to some GHC magic.
-}

module Main (main) where

import Control.Monad
import Data.List
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.PreProcess
import Distribution.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import System.Environment
import System.Exit

main :: IO ()
main = do args <- getArgs
          let (ghcArgs, args') = extractGhcArgs args
          let hooks = defaultUserHooks {
                  confHook = add_extra_deps
                           $ confHook defaultUserHooks,
                  buildHook = add_ghc_options ghcArgs
                            $ filter_modules_hook
                            $ buildHook defaultUserHooks,
                  instHook = filter_modules_hook
                           $ instHook defaultUserHooks }
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
type ConfHook = PackageDescription -> ConfigFlags -> IO LocalBuildInfo

-- type PDHook = PackageDescription -> ConfigFlags -> IO ()

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

filter_modules_hook :: Hook a -> Hook a
filter_modules_hook f pd lbi muhs x
 = let build_filter = case compilerFlavor $ compiler lbi of
                          GHC -> forGHCBuild
                          _ -> isPortableBuild
       lib' = case library pd of
                  Just lib ->
                      let ems = filter build_filter (exposedModules lib)
                      in lib { exposedModules = ems }
                  Nothing -> error "Expected a library"
       pd' = pd { library = Just lib' }
   in f pd' lbi muhs x

isPortableBuild :: String -> Bool
isPortableBuild s
 | "GHC" `isPrefixOf` s = False
 | "Data.Generics" `isPrefixOf` s = False
 | otherwise = s `notElem` ["Foreign.Concurrent", "System.Process"]

forGHCBuild :: String -> Bool
forGHCBuild = ("GHC.Prim" /=)

add_extra_deps :: ConfHook -> ConfHook
add_extra_deps f pd cf
 = do lbi <- f pd cf
      case compilerFlavor (compiler lbi) of
          GHC ->
              do -- Euch. We should just add the right thing to the lbi
                 -- ourselves rather than rerunning configure.
                 let pd' = pd { buildDepends = Dependency "rts" AnyVersion
                                             : buildDepends pd }
                 f pd' cf
          _ ->
              return lbi

