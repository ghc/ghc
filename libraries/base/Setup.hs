
{-
We need to do some ugly hacks here as base mix of portable and
unportable stuff, as well as home to some GHC magic.
-}

module Main (main) where

import Control.Monad
import Data.List
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple.LocalBuildInfo
import System.Environment
import System.Info

main :: IO ()
main = do args <- getArgs
          let (ghcArgs, args') = extractGhcArgs args
              (confArgs, args'') = extractConfigureArgs args'
              hooks = defaultUserHooks {
                  confHook = add_extra_deps
                           $ confHook defaultUserHooks,
                  postConf = add_configure_options confArgs
                           $ postConf defaultUserHooks,
                  buildHook = add_ghc_options ghcArgs
                            $ filter_modules_hook
                            $ buildHook defaultUserHooks,
                  makefileHook = add_ghc_options ghcArgs
                               $ filter_modules_hook
                               $ makefileHook defaultUserHooks,
                  regHook = add_extra_libs
                          $ regHook defaultUserHooks,
                  instHook = filter_modules_hook
                           $ instHook defaultUserHooks }
          withArgs args'' $ defaultMainWithHooks hooks

extractGhcArgs :: [String] -> ([String], [String])
extractGhcArgs = extractPrefixArgs "--ghc-option="

extractConfigureArgs :: [String] -> ([String], [String])
extractConfigureArgs = extractPrefixArgs "--configure-option="

extractPrefixArgs :: String -> [String] -> ([String], [String])
extractPrefixArgs the_prefix args
 = let f [] = ([], [])
       f (x:xs) = case f xs of
                      (wantedArgs, otherArgs) ->
                          case removePrefix the_prefix x of
                              Just wantedArg ->
                                  (wantedArg:wantedArgs, otherArgs)
                              Nothing ->
                                  (wantedArgs, x:otherArgs)
   in f args

removePrefix :: String -> String -> Maybe String
removePrefix "" ys = Just ys
removePrefix _  "" = Nothing
removePrefix (x:xs) (y:ys)
 | x == y = removePrefix xs ys
 | otherwise = Nothing

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()
type ConfHook = PackageDescription -> ConfigFlags -> IO LocalBuildInfo
type PostConfHook = Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo
                 -> IO ()

-- type PDHook = PackageDescription -> ConfigFlags -> IO ()

add_ghc_options :: [String] -> Hook a -> Hook a
add_ghc_options args f pd lbi uhs x
 = do let lib' = case library pd of
                     Just lib ->
                         let bi = libBuildInfo lib
                             opts = options bi ++ [(GHC, args)]
                             bi' = bi { options = opts }
                         in lib { libBuildInfo = bi' }
                     Nothing -> error "Expected a library"
          pd' = pd { library = Just lib' }
      f pd' lbi uhs x

add_configure_options :: [String] -> PostConfHook -> PostConfHook
add_configure_options args f as cfs pd lbi
 = f (as ++ args) cfs pd lbi

filter_modules_hook :: Hook a -> Hook a
filter_modules_hook f pd lbi uhs x
 = let build_filter = case compilerFlavor $ compiler lbi of
                          GHC -> forGHCBuild
                          _ -> isPortableBuild
       lib' = case library pd of
                  Just lib ->
                      let ems = filter build_filter (exposedModules lib)
                      in lib { exposedModules = ems }
                  Nothing -> error "Expected a library"
       pd' = pd { library = Just lib' }
   in f pd' lbi uhs x

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

add_extra_libs :: Hook a -> Hook a
add_extra_libs f pd lbi uhs x
 = let pd' = if (os == "mingw32") && (compilerFlavor (compiler lbi) == GHC)
             then case library pd of
                  Just lib ->
                      let lib_bi = libBuildInfo lib
                          lib_bi' = lib_bi { extraLibs = "wsock32"
                                                       : "msvcrt"
                                                       : "kernel32"
                                                       : "user32"
                                                       : "shell32"
                                                       : extraLibs lib_bi }
                          lib' = lib { libBuildInfo = lib_bi' }
                      in pd { library = Just lib' }
                  Nothing -> error "Expected a library"
             else pd
   in f pd' lbi uhs x

