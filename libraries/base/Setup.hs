{-
We need to do some ugly hacks here as base mix of portable and
unportable stuff, as well as home to some GHC magic.
-}

module Main (main) where

import Control.Monad
import Data.List
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import System.Cmd
import System.FilePath
import System.Info

main :: IO ()
main = do let hooks = defaultUserHooks {
                  confHook = add_extra_deps
                           $ confHook defaultUserHooks,
                  buildHook = build_primitive_sources
                            $ filter_modules_hook
                            $ buildHook defaultUserHooks,
                  makefileHook = filter_modules_hook
                               $ makefileHook defaultUserHooks,
                  regHook = add_extra_libs
                          $ regHook defaultUserHooks,
                  instHook = filter_modules_hook
                           $ instHook defaultUserHooks }
          defaultMainWithHooks hooks

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()
type ConfHook = PackageDescription -> ConfigFlags -> IO LocalBuildInfo

-- type PDHook = PackageDescription -> ConfigFlags -> IO ()

build_primitive_sources :: Hook a -> Hook a
build_primitive_sources f pd lbi uhs x
 = do when (compilerFlavor (compiler lbi) == GHC) $ do
          let genprimopcode = joinPath ["..", "..", "utils",
                                        "genprimopcode", "genprimopcode"]
              primops = joinPath ["..", "..", "compiler", "prelude",
                                  "primops.txt"]
              primhs = joinPath ["GHC", "Prim.hs"]
              primopwrappers = joinPath ["GHC", "PrimopWrappers.hs"]
          maybeExit $ system (genprimopcode ++ " --make-haskell-source < "
                           ++ primops ++ " > " ++ primhs)
          maybeExit $ system (genprimopcode ++ " --make-haskell-wrappers < "
                           ++ primops ++ " > " ++ primopwrappers)
      f pd lbi uhs x

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
 | otherwise = s `notElem` ["Foreign.Concurrent", "System.Timeout"]

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
