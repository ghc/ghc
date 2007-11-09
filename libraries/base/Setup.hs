{-
We need to do some ugly hacks here as base mix of portable and
unportable stuff, as well as home to some GHC magic.
-}

module Main (main) where

import Control.Monad
import Data.List
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import System.Cmd
import System.FilePath

main :: IO ()
main = do let hooks = defaultUserHooks {
                  buildHook = build_primitive_sources
                            $ filter_modules_hook
                            $ buildHook defaultUserHooks,
                  makefileHook = build_primitive_sources
                               $ filter_modules_hook
                               $ makefileHook defaultUserHooks,
                  haddockHook = build_primitive_sources
                               $ filter_modules_hook
                               $ haddockHook defaultUserHooks,
                   instHook = filter_modules_hook
                           $ instHook defaultUserHooks }
          defaultMainWithHooks hooks

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()

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
 = let lib' = case library pd of
                  Just lib ->
                      let ems = filter ("GHC.Prim" /=) (exposedModules lib)
                      in lib { exposedModules = ems }
                  Nothing -> error "Expected a library"
       pd' = pd { library = Just lib' }
   in f pd' lbi uhs x

