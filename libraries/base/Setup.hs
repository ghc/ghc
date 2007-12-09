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
import Distribution.Simple.Program
import Distribution.Version
import System.Cmd
import System.FilePath
import System.Exit
import System.Directory
import Control.Exception (try)

main :: IO ()
main = do let hooks = defaultUserHooks {
                  buildHook = build_primitive_sources
                            $ buildHook defaultUserHooks,
                  makefileHook = build_primitive_sources
                               $ makefileHook defaultUserHooks,
                  haddockHook = build_primitive_sources
                              $ add_prim
                              $ haddockHook defaultUserHooks }
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
              primhs_tmp = addExtension primhs "tmp"
              primopwrappers_tmp = addExtension primopwrappers "tmp"
          maybeExit $ system (genprimopcode ++ " --make-haskell-source < "
                           ++ primops ++ " > " ++ primhs_tmp)
          maybeUpdateFile primhs_tmp primhs
          maybeExit $ system (genprimopcode ++ " --make-haskell-wrappers < "
                           ++ primops ++ " > " ++ primopwrappers_tmp)
          maybeUpdateFile primopwrappers_tmp primopwrappers
      f pd lbi uhs x

add_prim_to_pd pd = pd { library = Just lib' }
  where
    lib' = case library pd of
      Just lib -> lib { exposedModules = "GHC.Prim" : exposedModules lib }
      Nothing  -> error "Expected a library"

add_prim :: Hook a -> Hook a
add_prim f pd lbi uhs x = do
  let mbHaddockProg = lookupProgram haddockProgram (withPrograms lbi)
  case mbHaddockProg of
    Nothing -> f pd lbi uhs x
    Just haddockProg -> do
      let
        Just version = programVersion haddockProg
        pd' = if version < Version [2,0] [] then add_prim_to_pd pd else pd
      f pd' lbi uhs x

-- Replace a file only if the new version is different from the old.
-- This prevents make from doing unnecessary work after we run 'setup makefile'
maybeUpdateFile :: FilePath -> FilePath -> IO ()
maybeUpdateFile source target = do
  r <- rawSystem "cmp" ["-s" {-quiet-}, source, target]
  case r of 
    ExitSuccess   -> removeFile source
    ExitFailure _ -> do try (removeFile target); renameFile source target

