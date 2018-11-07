
-- We need to do some ugly hacks here because of GHC magic

module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Text
import System.Cmd
import System.FilePath
import System.Exit
import System.Directory

main :: IO ()
main = do let hooks = simpleUserHooks {
                  regHook = addPrimModule
                          $ regHook simpleUserHooks,
                  buildHook = build_primitive_sources
                            $ buildHook simpleUserHooks,
                  haddockHook = addPrimModuleForHaddock
                              $ build_primitive_sources
                              $ haddockHook simpleUserHooks }
          defaultMainWithHooks hooks

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()

addPrimModule :: Hook a -> Hook a
addPrimModule f pd lbi uhs x =
    do let -- I'm not sure which one of these we actually need to change.
           -- It seems bad that there are two.
           pd' = addPrimModuleToPD pd
           lpd = addPrimModuleToPD (localPkgDescr lbi)
           lbi' = lbi { localPkgDescr = lpd }
       f pd' lbi' uhs x

addPrimModuleForHaddock :: Hook a -> Hook a
addPrimModuleForHaddock f pd lbi uhs x =
    do let pc = withPrograms lbi
           pc' = userSpecifyArgs "haddock" ["GHC/Prim.hs"] pc
           lbi' = lbi { withPrograms = pc' }
       f pd lbi' uhs x

addPrimModuleToPD :: PackageDescription -> PackageDescription
addPrimModuleToPD pd =
    case library pd of
    Just lib ->
        let ems = fromJust (simpleParse "GHC.Prim") : exposedModules lib
            lib' = lib { exposedModules = ems }
        in pd { library = Just lib' }
    Nothing ->
        error "Expected a library, but none found"

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

-- Replace a file only if the new version is different from the old.
-- This prevents make from doing unnecessary work after we run 'setup makefile'
maybeUpdateFile :: FilePath -> FilePath -> IO ()
maybeUpdateFile source target = do
  r <- rawSystem "cmp" ["-s" {-quiet-}, source, target]
  case r of
    ExitSuccess   -> removeFile source
    ExitFailure _ -> do exists <- doesFileExist target
                        when exists $ removeFile target
                        renameFile source target

