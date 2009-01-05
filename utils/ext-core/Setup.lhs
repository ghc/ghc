#!/usr/bin/env runhaskell
\begin{code}
{-# OPTIONS -Wall -cpp #-}

import Control.Monad
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import System.Cmd
import System.FilePath
import System.Exit
import System.Directory
import Control.Exception

main :: IO ()
main = do
   let hooks = simpleUserHooks {
                 buildHook = build_primitive_sources 
                           $ buildHook simpleUserHooks
            }
   defaultMainWithHooks hooks
\end{code}

Mostly snarfed from ghc-prim's Setup.hs.

\begin{code}
type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()


-- Hack: If PrimEnv.hs exists *and* genprimopcode or
-- primops.txt doesn't exist, don't rebuild PrimEnv.hs

build_primitive_sources :: Hook a -> Hook a
build_primitive_sources f pd lbi uhs x
 = do when (compilerFlavor (compiler lbi) == GHC) $ do
          let genprimopcode = joinPath ["..", "..", "utils",
                                        "genprimopcode", "genprimopcode"]
              primops = joinPath ["..", "..", "compiler", "prelude",
                                  "primops.txt"]
              primhs = joinPath ["Language", "Core", "PrimEnv.hs"]
              primhs_tmp = addExtension primhs "tmp"
          primEnvExists <- doesFileExist primhs
          genprimopcodeExists <- doesFileExist genprimopcode
          primopsExists <- doesFileExist primops
          unless (primEnvExists && not genprimopcodeExists && not primopsExists) $ do
             maybeExit $ system (genprimopcode ++ " --make-ext-core-source < "
                           ++ primops ++ " > " ++ primhs_tmp)
             maybeUpdateFile primhs_tmp primhs
             maybeExit $ system ("make -C lib/GHC_ExtCore")
      f pd lbi uhs x

-- Replace a file only if the new version is different from the old.
-- This prevents make from doing unnecessary work after we run 'setup makefile'
maybeUpdateFile :: FilePath -> FilePath -> IO ()
maybeUpdateFile source target = do
  r <- rawSystem "cmp" ["-s" {-quiet-}, source, target]
  case r of
    ExitSuccess   -> removeFile source
    ExitFailure _ -> do 
#if __GLASGOW_HASKELL__ >= 610
      (try :: IO () -> IO (Either IOException ()))
#else
      try
#endif 
       (removeFile target)
      renameFile source target
\end{code}