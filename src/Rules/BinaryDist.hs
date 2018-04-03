module Rules.BinaryDist where

import Expression
import GHC
import Oracles.Setting
import Settings
import Target
import Utilities

bindistRules :: Rules ()
bindistRules = do
    root <- buildRootRules
    phony "binary-dist" $ do
      -- We 'need' all binaries and libraries
      targets <- mapM pkgTarget =<< stagePackages Stage1
      need targets

      version <- setting ProjectVersion
      targetPlatform <- setting TargetPlatformFull

      let ghcBuildDir      = root -/- stageString Stage1
          bindistFilesDir  = root -/- "bindist" -/- ghcVersionPretty
          ghcVersionPretty = "ghc-" ++ version ++ "-" ++ targetPlatform

      -- we create the bindist directory at <root>/bindist/ghc-X.Y.Z-platform/
      -- and populate it with a stage2 build
      createDirectory bindistFilesDir
      copyDirectory (ghcBuildDir -/- "bin") bindistFilesDir
      copyDirectory (ghcBuildDir -/- "lib") bindistFilesDir
      {- SHOULD WE SHIP DOCS?
      need ["docs"]
      copyDirectory (root -/- "docs") bindistFilesDir
      -}

      -- we then 'need' all the files necessary to configure and install
      -- (as in, './configure [...] && make install') this build on some
      -- other machine.
      need $ map (bindistFilesDir -/-)
                 (["configure", "Makefile"] ++ bindistInstallFiles)

      -- finally, we create the archive, at
      -- <root>/bindist/ghc-X.Y.Z-platform.tar.xz
      command [Cwd $ root -/- "bindist"] "tar"
        [ "-c", "--xz", "-f"
        , ghcVersionPretty <.> "tar.xz"
        , ghcVersionPretty
        ]

    -- prepare binary distribution configure script
    -- (generated under <ghc root>/distrib/configure by 'autoreconf')
    root -/- "bindist" -/- "ghc-*" -/- "configure" %> \configurePath -> do
      ghcRoot <- topDirectory
      copyFile (ghcRoot -/- "aclocal.m4") (ghcRoot -/- "distrib" -/- "aclocal.m4")
      buildWithCmdOptions [] $
        target (vanillaContext Stage1 ghc) (Autoreconf $ ghcRoot -/- "distrib") [] []
      -- we clean after ourselves, moving the configure script we generated in
      -- our bindist dir
      removeFile (ghcRoot -/- "distrib" -/- "aclocal.m4")
      moveFile (ghcRoot -/- "distrib" -/- "configure") configurePath

    -- generate the Makefile that enables the "make install" part
    root -/- "bindist" -/- "ghc-*" -/- "Makefile" %> \makefilePath ->
      writeFile' makefilePath bindistMakefile

    -- copy over the various configure-related files needed for a working
    -- './configure [...] && make install' workflow
    -- (see the list of files needed in the 'binary-dist' rule above, before
    -- creating the archive).
    forM_ bindistInstallFiles $ \file ->
      root -/- "bindist" -/- "ghc-*" -/- file %> \dest -> do
        ghcRoot <- topDirectory
        copyFile (ghcRoot -/- fixup file) dest

  where fixup f
          | f `elem` ["INSTALL", "README"] = "distrib" -/- f
          | otherwise                      = f

-- | A list of files that allow us to support a simple
--   @./configure [--prefix=PATH] && make install@ workflow.
--
--   NOTE: the list surely is incomplete
bindistInstallFiles :: [FilePath]
bindistInstallFiles =
  [ "config.sub", "config.guess", "install-sh"
  , "mk" -/- "config.mk.in", "mk" -/- "install.mk.in"
  , "settings.in", "README", "INSTALL"
  ]

-- | Auxiliary function that gives us a 'Filepath' we can 'need' for
--   all libraries and programs that are needed for a complete build.
--
--   For libraries, it returns the path to the .conf file in the package db.
--   For executables, it returns the path to the compiled executable.
pkgTarget :: Package -> Action FilePath
pkgTarget pkg
  | isLibrary pkg = pkgConfFile (Context Stage1 pkg $ read "v")
  | otherwise     = programPath =<< programContext Stage1 pkg

-- TODO: augment this makefile to match the various parameters that
-- the current bindist scripts support.
-- | A trivial makefile that only takes @$prefix@ into account,
--   and not e.g @$datadir@ (for docs) and other variables, yet.
bindistMakefile :: String
bindistMakefile = unlines
  [ "MAKEFLAGS += --no-builtin-rules"
  , ".SUFFIXES:"
  , ""
  , "include mk/install.mk"
  , ""
  , ".PHONY: default"
  , "default:"
  , "\t@echo 'Run \"make install\" to install'"
  , "\t@false"
  , ""
  , ".PHONY: install"
  , "install:"
  , "\tmkdir -p $(prefix)"
  , "\tcp settings lib/settings"
  , "\tcp -R bin $(prefix)/"
  , "\tcp -R lib $(prefix)/"
  ]
