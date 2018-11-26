module Rules.BinaryDist where

import Hadrian.Haskell.Cabal

import Context
import Expression
import Oracles.Setting
import Packages
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
        version        <- setting ProjectVersion
        targetPlatform <- setting TargetPlatformFull
        cabalHostOs    <- cabalOsString <$> setting BuildOs
        cabalHostArch  <- cabalArchString <$> setting BuildArch
        rtsDir         <- pkgIdentifier rts

        let ghcBuildDir      = root -/- stageString Stage1
            bindistFilesDir  = root -/- "bindist" -/- ghcVersionPretty
            ghcVersionPretty = "ghc-" ++ version ++ "-" ++ targetPlatform
            distDir          = cabalHostArch ++ "-" ++ cabalHostOs ++ "-ghc-" ++ version
            rtsIncludeDir    = ghcBuildDir -/- "lib" -/- distDir -/- rtsDir
                               -/- "include"

        -- We create the bindist directory at <root>/bindist/ghc-X.Y.Z-platform/
        -- and populate it with Stage2 build results
        createDirectory bindistFilesDir
        copyDirectory (ghcBuildDir -/- "bin") bindistFilesDir
        copyDirectory (ghcBuildDir -/- "lib") bindistFilesDir
        copyDirectory (rtsIncludeDir)         bindistFilesDir
        {- TODO: Should we ship docs?
        need ["docs"]
        copyDirectory (root -/- "docs") bindistFilesDir -}

        -- We then 'need' all the files necessary to configure and install
        -- (as in, './configure [...] && make install') this build on some
        -- other machine.
        need $ map (bindistFilesDir -/-)
                   (["configure", "Makefile"] ++ bindistInstallFiles)
        need $ map ((bindistFilesDir -/- "wrappers") -/-) ["check-api-annotations"
                   , "check-ppr", "ghc", "ghc-iserv", "ghc-pkg", "ghc-split"
                   , "ghci-script", "ghci", "haddock", "hpc", "hp2ps", "hsc2hs"
                   , "runghc"]

        -- Finally, we create the archive <root>/bindist/ghc-X.Y.Z-platform.tar.xz
        command [Cwd $ root -/- "bindist"] "tar"
            [ "-c", "--xz", "-f"
            , ghcVersionPretty <.> "tar.xz"
            , ghcVersionPretty ]

    -- Prepare binary distribution configure script
    -- (generated under <ghc root>/distrib/configure by 'autoreconf')
    root -/- "bindist" -/- "ghc-*" -/- "configure" %> \configurePath -> do
        ghcRoot <- topDirectory
        copyFile (ghcRoot -/- "aclocal.m4") (ghcRoot -/- "distrib" -/- "aclocal.m4")
        buildWithCmdOptions [] $
            target (vanillaContext Stage1 ghc) (Autoreconf $ ghcRoot -/- "distrib") [] []
        -- We clean after ourselves, moving the configure script we generated in
        -- our bindist dir
        removeFile (ghcRoot -/- "distrib" -/- "aclocal.m4")
        moveFile (ghcRoot -/- "distrib" -/- "configure") configurePath

    -- Generate the Makefile that enables the "make install" part
    root -/- "bindist" -/- "ghc-*" -/- "Makefile" %> \makefilePath ->
        writeFile' makefilePath bindistMakefile

    root -/- "bindist" -/- "ghc-*" -/- "wrappers/*" %> \wrapperPath ->
        writeFile' wrapperPath $ wrapper (takeFileName wrapperPath)

    -- Copy various configure-related files needed for a working
    -- './configure [...] && make install' workflow
    -- (see the list of files needed in the 'binary-dist' rule above, before
    -- creating the archive).
    forM_ bindistInstallFiles $ \file ->
        root -/- "bindist" -/- "ghc-*" -/- file %> \dest -> do
            ghcRoot <- topDirectory
            copyFile (ghcRoot -/- fixup file) dest

  where
    fixup f | f `elem` ["INSTALL", "README"] = "distrib" -/- f
            | otherwise                      = f

-- TODO: This list is surely incomplete -- fix this.
-- | A list of files that allow us to support a simple
-- @./configure [--prefix=PATH] && make install@ workflow.
bindistInstallFiles :: [FilePath]
bindistInstallFiles =
    [ "config.sub", "config.guess", "install-sh", "mk" -/- "config.mk.in"
    , "mk" -/- "install.mk.in", "mk" -/- "project.mk", "settings.in", "README"
    , "INSTALL" ]

-- | This auxiliary function gives us a top-level 'Filepath' that we can 'need'
-- for all libraries and programs that are needed for a complete build.
-- For libraries, it returns the path to the @.conf@ file in the package
-- database. For programs, it returns the path to the compiled executable.
pkgTarget :: Package -> Action FilePath
pkgTarget pkg
    | isLibrary pkg = pkgConfFile (vanillaContext Stage1 pkg)
    | otherwise     = programPath =<< programContext Stage1 pkg

-- TODO: Augment this Makefile to match the various parameters that the current
-- bindist scripts support.
-- | A trivial Makefile that only takes @$prefix@ into account, and not e.g
-- @$datadir@ (for docs) and other variables, yet.
bindistMakefile :: String
bindistMakefile = unlines
    [ "MAKEFLAGS += --no-builtin-rules"
    , ".SUFFIXES:"
    , ""
    , "include mk/install.mk"
    , "include mk/config.mk"
    , ""
    , ".PHONY: default"
    , "default:"
    , "\t@echo 'Run \"make install\" to install'"
    , "\t@false"
    , ""
    , "#------------------------------------------------------------------------------"
    , "# INSTALL RULES"
    , ""
    , "# Hacky function to check equality of two strings"
    , "# TODO : find if a better function exists"
    , "eq=$(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))"
    , ""
    , "define installscript"
    , "# $1 = package name"
    , "# $2 = wrapper path"
    , "# $3 = bindir"
    , "# $4 = ghcbindir"
    , "# $5 = Executable binary path"
    , "# $6 = Library Directory"
    , "# $7 = Docs Directory"
    , "# $8 = Includes Directory"
    , "# We are installing wrappers to programs by searching corresponding wrappers."
    , "# If wrapper is not found, we are attaching the common wrapper to it "
    , "# This implementation is a bit hacky and depends on consistency of program"
    , "# names. For hadrian build this will work as programs have a consistent "
    , "# naming procefure. This file is tested on Linux(Ubuntu)"
    , "# TODO : Check implementation in other distributions"
    , "\trm -f $2"
    , "\t$(CREATE_SCRIPT) $2"
    , "\t@echo \"#!$(SHELL)\" >>  $2"
    , "\t@echo \"exedir=\\\"$4\\\"\" >> $2"
    , "\t@echo \"exeprog=\\\"$1\\\"\" >> $2"
    , "\t@echo \"executablename=\\\"$5\\\"\" >> $2"
    , "\t@echo \"bindir=\\\"$3\\\"\" >> $2"
    , "\t@echo \"libdir=\\\"$6\\\"\" >> $2"
    , "\t@echo \"docdir=\\\"$7\\\"\" >> $2"
    , "\t@echo \"includedir=\\\"$8\\\"\" >> $2"
    , "\t@echo \"\" >> $2 "
    , "\tcat wrappers/$1 >> $2"
    , "\t$(EXECUTABLE_FILE) $2 ;"
    , "endef"
    , ""
    , "# QUESTION : should we use shell commands?"
    , ""
    , "# Due to the fact that package database is configured relatively"
    , "# We do not change the relative paths of executables and libraries"
    , "# But instead use wrapper scripts whenever necessary"
    , "LIBPARENT = $(shell dirname $(libdir))"
    , "GHCBINDIR = \"$(LIBPARENT)/bin\""
    , ""
    , ".PHONY: install"
    , "install: install_bin install_lib install_includes"
    , ""
    , "# Check if we need to install docs"
    , "ifeq \"DOCS\" \"YES\""
    , "install: install_docs"
    , "endif"
    , ""
    , "# If the relative path of binaries and libraries are altered, we will need to"
    , "# install additional wrapper scripts at bindir."
    , "ifneq \"$(LIBPARENT)/bin\" \"$(bindir)\""
    , "install: install_wrappers"
    , "endif"
    , ""
    , "# We need to install binaries relative to libraries."
    , "BINARIES = $(wildcard ./bin/*)"
    , "install_bin:"
    , "\t@echo \"Copying Binaries to $(GHCBINDIR)\""
    , "\t$(INSTALL_DIR) \"$(GHCBINDIR)\""
    , "\tfor i in $(BINARIES); do \\"
    , "\t\tcp -R $$i \"$(GHCBINDIR)\"; \\"
    , "\tdone"
    , "\t@echo \"Copying and installing ghci\""
    , "\trm -f $(GHCBINDIR)/dir"
    , "\t$(CREATE_SCRIPT) $(GHCBINDIR)/ghci"
    , "\t@echo \"#!$(SHELL)\" >>  $(GHCBINDIR)/ghci"
    , "\tcat wrappers/ghci-script >> $(GHCBINDIR)/ghci"
    , "\t$(EXECUTABLE_FILE) $(GHCBINDIR)/ghci"
    , ""
    , "LIBRARIES = $(wildcard ./lib/*)"
    , "install_lib:"
    , "\t@echo \"Copying libraries to $(libdir)\""
    , "\t$(INSTALL_DIR) \"$(libdir)\""
    , "\tfor i in $(LIBRARIES); do \\"
    , "\t\tcp -R $$i \"$(libdir)/\"; \\"
    , "\tdone"
    , ""
    , "INCLUDES = $(wildcard ./include/*)"
    , "install_includes:"
    , "\t@echo \"Copying libraries to $(includedir)\""
    , "\t$(INSTALL_DIR) \"$(includedir)\""
    , "\tfor i in $(INCLUDES); do \\"
    , "\t\tcp -R $$i \"$(includedir)/\"; \\"
    , "\tdone"
    , ""
    , "DOCS = $(wildcard ./docs/*)"
    , "install_docs:"
    , "\t@echo \"Copying libraries to $(docdir)\""
    , "\t$(INSTALL_DIR) \"$(docdir)\""
    , "\tfor i in $(DOCS); do \\"
    , "\t\tcp -R $$i \"$(docdir)/\"; \\"
    , "\tdone"
    , ""
    , "BINARY_NAMES=$(shell ls ./bin/)"
    , "install_wrappers:"
    , "\t@echo \"Installing Wrapper scripts\""
    , "\t$(INSTALL_DIR) \"$(bindir)\""
    , "\t$(foreach p, $(BINARY_NAMES),\\"
    , "\t\t$(call installscript,$p,$(bindir)/$p,$(bindir),$(GHCBINDIR),$(GHCBINDIR)/$p,$(libdir),$(docdir),$(includedir)))"
    , ""
    , "# END INSTALL"
    , "# -----------------------------------------------------------------------------" ]

wrapper :: FilePath -> String
wrapper "ghc"         = ghcWrapper
wrapper "ghc-pkg"     = ghcPkgWrapper
wrapper "ghci"        = ghciWrapper
wrapper "ghci-script" = ghciScriptWrapper
wrapper "haddock"     = haddockWrapper
wrapper "hsc2hs"      = hsc2hsWrapper
wrapper "runghc"      = runGhcWrapper
wrapper _             = commonWrapper

-- | Wrapper scripts for different programs. Common is default wrapper.

ghcWrapper :: String
ghcWrapper = "exec \"$executablename\" -B\"$libdir\" ${1+\"$@\"}\n"

ghcPkgWrapper :: String
ghcPkgWrapper = unlines
    [ "PKGCONF=\"$libdir/package.conf.d\""
    , "exec \"$executablename\" --global-package-db \"$PKGCONF\" ${1+\"$@\"}" ]

ghciWrapper :: String
ghciWrapper = "exec \"$executablename\" --interactive \"$@\"\n"

haddockWrapper :: String
haddockWrapper = "exec \"$executablename\" -B\"$libdir\" -l\"$libdir\" ${1+\"$@\"}\n"

commonWrapper :: String
commonWrapper = "exec \"$executablename\" ${1+\"$@\"}\n"

hsc2hsWrapper :: String
hsc2hsWrapper = unlines
    [ "HSC2HS_EXTRA=\"--cflag=-fno-stack-protector --lflag=-fuse-ld=gold\""
    , "tflag=\"--template=$libdir/template-hsc.h\""
    , "Iflag=\"-I$includedir/\""
    , "for arg do"
    , "    case \"$arg\" in"
    , "# On OS X, we need to specify -m32 or -m64 in order to get gcc to"
    , "# build binaries for the right target. We do that by putting it in"
    , "# HSC2HS_EXTRA. When cabal runs hsc2hs, it passes a flag saying which"
    , "# gcc to use, so if we set HSC2HS_EXTRA= then we don't get binaries"
    , "# for the right platform. So for now we just don't set HSC2HS_EXTRA="
    , "# but we probably want to revisit how this works in the future."
    , "#        -c*)          HSC2HS_EXTRA=;;"
    , "#        --cc=*)       HSC2HS_EXTRA=;;"
    , "        -t*)          tflag=;;"
    , "        --template=*) tflag=;;"
    , "        --)           break;;"
    , "    esac"
    , "done"
    , "exec \"$executablename\" ${tflag:+\"$tflag\"} $HSC2HS_EXTRA ${1+\"$@\"} \"$Iflag\"" ]

runGhcWrapper :: String
runGhcWrapper = "exec \"$executablename\" -f \"$exedir/ghc\" ${1+\"$@\"}\n"

-- | We need to ship ghci executable, which basically just calls ghc with
-- | --interactive flag.
ghciScriptWrapper :: String
ghciScriptWrapper = unlines
    [ "DIR=`dirname \"$0\"`"
    , "executable=\"$DIR/ghc\""
    , "exec $executable --interactive \"$@\"" ]
