{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

-- $1_$2_$3_MOST_DIR_HC_OPTS = \
--  $$($1_$2_$3_MOST_HC_OPTS) \
--  -odir $1/$2/build -hidir $1/$2/build -stubdir $1/$2/build

-- # Some of the Haskell files (e.g. utils/hsc2hs/Main.hs) (directly or
-- # indirectly) include the generated includes files.
-- $$($1_$2_depfile_haskell) : $$(includes_H_CONFIG) $$(includes_H_PLATFORM)
-- 
-- $$($1_$2_depfile_haskell) : $$($1_$2_HS_SRCS) $$($1_$2_HS_BOOT_SRCS) $$$$($1_$2_HC_MK_DEPEND_DEP) | $$$$(dir $$$$@)/.
--     $$(call removeFiles,$$@.tmp)
-- ifneq "$$($1_$2_HS_SRCS)" ""
--     "$$($1_$2_HC_MK_DEPEND)" -M \
--         $$($1_$2_$$(firstword $$($1_$2_WAYS))_MOST_DIR_HC_OPTS) \
--         $$($1_$2_MKDEPENDHS_FLAGS) \
--         $$($1_$2_HS_SRCS)
-- endif
--     echo "$1_$2_depfile_haskell_EXISTS = YES" >> $$@.tmp
-- ifneq "$$($1_$2_SLASH_MODS)" ""
--     for dir in $$(sort $$(foreach mod,$$($1_$2_SLASH_MODS),$1/$2/build/$$(dir $$(mod)))); do \
--         if test ! -d $$$$dir; then mkdir -p $$$$dir; fi \
--     done
-- endif

-- "inplace/bin/ghc-stage1.exe" -M -static  -H32m -O    -this-package-key deeps_FT5iVCELxOr62eHY0nbvnU -hide-all-packages -i -ilibraries/deepseq/. -ilibraries/deepseq/dist-install/build -ilibraries/deepseq/dist-install/build/autogen -Ilibraries/deepseq/dist-install/build -Ilibraries/deepseq/dist-install/build/autogen -Ilibraries/deepseq/.    -optP-include -optPlibraries/deepseq/dist-install/build/autogen/cabal_macros.h -package-key array_3w0nMK0JfaFJPpLFn2yWAJ -package-key base_469rOtLAqwTGFEOGWxSUiQ -package-key ghcpr_FgrV6cgh2JHBlbcx1OSlwt -Wall -XHaskell2010 -O2  -no-user-package-db -rtsopts      -odir libraries/deepseq/dist-install/build -hidir libraries/deepseq/dist-install/build -stubdir libraries/deepseq/dist-install/build -dep-makefile libraries/deepseq/dist-install/build/.depend-v-p.haskell.tmp -dep-suffix "" -dep-suffix "p_" -include-pkg-deps  libraries/deepseq/./Control/DeepSeq.hs

-- $1_$2_$3_MOST_HC_OPTS = \
--  $$(WAY_$3_HC_OPTS) \
--  $$(CONF_HC_OPTS) \
--  $$(SRC_HC_OPTS) \
--  $$($1_HC_OPTS) \
--  $$($1_$2_HC_PKGCONF) \
--  $$(if $$($1_$2_PROG),, \
--         $$(if $$($1_PACKAGE),$$($4_THIS_PACKAGE_KEY) $$($1_$2_PACKAGE_KEY))) \
--  $$(if $$($1_PACKAGE),-hide-all-packages) \
--  -i $$(if $$($1_$2_HS_SRC_DIRS),$$(foreach dir,$$($1_$2_HS_SRC_DIRS),-i$1/$$(dir)),-i$1) \
--  -i$1/$2/build -i$1/$2/build/autogen \
--  -I$1/$2/build -I$1/$2/build/autogen \
--  $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) \
--  $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) \
--  $$(foreach inc,$$($1_$2_INCLUDE),-\#include "$$(inc)") \
--  $$(foreach opt,$$($1_$2_CPP_OPTS),-optP$$(opt)) \
--  $$(if $$($1_PACKAGE),-optP-include -optP$1/$2/build/autogen/cabal_macros.h) \
--  $$($1_$2_$4_DEP_OPTS) \
--  $$($1_$2_HC_OPTS) \
--  $$(CONF_HC_OPTS_STAGE$4) \
--  $$($1_$2_MORE_HC_OPTS) \
--  $$($1_$2_EXTRA_HC_OPTS) \
--  $$($1_$2_$3_HC_OPTS) \
--  $$($$(basename $$(subst ./,,$$<))_HC_OPTS) \
--  $$(SRC_HC_WARNING_OPTS) \
--  $$(EXTRA_HC_OPTS)

-- TODO: add $1_HC_OPTS
-- TODO: check that the package is not a program ($1_$2_PROG == "")
-- TODO: handle empty $1_PACKAGE (can it be empty?)
-- TODO: $1_$2_INCLUDE appears to be not set. Safe to skip?
-- Option CONF_HC_OPTS is skipped
buildPackageDependencies :: Package -> TodoItem -> Rules ()
buildPackageDependencies pkg @ (Package name path _) (stage, dist, settings) =
    let buildDir = path </> dist
    in
    (buildDir </> "build" </> name <.> "m") %> \out -> do
        need ["shake/src/Package/Dependencies.hs"] -- Track changes in this file
        let pkgData = buildDir </> "package-data.mk"
        usePackageKey <- SupportsPackageKey || stage /= Stage0 -- TODO: check reasoning (distdir-way-opts)
        mods    <- map (replaceEq '.' pathSeparator) <$> arg (Modules pkgData)
        srcDirs <- arg $ SrcDirs pkgData
        srcs    <- getDirectoryFiles "" $ [path </> dir </> mPath <.> ext | dir <- srcDirs, mPath <- mods, ext <- ["hs", "lhs"]]
        run (Ghc stage) $ mconcat
            [ arg "-M"
            , wayHcOpts vanilla -- TODO: i) is this needed? ii) shall we run GHC -M multiple times?
            , arg SrcHcOpts
            , when (stage == Stage0) $ arg "-package-db libraries/bootstrapping.conf"
            -- TODO: check reasoning ($$($4_THIS_PACKAGE_KEY) $$($1_$2_PACKAGE_KEY))
            , arg $ if usePackageKey then "-this-package-key" else "-package-name"
            , arg $ PackageKey pkgData
            , arg "-hide-all-packages"
            , arg "-i" -- resets the search path to nothing; TODO: check if really needed
            , arg $ map (\d -> "-i" ++ path </> d) srcDirs
            , arg $ do
                prefix <- ["-i", "-I"] -- 'import' and '#include' search paths
                suffix <- ["build", "build/autogen"]
                return $ prefix ++ buildDir </> suffix
            , map (\d -> "-I" ++ path </> d) <$> filter isRelative <$> arg (IncludeDirs pkgData)
            , map (\d -> "-I" ++          d) <$> filter isAbsolute <$> arg (IncludeDirs pkgData)
            , args "-optP-include" ("-optP" ++ buildDir </> "build/autogen/cabal_macros.h")
            , if usePackageKey 
              then map ("-package-key " ++) <$> arg (DepKeys pkgData)
              else map ("-package "     ++) <$> arg (Deps    pkgData)
            , arg "-no-user-package-db"
            , args "-odir"    (buildDir </> "build")
            , args "-stubdir" (buildDir </> "build")
            , joinArgsSpaced "-dep-makefile" out
            , concatMap (\w -> ["-dep-suffix", suffix w]) <$> ways settings
            , arg "-include-pkg-deps"
            , arg $ map normalise srcs
            ]

-- $1_$2_MKDEPENDHS_FLAGS = -dep-makefile $$($1_$2_depfile_haskell).tmp $$(foreach way,$$($1_$2_WAYS),-dep-suffix "$$(-- patsubst %o,%,$$($$(way)_osuf))")
-- $1_$2_MKDEPENDHS_FLAGS += -include-pkg-deps
