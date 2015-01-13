{-# LANGUAGE NoImplicitPrelude #-}
module Package.Library (buildPackageLibrary) where

import Package.Base

{- "/usr/bin/ar" q  
libraries/deepseq/dist-install/build/libHSdeeps_FT5iVCELxOr62eHY0nbvnU.a
@libraries/deepseq/dist-install/build/libHSdeeps_FT5iVCELxOr62eHY0nbvnU.a.contents
-}

--  "$$(XARGS)" $$(XARGS_OPTS) "$$($1_$2_AR)" $$($1_$2_AR_OPTS) $$($1_$2_EXTRA_AR_ARGS) $$@ < $$@.contents
-- AR_OPTS            = $(SRC_AR_OPTS) $(WAY$(_way)_AR_OPTS) $(EXTRA_AR_OPTS)

buildPackageLibrary :: Package -> TodoItem -> Rules ()
buildPackageLibrary (Package _ path _) (stage, dist, _) =
    let buildDir = path </> dist </> "build"
        pkgData  = path </> dist </> "package-data.mk"
    in
    (buildDir <//> "*a") %> \out -> do
        let way = detectWay $ tail $ takeExtension out
        need ["shake/src/Package/Library.hs"]
        depObjs <- pkgDepObjects path dist way
        need depObjs
        libObjs <- pkgLibObjects path dist stage way
        terseRun Ar $ arArgs <+> out <+> libObjs
