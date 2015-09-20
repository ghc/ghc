module Rules.Generate (generatePackageCode) where

import Expression
import Oracles
import Rules.Actions
import Rules.Resources
import Settings

-- The following generators and corresponding source extensions are supported:
knownGenerators :: [ (Builder, String) ]
knownGenerators =  [ (Alex   , ".x"  )
                   , (Happy  , ".y"  )
                   , (Happy  , ".ly" )
                   , (Hsc2Hs , ".hsc") ]

determineBuilder :: FilePath -> Maybe Builder
determineBuilder file = fmap fst $ find (\(_, e) -> e == ext) knownGenerators
  where
    ext = takeExtension file

generatePackageCode :: Resources -> PartialTarget -> Rules ()
generatePackageCode _ target @ (PartialTarget stage package) =
    let path        = targetPath stage package
        packagePath = pkgPath package
        buildPath   = path -/- "build"
    in do
        buildPath </> "*.hs" %> \file -> do
            dirs  <- interpretPartial target $ getPkgDataList SrcDirs
            files <- getDirectoryFiles "" $
                [ packagePath </> d </> takeBaseName file <.> "*" | d <- dirs ]
            let gens = [ (f, b) | f <- files, Just b <- [determineBuilder f] ]
                (src, builder) = head gens
            when (length gens /= 1) . putError $
                "Exactly one generator expected for " ++ file
                ++ "(found: " ++ show gens ++ ")."
            need [src]
            build $ fullTarget target builder [src] [file]

-- $1/$2/build/%.hs : $1/$3/%.ly | $$$$(dir $$$$@)/.
--     $$(call cmd,HAPPY) $$($1_$2_ALL_HAPPY_OPTS) $$< -o $$@

-- $1/$2/build/%.hs : $1/$3/%.y | $$$$(dir $$$$@)/.
--     $$(call cmd,HAPPY) $$($1_$2_ALL_HAPPY_OPTS) $$< -o $$@

-- $1/$2/build/%_hsc.c $1/$2/build/%_hsc.h $1/$2/build/%.hs : $1/$3/%.hsc $$$$(hsc2hs_INPLACE) | $$$$(dir $$$$@)/.
--     $$(call cmd,hsc2hs_INPLACE) $$($1_$2_ALL_HSC2HS_OPTS) $$< -o $$@

-- # Now the rules for hs-boot files.

-- $1/$2/build/%.hs-boot : $1/$3/%.hs-boot
--     "$$(CP)" $$< $$@

-- $1/$2/build/%.lhs-boot : $1/$3/%.lhs-boot
--     "$$(CP)" $$< $$@

