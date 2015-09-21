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
    in do -- TODO: do we need to copy *.(l)hs-boot files here? Never happens?
        buildPath </> "*.hs" %> \file -> do
            dirs  <- interpretPartial target $ getPkgDataList SrcDirs
            files <- getDirectoryFiles "" $
                [ packagePath </> d </> takeBaseName file <.> "*" | d <- dirs ]
            let gens = [ (f, b) | f <- files, Just b <- [determineBuilder f] ]
            when (length gens /= 1) . putError $
                "Exactly one generator expected for " ++ file
                ++ "(found: " ++ show gens ++ ")."
            let (src, builder) = head gens
            need [src]
            build $ fullTarget target builder [src] [file]
