module Rules.Program (buildProgram) where

import Expression hiding (splitPath)
import GHC (hsc2hs, haddock)
import Oracles
import Rules.Actions
import Rules.Library
import Rules.Resources
import Settings
import Settings.Builders.GhcCabal

-- TODO: Get rid of the Paths_hsc2hs.o hack.
-- TODO: Do we need to consider other ways when building programs?
buildProgram :: Resources -> PartialTarget -> Rules ()
buildProgram _ target @ (PartialTarget stage pkg) = do
    let path      = targetPath stage pkg
        buildPath = path -/- "build"
        program   = programPath stage pkg

            -- return $  [ ghciLib | needGhciLib == "YES" && stage == Stage1 ]
            --        ++ [ haddock | needHaddock          && stage == Stage1 ]
            --        ++ libs

    (\f -> program == Just f) ?> \bin -> do
        cSrcs <- cSources target -- TODO: remove code duplication (Library.hs)
        hSrcs <- hSources target
        let cObjs = [ buildPath -/- src -<.> osuf vanilla | src <- cSrcs   ]
            hObjs = [ buildPath -/- src  <.> osuf vanilla | src <- hSrcs   ]
                 ++ [ buildPath -/- "Paths_hsc2hs.o"      | pkg == hsc2hs  ]
                 ++ [ buildPath -/- "Paths_haddock.o"     | pkg == haddock ]
            objs  = cObjs ++ hObjs
        pkgs     <- interpretPartial target getPackages
        ways     <- interpretPartial target getWays
        depNames <- interpretPartial target $ getPkgDataList DepNames
        ghciFlag <- interpretPartial target $ getPkgData BuildGhciLib
        let deps = matchPackageNames (sort pkgs) (sort depNames)
            ghci = ghciFlag == "YES" && stage == Stage1
        libs <- fmap concat . forM deps $ \dep -> do
            let depTarget = PartialTarget stage dep
            compId <- interpretPartial depTarget $ getPkgData ComponentId
            libFiles <- fmap concat . forM ways $ \way -> do
                libFile  <- pkgLibraryFile stage dep compId           way
                lib0File <- pkgLibraryFile stage dep (compId ++ "-0") way
                dll0     <- needDll0 stage dep
                return $ [ libFile ] ++ [ lib0File | dll0 ]
            return $ libFiles ++ [ pkgGhciLibraryFile stage dep compId | ghci ]
        need $ objs ++ libs
        build $ fullTargetWithWay target (Ghc stage) vanilla objs [bin]
        synopsis <- interpretPartial target $ getPkgData Synopsis
        putSuccess $ "/--------\n| Successfully built program '"
            ++ pkgName pkg ++ "' (stage " ++ show stage ++ ")."
        putSuccess $ "| Executable: " ++ bin
        putSuccess $ "| Package synopsis: "
            ++ dropWhileEnd isPunctuation synopsis ++ "." ++ "\n\\--------"
