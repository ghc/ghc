module Rules.Program (buildProgram) where

import Expression hiding (splitPath)
import Oracles
import Rules.Actions
import Rules.Library
import Rules.Resources
import Settings

buildProgram :: Resources -> PartialTarget -> Rules ()
buildProgram _ target @ (PartialTarget stage pkg) = do
    let path      = targetPath stage pkg
        buildPath = path -/- "build"
        program   = programPath stage pkg

    (\f -> program == Just f) ?> \bin -> do
        cSrcs <- cSources target -- TODO: remove code duplication (Library.hs)
        hSrcs <- hSources target
        let cObjs = [ buildPath -/- src -<.> osuf vanilla | src <- cSrcs ]
            hObjs = [ buildPath -/- src  <.> osuf vanilla | src <- hSrcs ]
            objs  = cObjs ++ hObjs
        need objs
        build $ fullTargetWithWay target (Ghc stage) vanilla objs [bin]
        synopsis <- interpretPartial target $ getPkgData Synopsis
        putSuccess $ "/--------\n| Successfully built program '"
            ++ pkgName pkg ++ "' (stage " ++ show stage ++ ")."
        putSuccess $ "| Executable: " ++ bin
        putSuccess $ "| Package synopsis: "
            ++ dropWhileEnd isPunctuation synopsis ++ "." ++ "\n\\--------"
