module Rules.Generators.GhcVersionH (generateGhcVersionH) where

import Base
import Expression
import Oracles
import Settings.User

generateGhcVersionH :: Expr String
generateGhcVersionH = do
    when trackBuildSystem . lift $
        need [sourcePath -/- "Rules/Generators/GhcVersionH.hs"]
    version     <- getSetting ProjectVersionInt
    patchLevel1 <- getSetting ProjectPatchLevel1
    patchLevel2 <- getSetting ProjectPatchLevel2
    return . unlines $
        [ "#ifndef __GHCVERSION_H__"
        , "#define __GHCVERSION_H__"
        , ""
        , "#ifndef __GLASGOW_HASKELL__"
        , "# define __GLASGOW_HASKELL__ " ++ version
        , "#endif"
        , ""]
        ++
        [ "#define __GLASGOW_HASKELL_PATCHLEVEL1__ " ++ patchLevel1 | patchLevel1 /= "" ]
        ++
        [ "#define __GLASGOW_HASKELL_PATCHLEVEL2__ " ++ patchLevel2 | patchLevel2 /= "" ]
        ++
        [ ""
        , "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) (\\"
        , "   ((ma)*100+(mi)) <  __GLASGOW_HASKELL__ || \\"
        , "   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \\"
        , "          && (pl1) <  __GLASGOW_HASKELL_PATCHLEVEL1__ || \\"
        , "   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \\"
        , "          && (pl1) == __GLASGOW_HASKELL_PATCHLEVEL1__ \\"
        , "          && (pl2) <= __GLASGOW_HASKELL_PATCHLEVEL2__ )"
        , ""
        , "#endif /* __GHCVERSION_H__ */" ]
