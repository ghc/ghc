module Rules.Register (registerPackage) where

import Base
import Context
import Expression
import GHC
import Rules.Actions
import Rules.Libffi
import Settings
import Settings.Packages.Rts
import Target

-- Build package-data.mk by using GhcCabal to process pkgCabal file
registerPackage :: [(Resource, Int)] -> Context -> Rules ()
registerPackage rs context @ (Context {..}) = do
    let oldPath = pkgPath package -/- contextDirectory context -- TODO: remove, #113
        pkgConf = packageDbDirectory stage -/- pkgNameString package

    when (stage <= Stage1) $ matchVersionedFilePath pkgConf "conf" ?> \conf -> do
        -- This produces inplace-pkg-config. TODO: Add explicit tracking
        need [pkgDataFile context]

        -- Post-process inplace-pkg-config. TODO: remove, see #113, #148
        let pkgConfig  = oldPath -/- "inplace-pkg-config"
            fixPkgConf = unlines
                       . map (replace oldPath (contextPath context)
                       . replace (replaceSeparators '\\' $ oldPath)
                                 (contextPath context) )
                       . lines

        fixFile pkgConfig fixPkgConf

        buildWithResources rs $
            Target context (GhcPkg stage) [pkgConfig] [conf]

    when (package == rts && stage == Stage1) $ do
        packageDbDirectory Stage1 -/- "rts.conf" %> \conf -> do
            need [rtsConf]
            buildWithResources rs $
                Target context (GhcPkg stage) [rtsConf] [conf]

        rtsConf %> \_ -> do
            need [ pkgDataFile rtsContext, rtsConfIn ]
            build $ Target context HsCpp [rtsConfIn] [rtsConf]

            let fixRtsConf = unlines
                           . map
                           ( replace "\"\"" ""
                           . replace "rts/dist/build" rtsBuildPath )
                           . filter (not . null)
                           . lines

            fixFile rtsConf fixRtsConf
