module Rules.Register (registerPackage) where

import Data.Char

import Base
import Expression
import GHC
import Rules.Actions
import Rules.Resources
import Settings

-- matchPkgConf :: FilePath -> Bool
-- matchPkgConf file =

-- Build package-data.mk by using GhcCabal to process pkgCabal file
registerPackage :: Resources -> PartialTarget -> Rules ()
registerPackage rs target @ (PartialTarget stage pkg) = do
    let oldPath = pkgPath pkg -/- targetDirectory stage pkg -- TODO: remove, #113
        pkgConf = packageDbDirectory stage -/- pkgNameString pkg
        match f = case stripPrefix (pkgConf ++ "-") f of
            Nothing  -> False
            Just suf -> dropWhile (\c -> isDigit c || c == '.') suf == "conf"

    when (stage <= Stage1) $ match ?> \_ -> do
        -- This produces pkgConfig. TODO: Add explicit tracking
        need [pkgDataFile stage pkg]

        -- Post-process inplace-pkg-config. TODO: remove, see #113, #148
        let pkgConfig  = oldPath -/- "inplace-pkg-config"
            fixPkgConf = unlines
                       . map (replace oldPath (targetPath stage pkg)
                       . replace (replaceSeparators '\\' $ oldPath)
                                 (targetPath stage pkg) )
                       . lines

        fixFile pkgConfig fixPkgConf

        buildWithResources [(resGhcPkg rs, 1)] $
            fullTarget target (GhcPkg stage) [pkgConfig] []
