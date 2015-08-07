module Rules.Compile (compilePackage) where

import Way
import Base
import Util
import Builder
import Expression
import qualified Target
import Oracles.DependencyList
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources

compilePackage :: Resources -> StagePackageTarget -> Rules ()
compilePackage _ target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"
        cDepsFile = buildPath -/- "c.deps"
        hDepsFile = buildPath -/- "haskell.deps"

    matchBuildResult buildPath "hi" ?> \hi ->
        need [ hi -<.> osuf (detectWay hi) ]

    matchBuildResult buildPath "hi-boot" ?> \hiboot ->
        need [ hiboot -<.> obootsuf (detectWay hiboot) ]

    matchBuildResult buildPath "o" ?> \obj -> do
        let way  = detectWay obj
            cObj = takeFileName obj -<.> "o"
        cDeps <- dependencyList cDepsFile cObj
        hDeps <- dependencyList hDepsFile obj
        let hSrcDeps = filter ("//*hs" ?==) hDeps

        when (null cDeps && null hDeps) $
            putError $ "Cannot determine sources for '" ++ obj ++ "'."

        when (not (null cDeps) && not (null hDeps)) $
            putError $ "Both .c and .hs sources found for '" ++ obj ++ "'."

        need $ hDeps ++ cDeps

        if null cDeps
        then build $ fullTargetWithWay target hSrcDeps (Ghc stage) way [obj]
        else build $ fullTarget        target cDeps    (Gcc stage)     [obj]

    matchBuildResult buildPath "o-boot" ?> \obj -> do
        let way = detectWay obj
        hDeps <- dependencyList hDepsFile obj
        let hSrcDeps = filter ("//*hs-boot" ?==) hDeps

        when (null hDeps) $
            putError $ "Cannot determine sources for '" ++ obj ++ "'."

        need hDeps
        build $ fullTargetWithWay target hSrcDeps (Ghc stage) way [obj]

-- TODO: add support for -dyno
-- $1/$2/build/%.$$($3_o-bootsuf) : $1/$4/%.hs-boot
--     $$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@
--     $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno
--     $$(addsuffix .$$(dyn_osuf)-boot,$$(basename $$@)))
