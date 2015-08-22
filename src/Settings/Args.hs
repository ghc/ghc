module Settings.Args (args, getArgs, arPersistentArgsCount) where

import Expression
import Settings
import Settings.Builders.Ar
import Settings.Builders.Ld
import Settings.Builders.Ghc
import Settings.Builders.Gcc
import Settings.Builders.GhcPkg
import Settings.Builders.Haddock
import Settings.Builders.GhcCabal

args :: Args
args = defaultArgs <> userArgs

getArgs :: Expr [String]
getArgs = fromDiffExpr args

-- TODO: add all other settings
-- TODO: add src-hc-args = -H32m -O
-- TODO: GhcStage2HcOpts=-O2 unless GhcUnregisterised
-- TODO: compiler/stage1/build/Parser_HC_OPTS += -O0 -fno-ignore-interface-pragmas
-- TODO: compiler/main/GhcMake_HC_OPTS        += -auto-all
-- TODO: compiler_stage2_HADDOCK_OPTS += --optghc=-DSTAGE=2
-- TODO: compiler/prelude/PrimOp_HC_OPTS  += -fforce-recomp
-- TODO: is GhcHcOpts=-Rghc-timing needed?
defaultArgs :: Args
defaultArgs = mconcat
    [ cabalArgs
    , ghcPkgArgs
    , ghcMArgs
    , gccMArgs
    , ghcArgs
    , gccArgs
    , arArgs
    , ldArgs
    , ghcCabalHsColourArgs
    , haddockArgs
    , customPackageArgs ]
