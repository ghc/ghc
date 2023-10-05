module Settings.Builders.MergeObjects (mergeObjectsBuilderArgs) where

import Settings.Builders.Common
import GHC.Toolchain
import GHC.Toolchain.Program

mergeObjectsBuilderArgs :: Args
mergeObjectsBuilderArgs = builder MergeObjects ? mconcat
    [ maybe [] (prgFlags . mergeObjsProgram) . tgtMergeObjs <$> getStagedTarget
    , arg "-o", arg =<< getOutput
    , getInputs ]
