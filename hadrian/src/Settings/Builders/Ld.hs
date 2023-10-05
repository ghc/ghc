module Settings.Builders.Ld (ldBuilderArgs) where

import Settings.Builders.Common
import GHC.Toolchain
import GHC.Toolchain.Program

ldBuilderArgs :: Args
ldBuilderArgs = builder Ld ? mconcat [ prgFlags . ccLinkProgram . tgtCCompilerLink <$> getStagedTarget
                                     , arg "-o", arg =<< getOutput
                                     , getInputs ]
