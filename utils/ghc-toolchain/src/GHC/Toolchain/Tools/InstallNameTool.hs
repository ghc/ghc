{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Toolchain.Tools.InstallNameTool where

import Control.Monad

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

newtype InstallNameTool = InstallNameTool { installNameToolProgram :: Program
                                          }
    deriving (Show, Read, Eq, Ord)

findInstallNameTool :: ProgOpt -> M InstallNameTool
findInstallNameTool progOpt = checking "for install_name_tool" $ do
    installNameToolProgram <- findProgram "install_name_tool utility" progOpt ["install_name_tool"]
    return InstallNameTool {..}
