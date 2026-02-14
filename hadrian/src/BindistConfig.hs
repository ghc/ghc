module BindistConfig where

import Expression
import Oracles.Flag

data BindistConfig = BindistConfig
  { -- | The stage compiler which builds the libraries
    library_stage :: Stage,
    -- | The stage compiler which builds the executables
    executable_stage :: Stage,
    -- | Which ghc-pkg to use (`targetBindist`'s
    -- ghc-pkg cannot run on the build platform)
    use_inplace_ghcPkg :: Bool
  }

-- | A bindist for when the host = target, non cross-compilation setting.
-- Both the libraries and final executables are built with stage1 compiler.
normalBindist :: BindistConfig
normalBindist = BindistConfig {library_stage = Stage1, executable_stage = Stage1, use_inplace_ghcPkg = True}

-- | A bindist which contains a cross compiler (when host /= target)
-- The cross compiler is produced by the stage1 compiler, but then we must compile
-- all the boot libraries with the cross compiler (hence stage2 for libraries)
crossBindist :: BindistConfig
crossBindist = BindistConfig {library_stage = Stage2, executable_stage = Stage1, use_inplace_ghcPkg = True}

-- | A bindist which contains executables for the target, which produce code for the
-- target. These are produced as "Stage3" build products, produced by a stage2 cross compiler.
targetBindist :: BindistConfig
targetBindist = BindistConfig {library_stage = Stage2, executable_stage = Stage2, use_inplace_ghcPkg = False}

-- | The implicit bindist config, if we don't know any better.
implicitBindistConfig :: Action BindistConfig
implicitBindistConfig = do
  -- A "normal" bindist doesn't make sense when cross compiled because there would be
  -- libraries built for the host, but the distributed compiler would produce files for
  -- the target.
  cross <- flag CrossCompiling
  return $ if cross then crossBindist else normalBindist

-- | Are we building things in this stage for the final target?
buildingForTarget :: Stage -> Action Bool
buildingForTarget st = do
  cfg <- implicitBindistConfig
  return $ st >= (library_stage cfg)
