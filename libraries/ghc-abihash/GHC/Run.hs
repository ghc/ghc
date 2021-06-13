module GHC.Run (runGhcWithAbiHashes, runGhcTWithAbiHashes, initGhcMonadWithAbiHashes) where

import GHC.AbiHashes
import GHC.Utils.Exception
import GHC

-- | The same as 'runGhc', but passes in the ABI hashes of
-- GHC and all its dependencies
-- Use this function when you don't have any additional requirements
-- for linking consistency in your GHC-API program, i.e. if you don't
-- have your own runtime loading/plugin system other than GHCs plugin
-- and TH support.
-- If you do have your own plugin system on top of GHCs, it is probably a good
-- idea to add the ABI hashes of the plugin interface and all its dependencies
-- to 'ghcAbiHashes' and use 'GHC.runGhc' directly
-- See Note [Loader Consistency Checks] for more details
runGhcWithAbiHashes
 :: Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
 -> Ghc a           -- ^ The action to perform.
 -> IO a
runGhcWithAbiHashes = runGhc ghcAbiHashes

-- | The same as 'runGhcT', but passes in the ABI hashes of
-- GHC and all its dependencies
-- Use this function when you don't have any additional requirements
-- for linking consistency in your GHC-API program, i.e. if you don't
-- have your own runtime loading/plugin system other than GHCs plugin
-- and TH support.
-- If you do have your own plugin system on top of GHCs, it is probably a good
-- idea to add the ABI hashes of the plugin interface and all its dependencies
-- to 'ghcAbiHashes' and use 'GHC.runGhc' directly
-- See Note [Loader Consistency Checks] for more details
runGhcTWithAbiHashes :: ExceptionMonad m
        => Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
        -> GhcT m a        -- ^ The action to perform.
        -> m a
runGhcTWithAbiHashes = runGhcT ghcAbiHashes

-- | The same as 'initGhcMonad', but passes in the ABI hashes of
-- GHC and all its dependencies
-- Use this function when you don't have any additional requirements
-- for linking consistency in your GHC-API program, i.e. if you don't
-- have your own runtime loading/plugin system other than GHCs plugin
-- and TH support.
-- If you do have your own plugin system on top of GHCs, it is probably a good
-- idea to add the ABI hashes of the plugin interface and all its dependencies
-- to 'ghcAbiHashes' and use 'GHC.runGhc' directly
-- See Note [Loader Consistency Checks] for more details
initGhcMonadWithAbiHashes :: GhcMonad m => Maybe FilePath -> m ()
initGhcMonadWithAbiHashes = initGhcMonad ghcAbiHashes
