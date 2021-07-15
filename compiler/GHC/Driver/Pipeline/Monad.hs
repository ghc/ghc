{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The 'TPipelineClass' and 'MonadUse' classes and associated types
module GHC.Driver.Pipeline.Monad (
  TPipelineClass, MonadUse(..)

  , PipeEnv(..)
  , PipelineOutput(..)
  , getLocation
  ) where

import GHC.Prelude
import Control.Monad.IO.Class
import qualified Data.Kind as K
import GHC.Driver.Phases
import GHC.Utils.TmpFs
import GHC.Driver.Session
import GHC.Types.SourceFile
import GHC.Unit.Module
import GHC.Unit.Finder
import Control.Monad.Catch

-- The interface that the pipeline monad must implement.
type TPipelineClass (f :: K.Type -> K.Type) (m :: K.Type -> K.Type)
  = (Functor m, MonadIO m, Applicative m, Monad m, MonadUse f m)

-- | Lift a `f` action into an `m` action.
class MonadUse f m where
  use :: f a -> m a

-- PipeEnv: invariant information passed down through the pipeline
data PipeEnv = PipeEnv {
       stop_phase   :: StopPhase,   -- ^ Stop just after this phase
       src_filename :: String,      -- ^ basename of original input source
       src_basename :: String,      -- ^ basename of original input source
       src_suffix   :: String,      -- ^ its extension
       output_spec  :: PipelineOutput -- ^ says where to put the pipeline output
  }

-- | Calculate the ModLocation from the provided DynFlags
getLocation :: PipeEnv -> DynFlags -> HscSource -> ModuleName -> IO ModLocation
getLocation pipe_env dflags src_flavour mod_name = do
    let PipeEnv{ src_basename=basename,
             src_suffix=suff } = pipe_env
    location1 <- mkHomeModLocation2 dflags mod_name basename suff

    -- Boot-ify it if necessary
    let location2
          | HsBootFile <- src_flavour = addBootSuffixLocnOut location1
          | otherwise                 = location1


    -- Take -ohi into account if present
    -- This can't be done in mkHomeModuleLocation because
    -- it only applies to the module being compiles
    let ohi = outputHi dflags
        location3 | Just fn <- ohi = location2{ ml_hi_file = fn }
                  | otherwise      = location2

    -- Take -o into account if present
    -- Very like -ohi, but we must *only* do this if we aren't linking
    -- (If we're linking then the -o applies to the linked thing, not to
    -- the object file for one module.)
    -- Note the nasty duplication with the same computation in compileFile
    -- above
    let expl_o_file = outputFile dflags
        location4 | Just ofile <- expl_o_file
                  , isNoLink (ghcLink dflags)
                  = location3 { ml_obj_file = ofile }
                  | otherwise = location3
    return location4

data PipelineOutput
  = Temporary TempFileLifetime
        -- ^ Output should be to a temporary file: we're going to
        -- run more compilation steps on this output later.
  | Persistent
        -- ^ We want a persistent file, i.e. a file in the current directory
        -- derived from the input filename, but with the appropriate extension.
        -- eg. in "ghc -c Foo.hs" the output goes into ./Foo.o.
  | SpecificFile
        -- ^ The output must go into the specific outputFile in DynFlags.
        -- We don't store the filename in the constructor as it changes
        -- when doing -dynamic-too.
  | NoOutputFile
        -- ^ No output should be created, like in Interpreter or NoBackend.
    deriving Show
