{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The 'TPipelineClass' and 'MonadUse' classes and associated types
module GHC.Driver.Pipeline.Monad (
  TPipelineClass, MonadUse(..)

  , PipeEnv(..)
  , PipelineOutput(..)
  ) where

import GHC.Prelude
import Control.Monad.IO.Class
import qualified Data.Kind as K
import GHC.Driver.Phases
import GHC.Utils.TmpFs

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
