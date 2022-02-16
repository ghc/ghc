{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module GHC.Driver.Pipeline where


import {-# SOURCE #-} GHC.Driver.Pipeline.Phases ( TPhase )
import GHC.Driver.Pipeline.Monad ( TPipelineClass, PipeEnv )
import {-# SOURCE #-} GHC.Driver.Env.Types ( HscEnv )
import GHC.ForeignSrcLang ( ForeignSrcLang )
import GHC.Prelude (Maybe, Bool, FilePath, IO)
import GHC.Unit.Module.Location ( ModLocation )
import GHC.Unit.Module.Name ( ModuleName )
import GHC.Driver.Phases (Phase)
import {-# SOURCE #-} GHC.Driver.Session ( DynFlags )

-- These are used in GHC.Driver.Pipeline.Execute, but defined in terms of runPipeline
compileForeign :: HscEnv -> ForeignSrcLang -> FilePath -> IO FilePath
compileEmptyStub :: DynFlags -> HscEnv -> FilePath -> ModLocation -> ModuleName -> IO ()

asPipeline :: TPipelineClass TPhase m
           => Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
llvmPipeline :: TPipelineClass TPhase m
           => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
viaCPipeline :: TPipelineClass TPhase m
           => Phase -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
