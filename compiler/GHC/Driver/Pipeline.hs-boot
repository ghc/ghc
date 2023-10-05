module GHC.Driver.Pipeline where


import GHC.Driver.Env.Types ( HscEnv )
import GHC.ForeignSrcLang ( ForeignSrcLang )
import GHC.Prelude (FilePath, IO)
import GHC.Unit.Module.Location (ModLocation)
import GHC.Driver.Session (DynFlags)

import Language.Haskell.Syntax.Module.Name

-- These are used in GHC.Driver.Pipeline.Execute, but defined in terms of runPipeline
compileForeign :: HscEnv -> ForeignSrcLang -> FilePath -> IO FilePath
compileEmptyStub :: DynFlags -> HscEnv -> FilePath -> ModLocation -> ModuleName -> IO ()
