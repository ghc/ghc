module GHC.Driver.Make where

import qualified Data.Map as M
import Data.Time.Clock
import GHC.Data.StringBuffer
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Prelude
import GHC.Types.PkgQual
import GHC.Types.SrcLoc
import GHC.Unit.Home
import GHC.Unit.Module.ModSummary
import GHC.Unit.Types
import Language.Haskell.Syntax

data SummariseResult =
        FoundInstantiation InstantiatedUnit
      | FoundHomeWithError (UnitId, DriverMessages)
      | FoundHome ModSummary
      | External UnitId
      | NotThere

summariseModule
          :: HscEnv
          -> HomeUnit
          -> M.Map (UnitId, FilePath) ModSummary
          -- ^ Map of old summaries
          -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
          -> Located ModuleName -- Imported module to be summarised
          -> PkgQual
          -> Maybe (StringBuffer, UTCTime)
          -> [ModuleName]               -- Modules to exclude
          -> IO SummariseResult
