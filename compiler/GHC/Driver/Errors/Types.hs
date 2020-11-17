
module GHC.Driver.Errors.Types (
    GhcError(..)
  , GhcWarning(..)
  , DriverError(..)

  -- * Converting an ErrDoc into a GhcError in a lossy way
  , ghcErrorRawErrDoc
  ) where

import GHC.Driver.Session ( DynFlags )
import GHC.Prelude ( String )
import GHC.Tc.Errors.Types ( DsError, TcRnError(..) )
import GHC.Types.Error ( ErrDoc )
import GHC.Unit.Finder.Types ( FindResult )
import GHC.Unit.Module.Name ( ModuleName )
import GHC.Unit.Types ( UnitId, Module )
import qualified GHC.Driver.CmdLine as CmdLine
import qualified GHC.Parser.Errors as Parser

-- | The umbrella type that encompasses all the different warnings that GHC might raise during the
-- different compilation stages.
data GhcWarning
  = GhcWarningPs  Parser.Warning
    -- ^ A warning raised during the the parsing phase.
  | GhcWarningCmdLine CmdLine.Warn
  | GhcWarningRaw ErrDoc
    -- ^ The escape hatch to convert an 'ErrDoc' into a 'GhcWarning'. Same caveats applies as for a
    -- 'GhcErrorRaw'.

-- | The umbrella type that encompasses all the different errors that GHC might raise during the
-- different compilation stages.
data GhcError
  = GhcErrorPs Parser.Error
    -- ^ An error that happens in the parsing phase.
  | GhcErrorTcRn TcRnError
    -- ^ An error that happens in the typecheck/renaming phase.
  | GhcErrorDs DsError
    -- ^ An error that happens in the desugaring phase.
  | GhcErrorDriver DriverError
    -- ^ An error that happens in the driver.
  | GhcFatalWarning GhcWarning
    -- ^ A 'GhcWarning' that is considered fatal, so it has to be treated as an error. Whether or not a
    -- 'GhcWarning' has to be considered an error is a decision that happens towards the top of the
    -- compilation pipeline, and is driven by the 'DynFlags' settings (e.g. if \"-Werror\" is enabled).
  | GhcErrorRaw ErrDoc
    -- ^ The escape hatch to convert an 'ErrDoc' into a 'GhcError'. Use with care (ideally, don't)
    -- as an 'ErrDoc' is a very lossy representation of an error, which IDEs will have a harder time to
    -- digest.

ghcErrorRawErrDoc :: ErrDoc -> GhcError
ghcErrorRawErrDoc = GhcErrorRaw

data DriverError
  = DriverCannotFindModule DynFlags ModuleName FindResult
  | DriverNotAnExpression String
  | DriverParseErrorImport
  | DriverPkgRequiredTrusted DynFlags UnitId
  | DriverCantLoadIfaceForSafe Module
  | DriverError ErrDoc

