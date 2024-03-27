module GHC.Driver.Errors.Types where

import GHC.Types.Error

data DriverMessage

driverUnknownMessage :: UnknownDiagnostic (DiagnosticOpts DriverMessage) -> DriverMessage
