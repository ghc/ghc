{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {DriverMessage, GhcMessage}

module GHC.Driver.Errors.Ppr where

import {-# SOURCE #-} GHC.Driver.Errors.Types (DriverMessage)

import GHC.Types.Error

instance Diagnostic DriverMessage
