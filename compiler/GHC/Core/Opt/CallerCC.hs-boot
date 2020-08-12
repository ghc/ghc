module GHC.Core.Opt.CallerCC where

import GHC.Prelude

-- Necessary due to import in GHC.Driver.Session.
data CallerCcFilter

parseCallerCcFilter :: String -> Either String CallerCcFilter
