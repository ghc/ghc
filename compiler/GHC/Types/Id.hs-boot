module GHC.Types.Id where

import GHC.Prelude ( Maybe )
import {-# SOURCE #-} GHC.Core.DataCon ( DataCon )
import {-# SOURCE #-} GHC.Types.Name
import {-# SOURCE #-} GHC.Types.Var

idName   :: Id -> Name
isDataConWorkId_maybe :: Id -> Maybe DataCon
