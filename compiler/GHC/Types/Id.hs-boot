module GHC.Types.Id where

import GHC.Prelude ( Maybe )
import {-# SOURCE #-} GHC.Core.DataCon ( DataCon )
import {-# SOURCE #-} GHC.Core.TyCon ( TyCon )
import {-# SOURCE #-} GHC.Types.Name
import {-# SOURCE #-} GHC.Types.Var

idName   :: Id -> Name
sourceTableId_maybe :: Id -> Maybe TyCon
isDataConWorkId_maybe :: Id -> Maybe DataCon
isDataConWrapId_maybe :: Id -> Maybe DataCon
