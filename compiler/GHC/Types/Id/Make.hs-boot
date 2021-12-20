module GHC.Types.Id.Make where
import GHC.Types.Name( Name )
import GHC.Types.Var( Id )
import GHC.Core.Class( Class )
import {-# SOURCE #-} GHC.Core.DataCon( DataCon )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id
