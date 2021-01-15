module GHC.Types.Id.Make where
import GHC.Types.Name( Name )
import GHC.Types.Var( Id )
import GHC.Core.Class( Class )
import {-# SOURCE #-} GHC.Core.DataCon( DataCon )
import {-# SOURCE #-} GHC.Builtin.PrimOps( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id
voidPrimId      :: Id

magicDictId :: Id
