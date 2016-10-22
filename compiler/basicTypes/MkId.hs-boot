module MkId where
import Name( Name )
import Var( Id )
import Class( Class )
import {-# SOURCE #-} DataCon( DataCon, DataConRep )
import {-# SOURCE #-} PrimOp( PrimOp )

data DataConBoxer

mkSimpleDataConRep :: Name -> DataCon -> DataConRep

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
