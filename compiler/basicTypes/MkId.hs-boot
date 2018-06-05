module MkId where
import Name( Name )
import Var( Id )
import Class( Class )
import {-# SOURCE #-} DataCon( DataCon, DataConRep )
import {-# SOURCE #-} PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDataConRepSimple :: Name -> DataCon -> DataConRep
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
