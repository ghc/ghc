module MkId where
import Name( Name )
import Var( Id )
import Class( Class )
import Platform( Platform )
import {-# SOURCE #-} DataCon( DataCon )
import {-# SOURCE #-} PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: Platform -> PrimOp -> Id

magicDictId :: Id
