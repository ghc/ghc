module MkId where
import Name( Name )
import Var( Id )
import Class( Class )
import {-# SOURCE #-} DataCon( DataCon )
import {-# SOURCE #-} PrimOp( PrimOp )
import {-# SOURCE #-} PrimOp.Cache( PrimOpCache )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOpCache -> PrimOp -> Id

magicDictId :: Id
