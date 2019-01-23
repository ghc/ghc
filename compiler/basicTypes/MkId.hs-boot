module MkId where
import Name( Name )
import Var( Id )
import Class( Class )
import {-# SOURCE #-} DataCon( DataCon )
import {-# SOURCE #-} PrimOp( PrimOp )
import {-# SOURCE #-} TyCoRep (Type)

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id

varTId, conTId, appTId, arrowTId, mkNameG_tcId, unpackCStringId, mkNameLId, forallTId,
  kindedTVId, starKId, cxtId, mkNameG_dId, promotedTId, numTyLitId, strTyLitId,
  litTId :: Id


tyVarBndrQ, typeQ, integerTy :: Type

