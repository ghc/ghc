
-- | Computations in the vectorisation monad concerned with naming
--   and fresh variable generation.
module Vectorise.Monad.Naming
	( cloneName
	, cloneId
	, cloneVar
	, newExportedVar
	, newLocalVar
	, newLocalVars
	, newDummyVar
	, newTyVar)
where
import Vectorise.Monad.Base

import DsMonad
import Type
import Var
import Name
import SrcLoc
import Id
import FastString
import Control.Monad


-- Naming ---------------------------------------------------------------------	
-- | Clone a name, using the provide function to transform its `OccName`.	
cloneName :: (OccName -> OccName) -> Name -> VM Name
cloneName mk_occ name = liftM make (liftDs newUnique)
  where
    occ_name = mk_occ (nameOccName name)

    make u | isExternalName name = mkExternalName u (nameModule name)
                                                    occ_name
                                                    (nameSrcSpan name)
           | otherwise           = mkSystemName u occ_name


-- | Clone an `Id`, using the provided function to transform its `OccName`. 
cloneId :: (OccName -> OccName) -> Id -> Type -> VM Id
cloneId mk_occ id ty
  = do
      name <- cloneName mk_occ (getName id)
      let id' | isExportedId id = Id.mkExportedLocalId name ty
              | otherwise       = Id.mkLocalId         name ty
      return id'


-- | Make a fresh instance of this var, with a new unique.
cloneVar :: Var -> VM Var
cloneVar var = liftM (setIdUnique var) (liftDs newUnique)


-- | Make a fresh exported variable with the given type.
newExportedVar :: OccName -> Type -> VM Var
newExportedVar occ_name ty 
 = do mod <- liftDs getModuleDs
      u   <- liftDs newUnique

      let name = mkExternalName u mod occ_name noSrcSpan
      
      return $ Id.mkExportedLocalId name ty


-- | Make a fresh local variable with the given type.
--   The variable's name is formed using the given string as the prefix.
newLocalVar :: FastString -> Type -> VM Var
newLocalVar fs ty
 = do u <- liftDs newUnique
      return $ mkSysLocal fs u ty


-- | Make several fresh local varaiables with the given types.
--   The variable's names are formed using the given string as the prefix.
newLocalVars :: FastString -> [Type] -> VM [Var]
newLocalVars fs = mapM (newLocalVar fs)


-- | Make a new local dummy variable.
newDummyVar :: Type -> VM Var
newDummyVar = newLocalVar (fsLit "vv")


-- | Make a fresh type variable with the given kind.
--   The variable's name is formed using the given string as the prefix.
newTyVar :: FastString -> Kind -> VM Var
newTyVar fs k
 = do u <- liftDs newUnique
      return $ mkTyVar (mkSysTvName u fs) k

