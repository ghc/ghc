module DynamicLoading where

import HscTypes(HscEnv)
import Name(Name)
import TyCon(TyCon)
import Type(Type)
import Module(ModuleName)
import RdrName(RdrName)

forceLoadTyCon :: HscEnv -> Name -> IO TyCon
getValueSafely :: HscEnv -> Name -> Type -> IO (Maybe a)
lookupRdrNameInModuleForPlugins :: HscEnv -> ModuleName -> RdrName -> IO (Maybe Name)
