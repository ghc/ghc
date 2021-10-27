module Datatypes where

import Base
import Pretty


getConstructorData :: Monad m => QName -> m Definition
getConstructorData = getConstInfo

getConType :: QName -> IO a
getConType t = do
  _ <- prettyTCM t
  return udef
