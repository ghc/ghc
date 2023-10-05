module InternalToAbstract where

import Base
import {-# SOURCE #-} Datatypes (getConstructorData)

reify     :: Monad m => QName -> m Definition
reify c = getConstructorData c
