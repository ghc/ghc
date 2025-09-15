module Pretty where

import Control.Monad

import InternalToAbstract
import Base

prettyTCM :: Monad m => QName -> m Definition
prettyTCM x = reify x


