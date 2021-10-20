module T20200AgdaInternalToAbstract where

import T20200AgdaBase
import {-# SOURCE #-} T20200AgdaDatatypes (getConstructorData)

class Reify i  where
    reify     :: HasConstInfo m => i -> m Definition
    reifyWhen :: HasConstInfo m => i -> m Definition

instance Reify QName where
    reifyWhen = undefined
    reify c = do _ <- getConstructorData c
                 return undefined
