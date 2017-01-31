{-# LANGUAGE TypeInType #-}
module Example where

import Data.Typeable
import GHC.Exts

data Wat (a :: TYPEvis ('TupleRep '[])) = Wat a
