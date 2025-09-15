{-# LANGUAGE QuantifiedConstraints #-}
module T26020a where

import T26020a_help( N ) -- Not the MkN constructor
import Data.Coerce

coT :: (forall x. Coercible (N i) (N o))
    => N i -> N o
coT = coerce
