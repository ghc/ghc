{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T15850 where

import Data.Coerce (coerce)
import T15850_Lib (LibIdentity)

newtype LocalIdentity a = LocalIdentity a


difficultError :: LibIdentity (LocalIdentity ()) -> LibIdentity ()
difficultError = coerce
