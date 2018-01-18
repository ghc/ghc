{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}

module ValidSubsInteractions where

import Data.Kind


data SBool :: Bool -> Type where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

f :: SBool 'True
f = _
