{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Reject where

import Data.Kind (Type)

-- | A Boolean witness whose index determines its constructor.
data SBool (b :: Bool) where
    STrue :: SBool True
    SFalse :: SBool False

-- | Select a lifted result type.
type family Select (b :: Bool) :: Type where
    Select True = Int
    Select False = Bool

-- | Preserve an index without evaluating its witness.
keep :: SBool b -> Select b -> Select b
keep _ x = x

-- | Reject a Boolean value at the integer index.
bad :: Int
bad = keep STrue False
