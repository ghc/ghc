{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module FamilyArrowMatchability where

import Data.Kind (Type)
import GHC.Exts (FUN, Multiplicity (Many), U)

type family Expected :: Type where
    Expected = FUN @U 'Many Type Type

type Bad = (Maybe :: Expected)
