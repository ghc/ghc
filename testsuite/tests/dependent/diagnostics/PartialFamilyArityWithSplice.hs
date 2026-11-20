{-# LANGUAGE DataKinds, Modifiers, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE NoExperimentalDependentTypes, NoLiberalTypeSynonyms #-}
module PartialFamilyArityWithSplice where

import Data.Kind (Type)
import GHC.Matchability (U)
import Language.Haskell.TH qualified as TH

type family Select (b :: Bool) (a :: Type) :: Type where
    Select 'True a = Maybe a
    Select 'False a = [a]

data Accept (f :: Type %U -> Type)

-- The first field fails even though only the second field writes Select.
checked :: ( Accept ($(pure (TH.ConT ''Select)) 'False)
           , Accept (Select 'True) )
checked = undefined
